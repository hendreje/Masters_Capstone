rm(list = ls())

library(car)
library(plm)
library(stargazer)
library(lmtest)

data <- read.csv("Capstone.csv", header = T)
unique(data$Year)
data$Income <- log(data$Income)
data.panel <- pdata.frame(data, index = c("State", "Year"))
pdim(data.panel)

summary(data)
SummaryStats <- (data[3:8])

stargazer(SummaryStats, type = 'text', out = "Capstone Summary Statistics.htm", title = 'Summary Statistics', align = T, covariate.labels = c("Alcohol per Person (Gallons)", "Household Income (Percent)", "Smoking (Percent)", "High School Education (Percent)", "Black Population (Percent)", "Female Population (Percent)"), omit.summary.stat = c("p25", "p75"))

#General Model

GM <- plm(Income ~ smokingRate + alcoholConsumptionGallons + PercentHighSchoolOrHigher + Black + Female + Black*Female + factor(Year), data=data.panel)
summary(GM)

##Question 2
FE2 <- plm(Income ~ smokingRate + alcoholConsumptionGallons + PercentHighSchoolOrHigher + Black + Female + Black*Female + factor(Year), data=data.panel, model = "within")
summary(FE2)

data$msmokingRate <- (Within(data.panel$smokingRate) - data.panel$smokingRate)*(-1)
data$malcoholConsumptionGallons <- (Within(data.panel$alcoholConsumptionGallons) - data.panel$alcoholConsumptionGallons)*(-1)
data$mPercentHighSchoolOrHigher <- (Within(data.panel$PercentHighSchoolOrHigher) - data.panel$PercentHighSchoolOrHigher)*(-1)
data$mBlack <- (Within(data.panel$Black) - data.panel$Black)*(-1)
data$mFemale <- (Within(data.panel$Female) - data.panel$Female)*(-1)
data$mBlackFemale <- (Within(data.panel$Black*data.panel$Female) - data.panel$Black*data.panel$Female)*(-1)

CRE2 <-plm(Income ~ msmokingRate + smokingRate + malcoholConsumptionGallons + alcoholConsumptionGallons + mPercentHighSchoolOrHigher + PercentHighSchoolOrHigher + Black + mBlack + Female + mFemale + Black*Female + mBlackFemale + factor(Year), data=data, index=c("State", "Year"), model="random")

stargazer(FE2,CRE2, type="text", out="CapstoneFECRE.htm", title="Fixed vs. Correlated Random Effects", align = TRUE, 
          dep.var.labels = ("Household Income"),
          dep.var.labels.include = TRUE,
          column.labels = c("FE", "CRE"))

##Question 3
POLS3 <- plm(data = data.panel, Income ~ smokingRate + alcoholConsumptionGallons + PercentHighSchoolOrHigher + Black + Female + Black*Female + factor(Year), index=c("State", "Year"), model = "pooling")
FD3 <- plm(data = data.panel, Income ~ smokingRate + alcoholConsumptionGallons + PercentHighSchoolOrHigher + Black + Female + Black*Female + factor(Year), index=c("State", "Year"), model = "fd")
FE3 <- plm(data = data.panel, Income ~ smokingRate + alcoholConsumptionGallons + PercentHighSchoolOrHigher + Black + Female + Black*Female + factor(Year), index=c("State", "Year"), model = "within")
RE3 <- plm(data = data.panel, Income ~ smokingRate + alcoholConsumptionGallons + PercentHighSchoolOrHigher + Black + Female + Black*Female + factor(Year), index=c("State", "Year"), model = "random")

stargazer(POLS3,FD3, FE3, RE3, type="text", out="CapstoneAll.htm", title="Comparative Analysis", align = TRUE, 
          dep.var.labels = ("Household Income"),
          dep.var.labels.include = TRUE,
          covariate.labels = c("Smoking Rate", "Alcohol Consumption", "High School Grad", "Black", "Female", "BlackFemale", "Year 2020"),
          column.labels = c("POLS", "FD", "FE", "RE"))

##Question 4
POLS4 <- lm(data = data, Income ~ alcoholConsumptionGallons + PercentHighSchoolOrHigher + Black + Female + Black*Female + factor(Year))

stargazer(POLS3, POLS4, type="text", out="CapstonePOLS.htm", 
          title="POLS Comparision", 
          align = TRUE, dep.var.labels = ("Houshold Income"), 
          dep.var.labels.include = TRUE, 
          covariate.labels = c("Smoking Rate", "Alcohol Consumption", "High School Grad","Black", "Female", "BlackFemale", "Year 2020"))

bptest(POLS3) #Heteroskedasticity does exist, NOT ROBUST
bptest(POLS4) #Heteroskedasticity does exist, NOT ROBUST



stargazer(bp4,header = FALSE, type = "html", out = 'CAPSTONEBP.htm', 
          title = "Breusch-Pagan Test")


##Question 5
data$pols.res<-POLS4$residuals
data.panel<-pdata.frame(data, index=c("State", "Year"))
pdim(data.panel)

data.panel$pols.lagres<-lag(data.panel$pols.res)
sc.pols<-lm(pols.res~pols.lagres, data=data.panel)
summary(sc.pols)
###FTR: Alpha_i exists

##Question 6
library(car)

phtest(FE3, RE3) ###Reject the Null, one of the models is inconsistent, USE FE

CRE6UR <-plm(Income ~  smokingRate + msmokingRate + alcoholConsumptionGallons + malcoholConsumptionGallons + PercentHighSchoolOrHigher + mPercentHighSchoolOrHigher + Black + mBlack + Female + mFemale + Black*Female + mBlackFemale + factor(Year), data=data.panel, index=c("State", "Year"), model="random")
CRE6R <-plm(Income ~ msmokingRate + malcoholConsumptionGallons + mPercentHighSchoolOrHigher + mBlack + mFemale + mBlackFemale + factor(Year), data=data.panel, index=c("State", "Year"), model="random")

linearHypothesis(CRE6R, "msmoking + malcoholConsumptionGallons + mPercentHighSchoolOrHigher = 0", test = "F", method = "arellano")

#Question 7
library(plm)
pwfdtest(FD3)

#Question 8

pbg <- pbgtest(FE3) #serial correlation exists
pbg

pbgtest(FD3)

FE8SC <- coeftest(FE3, vcovHC(FE3, method = "arellano", type = "HC0"))
FE8SC

stargazer(FE8SC, FE3, type="text",
          out="Capstone FE SC test.htm", 
          title="Fixed Effect Serial Correlation Test", 
          align = TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels = c("Smoking Rate", "Alcohol Consumption", "High School Grad","Black", "Female", "BlackFemale", "Year 2020"))



rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
options("scipen"=10, "digits"=5) # adjust how scientific notation is displayed

library(Hmisc)
library(sem)
library(psych)
library(corrgram)
library(ggplot2)

# Load custom functions
source(file.path(getwd(),"functions","Steiger R library functions.txt"))
source(file.path(getwd(),"functions","AdvancedFactorFunctions.txt"))
source(file.path(getwd(),"functions","fa.promax.R"))

AthleticsData <- read.csv(file.path(getwd(),"datasets","AthleticsData.csv"))
attach(AthleticsData)

R<-cor(AthleticsData) # correlation matrix R of variables in AthleticsData
S<-cov(AthleticsData) # covariance matrix S of variables in AthleticsData)


fit.2<-factanal(AthleticsData,factors=2,rotation="varimax")
print(fit.2)


fit.3<-factanal(AthleticsData,factors=3,rotation="varimax")
print(fit.3)

print(fit.3, digits=2, vutoff=.2, sort=TRUE)

colnames(fit.3$loadings)<-c("Endurance","Strength","Hand-Eye") 
print(loadings(fit.3), digits = 2, cutoff = .2, sort = TRUE) 

fit.3.promax <- update(fit.3,rotation="promax") 
colnames(fit.3.promax$loadings)<-c("Endurance","Strength","Hand-Eye") 
print(fit.3.promax)
print(loadings(fit.3.promax), digits = 2, cutoff = .2, sort = TRUE) 

print(fit.3.promax)
print(fit.3.promax)

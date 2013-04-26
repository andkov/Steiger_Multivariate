#############################################################
# prepare : options, packages, data
rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
options("scipen"=10, "digits"=5) # adjust how scientific notation is displayed

library(Hmisc)
library(sem)
library(psych)
library(corrgram)
library(ggplot2)

# Load custom functions
source(file.path(getwd(),"R Support Materials/functions","Steiger R library functions.txt"))
source(file.path(getwd(),"R Support Materials/functions","AdvancedFactorFunctions.txt"))
source(file.path(getwd(),"R Support Materials/functions","fa.promax.R"))

# create new dataset without missing data  
data <-
  read.csv('http://www.statpower.net/Content/312/Homework/LongPerkinsTime1SCI.csv') 
### remove missing values 
data <- na.omit(data)
head(data)

# Items 2,6,8,11  are worded negatively
# # recode negativaly worded questions so that  "2" - greater SOC, "1" - lesser SOC
data$SCI2=ifelse( (data$SCI2 %in% c(2)),1,ifelse((data$SCI2 %in% c(1)),2,NA))
data$SCI6=ifelse( (data$SCI6 %in% c(2)),1,ifelse((data$SCI6 %in% c(1)),2,NA))
data$SCI8=ifelse( (data$SCI8 %in% c(2)),1,ifelse((data$SCI8 %in% c(1)),2,NA))
data$SCI11=ifelse( (data$SCI11 %in% c(2)),1,ifelse((data$SCI11 %in% c(1)),2,NA))
# head(data) # Original dataset after recoding the negatively worded questions

# Create custom order of variable for later use:
varnames<-c("SCI01","SCI02","SCI03","SCI04","SCI05","SCI06","SCI07","SCI08","SCI09","SCI10","SCI11","SCI12")
# the names of the variables in the order they appear in the dataset
varnames2<-c("01goodlive","02sharevalues","03wantsame","04irecognize","05feelhome","06knowme",
             "07carethink","08influence", "09probsovled","10important","11getalong","12livelong")
colnames(data)<-varnames2
# 01goodlive    # I think my block is a GOOD place for me to LIVE  
# 02sharevalues # People on this block do not SHARE the same VALUES (reversed) 
# 03wantsame    # My neighbors and I WANT the SAME things from the block.  
# 04irecognize  # I can RECOGNIZE most of the people who live on my block. 
# 05feelhome    # I FEEL at HOME on this block. 
# 06knowme      # Very few of my neighbors KNOW ME. (reverse) 
# 07carethink   # I CARE about what my neighbors think of my actions.  
# 08influence   # I have almost no INFLUENCE over what this block is like. (reversed) 
# 09probsovled  # If there is a PROBlem on this block people who live here can get it SOLVED.  
# 10important   # It is very IMPORTANT to me to live on this particular block.  
# 11getalong    # People on this block generally don't GET ALONG with each other. (reversed) 
# 12livelong    # I expect to LIVE on this block for a LONG time.  

R<-cor(data) # correlation matrix R of variables in AthleticsData
S<-cov(data) # covariance matrix S of variables in AthleticsData)

empty<-matrix(numeric(0),ncol(R),ncol(R))
rownames(empty)<-rownames(R)
colnames(empty)<-paste0("xC",1:ncol(R))

############################################################
# decomposition analysis

pathImageOut<-file.path(getwd(),"Homework Assignments/Homework 06 - Factor Analysis") # save files in...
# Correlation matrix of 12 SOC measures
title<-paste0("Correlation among 12 SOC measures")
pathFileOut<-file.path(pathImageOut,paste0("Correlation.png"))
png(filename = pathFileOut, width =5, height =5 , units = "in", res=400)
corrgram(R,upper.panel=panel.conf,lower.panel=panel.pie,type="cor",order=FALSE,
         main=title)
dev.off()

round(R,2) # print correlation matrix
summary(data)
# Vectors with factor names
nF1<-c("SOC")
nF4<-c("Needs","Group","Influence","Emotional")
sem()
# a.
###########################
a.fit1<-QuickCFA(cor(data),n.factors=1,n.obs=nrow(data),
                 model.name="pureCFA",
                 factor.names=nF1,
                 cutoff=.30,
                 factor.correlations=TRUE,
                 reference.indicators=FALSE)
summary(a.fit1)
a1pat<-GetPattern(a.fit1) # reproduces Perkins & Long table perfectly
#########################
a.fit4<-QuickCFA(R,n.factors=4,n.obs=nrow(data),
                 model.name="pureCFA4",
                 factor.names=nF4,
                 cutoff=.30,
                 factor.correlations=TRUE,
                 reference.indicators=FALSE)
summary(a.fit4)
a4pat<-GetPattern(a.fit4)
GetPrettyPattern(a.fit4) 




##########################################
# Some values availible in sem(fit.object)
fit<-a.fit4
str(fit)
########## MODEL STRUCTURE ########
fit$var.names # variable names
fit$semmod  # RAM1 model
fit$S # observed random variables correlation
fit$J # Identity matrix of dim(pXp)                                              
fit$C
fit$A
fit$P
fit$coeff # loadings and correlations of factors
fit$vcov  # 

# handle MODEL STRUCTURE 
all.equal(fit$S,R) # proves that $S is observed cov/cor matrix S/R
summary(fit)
print(fit)
GetPrettyPattern(fit)
FP<-GetPattern(fit)
F<-FP$F
Phi<-FP$Phi  #  sem(fit$loadings)
vcov<-fit$vcov # covMatrix of manifest and latent variables - not sure what this is

######### MODEL PERFORMANCE #######
str(fit)
fit$criterion # misfit, discrepancy function. ChiSQ: X2 = ($N-1)*$criterion
p<-fit$n # number of manifest random variables  n=p, ### df = (p*(p+1)/2) - (t)
N<-fit$N # number of individuals
m<-(fit$m-p) # number of latent variables(factors)
t<-fit$t # number of estimated (free) parameters

# handle MODEL PERFORMANCE
STATISTIC<-(fit$N-1)*fit$criterion # Chi-square
df<-((p*(p+1))/2) - (t)
rmsea<-RMSEA(fit)
##########################################

##########################################
# hm6 - c.
FA.Stats(R,n.factors=1:5,n.obs=nrow(data), RMSEA.cutoff=.05)
Scree.Plot(R)


# hm6 - d.
# 3-factor oblique solution
fit.QJ3<-QuickJoreskog(R, n.factors=3,n.obs=nrow(data), use.promax=TRUE)
print(fit.QJ3)
GetPattern(fit.QJ3)
str(fit.QJ3)

# 4-factor orthogonal solution 
fit.QJ4.varimax<-QuickJoreskog(R, n.factors=4,n.obs=nrow(data), use.promax=FALSE)
print(fit.QJ4.varimax)
GetPattern(fit.QJ4.varimax)

# 4-factor oblique solution
fit.QJ4.promax<-QuickJoreskog(R, n.factors=4,n.obs=nrow(data), use.promax=TRUE)
print(fit.QJ4.promax)
GetPattern(fit.QJ4.promax)

fit.MLFA<-MLFA(Correlation.Matrix=R,n.factor=4, n.obs=nrow(data),promax.m=4)
Loadings(fit.MLFA, cutoff=.3, num.digits=2)

str(fit.MLFA)

Unrotated.4.F<-fit.MLFA$Unrotated$F
Unrotated.4.F.names<-colnames(Unrotated.4.F)<-c("Sense of Community","Investedness","Isolation","Conflict") 
colnames(fit.MLFA$Unrotated$F)<-Unrotated.4.F.names

Varimax.4.F<-fit.MLFA$Varimax$F
Varimax.4.F.names<-colnames(Varimax.4.F)<-c("Investedness","Good Home","Social Power","Good Neighbor") 
colnames(fit.MLFA$Varimax$F)<-Varimax.4.F.names

Promax.4.F<-fit.MLFA$Promax$F
Promax.4.F.names<-c("Social Power","Investedness","Good Home","Good Neighbor") 
colnames(Promax.4.F)<-Promax.4.F.names
colnames(fit.MLFA$Promax$F)<-Promax.4.F.names
colnames(fit.MLFA$Promax$Phi)<-Promax.4.F.names 
rownames(fit.MLFA$Promax$Phi)<-Promax.4.F.names

Quartimin.4.F<-fit.MLFA$Quartimin$F
Quartimin.4.F.names<-c("Good Home","Investedness","Social Power","Good Neighbor") 
colnames(Quartimin.4.F)<-Quartimin.4.F.names
colnames(fit.MLFA$Quartimin$F)<-Quartimin.4.F.names
colnames(fit.MLFA$Quartimin$Phi)<-Quartimin.4.F.names 
rownames(fit.MLFA$Quartimin$Phi)<-Quartimin.4.F.names

Bifactor.4.F<-fit.MLFA$Bifactor$F
Bifactor.4.F.names<-c("Sense of Community","Investedness","Conflict","Good Neighbor") 
colnames(Bifactor.4.F)<-Bifactor.4.F.names
colnames(fit.MLFA$Bifactor$F)<-Bifactor.4.F.names

BifactorOblique.4.F<-fit.MLFA$BifactorOblique$F
BifactorOblique.4.F.names<-c("Sense of Community","Good Home","Investedness","Conflict") 
colnames(BifactorOblique.4.F)<-BifactorOblique.4.F.names
colnames(fit.MLFA$BifactorOblique$F)<-BifactorOblique.4.F.names
colnames(fit.MLFA$BifactorOblique$Phi)<-BifactorOblique.4.F.names
rownames(fit.MLFA$BifactorOblique$Phi)<-BifactorOblique.4.F.names

Loadings(fit.MLFA, cutoff=.3, num.digits=2)


#############################################################
# Image production


##################### REFERENCE #############################
####### Advanced Factor Functions for sem(fit.object) #######

# Handout: Advanced Confirmatory Factor Analysis with R
QuickCFA # input 1/0 factor pattern -> sem -> (sem.object)
GetPattern # factor pattern and correlation: Lambda and Phi
GetPrettyPattern # prints factor pattern with <cutoff ommited 
RMSEA # computes RMSEA point estimate and limits at given confidence level
mod.indices # sem() option, top 5 modification indices
CheckMod # selects the highest "legal" modification indix 
UseMod # updates the current model by including modification index from CheckMod()
QuickEFAtoCFA # forces loadings <cutoff to be 0
QuickJoreskog # takes F from PCA with m factors -> highes in row - 0 the rest 
FAtoREF # generates intermediate "reference solution" 

# Handout: EFA in R Using some Advanced Support Functions 
Scree.Plot # Eigenvalues over principle components
FA.Stats # compares 1:nfactor solutions to settle on nfactors to retain
MLFA # offers six (so far) simple structures to compare
Loadings # reformats the loadings 

# undocumented yet
FindBifactor()
FindBestPattern()
FixPattern
FindBifactorPattern
SetupCFAPattern
print.FLS()
print.MLFA
FAtoCFA
FAtoSEM
CFAModel 

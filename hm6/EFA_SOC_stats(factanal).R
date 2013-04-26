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
source(file.path(getwd(),"functions","Steiger R library functions.txt"))
source(file.path(getwd(),"functions","AdvancedFactorFunctions.txt"))
source(file.path(getwd(),"functions","fa.promax.R"))

# create new dataset without missing data  
data <-
  read.csv('http://www.statpower.net/Content/312/Homework/LongPerkinsTime1SCI.csv') 
### remove missing values 
data <- na.omit(data)
head(data)

fit.1<-fa.promax(data,factors=3, n.obs=100)
Loadings(fit.1)

# Items 2,6,8,11  are worded negatively
# # recode negativaly worded questions so that  "2" - greater SOC, "1" - lesser SOC
# data$SCI2=ifelse( (data$SCI2 %in% c(2)),1,ifelse((data$SCI2 %in% c(1)),2,NA))
# data$SCI6=ifelse( (data$SCI6 %in% c(2)),1,ifelse((data$SCI6 %in% c(1)),2,NA))
# data$SCI8=ifelse( (data$SCI8 %in% c(2)),1,ifelse((data$SCI8 %in% c(1)),2,NA))
# data$SCI11=ifelse( (data$SCI11 %in% c(2)),1,ifelse((data$SCI11 %in% c(1)),2,NA))
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
fit.4.varimax<-factanal(covmat=R,factors=4,rotation="varimax")

print(fit.4.varimax)

##########################################
# some values availible in principal(fit.object) 
fit<-fit.4.varimax  # chose some model to look at carefully
str(fit.4.varimax)
########## MODEL STRUCTURE ########

# handle MODEL STRUCTURE 

######### MODEL PERFORMANCE #######
str(fit)

# handle MODEL PERFORMANCE

##########################################


# 
# fit.3<-factanal(AthleticsData,factors=3,rotation="varimax")
# print(fit.3)
# 
# print(fit.3, digits=2, vutoff=.2, sort=TRUE)
# 
# colnames(fit.3$loadings)<-c("Endurance","Strength","Hand-Eye") 
# print(loadings(fit.3), digits = 2, cutoff = .2, sort = TRUE) 
# 
# fit.3.promax <- update(fit.3,rotation="promax") 
# colnames(fit.3.promax$loadings)<-c("Endurance","Strength","Hand-Eye") 
# print(fit.3.promax)
# print(loadings(fit.3.promax), digits = 2, cutoff = .2, sort = TRUE) 
# 
# print(fit.3.promax)
# print(fit.3.promax)



#############################################################
# Image production
pathImageOut<-file.path(getwd(),"hm6") # save files in...

title<-paste0("Correlation among 12 SOC measures")
pathFileOut<-file.path(pathImageOut,paste0("Correlation.png"))
png(filename = pathFileOut, width =5, height =5 , units = "in", res=400)
corrgram(R,upper.panel=panel.conf,lower.panel=panel.pie,type="cor",order=FALSE,
         main=title)
dev.off()

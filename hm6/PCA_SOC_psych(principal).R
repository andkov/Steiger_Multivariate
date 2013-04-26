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

pathScree<-file.path(getwd(),"functions","scree.R")
pathPattern<-file.path(getwd(),"functions","factor pattern.R")
colors<- c("darksalmon" ,"lightskyblue")


fit.4.none <- principal(r=data, nfactors=4, rotate="none") 
fit.4.varimax <- principal(r=data, nfactors=4, rotate="varimax") 
fit.4.promax <- principal(r=data, nfactors=4, rotate="promax") 

fit.12.none<- principal(r=data, nfactors=12, rotate="none") 
fit.12.varimax<- principal(r=data, nfactors=12, rotate="varimax") 
fit.12.promax<- principal(r=data, nfactors=12, rotate="promax") 

whatsolution
whatrotation
nfactors
print(fit.4.none)
print(fit.4.varimax)
print(fit.4.promax)

print(fit.12.none)
print(fit.12.varimax)
print(fit.12.promax)
# standard decompositions
eR<-eigen(R)
eR$values
# sR<-svd(R)
# sR$d

#################################################################################
# Chose labels for graph production
# solutions   [1]       [2]        [3]          [4]       [5]
solution<-c("eigen()", "svd()","principal()","factanal()","sem()")
whatsolution<- solution[3] #!!! Choose !!!#
#             uncorrelated factors <---||---> correlated factors
# rotation   [1]      [2]       [3]       [4]       [5]       [6]        [7]       [8]
rotation<-c("none","varimax","quatimax","promax","oblimin","simplimax","cluster","quartimin")
whatrotation<-as.character(rotation[4]) #!!! Choose !!!#

nfactors<-4  #ncol(R)

#####################################
# some values availible in principal(fit.object) 
fit<-fit.4.promax  # chose some model to look at carefully
str(fit)
########## MODEL STRUCTURE ########
fit$values       # (1:nfactors) eigenvalues    
fit$communality  # (1:nfactors) communalities  
fit$uniquenesses # (1:nfactors) uniquenesses

fit$residual     # (1:nfactors) SQuare Matrix 
fit$loadings     # (1:nfactors) SQuare Matrix 
#extract lower part of loadings
eigens<-colSums(fit$loadings*fit$loadings) # eigenvalues
explained<-colSums(fit$loading*fit$loading)/dim(fit$loading)[1]  # variance explained
cumulative<-cumsum(colSums(fit$loading*fit$loading)/dim(fit$loading)[1]) # cummulative
Dplus<-rbind(eigens,explained,cumulative)
fit$Phi          # matrix of interfactor correlations
fit$scores       # n.obs ~ nfactors
# Extract factor loading into a matrix
ifelse(nfactors<ncol(R),
       F<-cbind(fit$loadings[1:ncol(R),1:nfactors],empty[1:ncol(R),(nfactors+1):ncol(R)]), 
       F<- fit$loadings[1:ncol(R),1:nfactors]
)
F
######### MODEL PERFORMANCE #######
(fit$objective)
fit$n.obs
fit$fit
fit$fit.off
fit$fn  # what function was used for decomposition
fit$objective # misfit function, -2LL, likelihood function
fit$STATISTIC # Chi-square: X2 = ($n.obs)*($objective)

fit$R2




#############################################################
# Image production
pattern<-F   # matrix for the factor pattern
drawing<- "F"  # name of file with graph

pathImageOut<-file.path(getwd(),"hm6","SenseOfCommunity") # save files in...
title<- paste0("Pattern values from ",whatsolution," matrix:",drawing,"
               rotation : ",whatrotation,", Factors = ",nfactors)
ylims<-c(0,3)           # (min,max) for eigenvalue plot 
width<-5              # width of pattern in inches
height<-9             # height of pattern in inches
resolution<-200         # resolution of pattern 
source(pathPattern) #produces the graph of pattern loadings


title2<- paste0("Scree plot from ", whatsolution,"
rotation : ", whatrotation, " Factors = ",nfactors)
title3<- paste0("Variance Explained from ",whatsolution,"
rotation : ",whatrotation,", Factors = ",nfactors)
width2<-3.3             # width of scree in pixels
height2<-2.5           # height of scree in pixels
resolution2<-200         # resolution of pattern 
source(pathScree) 

Fcor<-round(fit$Phi,4)
Fcor<-Fcor[order(rownames(Fcor)),order(colnames(Fcor))]
title4<-paste0("Correlation among ",nfactors," factors"," in ",whatrotation," rotation")

pathFileOut<-file.path(pathImageOut,paste0(drawing,"_",nfactors,"_",whatrotation,"_Cor.png"))
png(filename = pathFileOut, width =5, height =5 , units = "in", res=200)
corrgram(Fcor,upper.panel=panel.conf,lower.panel=panel.pie,type="cor",order=FALSE,
         main=title4)
dev.off()



title5<-paste0("Residuals for ",nfactors," factors"," in ",whatrotation," rotation")
pathFileOut<-file.path(pathImageOut,paste0(drawing,"_",nfactors,"_",whatrotation,"_Res.png"))
png(filename = pathFileOut, width =5, height =5 , units = "in", res=200)
corrgram(fit$residual,upper.panel=panel.conf,lower.panel=panel.pie,type="cor",order=FALSE,
         main=title5)
dev.off()

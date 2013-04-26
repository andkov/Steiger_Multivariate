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

empty<-matrix(numeric(0),ncol(R),ncol(R))
rownames(empty)<-rownames(R)
colnames(empty)<-paste0("xC",1:ncol(R))

######   Producing graphs   # Ctrl+Alt+E - Run from line to end
pathScree<-file.path(getwd(),"functions","scree.R")
pathPattern<-file.path(getwd(),"functions","factor pattern.R")
colors<- c("darksalmon" ,"lightskyblue")
fa.promax
# solutions   [1]       [2]        [3]
solution<-c("eigen()", "svd()","principal()","factanal()")
whatsolution<- solution[3] #!!! Choose !!!#
#             uncorrelated factors <---||---> correlated factors
# rotation   [1]      [2]       [3]       [4]       [5]       [6]        [7]  
rotation<-c("none","varimax","quatimax","promax","oblimin","simplimax","cluster")
whatrotation<-as.character(rotation[4]) #!!! Choose !!!#
nfactors<-4  #ncol(R)

# Principal Component Analysis
fit <- principal(r=AthleticsData, nfactors=nfactors, rotate=whatrotation) 
whatsolution
whatrotation
nfactors
fit
str(fit)
fit$values
fit$loadings
# print results   
# fit$values # eigenvalues
# fit$loadings # PC loadings
# str(fit)
# fit$communality
# fit$uniquenesses
# fit$Phi



ifelse(nfactors<ncol(R),
       F<-cbind(fit$loadings[1:ncol(R),1:nfactors],empty[1:ncol(R),(nfactors+1):ncol(R)]), 
       F<- fit$loadings[1:ncol(R),1:nfactors]
)
F
# extract the parameter values from fitted object
eigens<-colSums(fit$loadings*fit$loadings) # eigenvalues
explained<-colSums(fit$loading*fit$loading)/dim(fit$loading)[1]  # variance explained
cumulative<-cumsum(colSums(fit$loading*fit$loading)/dim(fit$loading)[1]) # cummulative
# Create  D matrix with eigenvalues and thier informations
Dplus<-rbind(eigens,explained,cumulative)


pathImageOut<-file.path(getwd(),"PCA","Athletic") # save files in...
pattern<-F   # matrix for the factor pattern
drawing<- "F"  # name of file with graph

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








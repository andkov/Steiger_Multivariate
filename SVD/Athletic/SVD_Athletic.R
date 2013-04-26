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

# Apply eigen and single value decompositions
eigen<-eigen(R) # eigen decomposition        of R #  VDV' : $values -eigenvalues, $vectors
svd<-svd(R)     # single value decomposition of R #  UDV' : $d      -eigenvalues, $u,$v
Ve<-eigen$vectors              # eigenvectors V from VDV' of R
De<-diag(eigen$values)         # eigenvalues  D from VDV' of R
Us<-svd$u                      # eigenvectors U from UDV' of R
Ds<-diag(svd$d)                # eigenvalues  D from UDV' of R
Vs<-svd$v                      # eigenvectors V from UDV' of R

Fe<-(Ve %*% sympower(De,1/2))  # principal component pattern F=V(D^1/2) 
Fs<-(Vs) %*% sympower(Ds,1/2)  # same computed from UDV'

# Renaming for convenience
rownames(Ve)<-colnames(R) 
rownames(De)<-colnames(R)
rownames(Us)<-colnames(R)
rownames(Ds)<-colnames(R)
rownames(Vs)<-colnames(R)

colnames(Ve)<-paste0("PC",1:ncol(R)) # name the components
colnames(De)<-paste0("PC",1:ncol(R)) # name the components
colnames(Us)<-paste0("PC",1:ncol(R)) # name the components
colnames(Ds)<-paste0("PC",1:ncol(R)) # name the components
colnames(Vs)<-paste0("PC",1:ncol(R)) # name the components

rownames(Fe)<-colnames(R)      # names the variables
rownames(Fs)<-colnames(R)      # names the variables
colnames(Fe)<-paste0("PC",1:ncol(R)) # name the components
colnames(Fs)<-paste0("PC",1:ncol(R))# name the components

empty<-matrix(numeric(0),ncol(R),ncol(R))
rownames(empty)<-rownames(Ds)
colnames(empty)<-colnames(Ds)

# Ve       Eigen decomposition, eigenvectors,  V from VDV' of R
# De       Eigen decomposition, eigenvalues,   D from VDV' of R
# Us       SVD decomposition,   eigenvectors,  U from UDV' of R
# Ds       SVD decomposition,   eigenvalues,   D from UDV' of R
# Vs       SVD decomposition,   eigenvectors,  V from UDV' of R
# Dplus    Eigensvalues from solution of the dedicated routine
# Fe       principal component pattern F=V(D^1/2) , from EiGenDecomposition
# Fs       same computed from UDV' # from single value decomposition
# F        pattern loading from function
#          R            Correlation/Covariance matrix - R  
#      F      F'        Grahm-Factors                 - F
#   VD1/2   D1/2V'      Principle Component patterns  - pcPattern
#   V     D     V'      Earhart-Young decomposition   - V, D   
#      Rv = cv          Eigenvalues and Eigenvectors  

######   Producing graphs   # Ctrl+Alt+E - Run from line to end
pathScree<-file.path(getwd(),"functions","scree.R")
pathPattern<-file.path(getwd(),"functions","factor pattern.R")
colors<- c("darksalmon" ,"lightskyblue")

# solutions   [1]       [2]        [3]
solution<-c("eigen()", "svd()","principal()","factanal()")
whatsolution<- solution[2] #!!! Choose !!!#

# rotation   [1]      [2]       [3]       [4]       [5]       [6]        [7]  
rotation<-c("none","varimax","quatimax","promax","oblimin","simplimax","cluster")
whatrotation<-as.character(rotation[1]) #!!! Choose !!!#
nfactors<-ncol(R)

pathImageOut<-file.path(getwd(),"SVD","Athletic") # save files in...
pattern<-Us   # matrix for the factor pattern
drawing<- "Us"  # name of file with graph

title<- paste0("Pattern values from ",whatsolution," matrix:",drawing,"
               rotation : ",whatrotation,", Factors = ",nfactors)
ylims<-c(0,3)           # (min,max) for eigenvalue plot 
width<-450              # width of pattern in pixels
height<-900             # height of pattern in pixels
width2<-300             # width of scree in pixels
height2<-200            # height of scree in pixels

source(pathPattern) #produces the graph of pattern loadings


str(pattern)
















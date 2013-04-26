rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
source(file.path(getwd(),"R Support Materials","AdvancedFactorFunctions.txt"))
library(psych)
library(plotrix)
str(Harman74.cor$cov)
library(sem)

# from "Modern Factor Analysis" - Harman, 1974 
R <- as.matrix(Harman74.cor$cov)
Scree.Plot(R,main="SCREE Plot\n24 Psychological Variables Data (n=145)")

FA.Stats(R,n.factors=1:5,n.obs=145,
           main="RMSEA Plot\n24 Psychological Variables Data  (n=145)",
           RMSEA.cutoff=0.05)

out <- MLFA(Correlation.Matrix=R,n.factors=4,n.obs=145,promax.m=3)
# cutoff - at what value factor woudl "lock in" loading to itself
Loadings(out,cutoff=.3,num.digits=2)
# printing individual solutions
out$Varimax
print.FLS(out$Varimax,cutoff=.3,num.digits=2)
Loadings(out,cutoff=0.3,num.digits=2)

# now try with a new dataset
data(Thurstone)
Scree.Plot(Thurstone)
FA.Stats(Thurstone,n.factors=1:3,n.obs=213)
out.2 <- MLFA(Correlation.Matrix=Thurstone,n.factors=3,n.obs=213,promax.m=4)
str(out.2)
# Rotation options:
# Uncorrelated Factors: Unrotated - Varimax - Bifactor  - Quartimin
#   Correlated Factors:           - Promax  - BifactorOblique 
out.2$Unrotated
out.2$Varimax
out.2$Promax
out.2$Quartimin
out.2$Bifactor
out.2$BifactorOblique

# test through CFA
# FAtoCFA -takes variable names for loading that you keep
# FAtoSEM - 



FAtoCFA(out.2$Bifactor,
        model.name="fit")
FAtoSEM(out.2$Bifactor, 
        model.name="fit2",
        make.start.values=FALSE)
FAtoSEM(out.2$Bifactor, 
        model.name="fit3",
        make.start.values=TRUE)
library(sem)
cfa()








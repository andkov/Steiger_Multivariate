fa.promax = function(fo,factors=1,digits=4,sort=F,m=3,...)
{
   # ML factor analysis with varimax and promax rotation including correlation
   # matrix of factors, optional sorting of loadings, and specification of
   # promax power (m, default = 3 as recommended by many statistical packages;
   # higher values simplify loadings at the cost of additional correlation
   # between factors). If "raw" data (not a correlation or covariance matrix)
   # and the additional argument score='regression' are used, the output will
   # contain matrices of factor scores of the varimax and promax rotated
   # solutions.

   sort.loadings = function(ld)
   {
     f = dim(ld)[2]
     loadmax=abs(ld[,1:f])==apply(abs(ld[,1:f]),1,max)
     FL = as.list(colnames(ld)[1:f])
     for (i in 1:f)
     {
        FL[[i]]=ld[loadmax[,i]==T,]
        if (length(dim(FL[[i]])) > 0)
        {
          FL[[i]]=FL[[i]][order(abs(FL[[i]][,i]),decreasing=T),]
        }
        if (i == 1)
        {
           erg=FL[[1]]
        }
        else
        {
          erg=rbind(erg,FL[[i]])
          if (i == 2)
          {
            if (length(dim(FL[[1]])) == 0)
            {
               rownames(erg)[1] = rownames(ld)[which(loadmax[,1]==T)]
            }
          }
          if (i > 1)
          {
            if (length(dim(FL[[i]])) == 0)
            {
               rownames(erg)[dim(erg)[1]] = rownames(ld)[which(loadmax[,i]==T)]
            }
          }
        }
      }
      erg
   }

   res = factanal(fo,factors=factors,rotation="none",...)
   if (factors > 1)
   {
     vm = varimax(loadings(res))
     ssvm = diag(t(vm$loadings[]) %*% vm$loadings[])
     ssvm = rbind(ssvm,ssvm/dim(vm$loadings[])[1])
     pm = promax(loadings(vm),m=m) # m=3 is default in many programs
     sspm = diag(t(pm$loadings[]) %*% pm$loadings[])
     sspm = rbind(sspm,sspm/dim(pm$loadings[])[1])
     A = vm$rotmat %*% pm$rotmat
     phi = solve(t(A) %*% A)
     pmst = pm$loadings %*% phi
     unld = res$loadings[]
     vmld = vm$loadings[]
     pmld = pm$loadings[]
     SeqF = order(colSums(vmld**2),decreasing=T)
     ssvm = ssvm[,SeqF]
     ssvm = rbind(ssvm,cumsum(ssvm[2,]))
     rownames(ssvm)=c('SS loadings','Proportion Var','Cumulative Var')
     sspm = sspm[,SeqF]
     sspm = rbind(sspm,cumsum(sspm[2,]))
     rownames(sspm)=c('SS loadings','Proportion Var','Cumulative Var')
     unld = unld[,SeqF]
     vmld = vmld[,SeqF]
     pmld = pmld[,SeqF]
     pmst = pmst[,SeqF]
     phi = phi[,SeqF]
     phi = phi[SeqF,]     
     colnames(unld) = paste(rep('Factor',factors),1:factors,sep='')
     colnames(ssvm) = colnames(unld)
     colnames(sspm) = colnames(unld)
     colnames(vmld) = colnames(unld)
     colnames(pmld) = colnames(unld)
     colnames(pmst) = colnames(unld)
     colnames(phi)=colnames(unld)
     rownames(phi)=colnames(unld)
     if (length(res$scores) > 0)
     {
        FS = sqrt(res$n.obs/(res$n.obs-1))*scale(res$scores)
        vm.fs = FS %*% vm$rotmat
        pm.fs = sqrt(res$n.obs/(res$n.obs-1))*scale(FS %*% pm$rotmat)[,]
        FS = FS[,SeqF]
        vm.fs = vm.fs[,SeqF]
        pm.fs = pm.fs[,SeqF]
        colnames(FS) = colnames(unld)
        colnames(vm.fs)=colnames(unld)
        colnames(pm.fs)=colnames(unld)
     }
     if (sort==T)
     {
       uniqueness = cbind(sort(res$uniqueness))
       vmld = sort.loadings(vmld)
       Dummy = NULL
       for (i in 1:nrow(unld))
       {
         Dummy = rbind(Dummy,unld[which(rownames(unld)==rownames(vmld)[i]),])
       }
       rownames(Dummy)=rownames(vmld)
       unld = Dummy
       pmld = sort.loadings(pmld)
       pmst = sort.loadings(pmst)
     }
     else
     {
       uniqueness = cbind(res$uniqueness)
     }
     colnames(uniqueness) = "residual variance"
     if (length(res$scores) > 0)
     {
       erg = list(uniqueness=round(uniqueness,digits),
                  unrotated.loadings=round(unld,digits),
                  unrotated.factorscores=round(FS[,],digits),
                  varimax.SS = round(ssvm,digits),
                  varimax.loadings=round(vmld,digits),
                  varimax.factorscores = round(vm.fs,digits),
                  promax.SS = round(sspm,digits),
                  promax.loadings=round(pmld,digits),
                  promax.structure=round(pmst,digits),
                  corr.factors=round(phi,digits),
                  promax.factorscores = round(pm.fs,digits),
                  n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
     }
     else
     {
       erg = list(uniqueness=round(uniqueness,digits),
                  unrotated.loadings=round(unld,digits),
                  varimax.SS = round(ssvm,digits),
                  varimax.loadings=round(vmld,digits),
                  promax.SS = round(sspm,digits),
                  promax.loadings=round(pmld,digits),
                  promax.structure=round(pmst,digits),
                  corr.factors=round(phi,digits),
                  n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
     }
   }
   else
   {
     ss = diag(t(res$loadings[]) %*% res$loadings[])
     ss = rbind(ss,ss/dim(res$loadings[])[1])
     ss = rbind(ss,cumsum(ss[2,]))
     rownames(ss)=c('SS loadings','Proportion Var','Cumulative Var')
     if (sort==T)
     {
        uniqueness = cbind(sort(res$uniqueness))
        vmld=cbind(sign(res$loadings[order(abs(res$loadings),decreasing=T)])*
                   sort(abs(res$loadings[1:dim(res$loadings)[1],]),dec=T))
        colnames(vmld)="Factor1"
     }
     else
     {
        uniqueness = cbind(res$uniqueness)
        vmld=res$loadings[]
     }
     colnames(uniqueness) = "residual variance"
     if (length(res$scores) > 0)
     {
        FS = cbind(sqrt(res$n.obs/(res$n.obs-1))*scale(res$scores))
        colnames(FS)='Factor1'
        erg = list(uniqueness=round(uniqueness,digits),
                   SS = round(ss,digits),
                   loadings=round(vmld,digits),
                   factorscores = round(FS,digits),
                   n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
     }
     else
     {
       erg = list(uniqueness=round(uniqueness,digits),
                  SS = round(ss,digits),
                  loadings=round(vmld,digits),
                  n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
     }
   }
   erg
}

# Example (de-comment command lines 188-189, 192-193, and 196-197):

# One factor, not sorted (defaults):
# fa.promax(~general+picture+blocks+maze+reading+vocab,
#           covmat=ability.cov)

# Two factors, m = 3 (default), sorted:
# fa.promax(~general+picture+blocks+maze+reading+vocab,factors=2,sort=T,
#           covmat=ability.cov)

# Two factors, m = 2.5, sorted:
# fa.promax(~general+picture+blocks+maze+reading+vocab,factors=2,sort=T,m=2.5,
#           covmat=ability.cov)

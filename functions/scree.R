ds<-as.data.frame(t(Dplus))
ds$pc<-c(1:ncol(Dplus))

# Scree
p2<-ggplot(ds, aes(x=pc,y=eigens))+
  ggtitle(title2)+
  geom_abline(intercept=1,slope=0,color="red",linetype="dashed")+
  geom_line()+
  geom_point()+
  scale_y_continuous(limits=c(0,3))+
  scale_x_discrete (limits=rev(c(1:ncol(R))))
p2
pathFileOut<-file.path(pathImageOut,paste0(drawing,"_",nfactors,"_",whatrotation,"_Scree",".png"))
png(filename = pathFileOut,
    width =width2, height =height2 , units = "in",res=resolution2)
plot(p2)
dev.off()

# Variance Explained
p3<-ggplot(ds, aes(x=pc))+
  ggtitle(title3)+
  geom_line(y=explained,linetype=4)+
  geom_point(y=explained)+
  geom_line(y=cumulative,linetype=6)+
  geom_point(y=cumulative)+
  scale_y_continuous(limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_discrete (limits=rev(c(1:ncol(R))))
p3
c<-as.numeric(seq(1,ncol(R),1))

pathFileOut<-file.path(pathImageOut,paste0(drawing,"_",nfactors,"_",whatrotation,"_SSexp",".png"))
png(filename = pathFileOut,
    width =width2, height =height2 , units = "in",res=resolution2)
plot(p3)
dev.off()

?scale_x_discrete
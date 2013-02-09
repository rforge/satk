data(USLcalc)
sl1<-sflist(USLcalc)

data(SSUSLcalc)
sl2<-sflist(SSUSLcalc,data.cols=c("V1","V2"),starts=list(usl=c(alpha=.1,beta=.001)))

scaled.df<-rbind(build_fits_frame(sl1),build_fits_frame(sl2))
cols<-c("blue","red","green","purple","black")
xyplot(X_N~N|slname,scaled.df,groups=type,type=c("l","l","l","l","p"),
           distribute.type=T,scales=list(x="free",y="free"),
           col.line=cols,col="black",
            key=list(
             x=.7,y=.2,
             text=list(lab=unique(levels(scaled.df$type))),
             lines=list(
               pch=c(1,NA),
               col=cols,
               type=c("l","l","l","l","p"),
               cex=.7),
             transparent=T,border=F,rep=F),
           xlab="N",
           ylab="Throughput X(N)"
)



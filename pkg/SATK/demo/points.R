require(ggplot2)
data(postgreSQL)
postmat <- mapply(6:21,FUN=function(x) {
  subset.usl <- usl(postgreSQL[1:x,])
  predict_pts <- data.frame(N=postgreSQL$N)
  postgreSQL$X_N[[1]]*predict(subset.usl,predict_pts)
}
                  )
colnames(postmat) <- 6:21
postframe <- stack(as.data.frame(postmat))
postframe$N<-postgreSQL$N
names(postframe)<-c("X_N","Points","N")
postframe$Points <- factor(postframe$Points,levels=6:21)
post.gp <- ggplot(postframe,aes(N,X_N))+
  geom_line()+
  facet_wrap(~Points)+
  geom_point(data=postgreSQL,aes(N,X_N))

post.gp
               

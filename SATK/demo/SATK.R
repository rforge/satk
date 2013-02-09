data(USLcalc)
sUSL<-usl(USLcalc)
plot(sUSL)
plot(sUSL,plot.lin_scale=T)
sl1<-sflist(USLcalc)
plot(sl1)

plot(sl1[c("amdahl","usl")])
plot(sl1[c("usl","aloha")])
plot(sl1[c("usl","aloha")],plot.lin_scale=T)


data(SSUSLcalc)
sl2<-sflist(SSUSLcalc,data.cols=c("V1","V2"),starts=list(usl=c(alpha=.1,beta=.001)))
plot(sl2)

Nmax(sl1[["gustafson"]])
Xmax(sl1[["gustafson"]])
Xmax(sl1[["usl"]])
Nmax(sl1[["usl"]])
effic(sl1[["usl"]])
sst(sl1[["usl"]])
sse(sl1[["usl"]])

al1 <- aloha(USLcalc)

sl3 <- sflist(sUSL,al1)
plot(sl3)


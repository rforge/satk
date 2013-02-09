data(USLcalc)
gust1 <- gustafson(USLcalc)
plot(gust1,legend.params=list(x="top",legend=format_legend(gust1),ncol=2,box.col="red"),title.params=list(main="Scalability Analysis is Fun w/ SATK!!"),plot.lin_scale=T,lin_scale.params=list(col="blue",lty="dashed"))

# comparison TKP-RTB pages versus normal pages

library("ggplot2")
library("grid")
library("gridExtra")
alan <- read.csv("report_allesandere.csv", header=T)
alan <- alan[-length(alan[,1]),]
rtb <-  read.csv("report_rtb_tkp.csv", header=T)
rtb <- rtb[-length(rtb[,1]),]
alzu <- read.csv("report_alleszusammen.csv", header=T)
alzu <- alzu[-length(alzu[,1]),]

#plot(as.Date(alan$Datum,format="%d.%m.%y"),alan$Rohertrag/alan$Summe....Advertiser.,pch=20,col="blue")
#points(as.Date(rtb$Datum,format="%d.%m.%y"),rtb$Rohertrag/rtb$Summe....Advertiser.,pch=20,col="green")

var1 = alan$Rohertrag/alan$Summe....Advertiser.
var2 = 1:42
var3 = as.Date(alan$Datum,format="%d.%m.%y")
var4 = rtb$Rohertrag/rtb$Summe....Advertiser.
var5 = alzu$Rohertrag/alzu$Summe....Advertiser.
lo <- loess( var1~var2)
lo2 <- loess( var4~var2)
lo3 <-loess( var5~var2)
plot(var2,var4,pch=19,col="green")
points(var2,var1,pch=20,col="blue")
points(var2,var5,pch=20,col="brown")
lines(predict(lo), col='red', lwd=2)
lines(predict(lo2),col="magenta",lwd=2)
lines(predict(lo3),col="orange",lwd=2)

cor(var1,var4) # -0.05966195, unkorreliert
cor(var1,var5) # 0.1231708, bestenfalls schwach positiv korreliert
cor(var4,var5) # 0.9758194, fast 100%-ige Korrelation!

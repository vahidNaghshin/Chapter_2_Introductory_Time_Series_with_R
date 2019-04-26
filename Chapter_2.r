www<-file.path(getwd(), 'varnish.dat')
varnish.dat <- read.table (www, header=T)
png("Varnish.png")
plot(varnish.dat$x,varnish.dat$y, main="Varnish", ylab="drying time", xlab="Amount")
dev.off()
browseURL("Varnish.png")
print(cbind("The correlation in varnish data is: ", cor(varnish.dat$x, varnish.dat$y)))

www<-file.path(getwd(), 'guesswhat.dat')
guesswhat.dat <- read.table (www, header=T)
print(guesswhat.dat)
png("guesswhat.png")
plot(guesswhat.dat$x,guesswhat.dat$y, main="guesswhat")
dev.off()
browseURL("guesswhat.png")
print(cbind("The correlation in guesswhat data is: ", cor(guesswhat.dat$x, guesswhat.dat$y)))

SerendipityShirazVineyard = ts(c(39, 35, 16, 18, 7, 22, 13, 18, 20, 9, -12, -11, -19, -9, -2, 16))
CageyChardonnayVineyard = ts(c(47, -26, 42, -10, 27, -8, 16, 6, -1, 25, 11, 1, 25, 7, -5, 3))

png("SerendipityShirazVineyard.png")
plot(seq(from = 1, to =length(SerendipityShirazVineyard) , by = 1), SerendipityShirazVineyard, xlab='Volume', col='orange', lty=2)
lines(seq(from = 1, to =length(SerendipityShirazVineyard) , by = 1), SerendipityShirazVineyard, xlab='Volume', col='orange', lty=2)
lines(seq(from = 2, to =length(SerendipityShirazVineyard)+1 , by = 1), SerendipityShirazVineyard, type="o", pch=22, lty=2, col="red")
legend("bottomleft", legend=c("Lag 0", "Lag 1"), col=c("orange", "red"), lty=2, cex=0.8)
dev.off()
browseURL("SerendipityShirazVineyard.png")

png("CageyChardonnayVineyard.png")
plot(seq(from = 1, to =length(CageyChardonnayVineyard) , by = 1), CageyChardonnayVineyard, xlab='Volume', col='orange', lty=2)
lines(seq(from = 1, to =length(CageyChardonnayVineyard) , by = 1), CageyChardonnayVineyard, xlab='Volume', col='orange', lty=2)
lines(seq(from = 2, to =length(CageyChardonnayVineyard)+1 , by = 1), CageyChardonnayVineyard, type="o", pch=22, lty=2, col="red")
legend("bottomright", legend=c("Lag 0", "Lag 1"), col=c("orange", "red"), lty=2, cex=0.8)
dev.off()
browseURL("CageyChardonnayVineyard.png")

png("ACFSerendipityShirazVineyard.png")
acf(SerendipityShirazVineyard, main="SereSerendipityShirazVineyard")
dev.off()
browseURL("ACFSerendipityShirazVineyard.png")

png("ACFCageyChardonnayVineyard.png")
acf(CageyChardonnayVineyard, main="SereSerendipityShirazVineyard")
dev.off()
browseURL("ACFCageyChardonnayVineyard.png")

www<-file.path(getwd(), 'global.dat')
Global <- scan(www)
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)

png("GlobalTs.png")
plot(Global.ts, main='Temp', ylab="Temp")
dev.off()
browseURL("GlobalTs.png")

D <- decompose(Global.ts, type = c("additive"), filter = NULL)

png("decGlobalTs.png")
plot(D)
dev.off()
browseURL("decGlobalTs.png")




print(cbind("The std of original data: ", sd(Global.ts)))
print(cbind("The std of deseasoned data: ", sd(Global.ts-D$seasonal)))
plot(D$trend, ylab='temp', col="black", lwd=3)
#dev.off()
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  ## Save the color
  invisible(t.col)
}
mycol <- t_col("red", perc = 50, name = "lt.pink")
lines(Global.ts-D$seasonal, col=mycol, pch=16)
legend("topleft", legend=c("Trend", "Seasonal adjusted"), col=c("black", "red"), lty=1, cex=0.8)
dev.off()
len = length(D$rand)
acf(D$rand[-c(1:6,len-6+1:len)], main="ACF of the random part")

www<-file.path(getwd(), 'Fontdsdt.dat')
Fontdsdt.dat <- read.table(www, header=T)

adflow.ts <- ts(Fontdsdt.dat$adflow, frequency = 12)
Font.dec <- decompose(adflow.ts, type = c("additive"), filter = NULL)
plot(Font.dec)

print(cbind("The std of original data: ", sd(adflow.ts)))
print(cbind("The std of deseasoned data: ", sd(adflow.ts-Font.dec$seasonal)))
plot(Font.dec$trend, ylab='temp', col="black", lwd=3)
#dev.off()
mycol <- t_col("red", perc = 50, name = "lt.pink")
lines(adflow.ts-Font.dec$seasonal, col=mycol, pch=16)
legend("topleft", legend=c("Trend", "Seasonal adjusted"), col=c("black", "red"), lty=1, cex=0.8)
dev.off()
len = length(Font.dec$rand)
acf(Font.dec$rand[-c(1:6,len-6+1:len)], main="ACF of the random part")

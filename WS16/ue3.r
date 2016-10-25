dat <- read.table("body.txt", header=TRUE)[,c(1,12,22,23,24,25)]
#  V1 = Biacromial diameter (Schulter) (cm)
# V12 = Waist girth (Taille) (cm)
# V22 = Age (years)
# V23 = Weight (kg)
# V24 = Height (cm)
# V25 = Gender (1 - male, 0 - female)
names(dat) <- c("Biacromial","Waist","Age","Weight","Height","Gender")
datf <- subset(dat, Gender==0)
datm <- subset(dat, Gender==1)

# ue1.18
#========
# range returns minimum and maximum
xR <- range(dat$Waist)
yR <- range(dat$Weight)

par(mfrow=c(1,2))

plot(Weight ~ Waist, type="p", pch=21, data=datf,
     bg=ifelse(datf$Gender==0, "lightpink3", "lightblue3"),
     xlab="Waist [cm]", ylab="Weight [kg]",
     main="Female", cex=0.8, xlim=xR, ylim=yR)
modf <- lm(Weight ~ Waist, dat=datf)
abline(modf, lty=1, lwd=2, col="lightpink3")

plot(Weight ~ Waist, type="p", pch=21, data=datm,
     bg=ifelse(datm$Gender==0, "lightpink3", "lightblue3"),
     xlab="Waist [cm]", ylab="Weight [kg]",
     main="Male", cex=0.8, xlim=xR, ylim=yR)
modm <- lm(Weight ~ Waist, dat=datm)
abline(modm, lty=1, lwd=2, col="lightblue3")

par(mfrow=c(1,1))

coef(modf)
coef(modm)


# ue1.19
#========
x <- c(3,1,5,6,3,4)
y <- c(4,2,4,8,6,5)
plot(y ~ x, type="p", pch=21, bg="lightblue3",
     xlim=c(0,6), ylim=c(0,8), cex=2,
     main="KQ - Gerade durch den Nullpunkt")
mod1 <- lm(y ~ -1 + x)
coef(mod1)
abline(mod1, lwd=2, col="lightblue3")
mod2 <- lm(y ~ x)
coef(mod2)
abline(mod2, lty=2, lwd=2, col="lightblue3")


# ue1.20
#========
x <- c(-2,3,-1,0,-3,1,5,-3)
y <- c(7,15,3,1,11,6,20,16)
plot(y ~ x, type="p", pch=21, bg="lightblue3",
     xlim=c(-5,5), ylim=c(0,25), cex=2,
     main="KQ - Parabel")
mod <- lm(y ~ I(x^2))
coef(mod)
xnew <- data.frame(x=seq(-5, 5, by=0.1))
ypred <- predict(mod, newdata=xnew, interval="n")
lines(xnew$x, ypred, lwd=2, col="lightblue3")

# ue2.1
#=======
demere <- function(B, plotit=TRUE) {
  # anzahl der versuche
  versuche <- B
  # wuerfe insgesamt
  n <- versuche*4
  # matrix der zufalls wuerfe
  x <- matrix(sample(1:6, n, replace=TRUE), nrow=versuche, ncol=4)
  # wurde in den 4 wuerfen mindestens einmal eine 6 gewuerfelt?
  # zweiter param -> apply function ueber zeilen 
  sechs.in.4 <- apply(x==6, 1, any)
  # kumuliert die wahrscheinlichkeiten
  freq.6.in.4 <- cumsum(sechs.in.4)/(1:versuche)
  # je 24 wuerfe mit beiden wuerfeln
  n <- versuche*48
  x <- matrix(sample(1:6, n, TRUE), nrow=versuche, ncol=48)
  doppel.6.in.24 <- apply(x==6, 1, function(x) any(x[1:24] & x[25:48]))
  freq.doppel.6.in.24 <- cumsum(doppel.6.in.24)/(1:versuche)
  freq.6.in.4
  
  if (plotit) {
    plot(freq.6.in.4, ylim=0:1, log="x", type="l", bty="n", lwd=2,
         ylab="Rel. H?ufigkeit", xlab="Versuche", col="black")
    lines(1:versuche, freq.doppel.6.in.24, lty=1, lwd=2, col="red")
    lines(c(1,versuche), c(0.5,0.5), lty=1)
    legend("topright", legend=c("6 bei 4 W?rfen (1 W?rfel)", 
                                "Doppel-6 bei 24 W?rfen (2 W?rfel)"), lty=1, lwd=2, 
           col=c("black","red"))
  }
  invisible(list(freq.6.in.4=freq.6.in.4,
                 freq.doppel.6.in.24=freq.doppel.6.in.24))
}

demere(10000)

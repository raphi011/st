# ue1.10
#========
yr <- seq(1951, 2011, by=10)
x <- c(7568,9315,10062,10102,10349,11334,12995)
bpl <- barplot(x, axis.lty=1, space=2, xlab="", 
               ylab="Bevölkerung", names.arg=yr, main="Eisenstadt 1951 - 2011", 
               col="darkgray", ylim=c(0,13500))

# 10-jährliche Zunahme
#----------------------
# x[7]/x[1] -> faktor mit die bevoelkerungszahl gestiegen ist
# (x[7]/x[1])^(1/6) ist der faktor pro jahrzehnt
# ((x[7]/x[1])^(1/6) - 1)*100 ist die prozentuelle steigung pro jahrzehnt
(zu10 <- ((x[7]/x[1])^(1/6) - 1)*100)

# ueberpruefen, ob berechnung stimmt
x[1]*(1 + zu10/100)^6

# plotte die durchschnittliche 10 jährliche zunahme
lines(bpl, x[1]*(1 + zu10/100)^(0:6), type="o",
      lty=1, pch=19, lwd=2)

# grob steigende tendenz, jedoch teils starke abweichungen zu den
# exakten messdaten

# 1-jährliche  Zunahme
#---------------------
(zu1 <- ((x[7]/x[1])^(1/60) - 1)*100)
x[1]*(1 + zu1/100)^60

# Prognose 2030
#---------------
(zu1a <- ((x[7]/x[6])^(1/10) - 1)*100)
x[7]*(1 + zu1a/100)^19
x[6]*(1 + zu1a/100)^29



# ue1.11
#=======
# gewichteter mittelwert

compdata <- data.frame(anteil = c(0.1, 0.2, 0.5, 0.2), umsatz = c(35000, 42000, 52500, 28000))
compdata
sum(compdata$umsatz * compdata$anteil)


# ue1.12
#======== 


# ue1.14
#========
dat <- read.table("body.txt", header=TRUE)[,c(1,12,22,23,24,25)]
#  V1 = Biacromial diameter (Schulter) (cm)
# V12 = Waist girth (Taille) (cm)
# V22 = Age (years)
# V23 = Weight (kg)
# V24 = Height (cm)
# V25 = Gender (1 - male, 0 - female)
names(dat) <- c("Biacromial","Waist","Age","Weight","Height","Gender")



# Kennzahlen
kennz <- function(x, ro=4) {
  param <- c(mean(x), median(x), max(x)-min(x), var(x), 
             sd(x), sd(x)/mean(x), IQR(x), mean(abs(x-mean(x))), 
             mean(abs(x-median(x)))) 
  param.m <- matrix(param, ncol=1)
  dimnames(param.m) <- list(c("Mittel","Median","Spannweite",
                              "Varianz","Streuung","VarKoef","IQR","MAD.Mittel",
                              "MAD.Median"), "")
  round(param.m, ro)
}

attach(dat)
by(Height, Gender, kennz, ro=3)
detach(dat)


# ue1.15
#========
dat1 <- within(dat, { BMI <- Weight/(Height/100)^2 })
dat1f <- subset(dat1, Gender==0)
dat1m <- subset(dat1, Gender==1)

# Boxplots
#----------
boxplot(BMI ~ Gender, data=dat1, col="darkgrey",
        names=c("female","male"), pars=list(boxwex=0.6))
mtext("BMI [ kg/m? ]", side=2, line=2.5)
mtext("Boxplots", side=3, line=1)

# Histogramme
#-------------
brk <- seq(16, 40, by=2)
old.par <- par(mfrow=c(1,2))
hist(dat1m[,7], breaks=brk, prob=T, col="darkgrey", right=F,
     xlab="kg/m?", main="BMI: Men", xlim=c(15,40))
lines(density(dat1m[,7]), lwd=2)
abline(v=25, lty=2, lwd=2)
x25m <- sum(dat1m[,7] > 25)/length(dat1m[,7])*100
text(25, 0.15, labels=paste("> 25: ", round(x25m,1), "%"), 
     pos=4, cex=1.1) 
hist(dat1f[,7], breaks=brk, prob=T, col="darkgrey", right=F,
     xlab="kg/m?", main="BMI: Women", xlim=c(15,40))
lines(density(dat1f[,7]), lwd=2)
abline(v=25, lty=2, lwd=2)
x25f <- sum(dat1f[,7] > 25)/length(dat1f[,7])*100
text(25, 0.15, labels=paste("> 25: ", round(x25f, 1), "%"), 
     pos=4, cex=1.1) 
par(old.par)

# Kennzahlen
#------------
bowley <- function(x) {
  Q <- quantile(x, (1:3)/4)
  (Q[3]-2*Q[2]+Q[1])/(Q[3]-Q[1])
}
moors <- function(x) {
  A <- quantile(x, (1:7)/8)
  ((A[7]-A[5])+(A[3]-A[1]))/(A[6]-A[2])
} 

kennz2 <- function(x, ro=4) {
  param <- c(mean(x), median(x), max(x)-min(x), var(x), 
             sd(x), sd(x)/mean(x), IQR(x), mean(abs(x-median(x))),
             e1071::skewness(x), e1071::kurtosis(x), bowley(x),
             moors(x)) 
  param.m <- matrix(param, ncol=1)
  dimnames(param.m) <- list(c("Mittel","Median","Spannweite",
                              "Varianz","Streuung","VarKoef","IQR","MAD","Schiefe",
                              "Exzess","Bowley","Moors"), "")
  round(param.m, ro)
}

attach(dat1)
by(BMI, Gender, kennz2, ro=3)
detach(dat1)


# ue1.16
#========
require(UsingR)
data(brightness)
hist(brightness, breaks=25, prob=T, xlab="mag",
     main="", col="darkgrey")
lines(density(brightness), lwd=2)

kennz2(brightness)


# ue1.17
#========
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

attach(dat)
print(by(dat[,1:5], Gender, cor), digits=4)
detach(dat)
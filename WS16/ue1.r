# ue1.2
#=======

# lädt die daten in die "pkw" variable
pkw <- read.table("pkw-neuzul11.txt", header=TRUE, sep=";")

# summe von allen neuzulassungen
TO <- sum(pkw$TOTAL)

# neuzulassungen der gruppe "OTHER"
TO.other <- pkw$TOTAL[pkw$GROUP == "OTHER"]

# lädt alle gruppen außer "OTHER", welche einen größeren anteil 
# an neuzulassungen als 3% haben
pkw2 <- subset(pkw, (GROUP != "OTHER")&(100*pkw$TOTAL/TO >= 3))

# summiert die menge an neuzulassungen dieser gruppen
TO2 <- sum(pkw2$TOTAL)

# errechnet die menge der neuzulassungen der zusammengefassten
# gruppen
pkw2.other <- data.frame(GROUP="OTHER", TOTAL=TO-TO2)

# fügt die restlichen gruppen mit der neuen "OTHER" gruppe in der
# variable pkw3 wieder zusammen (das statement in klammern bewirkt,
# dass die variable in der konsole ausgegeben wird)
(pkw3 <- rbind(pkw2, pkw2.other))

# gibt die sortierten indexes des pkw3 frames zurück, ohne 
# "index.return=TRUE" würde die funktion die sortierten werte 
# zurückgeben
ra <- sort(pkw3$TOTAL, index.return=TRUE)

# erstellt das kreisdiagramm, [ra$ix] bewirkt, dass die werte und
# die labels in der sortierten reihenfolge ausgegeben werden, mit
# col werden die tortenstücke unterschiedlich grau eingefärbt,
# main ist die überschrift des diagramms
pie(pkw3$TOTAL[ra$ix], labels=pkw3$GROUP[ra$ix],
    col=gray(seq(0.4, 1.0, length=12)),
    main="PKW Neuzulassungen 2011 (Western Europe)")






# ue1.4
#=======

# initialisiert das array x mit den werten aus der angabe
x <- c(0,2,0,0,1,3,0,3,1,1,0,0,1,2,0,
       0,0,1,1,3,0,1,0,0,0,5,1,0,2,0)

# setzt für n den umfang der stichprobe
n <- length(x)

# erstellt eine tabelle, welche die liste der stichproben
# als faktoren interpretiert
tab <- table(x)

# hiermit bekommt man alle faktoren der tabelle
mr <- as.numeric(names(tab))

# der kleinste wert (faktor) 
n1 <- min(mr)

# .. und den größte
n2 <- max(mr)

# initialisiert einen vektor mit länge (n2-n1+1), wobei
# jeder wert auf 0 gesetzt ist
nk <- numeric(n2-n1+1)
nk[mr-n1+1] <- tab

# setze für die namen der spalten die jeweiligen faktoren
names(nk) <- n1:n2

old.par <- par(mar=c(5, 4.5, 4, 2) + 0.1)

# Balkendiagramm
# nk/n berechnet die relativen häufigkeiten
# axis.lty zeichnet die x achse
# spaces definiert den abstand zw. den balken
# xlab / ylab sind die achsen beschriftungen
barplot(nk/n, axis.lty=1, space=2, xlab="Zahl der Fehler",
        ylab="Relative  H?ufigkeit", main="Balkendiagramm", 
        col="darkgray")

# Summentreppe, eine treppenförmige Darstellung der kumulierten
# relativen Häufigkeiten. ECDF = empirische kumulative verteilungs
# funktion
# verticals = zeichne vertikale linien
# lwd = line width
# do.points = zeichne punkte bei werten
plot(ecdf(x), verticals=TRUE, do.points=FALSE, lwd=2,
     xlab="Zahl der Fehler", ylab=expression(hat(F)(x)),
     main="Summentreppe")
par(old.par)









# ue1.5
#=======
dat <- read.table("body.txt", header=TRUE)[,c(1,12,22,23,24,25)]
#  V1 = Biacromial diameter (Schulter) (cm)
# V12 = Waist girth (Taille) (cm)
# V22 = Age (years)
# V23 = Weight (kg)
# V24 = Height (cm)
# V25 = Gender (1 - male, 0 - female)

# setze bessere spaltennamen
names(dat) <- c("Biacromial","Waist","Age","Weight","Height","Gender")

# werte der frauen
datf <- subset(dat, Gender==0)

# werte der männer
datm <- subset(dat, Gender==1)

# [,2] gibt alle daten aus der zweiten spalte zurück (waist, V12)
ecdf.mf <- ecdf(dat[,2])
ecdf.m <- ecdf(datm[,2])
ecdf.f <- ecdf(datf[,2])

# sets graphical parameters and saves old values to old.par
# mar = margins des plots
old.par <- par(mar=c(5, 4.5, 4, 2) + 0.1)

# do.p = selbe wie do.points=FALSE, abkürzung
plot(ecdf.mf, do.p=F, verticals=T, xlab="Waist girth [cm]",
     ylab=expression(hat(F)[n](x)), lwd=1.5, 
     main="Empirische Verteilungsfunktion")

# zeichne werte der männer
lines(ecdf.m, do.p=F, verticals=T, col="darkblue", lwd=1.5)

# zeichne werte der frauen
lines(ecdf.f, do.p=F, verticals=T, col="red2", lwd=1.5)

# zeichne die legende
legend("bottomright", c("female","male","both"),
       lty=1, col=c("red2","darkblue","black"), lwd=1.5)

# lade alte graphical parameters
par(old.par)


# ue1.6
#=======
# Stem-and-leaf-Plot
#--------------------

# scale definiert in wieviele teile der stem aufgeteilt wird
stem(datm[,1], scale=2)
stem(datf[,1], scale=2)
stem(datf[,1])

# Back-to-Back Stem-and-Leaf Plot
#---------------------------------
install.packages("aplpack")
require(aplpack)
stem.leaf.backback(datf[,1], datm[,1], m=2, depths=F)









# ue1.7
#=======

# lade die werte
dat <- read.table("euroweight.txt", header=TRUE, skip=1)

# lade die spalten der table in variablen
attach(dat)

brk <- seq(7.200, 7.760, by=0.010)

# teilt den plot in ein 4x2 array auf
par(mfrow=c(4,2))

for (i in 1:8) {
  subdat <- weight[batch==i] 
  
  # erstellt ein histon gram der daten
  # breaks ist die einteilung der klassen
  # xlim = von bis auf der x achse
  # cex.main bestimmt die größe des titels
  hist(subdat, breaks=brk, freq=FALSE, main=paste("Batch",i), 
       xlim=c(7.35,7.65), xlab="weight [g]", col="lightgrey",
       cex.main=1)
  
  # gauss'sche kerndichteschätzung
  # ist eine (meist) symmetrische funktion um null, deren fläche eins ist.
  lines(density(subdat), lwd=2)
}

# setzt par optionen zurück
par(mfrow=c(1,1))
detach(dat)














# ue1.8
#=======
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

# Boxplot
# Quantile teilen einen datensatz in einem bestimmten verhältnis auf
# Der median ist das 50%-quantil
# Die Quartile im boxplot teilen den datensatz in etwa vier gleich große
# hälften, Q1 = x1/4, Q2 = x1/2 (median), Q3 = x3/4. zwischen dem 1. und 3.
# quartil liegen die mittleren 50% der daten
#
# Hinges - der untere hinge ist der median der ersten hälfte der (geordneten)
# daten, der obere hinge der median der zweiten hälfte, sie entsprechen dem
# 1. und 3. quartil 
# 
# Zuerst zeichnet man die box, also ein rechteck vom 1. zum 3. quartil, der
# median wird durch einen strich hervorgehoben, dann werden die fences
# bestimmt h = 1.5(Q3 - Q1) ... also das 1.5 fache höhe der box
# LF = Q1 - h, UP = Q3 + h
boxplot(Biacromial ~ Gender, data=dat, col="darkgrey",
        names=c("female","male"), pars=list(boxwex=0.6))
mtext("Biacromial [cm]", side=2, line=2.5)
mtext("Boxplots", side=3, line=1)

# Violinplot
# Ist eine kombination von boxplot und kerndichteschätzung
require(vioplot)
vioplot(datf$Biacromial, datm$Biacromial, col="lightgrey",
        names=c("female","male"))
mtext("Biacromial [cm]", side=2, line=2.5)
mtext("Violinplots", side=3, line=1)

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

# summary gibt min, Q1, Median (Q2), Mittelwert, Q3 und Max aus
by(Biacromial, Gender, summary)

# gibt tukeys five number summary aus, min, LH, median, UH, max
by(Biacromial, Gender, fivenum)

# varianz: Ist eine Kennzahl für die Charakterisierung des Streuungsverhaltens
# einer empirisch gegebenen Verteilung

# streuung: Die stichprobenstreuung ist die positive wurzel aus der varianz

# MAD: Mittlere absolute Abweichung ist ein natürliches streuungsmaß, hier summiert
# man die abstände von allen werten zum median und dividiert durch die menge der 
# werte
by(Biacromial, Gender, kennz, ro=3)
detach(dat)


# ue1.9
#=======
fivenum(1:100)
fivenum(c(1:75, (76:100)*10))
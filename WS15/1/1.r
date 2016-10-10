
getwd()

setwd("/home/raphi/code/st")

getwd()

dat <- read.csv("pkw-neuzul11.txt", header=TRUE, sep=";")


sorteddat <- dat[with(dat, order(TOTAL)),]
sorteddat
pie(sorteddat$TOTAL,bels = dat$GROUP)

?with
names(dat)
dat$GROUP

typeof(dat)
sort(dat)
?sort
attributes(dat)
dat <- order(dat)
dat
?pie
pie(dat$TOTAL,labels=dat$GROUP)
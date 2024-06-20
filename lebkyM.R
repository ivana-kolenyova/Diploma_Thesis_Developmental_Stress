## Nacitanie balickov

# install.packages("RRPP")
# install.packages("geomorph")
# install.packages("devtools")
# install.packages("shapes")
# install.packages("rgl")
# install.packages("ape")

library("RRPP")
library("geomorph")
library("devtools")
library("shapes")
library("rgl")
library("ape")

setwd("C:/Users/Asus/Desktop/Lebky")
DATA<-readland.tps("C:/Users/Asus/Desktop/Geomorph/muzi_data.tps",
                   specID="ID")
lnk<-as.matrix(read.table("C:/Users/Asus/Desktop/Geomorph/skuLinks.txt"))

class(DATA)
str(DATA)
dim(DATA)

dimnames(DATA)[[3]]
DATM<-two.d.array(DATA)
dim(DATM)

# vlozenie pohlavia a veku
sex<-as.factor(c("M","I","M","M","I","M","M","M","M","M",
                   "M","M","M","F","M","M","M","I","F","M","M",
                   "I","F","M","M","F","M","I","F","I","I","M",
                   "F","F","I","M","I","F","M","I","F","F","F","F","F",
                   "M","M","F","F","M","M","M"))
age.category<-as.factor(c("2","2","1","2","2","3","2","2","2","2","1","2","2",
                            "2","2","2","1","3","2","2","3","3","1","1","1","1",
                            "3","3","1","1","2","2","1","2","3","2","1","2","2",
                            "1","1","2","2","2","1","2","2","1","2","2","3","1"))

age<-data.frame(sex,age.category)
age<-age[age$sex=="M", "age.category"]
age==age.category
# error je dobry, kvoli rozdielom

# odhad chybajucich landmarkov
any(is.na(DATA))
ODHAD<-estimate.missing(DATA, method = c("TPS","Reg"))
any(is.na(ODHAD))

# 3D pole (ODHAD) na 2D maticu (DATMODHAD)
DATMODHAD <- two.d.array(ODHAD)
dim(DATMODHAD)

# vytvorenie geomorph.data.frame pre muzov
GPAODHAD<-gpagen(ODHAD,PrinAxes=FALSE)
gdfM<-geomorph.data.frame(GPAODHAD, age=age,specimen=dimnames(DATA)[[3]])
attributes(gdfM)

# 3D plot suradnic pre superpoziciu
# plotAllSpecimens(gdf$coords)
plotAllSpecimens(gdfM$coords, mean=TRUE, plot.param=list(pt.cex=0.1,mean.cex=6,
                                                        mean.bg="lightblue4"), lnk)


## PLOTS
ref <- mshape(gdfM$coords)

# zakryvenie tps priemerneho tvaru voci jedincovi 39
# plotRefToTarget(ref,gdf$coords[,,39],method="TPS")
# plotRefToTarget(ref,gdf$coords[,,39],mag=3, method="TPS")

# kde a ktorym smerom su najvacsie rozdiely voci jed. 39
# plotRefToTarget(ref,gdf$coords[,,39],method="vector", mag=3)

# porovnanie priem. tvaru voci jedincovi 39
# plotRefToTarget(ref,gdf$coords[,,39],method="points", mag=3, 
                #links = lnk)

# spravit data.frame - kazde vyzobrazit a dat vedla seba
# MUZI - 1., 2., 3.
means <- aggregate(two.d.array(gdfM$coords) ~ gdfM$age, FUN=mean)

# spravi zo strednych vektorov maticu
Allo.mn <- matrix(as.numeric(means[1,-1]), ncol=3, byrow=T)
ref <- mshape(gdfM$coords) # calculate mean shape
plotRefToTarget(ref, Allo.mn, mag=3,method="TPS")

# ANOVA
fit3 <- procD.lm(coords ~ age, data=gdfM, iter=0, print.progress = FALSE)
plotAllometry(fit3, size = gdfM$Csize, logsz = TRUE, method = "RegScore",
              pch = 19, col = as.numeric(gdfM$age))
summary(fit3)
# p-hodnota=1

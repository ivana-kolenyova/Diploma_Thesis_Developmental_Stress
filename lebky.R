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

setwd("C:/Users/Asus/Desktop/Geomorph")
DATA<-readland.tps("C:/Users/Asus/Desktop/Geomorph/koldata05.tps",
                   specID="ID")
lnk<-as.matrix(read.table("C:/Users/Asus/Desktop/Geomorph/skuLinks.txt"))

class(DATA)
str(DATA)
dim(DATA)

dimnames(DATA)[[3]]

# 3D pole (DATA) na 2D maticu (DATM)
DATM <- two.d.array(DATA)

# vypocet velkosti/dimenzionality matice DATM
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

# age<-data.frame(sex,age.category)
# age<-age[age$sex=="F", "age.category"]
# age==age.category

# zobrazenie kategorie oproti x suradnici prveho landmarku
# plot(age.category,DATM[,1])
# plot(sex,DATM[,1])

# odhad chybajucich landmarkov
any(is.na(DATA))
ODHAD<-estimate.missing(DATA, method = c("TPS","Reg"))
any(is.na(ODHAD))

# 3D pole (ODHAD) na 2D maticu (DATMODHAD)
DATMODHAD<-two.d.array(ODHAD)
dim(DATMODHAD)

# vytvorenie geomorph.data.frame pre vsetkych
GPAODHAD<-gpagen(ODHAD,PrinAxes=FALSE)
gdf<-geomorph.data.frame(GPAODHAD, age=age.category,specimen=dimnames(DATA)[[3]])
attributes(gdf)

# 3D plot suradnic pre superpoziciu
# plotAllSpecimens(gdf$coords)
plotAllSpecimens(gdf$coords, mean=TRUE, plot.param=list(pt.cex=0.1,mean.cex=6,
                                                        mean.bg="lightblue4"), lnk)


## ALOMETRIA
# jednoducha viacrozmerna regresia - korelacia tvaru s velkostou
fit1 <- procD.lm(coords ~ Csize, data=gdf, iter=0, print.progress = FALSE)
plotAllometry(fit1, size = gdf$Csize, logsz = TRUE, method = "RegScore",
              pch = 19, col = as.numeric(interaction(gdf$Csize)))
summary(fit1)
# p-hodnota=0.333 

# jednofaktorova MANOVA - tvar vs. pohlavie
fit2 <- procD.lm(coords ~ sex, data=gdf, iter=0, print.progress = FALSE)
plotAllometry(fit2, size = gdf$Csize, logsz = TRUE, method = "RegScore",
              pch = 19, col = as.numeric(gdf$sex))
summary(fit2)
# p-hodnota=0.333

# jednofaktorova MANOVA - tvar vs. vek
fit3 <- procD.lm(coords ~ age, data=gdf, iter=0, print.progress = FALSE)
plotAllometry(fit3, size = gdf$Csize, logsz = TRUE, method = "RegScore",
              pch = 19, col = as.numeric(gdf$age))
summary(fit3)
# p-hodnota=1

# viacfaktorova MANOVA - tvar vs. pohlavie a vek
fit4 <- procD.lm(coords ~ Csize * sex * age, data=gdf, iter=0, print.progress = FALSE)
plotAllometry(fit4, size = gdf$Csize, logsz = TRUE, method = "RegScore",
              pch = 19, col = as.numeric(interaction(gdf$Csize,gdf$sex, gdf$age)))
summary(fit4)
# toto okrem ineho vratilo podobne hodnoty, ale aj zaporne


## PLOTS
ref <- mshape(gdf$coords)
# plot(ref,lnk)

# zakryvenie tps priemerneho tvaru voci jedincovi 39
# plotRefToTarget(ref,gdf$coords[,,39],method="TPS")
# plotRefToTarget(ref,gdf$coords[,,39],mag=3, method="TPS")

# kde a ktorym smerom su najvacsie rozdiely voci jed. 39
# plotRefToTarget(ref,gdf$coords[,,39],method="vector", mag=3)

# porovnanie priem. tvaru voci jedincovi 39
# plotRefToTarget(ref,gdf$coords[,,39],method="points", mag=3, 
                #links = lnk)

# spravit data.frame - kazde vyzobrazit a dat vedla seba
# subor
means <- aggregate(two.d.array(gdf$coords) ~ gdf$age, FUN=mean)

# spravi zo strednych vektorov maticu
Allo.mn <- matrix(as.numeric(means[1,-1]), ncol=3, byrow=T)
ref <- mshape(gdf$coords) # pocita priemerny tvar
plotRefToTarget(ref, Allo.mn, mag=3,method="TPS")

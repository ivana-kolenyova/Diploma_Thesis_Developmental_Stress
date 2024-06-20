setwd("C:/Users/Asus/Desktop/Vlo�i�")
anov<-read.delim2("zbierky.txt", header=TRUE, sep="\t")
attach(anov)
library(stringr)

pokus<-as.data.frame(str_split_fixed(anov$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
dataP<-cbind(anov, pokus)
dataA<-data.frame(dataP[dataP$measur!="b",])

str(dataA)
summary(dataA)

library(nortest)
library(car)

data1<-dataA[dataA$age.category %in% c("1","2","3"), c("age.category", "sex", "vbl1")]
data1$age.category<-factor(data1$age.category)
summary(data1)


## VYSKA SPODINY LEBECNEJ U MUZOV vs. VEK
# H0: vek (faktor) nema vplyv na vysku spodiny lebecnej
# H1: vek ma vplyv na vysku spodiny lebecnej
# testovanie strednej hodnoty VSL medzi roznymi vekovymi kategoriami

dataM<-data1[data1$sex %in% c("M"), c("age.category", "vbl1")]
dataM
m1<-mean(dataM[dataM$age.category=="1", "vbl1"])  
m2<-mean(dataM[dataM$age.category=="2", "vbl1"])
m3<-mean(dataM[dataM$age.category=="3", "vbl1"])
m<-mean(dataM[,"vbl1"])
sd1<-sd(dataM[dataM$age.category=="1", "vbl1"])  
sd2<-sd(dataM[dataM$age.category=="2", "vbl1"])
sd3<-sd(dataM[dataM$age.category=="3", "vbl1"])
sd<-sd(dataM[,"vbl1"])

boxplot(vbl1~age.category, data=dataM, var.width=T, noch=T, xlab="Vekov� skupina - mu�i",
        ylab="V��ka spodiny lebe�nej")
points(1:3, c(m1,m2,m3), col="red", pch=16)

qqPlot(dataM[dataM$age.category=="1","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek <25", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(dataM[dataM$age.category=="1","vbl1"])
# normalita potvrdena

qqPlot(dataM[dataM$age.category=="2","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek 25-50", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(dataM[dataM$age.category=="2","vbl1"])
# normalita potvrdena

qqPlot(dataM[dataM$age.category=="3","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek >50", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(dataM[dataM$age.category=="3","vbl1"])
# normalita potvrdena

# overenie homogenity dat - hladina vyznamnosti 0.05
bartlett.test(vbl1~age.category, data=dataM)
leveneTest(vbl1~age.category, data=dataM)
# hypotezu o homogenite rozptylov dat nezamietame - poziadavky na anovu splnene

aov<-lm(vbl1~age.category, data=dataM)
anova(aov)
# nezamietame hypotezu, ze vek (muzi) nema vplyv na vysku spodiny lebecnej


## VYSKA SPODINY LEBECNEJ U ZIEN vs. VEK
# H0: vek (faktor) nema vplyv na vysku spodiny lebecnej
# H1: vek ma vplyv na vysku spodiny lebecnej
# testovanie strednej hodnoty VSL medzi roznymi vekovymi kategoriami

dataF<-data1[data1$sex %in% c("F"), c("age.category", "vbl1")]
dataF
m1<-mean(dataF[dataF$age.category=="1", "vbl1"])  
m2<-mean(dataF[dataF$age.category=="2", "vbl1"])
m3<-mean(dataF[dataF$age.category=="3", "vbl1"])
m<-mean(dataF[,"vbl1"])
sd1<-sd(dataF[dataF$age.category=="1", "vbl1"])  
sd2<-sd(dataF[dataF$age.category=="2", "vbl1"])
sd3<-sd(dataF[dataF$age.category=="3", "vbl1"])
sd<-sd(dataF[,"vbl1"])

boxplot(vbl1~age.category, data=dataF, var.width=T, noch=T, xlab="Vekov� skupina - �eny",
        ylab="V��ka spodiny lebe�nej")
points(1:3, c(m1,m2,m3), col="red", pch=16)

qqPlot(dataF[dataF$age.category=="1","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek <25", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(dataF[dataF$age.category=="1","vbl1"])
# normalita potvrdena len graficky - malo jedincov

qqPlot(dataF[dataF$age.category=="2","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek 25-50", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(dataF[dataF$age.category=="2","vbl1"])
# normalita potvrdena

qqPlot(dataF[dataF$age.category=="3","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek >50", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(dataF[dataF$age.category=="3","vbl1"])
# normalita potvrdena len graficky, na shapiro.test malo jedincov

# overenie homogenity rozptylu dat - hladina vyznamnosti 0.05
bartlett.test(vbl1~age.category, data=dataF)
leveneTest(vbl1~age.category, data=dataF)
# hypotezu o homogenite rozptylov dat nezamietame - poziadavky na anovu splnene

aov<-lm(vbl1~age.category, data=dataF)
anova(aov)
# skupinovy sucet stvrcov SA=8.99, pocet stupnov volnosti fA=2, 
# rezidualny sucet stvorcov SE=2608.14, pocet stupnov volnosti fE=127,
# testovacia statistika FA=0.2189, p-hodnota 0.8037
# nezamietame hypotezu, ze vek (zeny) nema vplyv na vysku spodiny lebecnej

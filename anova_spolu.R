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


## VYSKA SPODINY LEBECNEJ vs. VEK
# H0: vek (faktor) nema vplyv na vysku spodiny lebecnej
# H1: vek ma vplyv na vysku spodiny lebecnej
# testovanie strednej hodnoty VSL medzi roznymi vekovymi kategoriami

data1<-dataA[dataA$age.category %in% c("1","2","3"), c("age.category", "vbl1")]
data1$age.category<-factor(data1$age.category)
summary(data1)

m1<-mean(data1[data1$age.category=="1", "vbl1"])  
m2<-mean(data1[data1$age.category=="2", "vbl1"])
m3<-mean(data1[data1$age.category=="3", "vbl1"])
m<-mean(data1[,"vbl1"])
sd1<-sd(data1[data1$age.category=="1", "vbl1"])  
sd2<-sd(data1[data1$age.category=="2", "vbl1"])
sd3<-sd(data1[data1$age.category=="3", "vbl1"])
sd<-sd(data1[,"vbl1"])

boxplot(vbl1~age.category, data=data1, var.width=T, noch=T, xlab="Vekov� skupina",
        ylab="V��ka spodiny lebe�nej")
points(1:3, c(m1,m2,m3), col="red", pch=16)

qqPlot(data1[data1$age.category=="1","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek <25", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(data1[data1$age.category=="1","vbl1"])
lillie.test(data1[data1$age.category=="1","vbl1"])
# normalita potvrdena

qqPlot(data1[data1$age.category=="2","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek 25-50", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(data1[data1$age.category=="2","vbl1"])
lillie.test(data1[data1$age.category=="2","vbl1"])
# normalita potvrdena

qqPlot(data1[data1$age.category=="3","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "Vek >50", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(data1[data1$age.category=="3","vbl1"])
lillie.test(data1[data1$age.category=="3","vbl1"])
# normalita potvrdena

# overenie homogenity dat - hladina vyznamnosti 0.05
bartlett.test(vbl1~age.category, data=data1)
leveneTest(vbl1~age.category, data=data1)
# hypotezu o homogenite rozptylov dat nezamietame - poziadavky na anovu splnene

aov<-lm(vbl1~age.category, data=data1)
anova(aov)
# skupinovy sucet stvrcov SA=8.99, pocet stupnov volnosti fA=2, 
# rezidualny sucet stvorcov SE=2608.14, pocet stupnov volnosti fE=127,
# testovacia statistika FA=0.2189, p-hodnota 0.8037
# nezamietame hypotezu, ze vek nema vplyv na vysku spodiny lebecnej

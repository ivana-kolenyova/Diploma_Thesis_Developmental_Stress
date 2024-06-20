setwd("C:/Users/Asus/Desktop/Hotov�")
anov<-read.delim2("testy.txt", header=TRUE, sep="\t")
attach(anov)
library(stringr)

# uprava dat, po nej n=130
pokus<-as.data.frame(str_split_fixed(anov$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
dataP<-cbind(anov, pokus)
dataA<-data.frame(dataP[dataP$measur!="b",])
data<-dataA[,-c(6, 22:160)]
data<-na.omit(data)
str(data)

library(nortest)
library(car)

# HO: stredna hodnota VSL sa medzi pohlaviami nelisi
# H1: stredna hodnota VSL sa medzi pohlaviami lisi

DATA<-data[data$sex %in% c("M","F"), c("sex", "vbl1")]
DATA$sex<-factor(DATA$sex)
summary(DATA)

table(DATA$sex)
# F=35, M=85
sum(table(DATA$sex))
# spolu 120
m1<-mean(DATA[DATA$sex=="M", "vbl1"])  
m1
m2<-mean(DATA[DATA$sex=="F", "vbl1"])
m2
m<-mean(DATA[,"vbl1"])
sd1<-sd(DATA[DATA$sex=="M", "vbl1"])  
sd2<-sd(DATA[DATA$sex=="F", "vbl1"])
sd<-sd(DATA[,"vbl1"])

boxplot(vbl1~sex, data=DATA, var.width=T, notch=T, xlab="Pohlavie",
        ylab="V��ka spodiny lebe�nej")
points(1:2, c(m2,m1), col="red",pch=16)

qqPlot(DATA[DATA$sex=="M","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "VSL - mu�i", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(DATA[DATA$sex=="M","vbl1"])
lillie.test(DATA[DATA$sex=="M","vbl1"])
# u muzov je potvrdena normalita dat graficky aj na zaklade p-hodnot

qqPlot(DATA[DATA$sex=="F","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "VSL - �eny", col = "red2")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(DATA[DATA$sex=="F","vbl1"]) # p-honota=0.4264
lillie.test(DATA[DATA$sex=="F","vbl1"]) # p-hodnota=0.3281
# u zien je tiez overena normalita dat

# overenie homogenity dat
bartlett.test(vbl1~sex, data=DATA)
leveneTest(vbl1~sex, data=DATA)
# hypotezu o homogenite rozptylov nezamietame
# poziadavky na anovu splnene (p>0.05)

aov<-aov(vbl1~sex, data=DATA)
anova(aov)
# skupinovy sucet stvrcov SA=83.73, pocet stupnov volnosti fA=1, 
# rezidualny sucet stvorcov SE=2290.95, pocet stupnov volnosti fE=118,
# testovacia statistika FA=4.3126, p-hodnota 0.04,
# hypotezu zamietame - vyska spodiny lebecnej sa medzi pohlaviami lisi (sexualny dimorfizmus)

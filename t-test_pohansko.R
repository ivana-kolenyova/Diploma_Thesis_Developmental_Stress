# porovnanie prvej a druhej vekovej katergorie u muzov
# eliminacia tretej kvoli malemu poctu jedincov,
# kvoli malemu poctu zien a sex.dimorf. hodnotime len jedno pohlavie

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


## VYSKA SPODINY LEBECNEJ U MUZOV
# porovnanie stredny hodnot VSL medzi 1. a 2. vek.kategoriou
# H0: m1-m2=0 - rozdiel strednych hodnot je rovny 0
# H1: m1-m2 sa nerovna 0 - rozdiel strednych hodnot nie je rovny 0
# testovanie strednej hodnoty VSL medzi roznymi vekovymi kategoriami

data1<-dataA[dataA$age.category %in% c("1","2"), c("age.category","sex","sample","vbl1")]
data1$age.category<-factor(data1$age.category)
summary(data1)

dataP<-data1[data1$sample %in% c("P"), c("age.category","sex","vbl1")]
dataM<-dataP[dataP$sex %in% c("M"), c("age.category","vbl1")]

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

t.test(dataM[dataM$age.category=="1","vbl1"], 
             dataM[dataM$age.category=="2","vbl1"], paired=F)
# p-hodnota>0.05 nezamietame hypotezu o zhode strednych hodnot

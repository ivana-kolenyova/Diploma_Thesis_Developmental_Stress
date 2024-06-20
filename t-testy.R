setwd("C:/Users/Asus/Desktop/Hotov�")
euklid<-read.delim2("testy.txt", header=TRUE, sep="\t")
attach(euklid)
data1<-euklid[,-c(2, 26:160)]
data2<-data1[-c(107:185),]

library(stringr)
pokus<-as.data.frame(str_split_fixed(data2$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
data<-cbind(data2, pokus)
str(data)

dataA<-data.frame(data[data$measur=="a",])
dataB<-data.frame(data[data$measur=="b",])

dim(dataA)
dim(dataB)
sum(dataA$sex=="M")
sum(dataA$sex=="F")
sum(dataA$sex=="I")
sum(dataB$sex=="M")
sum(dataB$sex=="F")
sum(dataB$sex=="I")


## Priemerne dlzky a smerodatne odchylky rucneho merania (A,B)
# vyska spodiny lebecnej
# A
mean(dataA$vbl2)
sd(dataA$vbl2)
min(dataA$vbl2)
max(dataA$vbl2)
mean(dataA$vbl2[dataA$sex=="F"])
sd(dataA$vbl2[dataA$sex=="F"])
min(dataA$vbl2[dataA$sex=="F"])
max(dataA$vbl2[dataA$sex=="F"])
mean(dataA$vbl2[dataA$sex=="M"])
sd(dataA$vbl2[dataA$sex=="M"])
min(dataA$vbl2[dataA$sex=="M"])
max(dataA$vbl2[dataA$sex=="M"])

# B
mean(dataB$vbl2)
sd(dataB$vbl2)
min(dataB$vbl2)
max(dataB$vbl2)
mean(dataB$vbl2[dataB$sex=="F"])
sd(dataB$vbl2[dataB$sex=="F"])
min(dataB$vbl2[dataB$sex=="F"])
max(dataB$vbl2[dataB$sex=="F"])
mean(dataB$vbl2[dataB$sex=="M"])
sd(dataB$vbl2[dataB$sex=="M"])
min(dataB$vbl2[dataB$sex=="M"])
max(dataB$vbl2[dataB$sex=="M"])

# sirka spodiny lebecnej
# A
mean(dataA$podx.posin2)
sd(dataA$podx.posin2)
min(dataA$podx.posin2)
max(dataA$podx.posin2)
mean(dataA$podx.posin2[dataA$sex=="F"])
sd(dataA$podx.posin2[dataA$sex=="F"])
min(dataA$podx.posin2[dataA$sex=="F"])
max(dataA$podx.posin2[dataA$sex=="F"])
mean(dataA$podx.posin2[dataA$sex=="M"])
sd(dataA$podx.posin2[dataA$sex=="M"])
min(dataA$podx.posin2[dataA$sex=="M"])
max(dataA$podx.posin2[dataA$sex=="M"])

# B
mean(dataB$podx.posin2)
sd(dataB$podx.posin2)
min(dataB$podx.posin2)
max(dataB$podx.posin2)
mean(dataB$podx.posin2[dataB$sex=="F"])
sd(dataB$podx.posin2[dataB$sex=="F"])
min(dataB$podx.posin2[dataB$sex=="F"])
max(dataB$podx.posin2[dataB$sex=="F"])
mean(dataB$podx.posin2[dataB$sex=="M"])
sd(dataB$podx.posin2[dataB$sex=="M"])
min(dataB$podx.posin2[dataB$sex=="M"])
max(dataB$podx.posin2[dataB$sex=="M"])

# priemerna vzdialenost bodu basion k porion
# A
mean(dataA$mean2)
sd(dataA$mean2)
min(dataA$mean2)
max(dataA$mean2)
mean(dataA$mean2[dataA$sex=="F"])
sd(dataA$mean2[dataA$sex=="F"])
min(dataA$mean2[dataA$sex=="F"])
max(dataA$mean2[dataA$sex=="F"])
mean(dataA$mean2[dataA$sex=="M"])
sd(dataA$mean2[dataA$sex=="M"])
min(dataA$mean2[dataA$sex=="M"])
max(dataA$mean2[dataA$sex=="M"])

# B
mean(dataB$mean2)
sd(dataB$mean2)
min(dataB$mean2)
max(dataB$mean2)
mean(dataB$mean2[dataB$sex=="F"])
sd(dataB$mean2[dataB$sex=="F"])
min(dataB$mean2[dataB$sex=="F"])
max(dataB$mean2[dataB$sex=="F"])
mean(dataB$mean2[dataB$sex=="M"])
sd(dataB$mean2[dataB$sex=="M"])
min(dataB$mean2[dataB$sex=="M"])
max(dataB$mean2[dataB$sex=="M"])


## Priemerne dlzky a smerodatne odchylky 3D suradnic bodov (A,B)
# vyska spodiny lebecnej
# A
mean(dataA$vbl1)
sd(dataA$vbl1)
min(dataA$vbl1)
max(dataA$vbl1)
mean(dataA$vbl1[dataA$sex=="F"])
sd(dataA$vbl1[dataA$sex=="F"])
min(dataA$vbl1[dataA$sex=="F"])
max(dataA$vbl1[dataA$sex=="F"])
mean(dataA$vbl1[dataA$sex=="M"])
sd(dataA$vbl1[dataA$sex=="M"])
min(dataA$vbl1[dataA$sex=="M"])
max(dataA$vbl1[dataA$sex=="M"])

# B
mean(dataB$vbl1)
sd(dataB$vbl1)
min(dataB$vbl1)
max(dataB$vbl1)
mean(dataB$vbl1[dataB$sex=="F"])
sd(dataB$vbl1[dataB$sex=="F"])
min(dataB$vbl1[dataB$sex=="F"])
max(dataB$vbl1[dataB$sex=="F"])
mean(dataB$vbl1[dataB$sex=="M"])
sd(dataB$vbl1[dataB$sex=="M"])
min(dataB$vbl1[dataB$sex=="M"])
max(dataB$vbl1[dataB$sex=="M"])

# sirka spodiny lebecnej
# A
mean(dataA$podx.posin1)
sd(dataA$podx.posin1)
min(dataA$podx.posin1)
max(dataA$podx.posin1)
mean(dataA$podx.posin1[dataA$sex=="F"])
sd(dataA$podx.posin1[dataA$sex=="F"])
min(dataA$podx.posin1[dataA$sex=="F"])
max(dataA$podx.posin1[dataA$sex=="F"])
mean(dataA$podx.posin1[dataA$sex=="M"])
sd(dataA$podx.posin1[dataA$sex=="M"])
min(dataA$podx.posin1[dataA$sex=="M"])
max(dataA$podx.posin1[dataA$sex=="M"])

# B
mean(dataB$podx.posin1)
sd(dataB$podx.posin1)
min(dataB$podx.posin1)
max(dataB$podx.posin1)
mean(dataB$podx.posin1[dataB$sex=="F"])
sd(dataB$podx.posin1[dataB$sex=="F"])
min(dataB$podx.posin1[dataB$sex=="F"])
max(dataB$podx.posin1[dataB$sex=="F"])
mean(dataB$podx.posin1[dataB$sex=="M"])
sd(dataB$podx.posin1[dataB$sex=="M"])
min(dataB$podx.posin1[dataB$sex=="M"])
max(dataB$podx.posin1[dataB$sex=="M"])

# priemerna vzdialenost bodu basion k porion
# A
mean(dataA$mean1)
sd(dataA$mean1)
min(dataA$mean1)
max(dataA$mean1)
mean(dataA$mean1[dataA$sex=="F"])
sd(dataA$mean1[dataA$sex=="F"])
min(dataA$mean1[dataA$sex=="F"])
max(dataA$mean1[dataA$sex=="F"])
mean(dataA$mean1[dataA$sex=="M"])
sd(dataA$mean1[dataA$sex=="M"])
min(dataA$mean1[dataA$sex=="M"])
max(dataA$mean1[dataA$sex=="M"])

# B
mean(dataB$mean1)
sd(dataB$mean1)
min(dataB$mean1)
max(dataB$mean1)
mean(dataB$mean1[dataB$sex=="F"])
sd(dataB$mean1[dataB$sex=="F"])
min(dataB$mean1[dataB$sex=="F"])
max(dataB$mean1[dataB$sex=="F"])
mean(dataB$mean1[dataB$sex=="M"])
sd(dataB$mean1[dataB$sex=="M"])
min(dataB$mean1[dataB$sex=="M"])
max(dataB$mean1[dataB$sex=="M"])


## CIELE
# H0: m1-m2=0 - rozdiel strednych hodnot je rovny 0
# H1: m1-m2 sa nerovna 0 - rozdiel strednych hodnot nie je rovny 0
# pocet jedincov >30, tak na overenie normality pouzijeme lillie.test doplneny ad.testom
# testovanie normality dat na hladine vyznamnosti 0.05
# H0: data pochadzaju z normalneho rozdelenia
# H1: data nepochadzaju z normalneho rozdelenia
library(nortest)
library(car)

qqPlot(dataA$podx.posin2)
qqPlot(dataB$podx.posin2)
qqPlot(dataA$podx.posin1)
qqPlot(dataB$podx.posin1)

dataA1<-dataA[-c(8,24),]
dataB1<-dataB[-c(8,24),]

## RUCNE - graficke znazornenie nezamieta hypotezu o normalite dat (niektore hodnoty odlahle)
# A
lillie.test(dataA1$vbl2)
shapiro.test(dataA1$vbl2)
# p-hodnota>0.05 = nezamietame
qqnorm(dataA1$vbl2, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "cadetblue4")
qqline(dataA1$vbl2, col = "cadetblue4")
mtext(bquote(paste("Vyska spodiny lebecnej - rucne A1")), line = 0.3, cex = 0.8)

lillie.test(dataA1$podx.posin2)
# p-hodnota<0.05 = zamietame
shapiro.test(dataA1$podx.posin2)
# p-hodnota>0.05 = nezamietame
qqnorm(dataA1$podx.posin2, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "cadetblue4")
qqline(dataA1$podx.posin2, col = "cadetblue4")
mtext(bquote(paste("Sirka spodiny lebecnej - rucne A1")), line = 0.3, cex = 0.8)

lillie.test(dataA1$mean2)
shapiro.test(dataA1$mean2)
# p-hodnota>0.05 = nezamietame
qqnorm(dataA1$mean2, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "cadetblue4")
qqline(dataA1$mean2, col = "cadetblue4")
mtext(bquote(paste("Priemerna hodnota dlzky basion-porion - rucne A1")), line = 0.3, cex = 0.8)

# B
lillie.test(dataB1$vbl2)
shapiro.test(dataB1$vbl2)
# p-hodnota>0.05 = nezamietame
qqnorm(dataB1$vbl2, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "cadetblue4")
qqline(dataB1$vbl2, col = "cadetblue4")
mtext(bquote(paste("Vyska spodiny lebecnej - rucne B1")), line = 0.3, cex = 0.8)

lillie.test(dataB1$podx.posin2)
shapiro.test(dataB1$podx.posin2)
# p-hodnota>0.05 = nezamietame
qqnorm(dataB1$podx.posin2, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "cadetblue4")
qqline(dataB1$podx.posin2, col = "cadetblue4")
mtext(bquote(paste("Sirka spodiny lebecnej - rucne B1")), line = 0.3, cex = 0.8)

lillie.test(dataB1$mean2)
shapiro.test(dataB1$mean2)
# p-hodnota>0.05 = nezamietame
qqnorm(dataB1$mean2, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "cadetblue4")
qqline(dataB1$mean2, col = "cadetblue4")
mtext(bquote(paste("Priemerna hodnota dlzky basion-porion - rucne B1")), line = 0.3, cex = 0.8)


## 3D SURADNICE - graficke znazornenie nezamieta hypotezu o normalite dat (niektore hodnoty odlahle)
# A
lillie.test(dataA1$vbl1)
shapiro.test(dataA1$vbl1)
# p-hodnota>0.05 = nezamietame
qqnorm(dataA1$vbl1, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "chocolate1")
qqline(dataA1$vbl1, col = "chocolate1")
mtext(bquote(paste("Vyska spodiny lebecnej - 3D suradnice A1")), line = 0.3, cex = 0.8)

lillie.test(dataA1$podx.posin1)
shapiro.test(dataA1$podx.posin1)
# p-hodnota>0.05 = nezamietame
qqnorm(dataA1$podx.posin1, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "chocolate1")
qqline(dataA1$podx.posin1, col = "chocolate1")
mtext(bquote(paste("Sirka spodiny lebecnej - 3D suradnice A1")), line = 0.3, cex = 0.8)

lillie.test(dataA1$mean1)
shapiro.test(dataA1$mean1)
# p-hodnota>0.05 = nezamietame
qqnorm(dataA1$mean1, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "chocolate1")
qqline(dataA1$mean1, col = "chocolate1")
mtext(bquote(paste("Priemerna hodnota dlzky basion-porion - 3D suradnice A1")), line = 0.3, cex = 0.8)

# B
lillie.test(dataB1$vbl1)
shapiro.test(dataB1$vbl1)
# p-hodnota>0.05 = nezamietame
qqnorm(dataB1$vbl1, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "chocolate1")
qqline(dataB1$vbl1, col = "chocolate1")
mtext(bquote(paste("Vyska spodiny lebecnej - 3D suradnice B1")), line = 0.3, cex = 0.8)

lillie.test(dataB1$podx.posin1)
shapiro.test(dataB1$podx.posin1)
# p-hodnota>0.05 = nezamietame
qqnorm(dataB1$podx.posin1, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "chocolate1")
qqline(dataB1$podx.posin1, col = "chocolate1")
mtext(bquote(paste("Sirka spodiny lebecnej - 3D suradnice B1")), line = 0.3, cex = 0.8)

lillie.test(dataB1$mean1)
shapiro.test(dataB1$mean1)
# p-hodnota>0.05 = nezamietame
qqnorm(dataB1$mean1, xlab = "teoreticky kvantil", ylab = "vyberovy kvantil",
       main = "Q-Q graf", col = "chocolate1")
qqline(dataB1$mean1, col = "chocolate1")
mtext(bquote(paste("Priemerna hodnota dlzky basion-porion - 3D suradnice B1")), line = 0.3, cex = 0.8)

library(car)
qqPlot(dataA$podx.posin2)
qqPlot(dataB$podx.posin2)
qqPlot(dataA$podx.posin1)
qqPlot(dataB$podx.posin1)

# po odstraneni dvoch odlahlych hodnot zo suboru hypotezu o normalite dat nezamietame
# data teda pochadzaju z normalneho rozdelenia a 
# odlahle hodnoty ponechame pri analyze variability spodiny lebecnej
# k analyze pouzijeme (dvojvyberovy) parovy t-test - parametricky


## RUCNE
# 1.1. rozdiel m vysky spodiny lebecnej 1. a 2. merania je rovny 0
t.test(dataA1$vbl2, dataB1$vbl2, paired=T)
# p>0.05 - nezamietame - rozdiel strednych hodnot je rovny 0

# 1.2. rozdiel m sirsky SL 1. a 2. merania je rovny 0
t.test(dataA1$podx.posin2, dataB1$podx.posin2, paired=T)
# p>0.05 - nezamietame - rozdiel strednych hodnot je rovny 0

# 1.3. rozdiel m priemernej vzd. bodu ba-po 1. a 2. merania je rovny 0
t.test(dataA1$mean2, dataB1$mean2, paired=T)
# >0.05 - nezamietame - rozdiel strednych hodnot je rovny 0
# prve a druhe rucne meranie sa od seba vyznamne lelisi


## 3D SURADNICE BODOV
# 2.1. rozdiel m vysky SL 3D suradnic bodov 1. a 2. snimania je rovny 0
t.test(dataA1$vbl1, dataB1$vbl1, paired=T)
# p>0.05 - nezamietame - rozdiel strednych hodnot je rovny 0

# 2.2. rozdiel m sirsky SL 3D suradnic bodov 1. a 2. snimania je rovny 0
t.test(dataA1$podx.posin1, dataB1$podx.posin1, paired=T)
# p>0.05 - nezamietame - rozdiel strednych hodnot je rovny 0

# 2.3. rozdiel m priemernej vzd. bodu ba-po 1. a 2. snimania je rovny 0
t.test(dataA1$mean1, dataB1$mean1, paired=T)
# p>0.05 - nezamietame - rozdiel strednych hodnot je rovny 0
# prve a druhe meranie 3D suradnic bodov sa od seba vyznamne nelisi


## RUCNE S 3D SURADNICAMI BODOV 
# 3.1.a. rozdiel m vysky SL 1. rucneho a 1. digitalneho merania je rovny 0
t.test(dataA1$vbl2, dataA1$vbl1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.1.b. rozdiel m vysky SL 1. rucneho a 2. digitalneho merania je rovny 0
t.test(dataA1$vbl2, dataB1$vbl1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.1.c. rozdiel m vysky SL 2. rucneho a 1. digitalneho merania je rovny 0
t.test(dataB1$vbl2, dataA1$vbl1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.1.d. rozdiel m vysky SL 2. rucneho a 2. digitalneho merania je rovny 0
t.test(dataB1$vbl2, dataB1$vbl1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0
# rozdiel strednych hodnot oboch merani rucneho aj 3D snimania bodov 
# vysky spodiny lebecnej sa nerovnaju 0 - stredne hodnoty sa od seba vyznamne lisia

# 3.2.a. rozdiel m sirky SL 1. rucneho a 1. digitalneho merania je rovny 0
t.test(dataA1$podx.posin2, dataA1$podx.posin1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.2.b. rozdiel m sirky SL 1. rucneho a 2. digitalneho merania je rovny 0
t.test(dataA1$podx.posin2, dataB1$podx.posin1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.2.c. rozdiel m sirky SL 2. rucneho a 1. digitalneho merania je rovny 0
t.test(dataB1$podx.posin2, dataA1$podx.posin1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.2.d. rozdiel m sirky SL 2. rucneho a 2. digitalneho merania je rovny 0
t.test(dataB1$podx.posin2, dataB1$podx.posin1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0
# rozdiel strednych hodnot oboch merani rucneho aj 3D snimania bodov 
# sirky spodiny lebecnej sa nerovnaju 0 - stredne hodnoty sa od seba vyznamne lisia

# 3.3.a. rozdiel m priem.vzd. ba-po 1. rucneho a 1. digitalneho merania je rovny 0
t.test(dataA1$mean2, dataA1$mean1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.3.b. rozdiel m priem.vzd. ba-po 1. rucneho a 2. digitalneho merania je rovny 0
t.test(dataA1$mean2, dataB1$mean1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.3.c. rozdiel m priem.vzd. ba-po 2. rucneho a 1. digitalneho merania je rovny 0
t.test(dataB1$mean2, dataA1$mean1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0

# 3.3.d. rozdiel m priem.vzd. ba-po 2. rucneho a 2. digitalneho merania je rovny 0
t.test(dataB1$mean2, dataB1$mean1, paired=T)
# p<0.05 - zamietame - rozdiel strednych hodnot nie je rovny 0
# rozdiel strednych hodnot oboch merani rucneho aj 3D snimania bodov 
# priem.vzd. ba-po sa nerovnaju 0 - stredne hodnoty sa od seba vyznamne lisia

# zistili sme, ze nie su rozdiely medzi rovnakym sposobom zberu dat pri opakovanom merani
# vyznamne su vask rozdiely pri odlisnom sposobe zberu dat, na zaklade odlisnosti
# zistenej medzi rucnym a 3D snimanim suradnic bodov

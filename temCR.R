setwd("C:/Users/Asus/Desktop/V�sledky")
temCR<-read.delim2("testy.txt", header=TRUE, sep="\t")
attach(temCR)
data1<-temCR[,-c(2, 26:160)]
data2<-data1[-c(107:185),]

library(stringr)
pokus<-as.data.frame(str_split_fixed(data2$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
data<-cbind(data2, pokus)
str(data)

dataA<-data.frame(data[data$measur=="a",])
dataB<-data.frame(data[data$measur=="b",])


## RUCNE
# 1.1. rozdiel m vysky spodiny lebecnej 1. a 2. merania
# rozdiel d
(dataA$vbl2)-(dataB$vbl2)
# mocnina rozdielu
((dataA$vbl2)-(dataB$vbl2))^2
# sucet mocniny rozdielu
sum(((dataA$vbl2)-(dataB$vbl2))^2)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.3885143 mm
# stupen presnosti merania pri opakovanom merani
sqrt(sum(((dataA$vbl2)-(dataB$vbl2))^2)/(2*length(dataA$vbl2)))
# priemer celeho suboru - 19.79245 mm
mean(c(dataA$vbl2, dataB$vbl2))
## relTEM - technicka chyba je mensia ako 5% - tvori 1.962942 % z TEM
((sqrt(sum(((dataA$vbl2)-(dataB$vbl2))^2)/
         (2*length(dataA$vbl2))))/(mean(c(dataA$vbl2, dataB$vbl2))))*100
## sd - 5.5079777
sd(c(dataA$vbl2, dataB$vbl2))
## CR - 0.9941504 - podiel variancie
1-((sqrt(sum(((dataA$vbl2)-(dataB$vbl2))^2)/(2*length(dataA$vbl2))))^2/
  (sd(c(dataA$vbl2, dataB$vbl2)))^2)

# 1.2. rozdiel m sirsky SL 1. a 2. merania
(dataA$podx.posin2)-(dataB$podx.posin2)
((dataA$podx.posin2)-(dataB$podx.posin2))^2
sum(((dataA$podx.posin2)-(dataB$podx.posin2))^2)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.4952606 mm
sqrt(sum(((dataA$podx.posin2)-(dataB$podx.posin2))^2)/(2*length(dataA$podx.posin2)))
# priemer - 116.566 mm
mean(c(dataA$podx.posin2, dataB$podx.posin2))
## relTEM - technicka chyba je mensia ako 5% - tvori 0.4248755 % z TEM
((sqrt(sum(((dataA$podx.posin2)-(dataB$podx.posin2))^2)/
         (2*length(dataA$podx.posin2))))/(mean(c(dataA$podx.posin2, dataB$podx.posin2))))*100
## sd - 5.68711
sd(c(dataA$podx.posin2, dataB$podx.posin2))
## CR - 0.9924162 - podiel variancie
1-((sqrt(sum(((dataA$podx.posin2)-(dataB$podx.posin2))^2)/(2*length(dataA$podx.posin2))))^2/
     (sd(c(dataA$podx.posin2, dataB$podx.posin2)))^2)

# 1.3. rozdiel m priemernej vzd. bodu ba-po 1. a 2. merania je rovny 0
(dataA$mean2)-(dataB$mean2)
((dataA$mean2)-(dataB$mean2))^2
sum(((dataA$mean2)-(dataB$mean2))^2)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.2950195 mm
sqrt(sum(((dataA$mean2)-(dataB$mean2))^2)/(2*length(dataA$mean2)))
# priemer - 61.74616
mean(c(dataA$mean2, dataB$mean2))
## relTEM - technicka chyba je mensia ako 5% - tvori 0.4777941 % z TEM
((sqrt(sum(((dataA$mean2)-(dataB$mean2))^2)/
         (2*length(dataA$mean2))))/(mean(c(dataA$mean2, dataB$mean2))))*100
## sd - 3.118605
sd(c(dataA$mean2, dataB$mean2))
## CR - 0.9910509 - podiel variancie
1-((sqrt(sum(((dataA$mean2)-(dataB$mean2))^2)/(2*length(dataB$mean2))))^2/
     (sd(c(dataA$mean2, dataB$mean2)))^2)


## 3D SURADNICE BODOV
# 2.1. rozdiel m vysky SL 3D suradnic bodov 1. a 2. snimania
(dataA$vbl1)-(dataB$vbl1)
((dataA$vbl1)-(dataB$vbl1))^2
sum(((dataA$vbl1)-(dataB$vbl1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.2800031 mm
sqrt(sum(((dataA$vbl1)-(dataB$vbl1))^2)/(2*length(dataA$vbl1)))
# priemer - 19.2004 mm
mean(c(dataA$vbl1, dataB$vbl1))
## relTEM - technicka chyba je mensia ako 5% - tvori 1.458319 % z TEM
((sqrt(sum(((dataA$vbl1)-(dataB$vbl1))^2)/
         (2*length(dataA$vbl1))))/(mean(c(dataA$vbl1, dataB$vbl1))))*100
## sd - 5.139304 mm
sd(c(dataA$vbl1, dataB$vbl1))
## CR - 0.9970316 - podiel variancie
1-((sqrt(sum(((dataA$vbl1)-(dataB$vbl1))^2)/(2*length(dataA$vbl1))))^2/
     (sd(c(dataA$vbl1, dataB$vbl1)))^2)

# 2.2. rozdiel m sirsky SL 3D suradnic bodov 1. a 2. snimania
(dataA$podx.posin1)-(dataB$podx.posin1)
((dataA$podx.posin1)-(dataB$podx.posin1))^2
sum(((dataA$podx.posin1)-(dataB$podx.posin1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.4714937 mm
sqrt(sum(((dataA$podx.posin1)-(dataB$podx.posin1))^2)/(2*length(dataA$podx.posin1)))
# priemer - 118.5854
mean(c(dataA$podx.posin1, dataB$podx.posin1))
## relTEM - technicka chyba je mensia ako 5% - tvori 0.3975984 % z TEM
((sqrt(sum(((dataA$podx.posin1)-(dataB$podx.posin1))^2)/
         (2*length(dataA$podx.posin1))))/(mean(c(dataA$podx.posin1, dataB$podx.posin1))))*100
## sd - 5.65743
sd(c(dataA$podx.posin1, dataB$podx.posin1))
## CR - 0.9930543 - podiel variancie
1-((sqrt(sum(((dataA$podx.posin1)-(dataB$podx.posin1))^2)/(2*length(dataA$podx.posin1))))^2/
     (sd(c(dataA$podx.posin1, dataB$podx.posin1)))^2)

# 2.3. rozdiel m priemernej vzd. bodu ba-po 1. a 2. snimania
(dataA$mean1)-(dataB$mean1)
((dataA$mean1)-(dataB$mean1))^2
sum(((dataA$mean1)-(dataB$mean1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.2537352 mm
sqrt(sum(((dataA$mean1)-(dataB$mean1))^2)/(2*length(dataA$mean1)))
# priemer - 62.51984
mean(c(dataA$mean1, dataB$mean1))
## relTEM - technicka chyba je mensia ako 5% - tvori 0.4058476 % z TEM
((sqrt(sum(((dataA$mean1)-(dataB$mean1))^2)/
         (2*length(dataA$mean1))))/(mean(c(dataA$mean1, dataB$mean1))))*100
## sd - 3.154297
sd(c(dataA$mean1, dataB$mean1))
## CR - 0.9935292 - podiel variancie
1-((sqrt(sum(((dataA$mean1)-(dataB$mean1))^2)/(2*length(dataA$mean1))))^2/
     (sd(c(dataA$mean1, dataB$mean1)))^2)


## RUCNE S 3D SURADNICAMI BODOV 
# 3.1.a. rozdiel m vysky SL 1. rucneho a 1. digitalneho merania
(dataA$vbl2)-(dataA$vbl1)
((dataA$vbl2)-(dataA$vbl1))^2
sum(((dataA$vbl2)-(dataA$vbl1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.6102279 mm
sqrt(sum(((dataA$vbl2)-(dataA$vbl1))^2)/(2*length(dataA$vbl2)))
# priemer - 19.50015
mean(c(dataA$vbl2, dataA$vbl1))
## relTEM - technicka chyba je mensia ako 5% - tvori 3.12935 % z TEM
((sqrt(sum(((dataA$vbl2)-(dataA$vbl1))^2)/
         (2*length(dataA$vbl2))))/(mean(c(dataA$vbl2, dataA$vbl1))))*100
## sd - 5.109966
sd(c(dataA$vbl2, dataA$vbl1))
## CR - 0.9857391 - podiel variancie
1-((sqrt(sum(((dataA$vbl2)-(dataA$vbl1))^2)/(2*length(dataA$vbl2))))^2/
     (sd(c(dataA$vbl2, dataA$vbl1)))^2)

# 3.1.b. rozdiel m vysky SL 1. rucneho a 2. digitalneho merania
(dataA$vbl2)-(dataB$vbl1)
((dataA$vbl2)-(dataB$vbl1))^2
sum(((dataA$vbl2)-(dataB$vbl1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.5931379 mm
sqrt(sum(((dataA$vbl2)-(dataB$vbl1))^2)/(2*length(dataA$vbl2)))
# priemer - 19.51157
mean(c(dataA$vbl2, dataB$vbl1))
## relTEM - technicka chyba je mensia ako 5% - tvori 3.039929 % z TEM
((sqrt(sum(((dataA$vbl2)-(dataB$vbl1))^2)/
         (2*length(dataA$vbl2))))/(mean(c(dataA$vbl2, dataB$vbl1))))*100
## sd - 5.122847
sd(c(dataA$vbl2, dataB$vbl1))
## CR - 0.9865943 - podiel variancie
1-((sqrt(sum(((dataA$vbl2)-(dataB$vbl1))^2)/(2*length(dataA$vbl2))))^2/
     (sd(c(dataA$vbl2, dataB$vbl1)))^2)

# 3.1.c. rozdiel m vysky SL 2. rucneho a 1. digitalneho merania
(dataB$vbl2)-(dataA$vbl1)
((dataB$vbl2)-(dataA$vbl1))^2
sum(((dataB$vbl2)-(dataA$vbl1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.593761 mm
sqrt(sum(((dataB$vbl2)-(dataA$vbl1))^2)/(2*length(dataB$vbl2)))
# priemer - 19.48128
mean(c(dataB$vbl2, dataA$vbl1))
## relTEM - technicka chyba je mensia ako 5% - tvori 3.047854 % z TEM
((sqrt(sum(((dataB$vbl2)-(dataA$vbl1))^2)/
         (2*length(dataB$vbl2))))/(mean(c(dataB$vbl2, dataA$vbl1))))*100
## sd - 5.113657
sd(c(dataB$vbl2, dataA$vbl1))
## CR - 0.9865178 - podiel variancie
1-((sqrt(sum(((dataB$vbl2)-(dataA$vbl1))^2)/(2*length(dataB$vbl2))))^2/
     (sd(c(dataB$vbl2, dataA$vbl1)))^2)

# 3.1.d. rozdiel m vysky SL 2. rucneho a 2. digitalneho merania
(dataB$vbl2)-(dataB$vbl1)
((dataB$vbl2)-(dataB$vbl1))^2
sum(((dataB$vbl2)-(dataB$vbl1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.5930088 mm
sqrt(sum(((dataB$vbl2)-(dataB$vbl1))^2)/(2*length(dataB$vbl2)))
# priemer - 19.4927
mean(c(dataB$vbl2, dataB$vbl1))
##  relTEM - technicka chyba je mensia ako 5% - tvori 3.042209 % z TEM
((sqrt(sum(((dataB$vbl2)-(dataB$vbl1))^2)/
         (2*length(dataB$vbl2))))/(mean(c(dataB$vbl2, dataB$vbl1))))*100
## sd - 5.126572
sd(c(dataB$vbl2, dataB$vbl1))
## CR - 0.9866196 - podiel variancie
1-((sqrt(sum(((dataB$vbl2)-(dataB$vbl1))^2)/(2*length(dataB$vbl2))))^2/
     (sd(c(dataB$vbl2, dataB$vbl1)))^2)


# 3.2.a. rozdiel m sirky SL 1. rucneho a 1. digitalneho merania
(dataA$podx.posin2)-(dataA$podx.posin1)
((dataA$podx.posin2)-(dataA$podx.posin1))^2
sum(((dataA$podx.posin2)-(dataA$podx.posin1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 1.503474 mm
sqrt(sum(((dataA$podx.posin2)-(dataA$podx.posin1))^2)/(2*length(dataA$podx.posin2)))
# priemer - 117.6137
mean(c(dataA$podx.posin2, dataA$podx.posin1))
## relTEM - technicka chyba je mensia ako 5% - tvori 1.278316 % z TEM
((sqrt(sum(((dataA$podx.posin2)-(dataA$podx.posin1))^2)/
         (2*length(dataA$podx.posin2))))/(mean(c(dataA$podx.posin2, dataA$podx.posin1))))*100
## sd - 5.748129
sd(c(dataA$podx.posin2, dataA$podx.posin1))
## CR - 0.931587 - podiel variancie
1-((sqrt(sum(((dataA$podx.posin2)-(dataA$podx.posin1))^2)/(2*length(dataA$podx.posin2))))^2/
     (sd(c(dataA$podx.posin2, dataA$podx.posin1)))^2)

# 3.2.b. rozdiel m sirky SL 1. rucneho a 2. digitalneho merania
(dataA$podx.posin2)-(dataB$podx.posin1)
((dataA$podx.posin2)-(dataB$podx.posin1))^2
sum(((dataA$podx.posin2)-(dataB$podx.posin1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 1.474808 mm
sqrt(sum(((dataA$podx.posin2)-(dataB$podx.posin1))^2)/(2*length(dataA$podx.posin2)))
# priemer - 117.5755
mean(c(dataA$podx.posin2, dataB$podx.posin1))
## relTEM - technicka chyba je mensia ako 5% - tvori 1.25435 % z TEM
((sqrt(sum(((dataA$podx.posin2)-(dataB$podx.posin1))^2)/
         (2*length(dataA$podx.posin2))))/(mean(c(dataA$podx.posin2, dataB$podx.posin1))))*100
## sd - 5.74107
sd(c(dataA$podx.posin2, dataB$podx.posin1))
## CR - 0.9340089 - podiel variancie
1-((sqrt(sum(((dataA$podx.posin2)-(dataB$podx.posin1))^2)/(2*length(dataA$podx.posin2))))^2/
     (sd(c(dataA$podx.posin2, dataB$podx.posin1)))^2)

# 3.2.c. rozdiel m sirky SL 2. rucneho a 1. digitalneho merania
(dataB$podx.posin2)-(dataA$podx.posin1)
((dataB$podx.posin2)-(dataA$podx.posin1))^2
sum(((dataB$podx.posin2)-(dataA$podx.posin1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 1.56953 mm
sqrt(sum(((dataB$podx.posin2)-(dataA$podx.posin1))^2)/(2*length(dataB$podx.posin2)))
# priemer - 117.5759
mean(c(dataB$podx.posin2, dataA$podx.posin1))
## relTEM - technicka chyba je mensia ako 5% - tvori 1.334907 % z TEM
((sqrt(sum(((dataB$podx.posin2)-(dataA$podx.posin1))^2)/
         (2*length(dataB$podx.posin2))))/(mean(c(dataB$podx.posin2, dataA$podx.posin1))))*100
## sd - 5.783442
sd(c(dataB$podx.posin2, dataA$podx.posin1))
## CR - 0.9263511 - podiel variancie
1-((sqrt(sum(((dataB$podx.posin2)-(dataA$podx.posin1))^2)/(2*length(dataB$podx.posin2))))^2/
     (sd(c(dataB$podx.posin2, dataA$podx.posin1)))^2)

# 3.2.d. rozdiel m sirky SL 2. rucneho a 2. digitalneho merania
(dataB$podx.posin2)-(dataB$podx.posin1)
((dataB$podx.posin2)-(dataB$podx.posin1))^2
sum(((dataB$podx.posin2)-(dataB$podx.posin1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 1.522645 mm
sqrt(sum(((dataB$podx.posin2)-(dataB$podx.posin1))^2)/(2*length(dataB$podx.posin2)))
# priemer - 117.5378
mean(c(dataB$podx.posin2, dataB$podx.posin1))
## relTEM - technicka chyba je mensia ako 5% - tvori 1.295452 % z TEM
((sqrt(sum(((dataB$podx.posin2)-(dataB$podx.posin1))^2)/
         (2*length(dataB$podx.posin2))))/(mean(c(dataB$podx.posin2, dataB$podx.posin1))))*100
## sd - 5.776175
sd(c(dataB$podx.posin2, dataB$podx.posin1))
## CR - 0.9305109 - podiel variancie
1-((sqrt(sum(((dataB$podx.posin2)-(dataB$podx.posin1))^2)/(2*length(dataB$podx.posin2))))^2/
     (sd(c(dataB$podx.posin2, dataB$podx.posin1)))^2)


# 3.3.a. rozdiel m priem.vzd. ba-po 1. rucneho a 1. digitalneho merania
(dataA$mean2)-(dataA$mean1)
((dataA$mean2)-(dataA$mean1))^2
sum(((dataA$mean2)-(dataA$mean1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.6004762 mm
sqrt(sum(((dataA$mean2)-(dataA$mean1))^2)/(2*length(dataA$mean2)))
# priemer - 62.15159
mean(c(dataA$mean2, dataA$mean1))
## relTEM - technicka chyba je mensia ako 5% - tvori 0.9661477 % z TEM
((sqrt(sum(((dataA$mean2)-(dataA$mean1))^2)/
         (2*length(dataA$mean2))))/(mean(c(dataA$mean2, dataA$mean1))))*100
## sd - 3.152559
sd(c(dataA$mean2, dataA$mean1))
## CR - 0.9637202 - podiel variancie
1-((sqrt(sum(((dataA$mean2)-(dataA$mean1))^2)/(2*length(dataA$mean2))))^2/
     (sd(c(dataA$mean2, dataA$mean1)))^2)

# 3.3.b. rozdiel m priem.vzd. ba-po 1. rucneho a 2. digitalneho merania
(dataA$mean2)-(dataB$mean1)
((dataA$mean2)-(dataB$mean1))^2
sum(((dataA$mean2)-(dataB$mean1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.5861368 mm
sqrt(sum(((dataA$mean2)-(dataB$mean1))^2)/(2*length(dataA$mean2)))
# priemer - 62.13676
mean(c(dataA$mean2, dataB$mean1))
## relTEM - technicka chyba je mensia ako 5% - tvori 0.9433013 % z TEM
((sqrt(sum(((dataA$mean2)-(dataB$mean1))^2)/
         (2*length(dataA$mean2))))/(mean(c(dataA$mean2, dataB$mean1))))*100
## sd - 3.174317
sd(c(dataA$mean2, dataB$mean1))
## CR - 0.9659045 - podiel variancie
1-((sqrt(sum(((dataA$mean2)-(dataB$mean1))^2)/(2*length(dataA$mean2))))^2/
     (sd(c(dataA$mean2, dataB$mean1)))^2)

# 3.3.c. rozdiel m priem.vzd. ba-po 2. rucneho a 1. digitalneho merania
(dataB$mean2)-(dataA$mean1)
((dataB$mean2)-(dataA$mean1))^2
sum(((dataB$mean2)-(dataA$mean1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.6416768 mm
sqrt(sum(((dataB$mean2)-(dataA$mean1))^2)/(2*length(dataB$mean2)))
# priemer - 62.12923
mean(c(dataB$mean2, dataA$mean1))
## relTEM - technicka chyba je mensia ako 5% - tvori 1.03281 % z TEM
((sqrt(sum(((dataB$mean2)-(dataA$mean1))^2)/
         (2*length(dataB$mean2))))/(mean(c(dataB$mean2, dataA$mean1))))*100
## sd - 3.146603
sd(c(dataB$mean2, dataA$mean1))
## CR - 0.9584138 - podiel variancie
1-((sqrt(sum(((dataB$mean2)-(dataA$mean1))^2)/(2*length(dataB$mean2))))^2/
     (sd(c(dataB$mean2, dataA$mean1)))^2)

# 3.3.d. rozdiel m priem.vzd. ba-po 2. rucneho a 2. digitalneho merania
(dataB$mean2)-(dataB$mean1)
((dataB$mean2)-(dataB$mean1))^2
sum(((dataB$mean2)-(dataB$mean1))^1)
## TEM - na jedno meranie pripada technicka priemerna chyba 0.628818 mm
sqrt(sum(((dataB$mean2)-(dataB$mean1))^2)/(2*length(dataB$mean2)))
# priemer - 62.1144
mean(c(dataB$mean2, dataB$mean1))
## relTEM - technicka chyba je mensia ako 5% - tvori 1.012355 % z TEM
((sqrt(sum(((dataB$mean2)-(dataB$mean1))^2)/
         (2*length(dataB$mean2))))/(mean(c(dataB$mean2, dataB$mean1))))*100
## sd - 3.168297
sd(c(dataB$mean2, dataB$mean1))
## CR - 0.9606089 - podiel variancie
1-((sqrt(sum(((dataB$mean2)-(dataB$mean1))^2)/(2*length(dataB$mean2))))^2/
     (sd(c(dataB$mean2, dataB$mean1)))^2)

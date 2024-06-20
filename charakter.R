setwd("C:/Users/Asus/Desktop/V�sledky")
korel<-read.delim2("testy.txt", header=TRUE, sep="\t")
attach(korel)
library(stringr)

# uprava dat
pokus<-as.data.frame(str_split_fixed(korel$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
dataP<-cbind(korel, pokus)
dataA<-data.frame(dataP[dataP$measur!="b",])
data<-dataA[,-c(26:160)]
str(data)
na.omit(data)
summary(data)

dim(data)
table(data$sex=="M")
table(data$sex=="F")
table(data$sex=="I")


## SUBOR
# vyska spodiny lebecnej
mean(data$vbl1)
sd(data$vbl1)
min(data$vbl1)
max(data$vbl1)

# sirka spodiny lebecnej
mean(data$podx.posin1)
sd(data$podx.posin1)
min(data$podx.posin1)
max(data$podx.posin1)

# priemerna vzdialenost bodu ba-po
mean(data$mean1)
sd(data$mean1)
min(data$mean1)
max(data$mean1)


## MUZI
# vyska spodiny lebecnej
mean(data$vbl1[data$sex=="M"], na.rm=TRUE)
sd(data$vbl1[data$sex=="M"], na.rm=TRUE)
min(data$vbl1[data$sex=="M"], na.rm=TRUE)
max(data$vbl1[data$sex=="M"], na.rm=TRUE)

# sirka spodiny lebecnej
mean(data$podx.posin1[data$sex=="M"], na.rm=TRUE)
sd(data$podx.posin1[data$sex=="M"], na.rm=TRUE)
min(data$podx.posin1[data$sex=="M"], na.rm=TRUE)
max(data$podx.posin1[data$sex=="M"], na.rm=TRUE)

# priemerna vzdialenost bodu ba-po
mean(data$mean1[data$sex=="M"], na.rm=TRUE)
sd(data$mean1[data$sex=="M"], na.rm=TRUE)
min(data$mean1[data$sex=="M"], na.rm=TRUE)
max(data$mean1[data$sex=="M"], na.rm=TRUE)


## ZENY
# vyska spodiny lebecnej
mean(data$vbl1[data$sex=="F"], na.rm=TRUE)
sd(data$vbl1[data$sex=="F"], na.rm=TRUE)
min(data$vbl1[data$sex=="F"], na.rm=TRUE)
max(data$vbl1[data$sex=="F"], na.rm=TRUE)

# sirka spodiny lebecnej
mean(data$podx.posin1[data$sex=="F"], na.rm=TRUE)
sd(data$podx.posin1[data$sex=="F"], na.rm=TRUE)
min(data$podx.posin1[data$sex=="F"], na.rm=TRUE)
max(data$podx.posin1[data$sex=="F"], na.rm=TRUE)

# priemerna vzdialenost bodu ba-po
mean(data$mean1[data$sex=="F"], na.rm=TRUE)
sd(data$mean1[data$sex=="F"], na.rm=TRUE)
min(data$mean1[data$sex=="F"], na.rm=TRUE)
max(data$mean1[data$sex=="F"], na.rm=TRUE)


## INDIFERENTNI
# vyska spodiny lebecnej
mean(data$vbl1[data$sex=="I"], na.rm=TRUE)
sd(data$vbl1[data$sex=="I"], na.rm=TRUE)
min(data$vbl1[data$sex=="I"], na.rm=TRUE)
max(data$vbl1[data$sex=="I"], na.rm=TRUE)

# sirka spodiny lebecnej
mean(data$podx.posin1[data$sex=="I"], na.rm=TRUE)
sd(data$podx.posin1[data$sex=="I"], na.rm=TRUE)
min(data$podx.posin1[data$sex=="I"], na.rm=TRUE)
max(data$podx.posin1[data$sex=="I"], na.rm=TRUE)

# priemerna vzdialenost bodu ba-po
mean(data$mean1[data$sex=="I"], na.rm=TRUE)
sd(data$mean1[data$sex=="I"], na.rm=TRUE)
min(data$mean1[data$sex=="I"], na.rm=TRUE)
max(data$mean1[data$sex=="I"], na.rm=TRUE)

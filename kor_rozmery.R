setwd("C:/Users/Asus/Desktop/Hotov�")
korel<-read.delim2("rozmery.txt", header=TRUE, sep="\t")
attach(korel)
library(stringr)

pokus<-as.data.frame(str_split_fixed(korel$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
dataP<-cbind(korel, pokus)
data<-data.frame(dataP[dataP$measur!="b",])
str(data)

dataK<-data.frame(data$height, data$podx.posin1, data$vbl1, data$skull.L,
                  data$skull.H, data$bl.L, data$fm.L, data$fm.W, data$ast.W,
                  data$ssq.W, data$ba.g.L)
summary(dataK)


## ZAVISLOST VYSKY, SIRKY a DLZKY SPODINY LEB., TELESNEJ VYSKY,
# DLZKY A SIRKY LEBKY A FM, VZD. BODOV AST, SSHSQ, a DLZKY BODOV BA-G
# H1: veliciny nie su navzajom korelovane
# H0: veliciny su navzajom korelovane

# testovanie hypotezy o nulovosti korelacnych koeficientov
library(Hmisc)

colnames(dataK)[colnames(dataK) %in% c("data.height","data.podx.posin1",
                                    "data.vbl1","data.skull.L","data.skull.H",
                                    "data.bl.L","data.fm.L","data.fm.W","data.ast.W","data.ssq.W",
                                    "data.ba.g.L")]<- c("tel.v��ka","SSL","VSL","d�ka.lebky",
                                                        "v��ka.lebky","d�ka.SL","d�ka.fom",
                                                        "s�rka.fomL","��rka.ast","��rka.sphsq",
                                                        "vzd.ba.g")

DATA<-rcorr(as.matrix(dataK))

library(corrplot)
library(RColorBrewer)
windows()
# koeficient korelacie
corrplot(DATA$r, type = "upper", order = "hclust", tl.col = "blacK", tl.srt = 45)

# koef.korelacie, p-hodnota (sig.level = 0.05 - hladina vyznamnosti)
corrplot(DATA$r, type="upper", p.mat = DATA$P, sig.level = 0.05, insig = "pch")
corrplot(DATA$r, type="upper", p.mat = DATA$P, sig.level = 0.05, insig = "blank")

# zobrazene
corrplot(DATA$r, type="upper", order="hclust", tl.col="black", tl.srt=45,
         p.mat=DATA$P, sig.level=0.05, insig="blank",
         col=brewer.pal(n=11, name="RdYlBu"), diag=FALSE,
         addCoef.col = "black")

setwd("C:/Users/Asus/Desktop/Lebky")
data<-read.table("kol_soubory.txt", header=TRUE, sep="\t")

data<-data[-c(2652:2753, 3672:3773, 3774:3875, 5100:5201),]
attach(data)
na.omit(data)

podx.x<-data[LM.49 == "po.dx", "X"]
podx.y<-data[LM.49 == "po.dx", "Y"]
podx.z<-data[LM.49 == "po.dx", "Z"]
POdx<-data.frame(podx.x, podx.y, podx.z)
POdx

posin.x<-data[LM.49 == "po.sin", "X"]
posin.y<-data[LM.49 == "po.sin", "Y"]
posin.z<-data[LM.49 == "po.sin", "Z"]
POsin<-data.frame(posin.x, posin.y, posin.z)
POsin

b.x<-data[LM.49 == "b", "X"]
b.y<-data[LM.49 == "b", "Y"]
b.z<-data[LM.49 == "b", "Z"]
B<-data.frame(b.x, b.y, b.z)
B

ba.x<-data[LM.49 == "ba", "X"]
ba.y<-data[LM.49 == "ba", "Y"]
ba.z<-data[LM.49 == "ba", "Z"]
BA<-data.frame(ba.x, ba.y, ba.z)
BA

n.x<-data[LM.49 == "n", "X"]
n.y<-data[LM.49 == "n", "Y"]
n.z<-data[LM.49 == "n", "Z"]
N<-data.frame(n.x, n.y, n.z)
N

i.x<-data[LM.49 == "i", "X"]
i.y<-data[LM.49 == "i", "Y"]
i.z<-data[LM.49 == "i", "Z"]
I<-data.frame(i.x, i.y, i.z)
I

o.x<-data[LM.49 == "o", "X"]
o.y<-data[LM.49 == "o", "Y"]
o.z<-data[LM.49 == "o", "Z"]
O<-data.frame(o.x, o.y, o.z)
O

g.x<-data[LM.49 == "g", "X"]
g.y<-data[LM.49 == "g", "Y"]
g.z<-data[LM.49 == "g", "Z"]
G<-data.frame(g.x, g.y, g.z)
G

astdx.x<-data[LM.49 == "ast.dx", "X"]
astdx.y<-data[LM.49 == "ast.dx", "Y"]
astdx.z<-data[LM.49 == "ast.dx", "Z"]
ASTdx<-data.frame(astdx.x, astdx.y, astdx.z)
ASTdx

astsin.x<-data[LM.49 == "ast.sin", "X"]
astsin.y<-data[LM.49 == "ast.sin", "Y"]
astsin.z<-data[LM.49 == "ast.sin", "Z"]
ASTsin<-data.frame(astsin.x, astsin.y, astsin.z)
ASTsin

fomLdx.x<-data[LM.49 == "fomL.dx", "X"]
fomLdx.y<-data[LM.49 == "fomL.dx", "Y"]
fomLdx.z<-data[LM.49 == "fomL.dx", "Z"]
fomLdx<-data.frame(fomLdx.x, fomLdx.y, fomLdx.z)
fomLdx

fomLsin.x<-data[LM.49 == "fomL.sin", "X"]
fomLsin.y<-data[LM.49 == "fomL.sin", "Y"]
fomLsin.z<-data[LM.49 == "fomL.sin", "Z"]
fomLsin<-data.frame(fomLsin.x, fomLsin.y, fomLsin.z)
fomLsin

sphsqsin.x<-data[LM.49 == "sphsq.sin", "X"]
sphsqsin.y<-data[LM.49 == "sphsq.sin", "Y"]
sphsqsin.z<-data[LM.49 == "sphsq.sin", "Z"]
sphSQsin<-data.frame(sphsqsin.x, sphsqsin.y, sphsqsin.z)
sphSQsin

sphsqdx.x<-data[LM.49 == "sphsq.dx", "X"]
sphsqdx.y<-data[LM.49 == "sphsq.dx", "Y"]
sphsqdx.z<-data[LM.49 == "sphsq.dx", "Z"]
sphSQdx<-data.frame(sphsqdx.x, sphsqdx.y, sphsqdx.z)
sphSQdx

POdx=as.matrix(POdx)
POsin=as.matrix(POsin)
BA=as.matrix(BA)
N=as.matrix(N)
I=as.matrix(I)
O=as.matrix(O)
B=as.matrix(B)
G=as.matrix(G)
ASTdx=as.matrix(ASTdx)
ASTsin=as.matrix(ASTsin)
fomLdx=as.matrix(fomLdx)
fomLsin=as.matrix(fomLsin)
sphSQsin=as.matrix(sphSQsin)
sphSQdx=as.matrix(sphSQdx)

DATA<-data.frame(POdx, POsin, BA, B, G)
data[grepl("ID", data[,"LM.49"]),"LM.49"]

DATA<-data.frame(POdx, POsin, BA, N, I, O, B, G, ASTdx, ASTsin, fomLdx, 
                 fomLsin, sphSQsin, sphSQdx)
data[grepl("ID", data[,"LM.49"]),"LM.49"]

# ulozenie dat do excelu
install.packages("writexl")
library(writexl)
write_xlsx(x = DATA, path = "Landmarky.xslx", col_names = TRUE)


## Euclid.vzdialenost
BA.POdx.diff = BA-POdx
BA.POsin.diff = BA-POsin
POdx.POsin.diff = POdx-POsin
N.I.diff = N-I
N.BA.diff = N-BA
BA.O.diff = BA-O
ASTsin.ASTdx.diff = ASTsin-ASTdx
fomLsin.fomLdx.diff = fomLsin-fomLdx
sphSQsin.sphSQdx.diff = sphSQsin-sphSQdx
BA.B.diff = BA-B
BA.G.diff= BA-G

euclid.vzdalenost = function(d){return(sqrt(sum(d^2)))}

euclid.vzdalenost = function(d){
  dist=(sqrt(sum(d^2)))
  return(dist)
}

# overenie vzdialenosti = 62.01506
euclid.vzdalenost(BA.POdx.diff[1,])
euclid.vzdalenost(BA.POsin.diff[1,])
euclid.vzdalenost(POdx.POsin.diff[1,])
euclid.vzdalenost(N.I.diff[1,])
euclid.vzdalenost(N.BA.diff[1,])
euclid.vzdalenost(BA.O.diff[1,])
euclid.vzdalenost(ASTsin.ASTdx.diff[1,])
euclid.vzdalenost(fomLsin.fomLdx.diff[1,])
euclid.vzdalenost(sphSQsin.sphSQdx.diff[1,])
euclid.vzdalenost(BA.B.diff[1,])
euclid.vzdalenost(BA.G.diff[1,])

dist.POdx.BA = apply(BA.POdx.diff, 1, euclid.vzdalenost)
POdx.BA = dist.POdx.BA

dist.POsin.BA = apply(BA.POsin.diff, 1, euclid.vzdalenost)
POsin.BA = dist.POsin.BA

dist.POdx.POsin = apply(POdx.POsin.diff, 1, euclid.vzdalenost)
POdx.POsin = dist.POdx.POsin

dist.N.I = apply(N.I.diff, 1, euclid.vzdalenost)
skull.L = dist.N.I

dist.N.BA = apply(N.BA.diff, 1, euclid.vzdalenost)
length.N.BA = dist.N.BA

dist.BA.O = apply(BA.O.diff, 1, euclid.vzdalenost)
length.FM = dist.BA.O

dist.ASTsin.ASTdx = apply(ASTsin.ASTdx.diff, 1, euclid.vzdalenost)
width.AST = dist.ASTsin.ASTdx

dist.fomLsin.fomLdx = apply(fomLsin.fomLdx.diff, 1, euclid.vzdalenost)
width.FM = dist.fomLsin.fomLdx

dist.sphSQsin.sphSQdx = apply(sphSQsin.sphSQdx.diff, 1, euclid.vzdalenost)
width.SSQ = dist.sphSQsin.sphSQdx

dist.BA.B = apply(BA.B.diff, 1, euclid.vzdalenost)
height.L = dist.BA.B

dist.BA.G = apply(BA.G.diff, 1, euclid.vzdalenost)
length.BA.G = dist.BA.G


hodn<-data.frame(skull.L, length.N.BA, length.FM, width.AST, width.FM,
                 width.SSQ, height.L, length.BA.G)
write_xlsx(x = hodn, path = "Hodnoty.xlsx", col_names = TRUE)


library(circular)

#### SPODINA LEBECNA - vypocty, hodnoty, uhly, histogram
# vzdialenost bodov po.dx a po.sin - sirka spodiny lebecnej
width.PO = dist.POdx.POsin
width.PO/2

# vnutorny uhol
uhol.cos.veta = function(a, b, c){return(acos((-c^2+a^2+b^2)/(2*(a*b))))}
uhol.cos.veta(dist.POdx.BA, dist.POsin.BA, dist.POdx.POsin)

rad.na.stupne = function(rad){
  return(180/pi*rad)
}

stupne.na.rad = function(stup){
  return(pi/180*stup)
}  

print(rad.na.stupne(2*pi))
print(stupne.na.rad(360))

# kontrola fukncii
stopifnot(round(rad.na.stupne(stupne.na.rad(100))) == 100)

uhol.rad.BA = uhol.cos.veta(dist.POdx.BA, dist.POsin.BA, dist.POdx.POsin)
uhol.rad.BA
uhol.rad.POdx = uhol.cos.veta(dist.POdx.BA, dist.POdx.POsin, dist.POsin.BA)
uhol.rad.POdx
uhol.rad.POsin = uhol.cos.veta(dist.POsin.BA, dist.POdx.POsin, dist.POdx.BA)
uhol.rad.POsin

base = rad.na.stupne(uhol.rad.BA)

# kontrola - uhol je biologicky realny (uhol u ba je vetsi nez 130 stupnov)
stopifnot(all(base > 130))
base>130
# 4 lebky s mensim uhlom nez 130 stupnov
# ID=Z_H313_a, ID=Z_H313_b, ID=H_H815_a, ID=H_H815_b, ID=H_H818_a, ID=H_H818_b, ID=D_H98_a, ID=D_H98_b
# vsetky lebky su uzsie s vacsou hodnotou vysky spodiny lebecnej oproti ostatnym lebkam
# Z_H313, H_H818, H_H815, D_H98 


## HISTOGRAMY (spodina lebecna)
windows(9,4)
par(mar=c(0,0,0,0), mfcol=c(1,3))

x = circular(uhol.rad.POdx, type="angles", units="radians", template="none", rotation="counter")
rose.diag(x, col="cornflowerblue", prop=2, shrink=1)
lines(density.circular(x, bw=7.5))
mtext("uhol v bode POdx", side=1, line=-3)

x = circular(uhol.rad.POsin, type="angles", units="radians", template="none", rotation="counter")
rose.diag(x, col="cornflowerblue", prop=2, shrink=1)
lines(density.circular(x, bw=7.5))
mtext("uhol v bode POsin", side=1, line=-3)

x = circular(uhol.rad.BA, type="angles", units="radians", template="none", rotation="counter")
rose.diag(x, col="cadetblue", prop=2, shrink=1)
lines(density.circular(x, bw=7.5))
mtext("uhol v bode BA", side=1, line=-3)


## KORELACNY KOEFICIENT
# vyska spodiny lebecnej
height.BA = sin(uhol.rad.POsin)*dist.POsin.BA
height.BA
# vyska sedi s hodnotami z excelu nameranymi rucne

# LIN-UHLOVY KORELACNY KOEFICIENT (vyska SL, vyska lebky)
r.lin.uhl = function(x, y){
  # linearne-uhlovy Pearsonuv korelacny koeficient
  rxc = cor(x, cos(y))
  rxs = cor(x, sin(y))
  rsc = cor(cos(y), sin(y))
  
  r = sqrt((rxc^2 + rxs^2 - 2*rxc*rxs*rsc) / (1 - rsc^2))
  return(r)
}

base.A.rad = stupne.na.rad(height.BA)

r.skull.base = r.lin.uhl(height.L, base.A.rad)
stopifnot(r.skull.base > 0.6)
cat("Linearne-uhlovy korelacny koeficient - subor:", r.skull.base)
# 0.7947889
# vysoky stupen zavislosti medzi uhlom zvierajucim linie, ktore
# prechadzaju bodom basion a bodmi porion a vyskou lebky


# LIN-UHLOVY KORELACNY KOEFICIENT (vyska SL, dlzka SL v bode glabela)
r.lin.uhl = function(x, y){
  # linearne-uhlovy Pearsonuv korelacny koeficient
  rxc = cor(x, cos(y))
  rxs = cor(x, sin(y))
  rsc = cor(cos(y), sin(y))
  
  r = sqrt((rxc^2 + rxs^2 - 2*rxc*rxs*rsc) / (1 - rsc^2))
  return(r)
}

base.A.rad = stupne.na.rad(height.BA)

r.glab.base = r.lin.uhl(length.BA.G, base.A.rad)
stopifnot(r.glab.base > 0.1)
cat("Linearne-uhlovy korelacny koeficient - subor:", r.skull.base)
# 0.7947889
# mierny stupen zavislosti medzi uhlom zvierajucim linie, ktore
# prechadzaju bodom basion a bodmi porion a dlzkou SL v bode glabela

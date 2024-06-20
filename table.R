setwd("C:/Users/Asus/Desktop/Diplomovka/Praktick� �as�")
data<-read.table("kol_soubory.txt", header=TRUE, sep="\t")
attach(data)

ast.sin.x<-data[LM.49 == "ast.sin", "X"]
ast.sin.y<-data[LM.49 == "ast.sin", "Y"]
ast.sin.z<-data[LM.49 == "ast.sin", "Z"]
ast.sin<-data.frame(ast.sin.x, ast.sin.y, ast.sin.z)
ast.sin

ast.dx.x<-data[LM.49 == "ast.dx", "X"]
ast.dx.y<-data[LM.49 == "ast.dx", "Y"]
ast.dx.z<-data[LM.49 == "ast.dx", "Z"]
ast.dx<-data.frame(ast.dx.x, ast.dx.y, ast.dx.z)
ast.dx

au.sin.x<-data[LM.49 == "au.sin", "X"]
au.sin.y<-data[LM.49 == "au.sin", "Y"]
au.sin.z<-data[LM.49 == "au.sin", "Z"]
au.sin<-data.frame(au.sin.x, au.sin.y, au.sin.z)
au.sin

au.dx.x<-data[LM.49 == "au.dx", "X"]
au.dx.y<-data[LM.49 == "au.dx", "Y"]
au.dx.z<-data[LM.49 == "au.dx", "Z"]
au.dx<-data.frame(au.dx.x, au.dx.y, au.dx.z)
au.dx

carL.sin.x<-data[LM.49 == "carL.sin", "X"]
carL.sin.y<-data[LM.49 == "carL.sin", "Y"]
carL.sin.z<-data[LM.49 == "carL.sin", "Z"]
carL.sin<-data.frame(carL.sin.x, carL.sin.y, carL.sin.z)
carL.sin

carL.dx.x<-data[LM.49 == "carL.dx", "X"]
carL.dx.y<-data[LM.49 == "carL.dx", "Y"]
carL.dx.z<-data[LM.49 == "carL.dx", "Z"]
carL.dx<-data.frame(carL.dx.x, carL.dx.y, carL.dx.z)
carL.dx

carM.sin.x<-data[LM.49 == "carM.sin", "X"]
carM.sin.y<-data[LM.49 == "carM.sin", "Y"]
carM.sin.z<-data[LM.49 == "carM.sin", "Z"]
carM.sin<-data.frame(carM.sin.x, carM.sin.y, carM.sin.z)
carM.sin

carM.dx.x<-data[LM.49 == "carM.dx", "X"]
carM.dx.y<-data[LM.49 == "carM.dx", "Y"]
carM.dx.z<-data[LM.49 == "carM.dx", "Z"]
carM.dx<-data.frame(carM.dx.x, carM.dx.y, carM.dx.z)
carM.dx

en.sin.x<-data[LM.49 == "en.sin", "X"]
en.sin.y<-data[LM.49 == "en.sin", "Y"]
en.sin.z<-data[LM.49 == "en.sin", "Z"]
en.sin<-data.frame(en.sin.x, en.sin.y, en.sin.z)
en.sin

en.dx.x<-data[LM.49 == "en.dx", "X"]
en.dx.y<-data[LM.49 == "en.dx", "Y"]
en.dx.z<-data[LM.49 == "en.dx", "Z"]
en.dx<-data.frame(en.dx.x, en.dx.y, en.dx.z)
en.dx

fomL.sin.x<-data[LM.49 == "fomL.sin", "X"]
fomL.sin.y<-data[LM.49 == "fomL.sin", "Y"]
fomL.sin.z<-data[LM.49 == "fomL.sin", "Z"]
fomL.sin<-data.frame(fomL.sin.x, fomL.sin.y, fomL.sin.z)
fomL.sin

fomL.dx.x<-data[LM.49 == "fomL.dx", "X"]
fomL.dx.y<-data[LM.49 == "fomL.dx", "Y"]
fomL.dx.z<-data[LM.49 == "fomL.dx", "Z"]
fomL.dx<-data.frame(fomL.dx.x, fomL.dx.y, fomL.dx.z)
fomL.dx

fovA.sin.x<-data[LM.49 == "fovA.sin", "X"]
fovA.sin.y<-data[LM.49 == "fovA.sin", "Y"]
fovA.sin.z<-data[LM.49 == "fovA.sin", "Z"]
fovA.sin<-data.frame(fovA.sin.x, fovA.sin.y, fovA.sin.z)
fovA.sin

fovA.dx.x<-data[LM.49 == "fovA.dx", "X"]
fovA.dx.y<-data[LM.49 == "fovA.dx", "Y"]
fovA.dx.z<-data[LM.49 == "fovA.dx", "Z"]
fovA.dx<-data.frame(fovA.dx.x, fovA.dx.y, fovA.dx.z)
fovA.dx

fovP.sin.x<-data[LM.49 == "fovP.sin", "X"]
fovP.sin.y<-data[LM.49 == "fovP.sin", "Y"]
fovP.sin.z<-data[LM.49 == "fovP.sin", "Z"]
fovP.sin<-data.frame(fovP.sin.x, fovP.sin.y, fovP.sin.z)
fovP.sin

fovP.dx.x<-data[LM.49 == "fovP.dx", "X"]
fovP.dx.y<-data[LM.49 == "fovP.dx", "Y"]
fovP.dx.z<-data[LM.49 == "fovP.dx", "Z"]
fovP.dx<-data.frame(fovP.dx.x, fovP.dx.y, fovP.dx.z)
fovP.dx

fmt.sin.x<-data[LM.49 == "fmt.sin", "X"]
fmt.sin.y<-data[LM.49 == "fmt.sin", "Y"]
fmt.sin.z<-data[LM.49 == "fmt.sin", "Z"]
fmt.sin<-data.frame(fmt.sin.x, fmt.sin.y, fmt.sin.z)
fmt.sin

fmt.dx.x<-data[LM.49 == "fmt.dx", "X"]
fmt.dx.y<-data[LM.49 == "fmt.dx", "Y"]
fmt.dx.z<-data[LM.49 == "fmt.dx", "Z"]
fmt.dx<-data.frame(fmt.dx.x, fmt.dx.y, fmt.dx.z)
fmt.dx

jugL.sin.x<-data[LM.49 == "jugL.sin", "X"]
jugL.sin.y<-data[LM.49 == "jugL.sin", "Y"]
jugL.sin.z<-data[LM.49 == "jugL.sin", "Z"]
jugL.sin<-data.frame(jugL.sin.x, jugL.sin.y, jugL.sin.z)
jugL.sin

jugL.dx.x<-data[LM.49 == "jugL.dx", "X"]
jugL.dx.y<-data[LM.49 == "jugL.dx", "Y"]
jugL.dx.z<-data[LM.49 == "jugL.dx", "Z"]
jugL.dx<-data.frame(jugL.dx.x, jugL.dx.y, jugL.dx.z)
jugL.dx

jugM.sin.x<-data[LM.49 == "jugM.sin", "X"]
jugM.sin.y<-data[LM.49 == "jugM.sin", "Y"]
jugM.sin.z<-data[LM.49 == "jugM.sin", "Z"]
jugM.sin<-data.frame(jugM.sin.x, jugM.sin.y, jugM.sin.z)
jugM.sin

jugM.dx.x<-data[LM.49 == "jugM.dx", "X"]
jugM.dx.y<-data[LM.49 == "jugM.dx", "Y"]
jugM.dx.z<-data[LM.49 == "jugM.dx", "Z"]
jugM.dx<-data.frame(jugM.dx.x, jugM.dx.y, jugM.dx.z)
jugM.dx

k.sin.x<-data[LM.49 == "k.sin", "X"]
k.sin.y<-data[LM.49 == "k.sin", "Y"]
k.sin.z<-data[LM.49 == "k.sin", "Z"]
k.sin<-data.frame(k.sin.x, k.sin.y, k.sin.z)
k.sin

k.dx.x<-data[LM.49 == "k.dx", "X"]
k.dx.y<-data[LM.49 == "k.dx", "Y"]
k.dx.z<-data[LM.49 == "k.dx", "Z"]
k.dx<-data.frame(k.dx.x, k.dx.y, k.dx.z)
k.dx

ms.sin.x<-data[LM.49 == "ms.sin", "X"]
ms.sin.y<-data[LM.49 == "ms.sin", "Y"]
ms.sin.z<-data[LM.49 == "ms.sin", "Z"]
ms.sin<-data.frame(ms.sin.x, ms.sin.y, ms.sin.z)
ms.sin

ms.dx.x<-data[LM.49 == "ms.dx", "X"]
ms.dx.y<-data[LM.49 == "ms.dx", "Y"]
ms.dx.z<-data[LM.49 == "ms.dx", "Z"]
ms.dx<-data.frame(ms.dx.x, ms.dx.y, ms.dx.z)
ms.dx

po.sin.x<-data[LM.49 == "po.sin", "X"]
po.sin.y<-data[LM.49 == "po.sin", "Y"]
po.sin.z<-data[LM.49 == "po.sin", "Z"]
po.sin<-data.frame(po.sin.x, po.sin.y, po.sin.z)
po.sin

po.dx.x<-data[LM.49 == "po.dx", "X"]
po.dx.y<-data[LM.49 == "po.dx", "Y"]
po.dx.z<-data[LM.49 == "po.dx", "Z"]
po.dx<-data.frame(po.dx.x, po.dx.y, po.dx.z)
po.dx

pter.sin.x<-data[LM.49 == "pter.sin", "X"]
pter.sin.y<-data[LM.49 == "pter.sin", "Y"]
pter.sin.z<-data[LM.49 == "pter.sin", "Z"]
pter.sin<-data.frame(pter.sin.x, pter.sin.y, pter.sin.z)
pter.sin

pter.dx.x<-data[LM.49 == "pter.dx", "X"]
pter.dx.y<-data[LM.49 == "pter.dx", "Y"]
pter.dx.z<-data[LM.49 == "pter.dx", "Z"]
pter.dx<-data.frame(pter.dx.x, pter.dx.y, pter.dx.z)
pter.dx

pyr.sin.x<-data[LM.49 == "pyr.sin", "X"]
pyr.sin.y<-data[LM.49 == "pyr.sin", "Y"]
pyr.sin.z<-data[LM.49 == "pyr.sin", "Z"]
pyr.sin<-data.frame(pyr.sin.x, pyr.sin.y, pyr.sin.z)
pyr.sin

pyr.dx.x<-data[LM.49 == "pyr.dx", "X"]
pyr.dx.y<-data[LM.49 == "pyr.dx", "Y"]
pyr.dx.z<-data[LM.49 == "pyr.dx", "Z"]
pyr.dx<-data.frame(pyr.dx.x, pyr.dx.y, pyr.dx.z)
pyr.dx

re.sin.x<-data[LM.49 == "re.sin", "X"]
re.sin.y<-data[LM.49 == "re.sin", "Y"]
re.sin.z<-data[LM.49 == "re.sin", "Z"]
re.sin<-data.frame(re.sin.x, re.sin.y, re.sin.z)
re.sin

re.dx.x<-data[LM.49 == "re.dx", "X"]
re.dx.y<-data[LM.49 == "re.dx", "Y"]
re.dx.z<-data[LM.49 == "re.dx", "Z"]
re.dx<-data.frame(re.dx.x, re.dx.y, re.dx.z)
re.dx

sphsq.sin.x<-data[LM.49 == "sphsq.sin", "X"]
sphsq.sin.y<-data[LM.49 == "sphsq.sin", "Y"]
sphsq.sin.z<-data[LM.49 == "sphsq.sin", "Z"]
sphsq.sin<-data.frame(sphsq.sin.x, sphsq.sin.y, sphsq.sin.z)
sphsq.sin

sphsq.dx.x<-data[LM.49 == "sphsq.dx", "X"]
sphsq.dx.y<-data[LM.49 == "sphsq.dx", "Y"]
sphsq.dx.z<-data[LM.49 == "sphsq.dx", "Z"]
sphsq.dx<-data.frame(sphsq.dx.x, sphsq.dx.y, sphsq.dx.z)
sphsq.dx

sphzy.sin.x<-data[LM.49 == "sphzy.sin", "X"]
sphzy.sin.y<-data[LM.49 == "sphzy.sin", "Y"]
sphzy.sin.z<-data[LM.49 == "sphzy.sin", "Z"]
sphzy.sin<-data.frame(sphzy.sin.x, sphzy.sin.y, sphzy.sin.z)
sphzy.sin

sphzy.dx.x<-data[LM.49 == "sphzy.dx", "X"]
sphzy.dx.y<-data[LM.49 == "sphzy.dx", "Y"]
sphzy.dx.z<-data[LM.49 == "sphzy.dx", "Z"]
sphzy.dx<-data.frame(sphzy.dx.x, sphzy.dx.y, sphzy.dx.z)
sphzy.dx

ste.sin.x<-data[LM.49 == "ste.sin", "X"]
ste.sin.y<-data[LM.49 == "ste.sin", "Y"]
ste.sin.z<-data[LM.49 == "ste.sin", "Z"]
ste.sin<-data.frame(ste.sin.x, ste.sin.y, ste.sin.z)
ste.sin

ste.dx.x<-data[LM.49 == "ste.dx", "X"]
ste.dx.y<-data[LM.49 == "ste.dx", "Y"]
ste.dx.z<-data[LM.49 == "ste.dx", "Z"]
ste.dx<-data.frame(ste.dx.x, ste.dx.y, ste.dx.z)
ste.dx

ba.x<-data[LM.49 == "ba", "X"]
ba.y<-data[LM.49 == "ba", "Y"]
ba.z<-data[LM.49 == "ba", "Z"]
ba<-data.frame(ba.x, ba.y, ba.z)
ba

b.x<-data[LM.49 == "b", "X"]
b.y<-data[LM.49 == "b", "Y"]
b.z<-data[LM.49 == "b", "Z"]
b<-data.frame(b.x, b.y, b.z)
b

g.x<-data[LM.49 == "g", "X"]
g.y<-data[LM.49 == "g", "Y"]
g.z<-data[LM.49 == "g", "Z"]
g<-data.frame(g.x, g.y, g.z)
g

ho.x<-data[LM.49 == "ho", "X"]
ho.y<-data[LM.49 == "ho", "Y"]
ho.z<-data[LM.49 == "ho", "Z"]
ho<-data.frame(ho.x, ho.y, ho.z)
ho

i.x<-data[LM.49 == "i", "X"]
i.y<-data[LM.49 == "i", "Y"]
i.z<-data[LM.49 == "i", "Z"]
i<-data.frame(i.x, i.y, i.z)
i

l.x<-data[LM.49 == "l", "X"]
l.y<-data[LM.49 == "l", "Y"]
l.z<-data[LM.49 == "l", "Z"]
l<-data.frame(l.x, l.y, l.z)
l

n.x<-data[LM.49 == "n", "X"]
n.y<-data[LM.49 == "n", "Y"]
n.z<-data[LM.49 == "n", "Z"]
n<-data.frame(n.x, n.y, n.z)
n

o.x<-data[LM.49 == "o", "X"]
o.y<-data[LM.49 == "o", "Y"]
o.z<-data[LM.49 == "o", "Z"]
o<-data.frame(o.x, o.y, o.z)
o

sphba.x<-data[LM.49 == "sphba", "X"]
sphba.y<-data[LM.49 == "sphba", "Y"]
sphba.z<-data[LM.49 == "sphba", "Z"]
sphba<-data.frame(sphba.x, sphba.y, sphba.z)
sphba

DATA<-data.frame(ast.sin, ast.dx, au.sin, au.dx, carL.sin, carL.dx, carM.sin, carM.dx,
                 en.sin, en.dx, fomL.sin, fomL.dx, fovA.sin, fovA.dx, fovP.sin, fovP.dx,
                 fmt.sin, fmt.dx, jugL.sin, jugL.dx, jugM.sin, jugM.dx, k.sin, k.dx, ms.sin, ms.dx,
                 po.sin, po.dx, pter.sin, pter.dx, pyr.sin, pyr.dx, re.sin, re.dx, sphsq.sin, sphsq.dx,
                 sphzy.sin, sphzy.dx, ste.sin, ste.dx, ba, b, g, ho, i, l, n, o, sphba)
data[grepl("ID", data[,"LM.49"]),"LM.49"]

# ulozenie dat do excelu
install.packages("writexl")
library(writexl)
write_xlsx(x = DATA, path = "table.xlsx", col_names = TRUE)

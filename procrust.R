setwd("C:/Users/Asus/Desktop/Hotov�")
geom<-read.delim2("procrust.txt", header=TRUE, sep="\t")
attach(geom)
library(stringr)

pokus<-as.data.frame(str_split_fixed(geom$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
dataP<-cbind(geom, pokus)
data<-dataP[,-c(2)]
dataA<-data.frame(data[data$measur!="b",])
str(dataA)

library(rgl)
source('procrustes_algo.R')


## VYSKA SPODINY LEBECNEJ - PSC, PMS
# matica
BA = as.matrix(dataA[,c("ba.x", "ba.y", "ba.z")])
POdx = as.matrix(dataA[,c("po.dx.x", "po.dx.y", "po.dx.z")])
POsin = as.matrix(dataA[,c("po.sin.x", "po.sin.y", "po.sin.z")])

# z matice do pola
M = array(0, c(3, 3, nrow(dataA)))
M[1,,] = t(BA)
M[2,,] = t(POdx)
M[3,,] = t(POsin)

cat('Pocet jedincov je', nrow(dataA), '\n')
cat('Pocet landmarkov je', dim(M)[1], '\n')
dim(M)

PSC = procrust(M)
PMS = apply(PSC, c(1,2), mean)

open3d()
rgl.viewpoint(theta = 0,phi = 0,fov=30,zoom=0.7)
bg3d("beige")
par3d(windowRect=c(100,100,1600,1600))
n = 132
for(i in 1:n)
  spheres3d(PSC[,,i],radius=0.01,col="black",add=TRUE)
spheres3d(PMS, radius=0.02, col="red", add=TRUE)
lmks.mena = c("BA","POdx","POsin")
text3d(PMS+0.09 ,texts=lmks.mena, col=c("black"))
rgl.snapshot("3d.png","png")

# PMS pre muzov a zeny
sex = dataA$sex
PSC.F<-PSC[,,sex == "F"]
PMS.F<-apply(PSC.F, c(1,2), mean, na.rm=T)
PSC.M<-PSC[,,sex == "M"]
PMS.M<-apply(PSC.M, c(1,2), mean, na.rm=T)

open3d()
rgl.viewpoint(theta = 0,phi = 0,fov=30,zoom=0.7)
bg3d("beige")
par3d(windowRect=c(100,100,600,600))
nF = 35 
for(i in 1:nF)
  spheres3d(PSC.F[,,i],radius=0.01,col="red",add=TRUE)
spheres3d(PMS.F, radius=0.02, col="red", add=TRUE)
nM = 85
for(i in 1:nM)
  spheres3d(PSC.M[,,i],radius=0.01,col="blue",add=TRUE)
spheres3d(PMS.M, radius=0.02, col="blue", add=TRUE)
lmks.mena =  c("BA","POdx","POsin")
text3d(PMS+0.09 ,texts=lmks.mena, col=c("black"))
rgl.snapshot("pohlavie.png","png")


## VYSKA SPODINY LEBECNEJ - LINKY
landmarks = c("BA", "POdx", "POsin")
library(car)

# rovina
scatter3d(x=PMS[,1], y=PMS[,2], z=PMS[,3])

open3d()
rgl.viewpoint(theta = -30,phi = 0,fov=30,zoom=0.7)
bg3d("beige")
par3d(windowRect=c(100,100,1600,1600))
lmks1  <- c(1,2,3)
spheres3d(PMS[lmks1 ,],radius = 0.01,col="black",add=TRUE)
lmks.mena =  c("BA","POdx","POsin")
krivka1 <- c(1,2,2,3,3,1)
segments3d(PMS[krivka1 ,],col="black",lwd=5,add=TRUE)
text3d(PMS+0.07 ,texts=lmks.mena, col=c("black"))

# LINKY pre muzov a zeny
landmarks = c("BA", "POdx", "POsin")

open3d()
spheres3d(PMS.F,radius=0.01,col="red",add=TRUE)
# spheres3d(PMS.M,radius=0.01,col="blue",add=TRUE)
PMS.M = PMS.M + 10*(PMS.M - PMS.F)

rgl.viewpoint(theta = -30,phi = 0,fov=30,zoom=0.7)
bg3d("beige")
par3d(windowRect=c(100,100,1600,1600))

lmks1 <- c(1,2,3)
landmarks[lmks1]
spheres3d(PMS.M[lmks1 ,],radius = 0.01,col="blue",add=TRUE)
krivka1  <- c(1,2,2,3,3,1)
segments3d(PMS.M[krivka1 ,],col="blue",lwd=5,add=TRUE)

lmks2L  <- c(1,2,3)
landmarks[lmks2L]
spheres3d(PMS.F[lmks2L ,],radius = 0.01,col="red",add=TRUE)
krivka2L  <- c(1,2,2,3,3,1)
segments3d(PMS.F[krivka2L ,],col="red",lwd=5,add=TRUE)

lmks.mena =  c("BA","POdx","POsin")
text3d(PMS+0.07 ,texts=lmks.mena, col=c("black"))

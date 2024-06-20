setwd("C:/Users/Asus/Desktop/V�sledky")
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

# PMS pre zbierky
sample = dataA$sample
PSC.P<-PSC[,,sample == "P"]
PMS.P<-apply(PSC.P, c(1,2), mean, na.rm=T)
PSC.J<-PSC[,,sample == "J"]
PMS.J<-apply(PSC.J, c(1,2), mean, na.rm=T)
PSC.D<-PSC[,,sample == "D"]
PMS.D<-apply(PSC.D, c(1,2), mean, na.rm=T)
PSC.S<-PSC[,,sample == "S"]
PMS.S<-apply(PSC.S, c(1,2), mean, na.rm=T)
PSC.Z<-PSC[,,sample == "Z"]
PMS.Z<-apply(PSC.Z, c(1,2), mean, na.rm=T)
PSC.H<-PSC[,,sample == "H"]
PMS.H<-apply(PSC.H, c(1,2), mean, na.rm=T)
PSC.O<-PSC[,,sample == "O"]
PMS.O<-apply(PSC.O, c(1,2), mean, na.rm=T)

open3d()
rgl.viewpoint(theta = 0,phi = 0,fov=30,zoom=0.7)
bg3d("beige")
par3d(windowRect=c(100,100,600,600))
nP = 79 
for(i in 1:nP)
  spheres3d(PSC.P[,,i],radius=0.01,col="tan3",add=TRUE)
spheres3d(PMS.P, radius=0.02, col="tan3", add=TRUE)
nJ = 6
for(i in 1:nJ)
  spheres3d(PSC.J[,,i],radius=0.01,col="red2",add=TRUE)
spheres3d(PMS.J, radius=0.02, col="red2", add=TRUE)
nD = 12
for(i in 1:nD)
  spheres3d(PSC.D[,,i],radius=0.01,col="darkblue",add=TRUE)
spheres3d(PMS.D, radius=0.02, col="darkblue", add=TRUE)
nH = 3 
for(i in 1:nH)
  spheres3d(PSC.H[,,i],radius=0.01,col="blue",add=TRUE)
spheres3d(PMS.H, radius=0.02, col="blue", add=TRUE)
nO = 2 
for(i in 1:nO)
  spheres3d(PSC.O[,,i],radius=0.01,col="cornflowerblue",add=TRUE)
spheres3d(PMS.O, radius=0.02, col="cornflowerblue", add=TRUE)
nS = 15
for(i in 1:nS)
  spheres3d(PSC.S[,,i],radius=0.01,col="chartreuse4",add=TRUE)
spheres3d(PMS.S, radius=0.02, col="chartreuse4", add=TRUE)
nZ = 15
for(i in 1:nZ)
  spheres3d(PSC.Z[,,i],radius=0.01,col="chartreuse3",add=TRUE)
spheres3d(PMS.Z, radius=0.02, col="chartreuse3", add=TRUE)
lmks.mena =  c("BA","POdx","POsin")
text3d(PMS+0.09 ,texts=lmks.mena, col=c("black"))
rgl.snapshot("zbierky.png","png")


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

# LINKY pre zbierky
landmarks = c("BA", "POdx", "POsin")

open3d()
spheres3d(PMS.J,radius=0.01,col="red2",add=TRUE)
spheres3d(PMS.P,radius=0.01,col="tan3",add=TRUE)
PMS.P = PMS.P + 10*(PMS.P - PMS.J)
spheres3d(PMS.D,radius=0.01,col="darkblue",add=TRUE)
PMS.P = PMS.P + 10*(PMS.P - PMS.D)
spheres3d(PMS.H,radius=0.01,col="blue",add=TRUE)
PMS.P = PMS.P + 10*(PMS.P - PMS.H)
spheres3d(PMS.O,radius=0.01,col="cornflowerblue",add=TRUE)
PMS.P = PMS.P + 10*(PMS.P - PMS.O)
spheres3d(PMS.S,radius=0.01,col="chartreuse4",add=TRUE)
PMS.P = PMS.P + 10*(PMS.P - PMS.S)
spheres3d(PMS.Z,radius=0.01,col="chartreuse3",add=TRUE)
PMS.P = PMS.P + 10*(PMS.P - PMS.Z)

rgl.viewpoint(theta = -30,phi = 0,fov=30,zoom=0.7)
bg3d("beige")
par3d(windowRect=c(100,100,1600,1600))

lmks1 <- c(1,2,3)
landmarks[lmks1]
spheres3d(PMS.P[lmks1 ,],radius = 0.01,col="tan3",add=TRUE)
krivka1  <- c(1,2,2,3,3,1)
segments3d(PMS.P[krivka1 ,],col="tan3",lwd=5,add=TRUE)

spheres3d(PMS.J[lmks1 ,],radius = 0.01,col="red2",add=TRUE)
segments3d(PMS.J[krivka1 ,],col="red2",lwd=5,add=TRUE)

spheres3d(PMS.D[lmks1 ,],radius = 0.01,col="darkblue",add=TRUE)
segments3d(PMS.D[krivka1 ,],col="darkblue",lwd=5,add=TRUE)

spheres3d(PMS.H[lmks1 ,],radius = 0.01,col="blue",add=TRUE)
segments3d(PMS.H[krivka1 ,],col="blue",lwd=5,add=TRUE)

spheres3d(PMS.O[lmks1 ,],radius = 0.01,col="cornflowerblue",add=TRUE)
segments3d(PMS.O[krivka1 ,],col="cornflowerblue",lwd=5,add=TRUE)

spheres3d(PMS.S[lmks1 ,],radius = 0.01,col="chartreuse4",add=TRUE)
segments3d(PMS.S[krivka1 ,],col="chartreuse4",lwd=5,add=TRUE)

spheres3d(PMS.Z[lmks1 ,],radius = 0.01,col="chartreuse3",add=TRUE)
segments3d(PMS.Z[krivka1 ,],col="chartreuse3",lwd=5,add=TRUE)

lmks.mena =  c("BA","POdx","POsin")
text3d(PMS+0.07 ,texts=lmks.mena, col=c("black"))

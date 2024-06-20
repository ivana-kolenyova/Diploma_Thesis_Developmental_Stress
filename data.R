DATA<-read.table("data.txt", header=TRUE, sep="\t")
attach(DATA)
DATA[X.1 == "po", "X"]

po.x<-DATA[X.1 == "po", "X"]
po.y<-DATA[X.1 == "po", "Y"]
po.z<-DATA[X.1 == "po", "Z"]

data_2<-data.frame(po.x, po.y, po.z)
data_2

DATA[grepl("ID", DATA[,"X.1"]),"X.1"]

bp<- function(tab, lt, aj1,aj2,sig){
na.omit(tab)
nm<-data.matrix(row.names(tab))
tab<-data.matrix(tab)
tab<-scale(tab,  scale = TRUE)
tb<-tab
tab<-tab[,lt]
cnm<-data.matrix(colnames(tab))
v<-cov(tab)
e<-eigen(v)
e$vectors
x<-e$vectors[,c(1)]
x<-data.matrix(x)
x<- sig*x
pc1<-tab%*%x
pc1<-scale(pc1)
x_2<-e$vectors[,c(2)]
x_2<-data.matrix(x_2)
x_2<- sig*x_2
pc2<-tab%*%x_2
pc2<-scale(pc2)
pcs<-cbind(pc1,pc2)
plot(pc1,pc2, xlim = aj1, ylim= aj1)
text(pc1, pc2, labels=nm[,c(1)], cex = 0.8, pos = 1)
vc<-cbind(0,0,x,x_2)
par(new = TRUE)
plot(vc[,c(3)], vc[,c(4)],  xlab = "", ylab = "", 
     xaxt = "n", yaxt = "n",xlim = aj2, ylim= aj2)
axis(4, las = 1)
axis(3)
text(vc[,c(3)], vc[,c(4)],labels=cnm[,c(1)],cex = 1.2, pos = 1, col = "red")
y<-vc
kk<-cnm
arrows(y[,c(1)],y[,c(2)],y[,c(3)],y[,c(4)], length = 0.08, cex = 0.1, las = 2, col = "red")
}
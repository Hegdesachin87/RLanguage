lr<-read.csv("/home/sachin/Downloads/Untitled Folder/train.csv")
lr_t<-read.csv("/home/sachin/Downloads/Untitled Folder/test.csv")
sum(is.na(lr))
which(is.na(lr$y))
lr<-lr[!is.na(lr$y),]
X<-lr$x
Y<-lr$y
n<-length(Y)
m=2;c=2;a=0.00029
iterations=100

yp = rep(0,n)
e = rep(0,n)
J = rep(0,iterations)
dJdc = rep(0,iterations)
dJdm = rep(0,iterations)

for (i in 1:iterations) {
  yp=m*X+c
  e=yp-Y
  J[i]=mean(e^2)
  dJdm[i]<-(2/n)*sum(e*X)
  dJdc[i]<-(2/n)*sum(e)
  m<- m-dJdm[i]*a
  c<- c-dJdc[i]*a
}
yp<-m*lr_t$x+c
e=yp-lr_t$y

SSE<-sum((lr_t$y-yp)^2)
SST<-sum((lr_t$y-mean(lr_t$y))^2)
R_square<-1-(SSE/SST)

J[iterations+1] = mean(e^2)
plot(J, type = 'l')

datas=50
plot(lr_t$x[1:datas],lr_t$y[1:datas])
lines(lr_t$x[1:datas],yp[1:datas])

# Code to plot gradient decient

# lr<-read.csv("/home/sachin/Desktop/Book1.csv")
# X<-lr$x
# Y<-lr$y
# m = c(-6:12)
# iterations=0
# yPred=rep(0,length(Y))
# J = rep(0,19)
# for (i in 1:19){
#   yPred = m[i]*X + 2.7
#   J[i] = mean((Y-yPred)^2)
# }
# plot(m,J)


# 
si_lm<-lm(lr$x~lr$y,data = lr)
summary(si_lm)

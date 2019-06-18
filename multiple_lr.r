lr_m<-read.csv("/home/sachin/Desktop/multipl_linearxlsx.csv")
m1=1;m2=1;m3=1;c=1;a=0.0001
n=length(lr_m$Y)
iterations=800
yp = rep(0,n)
e = rep(0,n)
J = rep(0,iterations)
dJdc = rep(0,iterations)
dJdm1 = rep(0,iterations)
dJdm2 = rep(0,iterations)
dJdm3 = rep(0,iterations)
for (i in 1:iterations)
  {
  yp=m1*lr_m$X1+m2*lr_m$X2+m3*lr_m$X3+c
   e=lr_m$Y - yp
     J[i]=mean(e^2)
  dJdm1[i]<-(-2/n)*sum(e*lr_m$X1)
  dJdm2[i]<-(-2/n)*sum(e*lr_m$X2)
  dJdm3[i]<-(-2/n)*sum(e*lr_m$X3)
  dJdc[i]<-(-2/n)*sum(e)
  m1<- m1-dJdm1[i]*a
  m2<- m2-dJdm2[i]*a
  m3<- m3-dJdm3[i]*a
  c<- c-dJdc[i]*a
}
# print(dJdm1)
# print(dJdm2)
# print(dJdm3)
# print(dJdc)
yp<-m1*lr_m$X1+m2*lr_m$X2+m3*lr_m$X3+c
e=lr_m$Y - yp

SSE<-sum((lr_m$Y-yp)^2)
SST<-sum((lr_m$Y-mean(lr_m$Y))^2)
R_square<-1-(SSE/SST)
# plot(J,type='l')


# using library
mlr<-lm(X1+X2+X3~Y,data = lr_m)
summary(mlr)

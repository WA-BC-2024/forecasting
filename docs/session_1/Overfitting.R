library(MASS)
x<-mvrnorm(n=41,mu=rep(0,20),Sigma=diag(rep(1,20),nrow=20,ncol=20))
y<- 20 + -0.5*x[,1] + 0.5*x[,2]+rnorm(dim(x)[1],0,1)
summary(lm(y~x[,1]+x[,2]))#the correct model
summary(lm(y~x[,1]+x[,2]+x[,3]))# R-squared always goes up
summary(lm(y~x[,1:20]))# R-squared always goes up
        
#Three ways to calculate adjusted R^2
as.numeric( summary(lm(y~x[,1:20]))[9])

R2<-as.numeric( summary(lm(y~x[,1:20]))[8])
1-((1-R2)*(41-1) )/(41-20-1)

SSR<-sum(summary(lm(y~x[,1:20]))$residuals^2)
SST<-sum((y-mean(y))^2)
1-(SSR/(41-20-1))/(SST/(41-1))


a<-x[,3]
summary(lm(y~a))
b<-x[,3]^2
c<-x[,3]^3
d<-x[,3]^4
e<-x[,3]^5
f<-x[,3]^6
g<-x[,3]^7
h<-x[,3]^8
i<-x[,3]^9
j<-x[,3]^10
k<-x[,3]^11
l<-x[,3]^12
m<-x[,3]^13
n<-x[,3]^14
o<-x[,3]^15
p<-x[,3]^16
q<-x[,3]^17
r<-x[,3]^18
s<-x[,3]^19
t<-x[,3]^20
summary(lm(y~a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t))






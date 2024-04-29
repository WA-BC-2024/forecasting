#Simulate data.  y is the response viable we're trying to forecast.
#The true model is Beverton-Holt 
a<-5
b<-3000
x<-runif(20,100,5000)
log.y<-log(a)+log(x)-log(1+a/b*x)+rnorm(length(x),0,0.1)
y<-exp(log.y)
plot(x,y)

#LOOCV: Leave one out cross validation
pred<-matrix(NA,length(x),2)#will hold predictions from 2 models
for (i in 1:length(x)){
  x.i<-x[-i]
  y.i<-y[-i]
  mod0<-lm(y.i~x.i)#wrong model
  mod1<-nls(formula=log(y.i)~log(a)+log(x.i)-log(1+a/b*x.i),data=data.frame(x.i,y.i),start=c(a=1,b=1000))#correct model
  pred[i,1]<- as.numeric(mod0$coef[1] + mod0$coef[2]*x[i])
  pred[i,2]<- as.numeric((coef(mod1)[1]*x[i])/(1+coef(mod1)[1]/coef(mod1)[2]*x[i]))
}
plot(y,pred[,1],col=2,ylim=c(min(pred),max(pred)),xlab='True',ylab='Predicted')
lines(y,pred[,2],col=3,type='p')
abline(0,1)
legend(x=min(y),y=max(pred),legend=c('Linear (wrong)',"Bev-Holt (correct)"),col=c(2,3),pch=c(1,1))

#MAD: Mean Absolute Deviation
mean(abs(y-pred[,1]))
mean(abs(y-pred[,2]))

#MAPE: Mean Absolute Percentage Error
mean((y-pred[,1])/y)
mean((y-pred[,2])/y)

#WAPE: Weighted Average Percentage Error
sum(abs(y-pred[,1]))/sum(abs(y))
sum(abs(y-pred[,2]))/sum(abs(y))

#MASE: Mean Absolute Scaled Error
sum(abs(y-pred[,1]))/sum(abs(y-mean(y))) #scaled to overall mean
sum(abs(y-pred[,2]))/sum(abs(y-mean(y))) #scaled to overall mean

#MSE: Mean Squared Error
mean((y-pred[,1])^2)
mean((y-pred[,2])^2)

#RMSE: Root Mean Squared Error
sqrt(mean((y-pred[,1])^2))
sqrt(mean((y-pred[,2])^2))

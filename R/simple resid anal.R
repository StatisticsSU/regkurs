
res.diagnostics<-function(lm_obj){

  fitted<-fitted(lm_obj)
  residuals<-resid(lm_obj)

  k<-summary(lm_obj)$df[1]
  n<-length(lm_obj$residuals)
  residueStud<-rstudent(lm_obj)
  lab_x<-names(summary(lm_obj)$coef[,1])[-1]
  tkrit<-qt(1-0.025,n-k-1)
  lab_y<-names(lm_obj$model)[1]
  y<-lm_obj$model[,1]
  x<-lm_obj$model[,-1]
  nx<- dim(lm_obj$model)[2]#number of covariates

    
  # Fitted values vs residuals
  p1<-plot(fitted, residuals,
           ylab='Residuals',xlab='Fitted values',pch=20)
  abline(0, 0,lty=2,lwd=2,col="red")
  
  
  
  p2<-plot(fitted, residueStud,col="blue",
           ylab='Stud.Residuals',xlab='Fitted values',pch=20)
  abline(0, 0,lty=2,lwd=2,col="red")
  abline(tkrit, 0,lty=2,lwd=2,col="brown")
  abline(0,0,lty=2,lwd=2,col="green")
  abline(-tkrit,0,lty=2,lwd=2,col="brown")
  
  
  p3<-boxplot(residueStud,xlab='Studentized residuals',col="dark green")
  
  
  histog<-function(x){
    h<-hist(x,  xlab="Residuals", col="light blue",freq=TRUE, main="")
    xfit<-seq(min(x),max(x),length=length(x))
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit, col="blue", lwd=2)    
    #return(hist(h))
  }  
  
  
  p4<-histog(residuals)
  
  p5<- qqnorm(residuals,main="",pch=20)
  qqline(residuals,lty=2,lwd=2,col="red")
  
  p6<-plot(fitted,y,pch=20,xlab="Fitted values",ylab=lab_y)
  abline(0,1,lty=2,lwd=2,col="red")
  
  
  
  for(i in 1:(nx-1)){
    
  if(nx==2){
    xx<-x
  }else{
xx<-x[,i]
  }
    
      plot(xx, residuals, ylab='Residuals', xlab=lab_x[i]) 
  abline(0, 0,lty=2,lwd=2,col="red")#add horizontal line at 0
  }

 

  plot_final<-(list(p1, p2,p3,p4,p5,p6))

}





while (dev.cur()>1) dev.off()
if(!is.null(dev.list())) dev.off()

rm(list=ls())

res.diagnostics<-function(lm_obj){
  # ta fram residualer och anpassade värden:
  fitted<-fitted(lm_obj)
  residuals<-resid(lm_obj)
  
  #x1=names(x1)
  k<-summary(lm_obj)$df[1]
  n<-length(lm_obj$residuals)
  residueStud<-rstudent(lm_obj)
  lab_x<-names(summary(lm_obj)$coef[,1])[-1]
  tkrit<-qt(1-0.025,n-k-1)
  lab_y<-names(lm_obj$model)[1]
  y<-lm_obj$model[,1]
  x<-lm_obj$model[,-1]
  nx<- dim(lm_obj$model)[2]#number of covariates
  #outliers<-ifelse(abs(residueStud)>tkrit,1,0)
  
  
 # resplot_table = NA
 # if (resplot){
    
    
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
  
  

  # histogram för residualer
  
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
  
#}
  
  
  for(i in 1:(nx-1)){
    
  if(nx==2){
    xx<-x
  }else{
xx<-x[,i]
  }
    
      plot(xx, residuals, ylab='Residuals', xlab=lab_x[i]) 
  abline(0, 0,lty=2,lwd=2,col="red")#add horizontal line at 0
  }

  
  #XtGrid = exp_var= lm_obj$model[,2]
  # xx=cat(names(lm_obj$model)[2], "\n")
  
  #regLineFit = predict(lm_obj,  newdata = data.frame(exp_var),interval = 'confidence')
  
 
  
  #p9<-plot(lm_obj$model[,2], y, xlab = "", ylab = "", main = "", col = "blue", pch = 19)
  #lines(XtGrid, regLineFit[,"fit"])
  #lines(XtGrid, regLineFit[,"lwr"], col = "red")
  #lines(XtGrid, regLineFit[,"upr"], col = "red")
  
  #predLineFit = predict(lm_obj2, newdata = data.frame(exp_var=XtGrid), interval = 'prediction')
  #lines(XtGrid, predLineFit[,"lwr"], col = "green")
  #lines(XtGrid, predLineFit[,"upr"], col = "green")
  
  #,p9,p7,p8

  plot_final<-(list(p1, p2,p3,p4,p5,p6))

  
  #invisible(list(resplot = p1, p2,p3,p4,p5,p6))
 # return(plot_final)
}


data<-read.table("marketing2.csv",sep=";",dec=",",header=TRUE)
#lapply(fit1, regdiagnostics)
data$Gender<-as.factor(data$Gender)
data$Age.groups<-as.factor(data$Age.groups)
data$youtube<-as.numeric(data$youtube)
data$facebook<-as.numeric(data$facebook)
data$newspaper<-as.numeric(data$newspaper)
data$sales<-as.numeric(data$sales)
#fit1<-lm(sales~youtube+facebook,data=data)
fit<-lm(sales~youtube+facebook,data=data)
#mrg1<-regsummary(fit1,anova=T,fit_measures = T,param = T,vif_factors = T)
#,resplot =T

res.diagnostics(fit)



#getwd()
png(file="C:/Users/b795/Desktop/Regressionsanalys med R/Struktur för inlämningsuppgift med RM/plots.png")
#layout(mat=matrix(c(1,1,2,3,4,4),nrow=3,ncol=3,byrow=T))
par(mfrow=c(3,3), mar=c(5,5,2,2))
res.diagnostics(fit)
dev.off()



View(res.diagnostics(fit))




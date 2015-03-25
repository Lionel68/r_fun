###############
#function that simulate from model to help model checking
#########
lm.test<-function(m){
  require(plyr)
  #the model frame
  dat<-model.frame(m)
  #the model matrix
  f<-formula(m)
  modmat<-model.matrix(f,dat)
  #the standard deviation of the residuals
  sd.resid<-sd(resid(m))
  #sample size
  n<-dim(dat)[1]
  #get the right-hand side of the formula  
  #rhs<-all.vars(update(f, 0~.))
  #simulate 8 response vectors from model
  ys<-lapply(1:8,function(x) rnorm(n,modmat%*%coef(m),sd.resid))
  #refit the models
  ms<-llply(ys,function(y) lm(y~modmat[,-1]))
  #put the residuals and fitted values in a list
  df<-llply(ms,function(x) data.frame(Fitted=fitted(x),Resid=resid(x)))
  #select a random number from 2 to 8
  rnd<-sample(2:8,1)
  #put the original data into the list
  df<-c(df[1:(rnd-1)],list(data.frame(Fitted=fitted(m),Resid=resid(m))),df[rnd:8])
  
  #plot 
  par(mfrow=c(3,3))
  l_ply(df,function(x){
    plot(Resid~Fitted,x,xlab="Fitted",ylab="Residuals")
    abline(h=0,lwd=2,lty=2)
  })

  l_ply(df,function(x){
    qqnorm(x$Resid)
    qqline(x$Resid)
  })
  
  out<-list(Position=rnd)
  return(out)
}

setwd("G:\\Project\\R\\Workspace")
rm(list=ls())
library(matlab)

load_data <- function(){
  X_train <<- read.table("data/X.dat",header=F,sep="");
  y_train <<- read.table("data/y.dat",header=F,sep="");
}

repmat <- function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

lwlr <- function(X,y,x,tau){
  m = nrow(X)/nrow(x);
  n = ncol(X)/ncol(x);
  theta = matrix(0,ncol(X),1);
  X = as.matrix(X);
  x = as.matrix(x);
  #compute weights
  #w = as.matrix(exp(as.matrix(-rowSums((X-repmat(x,m,n))^2)) / (2*tau)));
  w = as.matrix(exp(as.matrix(-rowSums((X-x)^2)) / (2*tau)));
  
  #perform Newton's method
  g = matrix(1,n,1);
  while(norm(g) > 1e-6){
    h = 1 / (1+exp(-X %*% theta));
    g = t(X) %*% (w * as.matrix(y - h)) - 1e-4*theta;
    H = -t(X) %*% diag(as.vector(w*h*(1-h))) %*% X - 1e-4*diag(ncol(X));
    theta = theta - solve(H) %*% g;
  }
  
  #return predicted new y
  return(as.double(x%*%theta-10));
}

plot_lwlr <- function(X,y,tau,res){
  x = matrix(0,2,1);
  pred = matrix(0,res,res);
  
  for(i in 1:res){
    for(j in 1:res){
      x[1] = 2*(i-1)/(res-1) - 1;
      x[2] = 2*(j-1)/(res-1) - 1;
      pred[j,i] = lwlr(X,y,x,tau);
    }
  }
  
  imagesc(pred,main=paste("tau=",tau))
}


par(mfrow=c(2,3))
load_data()


pred <- lwlr(subPizzaReddit_X,subPizzaReddit_y,subPizzaReddit_X,0.01);
pizzaReddit <- cbind(pizzaReddit,pred)
pizzaReddit$pre <- ifelse(pizzaReddit$requester_received_pizza,1,0)
p <- subset(pizzaReddit,pizzaReddit$request_id %in% pizzaReddit_test$request_id ,c("request_id","pred","pre"))
sum(ifelse(p$pre == p$pred,1,0))/nrow(p)
names(p) <- c("request_id","requester_received_pizza","Real Value")
write.csv(p,"G:\\Project\\GitHub\\pizzaPred\\data\\result.csv",row.names=F)
    

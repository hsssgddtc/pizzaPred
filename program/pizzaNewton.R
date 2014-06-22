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
  m = nrow(X);
  n = ncol(X);
  theta = matrix(0,n,1);
  X = as.matrix(X);
  #compute weights
  w = as.matrix(exp(as.matrix(-rowSums((X-repmat(t(x),m,1))^2)) / (2*tau)));
  
  #perform Newton's method
  g = matrix(1,n,1);
  while(norm(g) > 1e-6){
    h = 1 / (1+exp(-X %*% theta));
    g = t(X) %*% (w * as.matrix(y - h)) - 1e-4*theta;
    H = -t(X) %*% diag(as.vector(w*h*(1-h))) %*% X - 1e-4*diag(n);
    theta = theta - solve(H) %*% g;
  }
  
  #return predicted new y
  return(as.double(t(x)%*%theta));
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
plot_lwlr(X_train,y_train,0.01,50)
plot_lwlr(X_train,y_train,0.05,50)
plot_lwlr(X_train,y_train,0.1,50)
plot_lwlr(X_train,y_train,0.5,50)
plot_lwlr(X_train,y_train,1,50)
plot_lwlr(X_train,y_train,5,50)

y <- matrix(0,nrow(subPizzaRedditTest),1)
for(i in 1:nrow(subPizzaRedditTest)){
  x <- subPizzaRedditTest[i,];
  y[i] <- lwlr(subPizzaReddit_X,subPizzaReddit_y,x,0.01);
}

pred <- ifelse(y>0,1,0)
pizzaRedditTest <- cbind(pizzaRedditTest,pred)
pre <- pizzaRedditTest[,c("request_id","pred")]
names(pre) <- c("request_id","requester_received_pizza")
write.csv(pre,"G:\\Project\\GitHub\\pizzaPred\\data\\result.csv",row.names=F)

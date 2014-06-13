sigmoid <- function(z){
  #sigmoid <- matrix(0,nrow(z),ncol(z))
  g <- (1+exp(-z))^-1;
  return(g);
}

costFunction <- function(theta,X,y){
  m<-length(y);
  J<- (t(log(sigmoid(X * theta)))*y - t(log(1-sigmoid(X * theta))) * (1-y))/m;
  return(J)
}

gradientDescent <- function(theta,X,y,alpha,num_iters){
  m<-length(y);
  J_history <- matrix(0,num_iters,1);
  theta_temp <- matrix(0,nrow(theta),ncol(theta));
  
  for(iter in 1:num_iters){
    for(i in 1:length(theta_temp)){
      theta_temp[i]<-(t(X[,i])*sigmoid(X*theta)-y)/m;
    }
    theta <- theta-theta_temp;
    
    J_history(iter) <- costFunction(theta,X,y);
    
    return(theta)
  }
}

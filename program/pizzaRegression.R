Sys.setlocale(locale="US")

plotData <- function(X,y){
  pos = rownames(subset(y,Admitted==1))
  neg = rownames(subset(y,Admitted==0))
  plot(as.vector(X[pos,1]),as.vector(X[pos,2]),col="red",xlab="Exam1 Score",ylab="Exam2 Score",pch=3)
  points(as.vector(X[neg,1]),as.vector(X[neg,2]),col="blue")
  legend("topright",pch=1,col=c("red","blue"),legend=c("Admitted","Not Admitted"))
}

sigmoid <- function(z){
  #sigmoid <- matrix(0,nrow(z),ncol(z))
  g <- (1+exp(-z))^-1;
  return(g);
}

costFunction <- function(theta,X,y){
  m<-length(y);
  theta <- as.matrix(theta);
  X <- as.matrix(X);
  y <- as.matrix(y);
  J<- (t(log(sigmoid(X %*% theta))) %*% -y - t(log(1-sigmoid(X %*% theta))) %*% (1-y))/m;
  return(J)
}

gradient <- function(theta,X,y){
  m<-length(y);
  theta_temp <- matrix(0,nrow(theta),ncol(theta));
  
  for(i in 1:length(theta_temp)){
      theta_temp[i]<-(t(X[,i])%*%(sigmoid(X%*%theta)-y))*(1/m);
    }
  return(theta_temp);
}

grad.Descent <- function(theta,X,y,alpha,num_iters){
  J_history <- matrix(0,num_iters,1);
  #m<-length(y);
  #theta_temp <- theta;
  
  for(iter in 1:num_iters){
    theta <- theta-alpha * gradient(theta,X,y);
    J_history[iter] <- costFunction(theta,X,y);
  }
  return(theta)
}

grad.Ascent <- function(theta,X,y,alpha,num_iters){
  J_history <- matrix(0,num_iters,1);
  m<-length(y);
  
  for(iter in 1:num_iters){
      theta <- theta+alpha * gradient(theta,X,y);
      J_history[iter] <- costFunction(theta,X,y);
  } 
  return(theta)
}

predict <- function(theta,X){
  m <- nrow(X);
  p <- matrix(0,m,1);
  
  for(i in 1:m){
    p[i] <- ifelse(sigmoid(X[i,] %*% theta)>=0.5,1,0);
  }
  return(p)
}


#Processing the data
data <- read.table("ex2data1.txt",head=FALSE,sep=",",col.names=c("Exam1","Exam2","Admitted"))

#Ploting the data
plotData(subset(data,1==1,c(1,2)),subset(data,1==1,3))

#Test Sigmoid Function
sigmoid(0)

#Test Cost Function
options(digits=8)
X <- cbind(as.numeric(as.vector(data[,1])),as.numeric(as.vector(data[,2])))
y <- data.matrix(subset(data,1==1,3))
m <- nrow(X); n <- ncol(X);
temp <- vector("numeric", length = m) + 1;
X <- as.matrix(cbind(data.frame(temp),X))
initial_theta <- data.matrix(vector("numeric", length = n+1))
costFunction(initial_theta,X,y);

#Test gradient
theta <- data.matrix(vector("numeric", length = n+1));
theta_temp <- matrix(0,nrow(theta),ncol(theta));
gradient(theta,X,y)

#Test gradient Descent
alpha <- 1/length(y);
theta <- theta + alpha * gradient(theta,X,y);
cost <- costFunction(theta,X,y);


theta <- data.matrix(vector("numeric", length = n+1));
theta <- grad.Descent(theta,X,y,0.0001,100000000)
theta <- grad.Descent(theta,X,y,0.0001,4000)
costFunction(theta,X,y);

#Test Predict
theta <- c(-25.16127,0.20623,0.20147)
p <- predict(theta,X)
accuracy <- mean(as.numeric(p==y))

fn <- function(x)costFunction(theta,X,y)
gn <- function(x)gradient(theta,X,y)
optim(theta,fn,gn)

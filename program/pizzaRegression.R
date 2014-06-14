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
  theta <- data.matrix(theta);
  X <- data.matrix(X);
  y <- data.matrix(y);
  J<- (t(log(sigmoid(X %*% theta))) %*% y - t(log(1-sigmoid(X %*% theta))) %*% (1-y))/m;
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

#Processing the data
data <- read.csv("ex2data1.txt",head=FALSE,col.names=c("Exam1","Exam2","Admitted"))



#Ploting the data
plotData(subset(data,1==1,c(1,2)),subset(data,1==1,3))

#Test Sigmoid Function
sigmoid(0)

#Test Cost Function
data <- data.matrix(data)
X <- subset(data,1==1,c(1,2))
y <- subset(data,1==1,3)
m <- nrow(X); n <- ncol(X);
temp <- vector("numeric", length = m) + 1;
X <- cbind(data.frame(temp),X)
initial_theta <- data.matrix(vector("numeric", length = n+1))
costFunction(initial_theta,X,y)



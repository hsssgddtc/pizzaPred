library(jsonlite)
#library(sm)
library(qdap)
#library(car)
pizzaReddit <- fromJSON("G:\\Project\\GitHub\\pizzaPred\\data\\train.json")
pizzaReddit_test <- fromJSON("G:\\Project\\GitHub\\pizzaPred\\data\\test.json")
pizzaReddit <- fromJSON("G:\\Project\\GitHub\\pizzaPred\\data\\pizza_request_dataset.json")


pizzaReddit$Date <- as.Date(as.POSIXct(pizzaReddit$unix_timestamp_of_request, origin="1970-01-01"))
pizzaReddit$days_of_week <- weekdays(pizzaReddit$Date)
pizzaReddit$first_half_of_month <- as.numeric(format(pizzaReddit$Date,"%d")<16)
pizzaReddit$gratitude <- as.numeric(rownames(pizzaReddit) %in% grep('thank|appreciate|gratitude|grateful',pizzaReddit$request_text,ignore.case=TRUE))
pizzaReddit$with_pic <- as.numeric(rownames(pizzaReddit) %in% grep('.jpg',pizzaReddit$request_text,ignore.case=TRUE))
pizzaReddit$reciprocity <- as.numeric(rownames(pizzaReddit) %in% grep('pay it forward|pay it back|return the favor',pizzaReddit$request_text,ignore.case=TRUE))
pizzaReddit$length <- word_count(pizzaReddit$request_text)
length_mean <- mean(subset(pizzaReddit$length,complete.cases(pizzaReddit$length),))
length_sd <- sd(subset(pizzaReddit$length,complete.cases(pizzaReddit$length),))
pizzaReddit$length <- (pizzaReddit$length-length_mean)/length_sd
#pizzaReddit$success <- ifelse(pizzaReddit$requester_received_pizza,1,0)
craving <- "friend|girlfriend|craving|birthday|boyfriend|celebrate|party|game|games|movie|date|drunk|beer|celebrating|invited|drinks|crave|wasted|invite"
pizzaReddit$craving <- as.numeric(rownames(pizzaReddit) %in% grep(craving,pizzaReddit$request_text,ignore.case=TRUE))
family <- "family|mom|wife|parents|mother|husband|dad|son|daughter|father|parent|mum"
pizzaReddit$family <- as.numeric(rownames(pizzaReddit) %in% grep(family,pizzaReddit$request_text,ignore.case=TRUE))
job <- "work|job|paycheck|unemployment|interview|fired|employment|hired|hire"
pizzaReddit$job <- as.numeric(rownames(pizzaReddit) %in% grep(job,pizzaReddit$request_text,ignore.case=TRUE))
money <- "money|now|broke|week|until|time|last|day|when|today|tonight|paid|next|first|night|after|tomorrow|month|while|account|before|long|Friday|rent|buy|bank|still|bills|bills|ago|cash|due|due|soon|past|never|paycheck|check|spent|years|poor|till|yesterday|morning|dollars|financial|hour|bill|evening|credit|budget|loan|bucks|deposit|dollar|current|payed"
pizzaReddit$money <- as.numeric(rownames(pizzaReddit) %in% grep(money,pizzaReddit$request_text,ignore.case=TRUE))
account_age_mean <-  mean(pizzaReddit$requester_account_age_in_days_at_request)
account_age_sd <-  sd(pizzaReddit$requester_account_age_in_days_at_request)
pizzaReddit$requester_account_age_in_days_at_request <- (pizzaReddit$requester_account_age_in_days_at_request-account_age_mean)/account_age_sd
karma_mean <- mean(pizzaReddit$requester_upvotes_minus_downvotes_at_request)
karma_sd <- sd(pizzaReddit$requester_upvotes_minus_downvotes_at_request)
pizzaReddit$requester_upvotes_minus_downvotes_at_request <- (pizzaReddit$requester_upvotes_minus_downvotes_at_request-karma_mean)/karma_sd
post_mean <- mean(pizzaReddit$requester_number_of_posts_at_request)
post_sd <- sd(pizzaReddit$requester_number_of_posts_at_request)
pizzaReddit$requester_number_of_posts_at_request <- (pizzaReddit$requester_number_of_posts_at_request-post_mean)/post_sd

subPizzaReddit_X <- pizzaReddit[,c("requester_account_age_in_days_at_request","first_half_of_month","gratitude","with_pic","reciprocity","length","requester_upvotes_minus_downvotes_at_request","requester_number_of_posts_at_request","craving","family","job","money")]
subPizzaReddit_X[!complete.cases(subPizzaReddit_X),"length"] <- 0
subPizzaReddit_y <- as.matrix(as.numeric(pizzaReddit[rownames(pizzaReddit) %in% rownames(subPizzaReddit_X),"requester_received_pizza"]))

#table(subPizzaReddit$requester_received_pizza,subPizzaReddit$predict)/length(subPizzaReddit$requester_received_pizza)


#Test Cost Function
options(digits=8)
m <- nrow(subPizzaReddit_X); n <- ncol(subPizzaReddit_X);
temp <- vector("numeric", length = m) + 1;
subPizzaReddit_X <- as.matrix(cbind(data.frame(temp),subPizzaReddit_X))
initial_theta <- data.matrix(vector("numeric", length = n+1))
costFunction(initial_theta,subPizzaReddit_X,subPizzaReddit_y)
gradient(initial_theta,subPizzaReddit_X,subPizzaReddit_y)

theta <- grad.Descent(initial_theta,subPizzaReddit_X,subPizzaReddit_y,0.001,40000)


p <- predict(theta,subPizzaReddit_X)
accuracy <- mean(as.numeric(p==subPizzaReddit_y))
pizzaReddit <- cbind(pizzaReddit,p)
pre <- subset(pizzaReddit,pizzaReddit$request_id %in% pizzaReddit_test$request_id ,c("request_id","p"))
names(pre) <- c("request_id","requester_received_pizza")
write.csv(pre,"G:\\Project\\GitHub\\pizzaPred\\data\\result.csv")

pizzaReddit[pizzaReddit$p == 1,"request_id"]


fn <- function(x)costFunction(theta,subPizzaReddit_X,subPizzaReddit_y)
gn <- function(x)gradient(theta,subPizzaReddit_X,subPizzaReddit_y)
optim(theta,fn,gn)
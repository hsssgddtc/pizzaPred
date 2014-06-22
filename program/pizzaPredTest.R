library(jsonlite)
library(sm)
library(qdap)
library(car)
pizzaRedditTest <- fromJSON("G:\\Project\\GitHub\\pizzaPred\\data\\test.json")

pizzaRedditTest$Date <- as.Date(as.POSIXct(pizzaRedditTest$unix_timestamp_of_request, origin="1970-01-01"))
pizzaRedditTest$days_of_week <- weekdays(pizzaRedditTest$Date)
pizzaRedditTest$first_half_of_month <- as.numeric(format(pizzaRedditTest$Date,"%d")<16)
pizzaRedditTest$gratitude <- as.numeric(rownames(pizzaRedditTest) %in% grep('thank|appreciate|gratitude|grateful',pizzaRedditTest$request_text,ignore.case=TRUE))
pizzaRedditTest$with_pic <- as.numeric(rownames(pizzaRedditTest) %in% grep('.jpg',pizzaRedditTest$request_text,ignore.case=TRUE))
pizzaRedditTest$reciprocity <- as.numeric(rownames(pizzaRedditTest) %in% grep('pay it forward|pay it back|return the favor',pizzaRedditTest$request_text,ignore.case=TRUE))
pizzaRedditTest$length <- word_count(pizzaRedditTest$request_text)
length_mean <- mean(subset(pizzaRedditTest$length,complete.cases(pizzaRedditTest$length),))
length_sd <- sd(subset(pizzaRedditTest$length,complete.cases(pizzaRedditTest$length),))
pizzaRedditTest$length <- (pizzaRedditTest$length-length_mean)/length_sd

#pizzaRedditTest$success <- ifelse(pizzaRedditTest$requester_received_pizza,1,0)
craving <- "friend|girlfriend|craving|birthday|boyfriend|celebrate|party|game|games|movie|date|drunk|beer|celebrating|invited|drinks|crave|wasted|invite"
pizzaRedditTest$craving <- as.numeric(rownames(pizzaRedditTest) %in% grep(craving,pizzaRedditTest$request_text,ignore.case=TRUE))
family <- "family|mom|wife|parents|mother|husband|dad|son|daughter|father|parent|mum"
pizzaRedditTest$family <- as.numeric(rownames(pizzaRedditTest) %in% grep(family,pizzaRedditTest$request_text,ignore.case=TRUE))
job <- "work|job|paycheck|unemployment|interview|fired|employment|hired|hire"
pizzaRedditTest$job <- as.numeric(rownames(pizzaRedditTest) %in% grep(job,pizzaRedditTest$request_text,ignore.case=TRUE))
money <- "money|now|broke|week|until|time|last|day|when|today|tonight|paid|next|first|night|after|tomorrow|month|while|account|before|long|Friday|rent|buy|bank|still|bills|bills|ago|cash|due|due|soon|past|never|paycheck|check|spent|years|poor|till|yesterday|morning|dollars|financial|hour|bill|evening|credit|budget|loan|bucks|deposit|dollar|current|payed"
pizzaRedditTest$money <- as.numeric(rownames(pizzaRedditTest) %in% grep(money,pizzaRedditTest$request_text,ignore.case=TRUE))
account_age_mean <-  mean(pizzaRedditTest$requester_account_age_in_days_at_request)
account_age_sd <-  sd(pizzaRedditTest$requester_account_age_in_days_at_request)
pizzaRedditTest$requester_account_age_in_days_at_request <- (pizzaRedditTest$requester_account_age_in_days_at_request-account_age_mean)/account_age_sd
karma_mean <- mean(pizzaRedditTest$requester_upvotes_minus_downvotes_at_request)
karma_sd <- sd(pizzaRedditTest$requester_upvotes_minus_downvotes_at_request)
pizzaRedditTest$requester_upvotes_minus_downvotes_at_request <- (pizzaRedditTest$requester_upvotes_minus_downvotes_at_request-karma_mean)/karma_sd
post_mean <- mean(pizzaRedditTest$requester_number_of_posts_at_request)
post_sd <- sd(pizzaRedditTest$requester_number_of_posts_at_request)
pizzaRedditTest$requester_number_of_posts_at_request <- (pizzaRedditTest$requester_number_of_posts_at_request-post_mean)/post_sd

subPizzaRedditTest <- pizzaRedditTest[,c("requester_account_age_in_days_at_request","first_half_of_month","gratitude","with_pic","reciprocity","length","requester_upvotes_minus_downvotes_at_request","requester_number_of_posts_at_request","craving","family","job","money")]
subPizzaRedditTest[!complete.cases(subPizzaRedditTest),7] <- 0
options(digits=8)
m <- nrow(subPizzaRedditTest); n <- ncol(subPizzaRedditTest);
temp <- vector("numeric", length = m) + 1;
subPizzaRedditTest <- as.matrix(cbind(data.frame(temp),subPizzaRedditTest))






\
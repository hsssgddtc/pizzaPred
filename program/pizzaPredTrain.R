library(jsonlite)
library(sm)
library(qdap)
library(car)
pizzaReddit <- fromJSON("G:\\Project\\GitHub\\pizzaPred\\data\\train.json")

pizzaReddit$Date <- as.Date(as.POSIXct(pizzaReddit$unix_timestamp_of_request, origin="1970-01-01"))
pizzaReddit$days_of_week <- weekdays(pizzaReddit$Date)
pizzaReddit$first_half_of_month <- (format(pizzaReddit$Date,"%d")<16)
pizzaReddit$gratitude <- rownames(pizzaReddit) %in% grep（'thank|appreciate|gratitude|grateful',pizzaReddit$request_text,ignore.case=TRUE)
pizzaReddit$with_pic <- rownames(pizzaReddit) %in% grep（'.jpg',pizzaReddit$request_text,ignore.case=TRUE)
pizzaReddit$reciprocity <- rownames(pizzaReddit) %in% grep（'pay it forward|pay it back|return the favor',pizzaReddit$request_text,ignore.case=TRUE)
pizzaReddit$length <- word_count(pizzaReddit$request_text)
#pizzaReddit$success <- ifelse(pizzaReddit$requester_received_pizza,1,0)
craving <- "friend|girlfriend|craving|birthday|boyfriend|celebrate|party|game|games|movie|date|drunk|beer|celebrating|invited|drinks|crave|wasted|invite"
pizzaReddit$craving <- rownames(pizzaReddit) %in% grep（craving,pizzaReddit$request_text,ignore.case=TRUE)
family <- "family|mom|wife|parents|mother|husband|dad|son|daughter|father|parent|mum"
pizzaReddit$family <- rownames(pizzaReddit) %in% grep（family,pizzaReddit$request_text,ignore.case=TRUE)
job <- "work|job|paycheck|unemployment|interview|fired|employment|hired|hire"
pizzaReddit$job <- rownames(pizzaReddit) %in% grep（job,pizzaReddit$request_text,ignore.case=TRUE)
money <- "money|now|broke|week|until|time|last|day|when|today|tonight|paid|next|first|night|after|tomorrow|month|while|account|before|long|Friday|rent|buy|bank|still|bills|bills|ago|cash|due|due|soon|past|never|paycheck|check|spent|years|poor|till|yesterday|morning|dollars|financial|hour|bill|evening|credit|budget|loan|bucks|deposit|dollar|current|payed"
pizzaReddit$money <- rownames(pizzaReddit) %in% grep（money,pizzaReddit$request_text,ignore.case=TRUE)

subPizzaReddit <- pizzaReddit[,c("requester_received_pizza","requester_account_age_in_days_at_request","first_half_of_month","gratitude","with_pic","reciprocity","length","requester_upvotes_minus_downvotes_at_request","requester_number_of_posts_at_request","craving","family","job","money")]
cor(subPizzaReddit)
#scatterplotMatrix(subPizzaReddit)

multiple_fit <- glm(requester_received_pizza~requester_account_age_in_days_at_request+first_half_of_month+gratitude+with_pic+reciprocity+length+requester_upvotes_minus_downvotes_at_request+requester_number_of_posts_at_request+craving+family+job+money，data=subPizzaReddit,family=binomial())
subPizzaReddit$prob <- predict(multiple_fit,newdata=subPizzaReddit,type="response")
baseline <- mean(subset(subPizzaReddit,subPizzaReddit$requester_received_pizza==T&!is.na(subPizzaReddit$prob))$prob)
subPizzaReddit$predict <- ifelse(subPizzaReddit$prob<baseline|is.na(subPizzaReddit$prob),FALSE,TRUE)
table(subPizzaReddit$requester_received_pizza,subPizzaReddit$predict)/length(subPizzaReddit$requester_received_pizza)


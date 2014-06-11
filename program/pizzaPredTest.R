library(jsonlite)
library(sm)
library(qdap)
library(car)
pizzaRedditTest <- fromJSON("G:\\Project\\GitHub\\pizzaPred\\data\\test.json")

pizzaRedditTest$Date <- as.Date(as.POSIXct(pizzaRedditTest$unix_timestamp_of_request, origin="1970-01-01"))
pizzaRedditTest$days_of_week <- weekdays(pizzaRedditTest$Date)
pizzaRedditTest$first_half_of_month <- (format(pizzaRedditTest$Date,"%d")<16)
pizzaRedditTest$gratitude <- rownames(pizzaRedditTest) %in% grep（'thank|appreciate|gratitude|grateful',pizzaRedditTest$request_text,ignore.case=TRUE)
pizzaRedditTest$with_pic <- rownames(pizzaRedditTest) %in% grep（'.jpg',pizzaRedditTest$request_text,ignore.case=TRUE)
pizzaRedditTest$reciprocity <- rownames(pizzaRedditTest) %in% grep（'pay it forward|pay it back|return the favor',pizzaRedditTest$request_text,ignore.case=TRUE)
pizzaRedditTest$length <- word_count(pizzaRedditTest$request_text)
#pizzaRedditTest$success <- ifelse(pizzaRedditTest$requester_received_pizza,1,0)
craving <- "friend|girlfriend|craving|birthday|boyfriend|celebrate|party|game|games|movie|date|drunk|beer|celebrating|invited|drinks|crave|wasted|invite"
pizzaRedditTest$craving <- rownames(pizzaRedditTest) %in% grep（craving,pizzaRedditTest$request_text,ignore.case=TRUE)
family <- "family|mom|wife|parents|mother|husband|dad|son|daughter|father|parent|mum"
pizzaRedditTest$family <- rownames(pizzaRedditTest) %in% grep（family,pizzaRedditTest$request_text,ignore.case=TRUE)
job <- "work|job|paycheck|unemployment|interview|fired|employment|hired|hire"
pizzaRedditTest$job <- rownames(pizzaRedditTest) %in% grep（job,pizzaRedditTest$request_text,ignore.case=TRUE)
money <- "money|now|broke|week|until|time|last|day|when|today|tonight|paid|next|first|night|after|tomorrow|month|while|account|before|long|Friday|rent|buy|bank|still|bills|bills|ago|cash|due|due|soon|past|never|paycheck|check|spent|years|poor|till|yesterday|morning|dollars|financial|hour|bill|evening|credit|budget|loan|bucks|deposit|dollar|current|payed"
pizzaRedditTest$money <- rownames(pizzaRedditTest) %in% grep（money,pizzaRedditTest$request_text,ignore.case=TRUE)

subPizzaRedditTest <- pizzaRedditTest[,c("request_id","requester_account_age_in_days_at_request","first_half_of_month","gratitude","with_pic","reciprocity","length","requester_upvotes_minus_downvotes_at_request","requester_number_of_posts_at_request","craving","family","job","money")]
subPizzaRedditTest$prob <- predict(multiple_fit,newdata=subPizzaRedditTest,type="response")
subPizzaRedditTest$predict <- ifelse(subPizzaRedditTest$prob<baseline|is.na(subPizzaRedditTest$prob),0,1)
write.csv(subPizzaRedditTest[,c("request_id","predict")],"G:\\Project\\GitHub\\pizzaPred\\data\\result.csv",row.names=FALSE)

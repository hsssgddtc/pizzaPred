library(jsonlite)
library(sm)
library(qdap)
library(car)
pizzaReddit <- fromJSON("G:\\Project\\GitHub\\pizzaPred\\data\\pizza_request_dataset.json")

pizzaReddit$Date <- as.Date(as.POSIXct(pizzaReddit$unix_timestamp_of_request, origin="1970-01-01"))
pizzaReddit$days_of_week <- weekdays(pizzaReddit$Date)
pizzaReddit$first_half_of_month <- (format(pizzaReddit$Date,"%d")<16)
pizzaReddit$gratitude <- rownames(pizzaReddit) %in% grep（'thank|appreciate|gratitude|grateful',pizzaReddit$request_text,ignore.case=TRUE)
pizzaReddit$with_pic <- rownames(pizzaReddit) %in% grep（'.jpg',pizzaReddit$request_text,ignore.case=TRUE)
pizzaReddit$reciprocity <- rownames(pizzaReddit) %in% grep（'pay it forward|pay it back|return the favor',pizzaReddit$request_text,ignore.case=TRUE)
pizzaReddit$length <- word_count(pizzaReddit$request_text)
pizzaReddit$success <- ifelse(pizzaReddit$requester_received_pizza,1,0)



successGet <- subset(pizzaReddit,pizzaReddit$requester_received_pizza )
failGet <- subset(pizzaReddit,!pizzaReddit$requester_received_pizza )

getPizzaOrNot　<- table(pizzaReddit$requester_received_pizza)
lbl <- paste(names(getPizzaOrNot),"\n",getPizzaOrNot,sep="")
pie(getPizzaOrNot, labels=lbl,main="Pie Chart for Pizza Get Comparison")


attach(pizzaReddit)
summary(requester_account_age_in_days_at_retrieval)
par(lwd=2)
requester_received_pizza.f <- factor(requester_received_pizza, levels=c(TRUE,FALSE),labels=c("received_pizza","didn't_receive_pizza"))
sm.density.compare(requester_account_age_in_days_at_retrieval,requester_received_pizza,xlab="Account Age in Days")
colfill <- c(2:(1+length(levels(requester_received_pizza.f))))
legend("topright",levels(requester_received_pizza.f),fill=colfill)
detach(pizzaReddit)

plot(table((pizzaReddit$first_half_of_month),pizzaReddit$requester_received_pizza),xlab="first_half_of_month",ylab="received_pizza",main="receive_pizza_comparison_under_different_half_of_month")
plot(table(pizzaReddit$gratitude,pizzaReddit$requester_received_pizza),xlab="gratitude",ylab="received_pizza",main="receive_pizza_comparison_in_gratitude")
plot(table(pizzaReddit$with_pic,pizzaReddit$requester_received_pizza),xlab="with_pic",ylab="received_pizza",main="receive_pizza_comparison_with_pic")

fit <- lm(success~gratitude,data=pizzaReddit)
summary(fit)
fitted(fit)
residuals(fit)
with(pizzaReddit,plot(gratitude,success,xlab="request_text_gratitude",ylab="success_or_not"))
abline(fit)

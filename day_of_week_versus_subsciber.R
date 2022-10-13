

library(gt)
library(caret)
library(dplyr)

rider_trips<-trip
rider_trips$Day_of_Week <- 
  format(as.POSIXlt(trip$start_date,format="%m/%d/%Y %H:%M"),
         "%Y-%m-%d")
rider_trips$Start_Day_of_Week<-weekdays(as.Date(rider_trips$Day_of_Week))
head(rider_trips)


rider_trips <- rider_trips[, c(1,4,10:13)]

head(rider_trips)
class(data2$Start_Day_of_Week)
unique(data2$Start_Day_of_Week)              


rider_trips$Is_Weekday <- 
  ifelse((rider_trips$Start_Day_of_Week == "Monday" |
  rider_trips$Start_Day_of_Week == "Tuesday" |
  rider_trips$Start_Day_of_Week == "Wednesday" |
  rider_trips$Start_Day_of_Week == "Thursday" | 
  rider_trips$Start_Day_of_Week == "Friday"), 1, 0)


unique(rider_trips$Is_Weekday)            

day.df <- as.data.frame(table(rider_trips$Is_Weekday))
colnames(day.df) <- c(" ", "Total Trips")
day.df
day.df[,1]= c("Weekend","Weekday")
gt(day.df)

day.cust.df <- as.data.frame(table(rider_trips$Is_Weekday, rider_trips$subscription_type))
day.cust.df[,1]= c("Weekend","Weekday","Weekend","Weekday")
colnames(day.cust.df) <- c(" ", "Rider Type", "Total Trips")
gt(day.cust.df)


(sum(rider_trips$Is_Weekday == 1))/nrow(rider_trips)  #87.58 % of rides are on weekdays            
1-(sum(rider_trips$Is_Weekday == 1))/nrow(rider_trips) #12.41 % of rides are on weekends

View(rider_trips)


rider_trips
rider_trips <- rider_trips[ , -5]
head(rider_trips)


colnames(rider_trips)[1] <- c("trip_num")
colnames(rider_trips)[2] <- c("name")
head(rider_trips)

head(station)


#station <- station[,-c(1,2:4,6)]


#merge rider_trips with station name
#something is screwball with id


trips2 <- merge(rider_trips, station, by=c("name"))
head(trips2)


#no nulls in the id column
nrow(as.data.frame(unique(rider_trips$name)))
sum(table(unique(rider_trips$name)))
nrow(as.data.frame(unique(trips2$name)))
sum(table(unique(trips2$name)))

### hmmm... there are four less stations once the sets merge


log.ready.trips <- trips2 %>%
  mutate(zip_code = case_when(
    city=="San Jose" ~ 95113,
    city=="Mountain View" ~ 94041,
    city=="Palo Alto" ~ 94301,
    city=="Redwood City" ~ 94063,
    city=="San Francisco" ~ 94107
  ))


head(log.ready.trips)
nrow(as.data.frame(unique(log.ready.trips$trip_num)))

?duplicates

n_occur <- data.frame(table(log.ready.trips$trip_num))
head(n_occur)
log.ready.trips[log.ready.trips$trip_num %in% n_occur$Var1[n_occur$Freq > 1],]


n_occur$Freq <- as.numeric(n_occur$Freq)
n_occur %>% select(n_occur$Freq > 1)

#Voila
head(log.ready.trips)

####################################################
# Split data into train and test sets
###################################################

set.seed(123) 

trainIndex <- createDataPartition(rider_trips$Is_Weekday, p=0.80, list=FALSE)
train <- rider_trips[trainIndex,]
test <- rider_trips[-trainIndex,]

head(train)
head(test)

###
trainIndex2 <- createDataPartition(trips2$subscription_type, p=0.80, list=FALSE)
train2 <- trips2[trainIndex2,]
test2 <- trips2[-trainIndex2,]
###
trainIndex3 <- createDataPartition(trips2$Is_Weekday, p=0.80, list=FALSE)
train3 <- trips2[trainIndex3,]
test3 <- trips2[-trainIndex3,]


####################################################
# Fit a logistic regression model
###################################################
#model1 = glm(rider_trips ~ . , data = train, family= binomial(link="logit"))
#summary(model1)

#model2 = glm(rider_trips ~ subcription_type + student, data= train, family = binomial(link="logit"))
#summary(model2)


model.sub.cust <- glm(Is_Weekday ~ as.factor(subscription_type), 
                      data=train, family="binomial")
summary(model.sub.cust)

head(train2)
model.city <- glm(as.factor(subscription_type) ~ as.factor(city),
                     data=train2, family="binomial")
summary(model.city)

model.subtype.station <- glm(as.factor(subscription_type) ~ as.factor(name) + 
                               as.factor(city), data=train3, family="binomial")
summary(model.subtype.station)


train2$zip_code <- as.factor(train2$zip_code)
class(train2$zip_code)
model.station <- glm(Is_Weekday ~ zip_code, data=train2, family="binomial")
 

########## as.factor(stationname) as predictor
########## ( or zip )

# Note: class labels are not important as we are predicting the probability that an 
# observation belongs to a specific class

# Interpreting Model result: Reference: https://stats.idre.ucla.edu/r/dae/logit-regression/
# Refernce : https://rpubs.com/raoulbia/interpreting_glm_logistic_regression_output

# Display Regression Co-efficients (log-odds)
#coef(model2)

# Display Regression Co-efficients (odds)
#exp(coef(model2))

# Odds of default increase by a factor of 1.006 for each 1 unit increase in balance 


#View min, mean and max values for pedigree 
#summary(Default$balance)

# Create dataset to see how the probability changes for different values of balance
#testdata = data.frame(balance = c(min(Default$balance), mean(Default$balance),max(Default$balance)),
 #                     student="Yes")

#testdata$probs = predict(model2,testdata,type = 'response')
#testdata

####################################################
# Train set predictions
###################################################
# Make predictions on the test data using lambda.min
probabilities.train = predict (model.city, newdata = train2, type = "response")
predicted.classes.min = as.factor(ifelse(probabilities.train >= 0.6, "Subscriber","Customer"))

# Model Accuracy 
# Confusion Matrix :Reference: https://towardsdatascience.com/demystifying-confusion-matrix-confusion-9e82201592fd
# Confusion Matrix in Cran 
#              Actual Value 
#-------------------------------
# Prediction  |  No     Yes
# ------------------------------
# No          |  TN     FN
# Yes         |  FP     TP

unique(probabilities.train)
unique(predicted.classes.min)
levels(as.factor(train2$subscription_type))
levels(as.factor(predicted.classes.min))
confusionMatrix(predicted.classes.min, as.factor(train2$subscription_type), positive ='Subscriber')

# In this particular case we would rather have false positive than false negatives

# Accuracy = (TN+TP)/(TN + FP + FN + TP)
# Precision = TP/(FP + TP)
# Recall = TP/(TP + FN)
# Sensitivity = TP rate
# Specificity = TN/(TN + FP)

####################################################
# Test set predictions
###################################################

probabilities.test = predict(model2, newdata = test2 ,type = "response")
predicted.classes.min = as.factor(ifelse(probabilities.test>=0.5, "Yes", "No"))

# Model Accuracy 
confusionMatrix(predicted.classes.min, test$default, positive = 'Yes')

###### Plot the Receiever operator Charecteristic Curve
# Response varable values and the predicted probabilities

ROC1 = roc(test$default, probabilities.test)

plot(ROC1, col = "blue", ylab = "Sensitivity - TP Rate", xlab = "Specificity - FP Rate")

# Interpretation: An ideal model will have its ROC curve touching the extreme left corner and 
# a bad model will have the curve closer to the cross-sectional line. 

# Calculate the area under the ROC curve
auc = auc(ROC1)
auc

#### For demonstration only  
# convert the log odds to odds to probabilities
# Predict the log odds

pred_logodds = predict(model2,test)
head(pred_logodds)

# Exponentiate the log odds
pred_odds = exp(pred_logodds)
head(pred_odds)

# Calculate Probabilities from odds
probs = pred_odds/(1+pred_odds)
head(probs)

# Predicted Probability 
pred_probs = predict(model2,test, type = 'response')
head(pred_probs)


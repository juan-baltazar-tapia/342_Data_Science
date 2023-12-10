# Code for Kart Model

#install.packages(c("rpart", "rpart.plot")); 
library(rpart); library(rpart.plot)

d2 <- d.train
d3 <- d.test
head(d3)

d2$HURT.STATUS <- as.factor(d2$HURT.STATUS)
d3$HURT.STATUS <- as.factor(d3$HURT.STATUS)

d2$BOROUGH <- as.factor(d2$BOROUGH)
d2$Quarter <- as.factor(d2$Quarter)
d2$Time_Category <- as.factor(d2$Time_Category)
d2$Vehicle_Factors <- as.factor(d2$Vehicle_Factors)

d3$BOROUGH <- as.factor(d3$BOROUGH)
d3$Quarter <- as.factor(d3$Quarter)
d3$Time_Category <- as.factor(d3$Time_Category)
d3$Vehicle_Factors <- as.factor(d3$Vehicle_Factors)

d2$HURT.STATUS
names(d2)
#d2 is the training dataset, Hurt.status, vehicle factors, time_cat etc
# are from d3.
cart01 <- rpart(HURT.STATUS ~  Vehicle_Factors + Time_Category + Quarter + BOROUGH, 
                data = d2, 
                method = "class",
                control = list(cp = 0.00015))

rpart.plot(cart01, type = 4, extra = 108)
?rpart
# Note: To demonstrate CART models, I am using the entire data set
# For your projects, you build the CART model using the Training
# data set (We'll get to that later in Unit 02 :) )

cart01

rpart.plot(cart01)
rpart.plot(cart01, type = 4)

rpart.plot(cart01, type = 4, extra = 108)

# Now let's make a confusion matrix! ####

?predict
?predict.rpart

Confusion_Matrix<- predict(object = cart01, newdata = d3, type = "class")
table(Confusion_Matrix)

# Now we have our predictions!

# Let's build the table

t1 <- table(d3$HURT.STATUS, Confusion_Matrix)
t1b <- addmargins(A = t1, FUN = list(Total = sum))

t1
t1b

table(d.test$HURT.STATUS)
#   No Hurt.  Hurt
#
# No  20k       0
#
#Yes. 13k     
#Accuracy = 20k / 33k = 0.60
data <- data.frame("0" = rep(LETTERS[1:2],       # Create example data frame
                            each = 4),
                   "1" = c(letters[1:3],
                          letters[2:5],
                          "b"))


# Accuracy: 89.81%
(23566+569)/26874

# Sensitivity: ( True Positives ) / Total Actually Positive
# About 19%
569/2988
# Interpret: Out of all positive records, 
# our model found 19% of them
# (our model is not very good at finding positive outcomes)

# Specificity: (True Negatives) / Total Actually Negative
# About 98.7%
23566/23886

# Interpret: Out of all negative records,
# our model found 98.7% of them!
# (our model is great at finding negative outcomes)

# Precision: (True Positives) / Total Predicted Positive
# 64%
569/889
# Interpret: Out of all our positive classifications,
# we were right 64% of the time.

# Baseline Model: Root node classification
# For this ex., it's the "Everyone is a No" model.

# For the baseline model...
# Accuracy: ( 23886 + 0 ) / 26874 = About 88.88%
# Sens.: 0 / 2988 = 0%
# Spec.: 23886/23886 = 100%

nt <- data.frame("No" = c("20216","1"),
                   "Yes" = c("13148","1"))
rownames(nt) <- c("0", "1")
colnames(nt) <- c("0", "1")
nt <- table(nt)
nt[1,1] <- 20216
nt[1,2] <- 0
nt[2,1] <- 13148
nt[2,2] <- 0
20184 + 32

matrix <-  addmargins(A = nt, FUN = list(Sum = sum))




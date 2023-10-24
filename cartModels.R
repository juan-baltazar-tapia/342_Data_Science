#install.packages(c("rpart", "rpart.plot")); 
library(rpart); library(rpart.plot)

d2 <- d.train
head(d2)

d2$BOROUGH <- as.factor(d2$BOROUGH)
d2$Quarter <- as.factor(d2$Quarter)
d2$Time_Category <- as.factor(d2$Time_Category)
d2$Vehicle_Factors <- as.factor(d2$Vehicle_Factors)

d2$HURT.STATUS
names(d2)
cart01 <- rpart(formula = HURT.STATUS ~  Vehicle_Factors + Time_Category + Quarter, 
                data = d2, method = "class")

# Note: To demonstrate CART models, I am using the entire data set
# For your projects, you build the CART model using the Training
# data set (We'll get to that later in Unit 02 :) )

cart01

rpart.plot(cart01)
rpart.plot(cart01, type = 4)

rpart.plot(cart01, type = 4, extra = 108)

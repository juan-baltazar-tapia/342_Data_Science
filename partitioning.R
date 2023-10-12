install.packages("caret")
library(caret)

#setting the seed
set.seed(123)

#Partitioning the data
part01 <- createDataPartition(y = clean3$CONTRIBUTING.FACTOR.VEHICLE.1, p = .75, list = FALSE)

d <- clean3

d.train <- d[ part01, ] 
d.test <- d[ -part01, ]
dim(d)
dim(d.train)
dim(d.test)
# Validate the partition
# Categorical, you need a table where Rows are Train and Test
names(d.train)
d.train$trainortest <-
  rep("train", nrow(d.train))
head(d.train)
names(d.train)

names(d.test)
d.test$trainortest <-
  rep("test", nrow(d.test))
head(d.test)

d.all <- rbind(d.train, d.test)
head(d.all)

# For Chi-Sq test!
# 
# Variable: response
d.all$Hurt_Status
d.all$trainortest

t1 <- table(d.all$trainortest, d.all$Hurt_Status)
chisq.test(t1, correct = F)$p.value 
#0.62568


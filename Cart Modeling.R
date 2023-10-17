library(tidyverse)
library(rpart); library(rpart.plot)

clen <- read.csv("clean4cart.csv") %>% select(-X)

cleann <- clean %>% 
  select(CRASH.DATE, CRASH.TIME, BOROUGH, CONTRIBUTING.FACTOR.VEHICLE.1, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED) %>% 
  drop_na() %>% 
  mutate(Month = format(as.Date(CRASH.DATE, "%m/%d/%Y"), format = "%m")) %>% 
  mutate(Hour = format(hour, '%H')) %>% 
  mutate(Contributing.Factor = clen$CONTRIBUTING.FACTOR.VEHICLE.1) %>% 
  mutate(Hurt.Status = ifelse(NUMBER.OF.PERSONS.KILLED > 0 | NUMBER.OF.PERSONS.INJURED > 0, 1, 0)) %>% 
  select(-c(NUMBER.OF.PERSONS.KILLED, NUMBER.OF.PERSONS.INJURED, CONTRIBUTING.FACTOR.VEHICLE.1, CRASH.DATE, CRASH.TIME))

for (i in 1:length(cleann$CONTRIBUTING.FACTOR.VEHICLE.1)) {
  if (cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Driver_Factors == TRUE) {
    cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "driver" 
  } else if (cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Environmental_Factors == TRUE) {
    cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "environmental"
  } else if (cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Road_Factors == TRUE) {
    cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "road"
  }else if (cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Tech_Factors == TRUE) {
    cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "tech"
  } else if (cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Vehicle_Factors == TRUE) {
    cleann$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "vehicle"
  } else {}
  print(i)
}

install.packages("caret")
library(caret)
set.seed(0120)
d <- cleann
part01 <- createDataPartition(y = d$Contributing.Factor, p = .75, list = F)

d.train <- d[part01, ]
d.test <- d[-part01, ]

d.train$trainortest <-
  rep("train", nrow(d.train))

d.test$trainortest <-
  rep("test", nrow(d.test))

d.all <- rbind(d.train, d.test)
t1 <- table(d.all$trainortest, d.all$Hurt.Status)
chisq.test(t1, correct = F)$p.value

d2 <- cleann
d2$Month <- as.factor(d2$Month)
d2$Hour <- as.factor(d2$Hour)
d2$BOROUGH <- as.factor(d2$BOROUGH)
d2$Contributing.Factor <- as.factor(d2$Contributing.Factor)
d2$Hurt.Status <- as.factor(d2$Hurt.Status)

cart01 <- rpart(formula = Hurt.Status ~ Month + Hour + BOROUGH + Contributing.Factor, 
                data = d2, method = "class")

library(tidyverse)
library(rpart); library(rpart.plot)

cleann <- clean %>% 
  select(CRASH.DATE, CRASH.TIME, BOROUGH, CONTRIBUTING.FACTOR.VEHICLE.1, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED) %>% 
  drop_na() %>% 
  mutate(Date = as.Date(CRASH.DATE, "%m/%d/%Y")) %>% 
  subset(Date > "2020-12-31" | Date < "") %>% 
  mutate(contributing = ifelse(CONTRIBUTING.FACTOR.VEHICLE.1 %in% Driver_Factors == TRUE, "Driver", 
                               ifelse(CONTRIBUTING.FACTOR.VEHICLE.1 %in% Environmental_Factors == TRUE, "Enviromental", 
                                      ifelse(CONTRIBUTING.FACTOR.VEHICLE.1 %in% Road_Factors == TRUE, "Road",
                                             ifelse(CONTRIBUTING.FACTOR.VEHICLE.1 %in% Vehicle_Factors == TRUE, "Vehicle",
                                                    ifelse(CONTRIBUTING.FACTOR.VEHICLE.1 %in% Tech_Factors == TRUE, "Tech", F)))))) %>% 
  mutate(Month = as.numeric(format(Date, "%m"))) %>% 
  mutate(Quarter = ifelse(Month > 9, "Q4", 
                        ifelse(Month > 6, "Q3",
                               ifelse(Month > 3, "Q2", "Q1")))) %>% 
  mutate(Crash.Time = as.numeric(hour(as.POSIXlt(CRASH.TIME, format ="%H:%M")))) %>% 
  mutate(Time = ifelse(Crash.Time < 6 , "Night", 
                       ifelse(Crash.Time < 12, "Morning", 
                              ifelse(Crash.Time < 17, "Afternoon", 
                                     ifelse(Crash.Time < 22, "Evening", "Night"))))) %>% 
  mutate(Hurt.Status = ifelse(NUMBER.OF.PERSONS.KILLED > 0 | NUMBER.OF.PERSONS.INJURED > 0, 1, 0)) %>% 
  select(-c(NUMBER.OF.PERSONS.KILLED, NUMBER.OF.PERSONS.INJURED, CONTRIBUTING.FACTOR.VEHICLE.1, CRASH.DATE, CRASH.TIME, Month, Date, Crash.Time)) 

#morning 6 - 11
#afternoon 12 - 16
#evening 17 - 21
#night 22 - 5 

install.packages("caret")
library(caret)
set.seed(0120)

d <- cleann
part01 <- createDataPartition(y = d$contributing, p = .75, list = F)

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
d2$Quarter <- as.factor(d2$Quarter)
d2$Time <- as.factor(d2$Time)
d2$BOROUGH <- as.factor(d2$BOROUGH)
d2$contributing <- as.factor(d2$contributing)
d2$Hurt.Status <- as.factor(d2$Hurt.Status)

cart01 <- rpart(formula = Hurt.Status ~ Quarter + Time + BOROUGH + contributing, 
                data = d2, method = "class", cp = .0001)

rpart.plot(cart01, type = 5, extra = 108, cex = 0.7)

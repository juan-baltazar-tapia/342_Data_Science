library(tidyverse)
library(rpart); library(rpart.plot)

clen <- read.csv("clean4cart.csv") %>% select(-X)

cleann <- clean %>% 
  select(CRASH.DATE, CRASH.TIME, BOROUGH, CONTRIBUTING.FACTOR.VEHICLE.1, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED) %>% 
  drop_na() %>% 
  mutate(CRASH.DATE = format(as.Date(CRASH.DATE, "%m/%d/%Y"), format = "%m")) %>% 
  mutate(CRASH.TIME = format(hour, '%H')) %>% 
  mutate(CONTRIBUTING.FACTOR = clen$CONTRIBUTING.FACTOR.VEHICLE.1) %>% 
  mutate(HURT.STATUS = ifelse(NUMBER.OF.PERSONS.KILLED > 0 | NUMBER.OF.PERSONS.INJURED > 0, 1, 0)) %>% 
  select(-c(NUMBER.OF.PERSONS.KILLED, NUMBER.OF.PERSONS.INJURED, CONTRIBUTING.FACTOR.VEHICLE.1))

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

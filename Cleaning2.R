library(ggplot2)
library(dbplyr)
library(tidyverse)
load(file = "/Users/mrfxde/342_Data_Science/data2.RData")
clean4 <- clean
#converts chr dates into POSIXct format
clean4$DATE <- as.POSIXct(clean4$CRASH.DATE, format = "%m/%d/%Y")
#clean4_smaller <- head(clean4, 100000)

########################################
# Finding the indices of dates < 2021. #
########################################
# Convert character dates to Date objects
dates <- as.Date(clean4$CRASH.DATE, format = "%m/%d/%Y")

# Define the threshold year (e.g., 2022)
threshold_year <- 2021

# Find indices where year is less than the threshold
indices <- which(as.POSIXlt(dates)$year + 1900 < threshold_year)

# Remove rows #2,023,338
clean4<- clean4[-c(indices), ]
# 278,523 after

plot <- ggplot(clean5, aes(x = DATE, y = NUMBER.OF.PERSONS.INJURED)) +
  geom_point() +
  xlab("Year") +
  ylab("Number of People Injured") +
  ggtitle("Scatterplot of Number of People Injured")
plot
# Make df with contributing factors, month, borough
clean5 <- clean4 %>% 
  select(CRASH.DATE, CRASH.TIME, BOROUGH, CONTRIBUTING.FACTOR.VEHICLE.1, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED, DATE) %>% 
  drop_na() %>% 
  mutate(CRASH.DATE = format(as.Date(CRASH.DATE, "%m/%d/%Y"), format = "%m")) %>% 
  mutate(HURT.STATUS = ifelse(NUMBER.OF.PERSONS.KILLED > 0 | NUMBER.OF.PERSONS.INJURED > 0, 1, 0)) #%>% 
  #select(-c(NUMBER.OF.PERSONS.KILLED, NUMBER.OF.PERSONS.INJURED))

# 132,616 for clean5

clean6 <- clean5[4]
for (i in 1:dim(clean6)) {
  if (clean6[i,] %in% Driver_Factors == TRUE) {
    clean6[i,] <- "driver" 
  } else if (clean6[i,] %in% Environmental_Factors == TRUE) {
    clean6[i,] <- "environmental"
  } else if (clean6[i,] %in% Road_Factors == TRUE) {
    clean6[i,] <- "road"
  }else if (clean6[i,] %in% Tech_Factors == TRUE) {
    clean6[i,] <- "tech"
  } else if (clean6[i,] %in% Vehicle_Factors == TRUE) {
    clean6[i,] <- "vehicle"
  } else {}
  print(i)
}
# Date, 4 quarters
# Time 4 time zones, 
# morning: 6am-11pm
# morning: 6-11
# afternoon: 12pm-4pm
# afternoon: 12-16
# evening: 5pm-9pm
# evening: 17-21
# night: 10pm-5am
# night: 22-23,and 0-5
clean5 <- clean5 %>%
  mutate(Time_Category = case_when(
    as.numeric(sub(":.*", "", CRASH.TIME)) %in% 6:11 ~ "Morning",
    as.numeric(sub(":.*", "", CRASH.TIME)) %in% 12:16 ~ "Afternoon",
    as.numeric(sub(":.*", "", CRASH.TIME)) %in% 17:21 ~ "Evening",
    as.numeric(sub(":.*", "", CRASH.TIME)) %in% c(22:23, 0:5) ~ "Night"
  ))

#DATE
# Q1 -> 1-3
# Q2 -> 4-6
# Q3 -> 7-9
# Q4 -> 10-12
# changing into quarters in clean5
clean5 <- clean5 %>%
  mutate(CRASH.DATE = as.numeric(CRASH.DATE))
head(clean5,20)

clean5 <- clean5 %>%
  mutate(Quarter = case_when(
    CRASH.DATE %in% 1:3 ~ "Q1",
    CRASH.DATE %in% 4:6 ~ "Q2",
    CRASH.DATE %in% 7:9 ~ "Q3",
    CRASH.DATE %in% 10:12 ~ "Q4"
  ))
clean_final <- clean5 %>% 
  select(BOROUGH, Quarter, Time_Category,  HURT.STATUS)

clean_final <- clean_final %>%
  mutate(Vehicle_Factors = clean6$CONTRIBUTING.FACTOR.VEHICLE.1)

#Re organize variables
clean_final <- clean_final %>%
  select(BOROUGH, Quarter, Time_Category,  Vehicle_Factors, HURT.STATUS)

#  Construct a stacked bar graph that shows whether there is an 
#    association between someone's Borough,
#    and whether someone got hurt. The bars
#    correspond to a Borough,
#    and should be filled in based on hurt status.
# Assuming df is your data frame
clean_final$HURT.STATUS <- factor(clean_final$HURT.STATUS, levels = c(0, 1), labels = c("No", "Yes"))

# BOROUGH VS HURT STATUS
table1 <- table(clean_final$HURT.STATUS, clean_final$BOROUGH)
table1
p.table1 <- prop.table(table1, margin=2)
p.table1
d.table1 <- data.frame(p.table1)
d.table1
# table
ggplot(clean_final, aes(x = BOROUGH, fill = HURT.STATUS)) +
  geom_bar(position = "stack", stat = "count") +
  labs(x = "Borough", y = "Count", fill = "Hurt Status") +
  ggtitle("Association between Borough and Hurt Status") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

#proportional table
ggplot(d.table1, aes(Var2, Freq, fill = Var1)) + geom_col() +
  labs(y = "Proportion", fill = "Hurt Status", 
       title = " ") +
  theme_classic()

# Quarter VS HURT STATUS
table2 <- table(clean_final$HURT.STATUS, clean_final$Quarter)
table2
p.table2 <- prop.table(table2, margin=2)
p.table2
d.table2 <- data.frame(p.table2)
d.table2
# table
ggplot(clean_final, aes(x = Quarter, fill = HURT.STATUS)) +
  geom_bar(position = "stack", stat = "count") +
  labs(x = "Borough", y = "Count", fill = "Hurt Status") +
  ggtitle("Association between Borough and Hurt Status") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

#proportional table
ggplot(d.table2, aes(Var2, Freq, fill = Var1)) + geom_col() +
  labs(y = "Proportion", fill = "Hurt Status", 
       title = " ") +
  theme_classic()

# Time_Category VS HURT STATUS
table3 <- table(clean_final$HURT.STATUS, clean_final$Time_Category)
table3
p.table3 <- prop.table(table3, margin=2)
p.table3
d.table3 <- data.frame(p.table3)
d.table3
# table
ggplot(clean_final, aes(x = Time_Category, fill = HURT.STATUS)) +
  geom_bar(position = "stack", stat = "count") +
  labs(x = "Borough", y = "Count", fill = "Hurt Status") +
  ggtitle("Association between Borough and Hurt Status") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

#proportional table
ggplot(d.table3, aes(Var2, Freq, fill = Var1)) + geom_col() +
  labs(y = "Proportion", fill = "Hurt Status", 
       title = " ") +
  theme_classic()

# Vehicle_Factors VS HURT STATUS
table4 <- table(clean_final$HURT.STATUS, clean_final$Vehicle_Factors)
table4
p.table4 <- prop.table(table4, margin=2)
p.table4
d.table4 <- data.frame(p.table4)
d.table4
# table
ggplot(clean_final, aes(x = Vehicle_Factors, fill = HURT.STATUS)) +
  geom_bar(position = "stack", stat = "count") +
  labs(x = "Borough", y = "Count", fill = "Hurt Status") +
  ggtitle("Association between Borough and Hurt Status") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

#proportional table
ggplot(d.table4, aes(Var2, Freq, fill = Var1)) + geom_col() +
  labs(y = "Proportion", fill = "Hurt Status", 
       title = " ") +
  theme_classic()

save.image(file = "data2.RData")


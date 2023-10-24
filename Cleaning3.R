View(table(raw$CONTRIBUTING.FACTOR.VEHICLE.1))
TestTable <- table(clean$CONTRIBUTING.FACTOR.VEHICLE.1, clean$NUMBER.OF.PEDESTRIANS.KILLED)
TestTable
p.TestTable <- round(prop.table(TestTable, margin=2), digits=2)
p.TestTable
# Alcohol Involvement, Driver Inexperience,  Traffic Control Disregarded ,  Unsafe Speed, Other
# are the ones with the highest %

# dataframe with only accidents that had casualties
cas <- clean[clean$NUMBER.OF.PERSONS.KILLED > 0, ]

# Select 3 explantory and 1 respones variable
cas1 <- cas %>%
  select(CONTRIBUTING.FACTOR.VEHICLE.1, CRASH.DATE, CRASH.TIME, NUMBER.OF.PERSONS.KILLED) %>%
  drop_na()
# 1859 obs

# rename variables
colnames(cas1) <- c("Vehicle_Factors", "Date", "Time", "Casualties")

# categories of interest
Var <- c("Alcohol Involvement", "Driver Inattention/Distraction", "Failure to Yield Right-of-Way"
         ,"Traffic Control Disregarded", "Unsafe Speed")
#viewing which ones have the highest casualties
View(table(cas1$Vehicle_Factors))

# Any variable that's not category of interest, replace with other
cas2 <- cas1
cas2$Vehicle_Factors <- ifelse(cas1$Vehicle_Factors %in% Var, cas1$Vehicle_Factors, "other")

# fix date format
#cas3 is for graphing purposes
cas3$Date <- as.POSIXct(cas2$Date, format = "%m/%d/%Y")

plot <- ggplot(cas3, aes(x = Date, y = Casualties)) +
  geom_point() +
  xlab("Year") +
  ylab("Number of People Killed") +
  ggtitle("Scatterplot of Number of People Injured")
plot


# Change date into just month
cas4 <- cas2 %>% 
  mutate(Date = format(as.Date(Date, "%m/%d/%Y"), format = "%m")) 

#DATE
# Q1 -> 1-3
# Q2 -> 4-6
# Q3 -> 7-9
# Q4 -> 10-12
# changing into quarters in clean5
cas5 <- cas4 %>%
  mutate(Date = as.numeric(Date))

cas5 <- cas5 %>%
  mutate(Quarter = case_when(
    Date %in% 1:3 ~ "Q1",
    Date  %in% 4:6 ~ "Q2",
    Date  %in% 7:9 ~ "Q3",
    Date  %in% 10:12 ~ "Q4"
  ))

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
cas5 <- cas5 %>%
  mutate(Time_Category = case_when(
    as.numeric(sub(":.*", "", Time)) %in% 6:11 ~ "Morning",
    as.numeric(sub(":.*", "", Time)) %in% 12:16 ~ "Afternoon",
    as.numeric(sub(":.*", "", Time)) %in% 17:21 ~ "Evening",
    as.numeric(sub(":.*", "", Time)) %in% c(22:23, 0:5) ~ "Night"
  ))
cas6 <- cas5 %>%
  select(Quarter, Time_Category, Vehicle_Factors, Casualties)

#cas7 is for graphing time
cas7 <- cas5
cas7 <- cas7 %>% 
  mutate(Time2 = format(as.POSIXct(Time, format="%H:%M"))) 

#plotting time
plot1 <- ggplot(cas7, aes(x = Time2, y = Casualties)) +
  geom_point() +
  xlab("Time") +
  ylab("Number of People Killed") +
  ggtitle("Scatterplot of Number of People Injured")
plot1

# Vehicle Factors VS Casualties
table1 <- table(cas_final$Quarter, cas_final$Casualties)
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

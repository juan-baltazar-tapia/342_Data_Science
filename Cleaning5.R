# I kept attempting at looking which hour was the most deadly.

clean7 <- clean4 %>% 
  select(CRASH.DATE, CRASH.TIME, CONTRIBUTING.FACTOR.VEHICLE.1, NUMBER.OF.PERSONS.KILLED, DATE) %>% 
  drop_na() %>% 
  mutate(Casualty = ifelse(NUMBER.OF.PERSONS.KILLED > 0, "Yes", "No" )) 

#converts chr dates into POSIXct format
clean7$DATE <- as.POSIXct(clean7$CRASH.DATE, format = "%m/%d/%Y")

########################################
# Finding the indices of dates < 2021. #
########################################
# Convert character dates to Date objects
dates <- as.Date(clean7$CRASH.DATE, format = "%m/%d/%Y")

# Define the threshold year (e.g., 2022)
threshold_year <- 2022

# Find indices where year is less than the threshold
indices <- which(as.POSIXlt(dates)$year + 1900 < threshold_year)
length(indices)
# Remove rows #2,023,338
clean8 <- clean7[-c(indices), ]
# 278,523 after

plot <- ggplot(clean8, aes(x = DATE, y = NUMBER.OF.PERSONS.KILLED)) +
  geom_point() +
  xlab("Year") +
  ylab("Amount") +
  ggtitle("Scatterplot of Number of People Killed")
plot

# Convert to time object
time_obj <- strptime(clean8$CRASH.TIME, format = "%H:%M")

# Extract the hour
hour <- format(time_obj, "%H")
hour.df <- data.frame(hour)

# Dates into month and add hour
clean9 <- clean8 %>% 
  mutate(Month = format(as.Date(CRASH.DATE, "%m/%d/%Y"), format = "%m"))   %>%
  mutate(Hour = hour.df$hour)

clean9 <- clean9 %>%
  mutate(Time_Category = case_when(
    as.numeric(sub(":.*", "", CRASH.TIME)) %in% 5:10 ~ "Morning",
    as.numeric(sub(":.*", "", CRASH.TIME)) %in% 11:16 ~ "Afternoon",
    as.numeric(sub(":.*", "", CRASH.TIME)) %in% 17:20 ~ "Evening",
    as.numeric(sub(":.*", "", CRASH.TIME)) %in% c(21:23,0:4) ~ "Night"
  ))

table5 <- table(clean9$Month, clean9$Casualty) %>% prop.table(margin = 1) %>% round(digits = 5)
table5

table6 <- table(clean9$Casualty, clean9$Hour) %>% prop.table() %>% round(digits = 7)
table6

table7 <- table(clean9$Time_Category, clean9$Casualty) %>% prop.table(margin = 1) %>% round(digits = 5)
table7
# 17-20
# dip at 20 (4)
# 
# 21-23,00-04 (7)
# 
# 05 - 10 (6)
# 11-16 (6)


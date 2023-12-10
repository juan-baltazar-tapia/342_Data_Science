####################################
# GRAPH from 2019-2022.    #########
####################################
cleanGraph <- clean %>% 
  select(CRASH.DATE, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED)

#converts chr dates into POSIXct format
cleanGraph$DATE <- as.POSIXct(cleanGraph$CRASH.DATE, format = "%m/%d/%Y")

dates2 <- as.Date(cleanGraph$CRASH.DATE, format = "%m/%d/%Y")

# Define the threshold year (e.g., 2022)
low_bound <- 2018
high_bound <- 2022
# Find indices where year is less than the threshold
indices <- which((as.POSIXlt(dates2)$year + 1900 > low_bound) & 
                   (as.POSIXlt(dates2)$year + 1900 < high_bound))
length(indices)
# Remove rows
cleanGraph2 <- cleanGraph[c(indices), ]
#  after
nrow(cleanGraph2)
# 
cleanGraph2$CRASH.DATE <- as.Date(cleanGraph2$CRASH.DATE)

ggplot(cleanGraph2, aes(x = DATE, y = NUMBER.OF.PERSONS.KILLED)) +
  geom_point() +
  xlab("Year") +
  ylab("Amount") +
  ggtitle("Scatterplot of Number of People Injured")
####################################
# GRAPH after 2021.       #########
####################################
cleanGraph3 <- clean %>% 
  select(CRASH.DATE, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED)

dates3 <- as.Date(cleanGraph3$CRASH.DATE, format = "%m/%d/%Y")

# Define the threshold year (e.g., 2022)
low_bound <- 2021
# Find indices where year is less than the threshold
indices <- which((as.POSIXlt(dates3)$year + 1900 >= low_bound))
length(indices)
# Remove rows
cleanGraph3 <- cleanGraph[c(indices), ]
#  after
nrow(cleanGraph3)
# 
cleanGraph3$CRASH.DATE <- as.Date(cleanGraph3$CRASH.DATE)

ggplot(cleanGraph3, aes(x = DATE, y = NUMBER.OF.PERSONS.INJURED)) +
  geom_point() +
  xlab("Year") +
  ylab("Amount") +
  ggtitle("Scatterplot of Number of People Injured")



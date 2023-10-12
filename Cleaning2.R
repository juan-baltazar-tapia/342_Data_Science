View(clean4)
clean4 <- clean
#converts chr dates into POSIXct format
clean4$DATE <- as.POSIXct(clean4$CRASH.DATE, format = "%m/%d/%Y")
clean4_smaller <- head(clean4, 4000)

########################################
# Finding the indices of dates < 2020. #
########################################
# Convert character dates to Date objects
dates <- as.Date(clean4_smaller$CRASH.DATE, format = "%m/%d/%Y")

# Define the threshold year (e.g., 2022)
threshold_year <- 2020

# Find indices where year is less than the threshold
indices <- which(as.POSIXlt(dates)$year + 1900 < threshold_year)

# Print the indices
print(indices)
# Remove rows at index 2 and 3
clean4_smaller <- clean4_smaller[-c(indices), ]

ggplot(clean4_smaller, aes(x = DATE, y = NUMBER.OF.PERSONS.INJURED)) +
  geom_point() +
  xlab("X") +
  ylab("Y") +
  ggtitle("Title") + geom_smooth()




library(tidyverse)

#school computer
setwd("C:/Users/derasrodriguezc/OneDrive - Eastern Connecticut State University/23-24/Fall/MAT 342")

#personal computer
setwd("/Users/christianderas/Library/CloudStorage/OneDrive-EasternConnecticutStateUniversity/23-24/Fall/MAT 342")

#data as of 12/31/2022
raw <- read.csv("Motor_Vehicle_Collisions_-_Crashes.csv")

#Factor Categories
Driver_Factors <- c(
  "Aggressive Driving/Road Rage","Alcohol Involvement", "Driver Inattention/Distraction","Driver Inexperience", 
  "Drugs (illegal)", "Fatigued/Drowsy", "Fell Asleep", "Lost Consciousness", "Passenger Distraction", "Prescription Medication", 
  "Physical Disability","Reaction to Uninvolved Vehicle", "Reaction to Other Uninvolved Vehicle", 
  "Pedestrian/Bicyclist/Other Pedestrian Error/Confusion", "Eating or Drinking", "Illness"
  )

Vehicle_Factors <- c(
  "Accelerator Defective", "Brakes Defective", "Driverless/Runaway Vehicle", "Headlights Defective", "Other Lighting Defects", 
  "Other Vehicular", "Oversized Vehicle", "Steering Failure", "Tire Failure/Inadequate", "Tow Hitch Defective", "Vehicle Vandalism",
  "Windshield Inadequate", "Tinted Windows")
  
Environmental_Factors <- c(
  "Animals Action", "Glare", "Obstruction/Debris", "Pavement Defective", "Pavement Slippery","View Obstructed/Limited", "Traffic Control", 
  "Device Improper/Non-Working", "Shoulders Defective/Improper", "Outside Car Distraction", "Traffic Control Device Improper/Non-Working")

Road_Factors <- c(
  "Backing Unsafely", "Failure to Keep Right", "Failure to Yield Right-of-Way", "Lane Usage Improper", "Passing Too Closely", 
  "Traffic Control Disregarded", "Turning Improperly", "Unsafe Lane Changing", "Unsafe Speed", "Following Too Closely", 
  'Passing or Lane Usage Improper', "Lane Marking Improper/Inadequate")

Tech_Factors <- c(
  "Cell Phone (hand-held)", "Cell Phone (hands-free)", "Other Electronic Device", "Using On Board Navigation Device", "Texting", 
  "Listening/Using Headphones")

#Replacing weird values
clean <- raw

clean[clean == "Illnes"] <- "Illness"
clean[clean == "Drugs (Illegal)"] <- "Drugs (illegal)"
clean[clean == "Cell Phone (hand-Held)"] <- "Cell Phone (hand-held)"
clean[clean == "Unspecified"] <- NA
clean[clean == 80] <- NA
clean[clean == ""] <- NA
clean$CONTRIBUTING.FACTOR.VEHICLE.1[clean$CONTRIBUTING.FACTOR.VEHICLE.1 == 1] <- NA
clean$CONTRIBUTING.FACTOR.VEHICLE.2[clean$CONTRIBUTING.FACTOR.VEHICLE.2 == 1] <- NA
clean$CONTRIBUTING.FACTOR.VEHICLE.3[clean$CONTRIBUTING.FACTOR.VEHICLE.3 == 1] <- NA
clean$CONTRIBUTING.FACTOR.VEHICLE.4[clean$CONTRIBUTING.FACTOR.VEHICLE.4 == 1] <- NA
clean$CONTRIBUTING.FACTOR.VEHICLE.5[clean$CONTRIBUTING.FACTOR.VEHICLE.5 == 1] <- NA

#select variables of interests and convert target variable to categorical
clean1 <- clean %>% 
  select(CONTRIBUTING.FACTOR.VEHICLE.1, CONTRIBUTING.FACTOR.VEHICLE.2, CONTRIBUTING.FACTOR.VEHICLE.3, 
         CONTRIBUTING.FACTOR.VEHICLE.4, CONTRIBUTING.FACTOR.VEHICLE.5, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED) %>% 
  drop_na(CONTRIBUTING.FACTOR.VEHICLE.1, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED) 

#categorize
clean2 <- clean1

index <- clean1[1]
for (i in 1:dim(index)) {
  if (index[i,] %in% Driver_Factors == TRUE) {
    index[i,] <- "driver" 
  } else if (index[i,] %in% Environmental_Factors == TRUE) {
    index[i,] <- "environmental"
  } else if (index[i,] %in% Road_Factors == TRUE) {
    index[i,] <- "road"
  }else if (index[i,] %in% Tech_Factors == TRUE) {
    index[i,] <- "tech"
  } else if (index[i,] %in% Vehicle_Factors == TRUE) {
    index[i,] <- "vehicle"
  } else {}
  print(i)
}

#Clean 2 uses Killed and Injured with their counts
clean2 <- clean1 %>% 
  mutate(index, Combined = NUMBER.OF.PERSONS.INJURED + NUMBER.OF.PERSONS.KILLED) %>% 
  select(CONTRIBUTING.FACTOR.VEHICLE.1, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED, Combined)

write_csv(clean2, "clean(numeric).csv")



#Histograms
ggplot(clean2, aes(NUMBER.OF.PERSONS.INJURED)) + geom_histogram(aes(fill = CONTRIBUTING.FACTOR.VEHICLE.1), binwidth = 1) + scale_x_continuous(name = "Number of Persons Injured") + ggtitle("Total Number of Persons Injured") + ylab("Count") + labs(fill = "Contributing Factor")
ggplot(clean2, aes(NUMBER.OF.PERSONS.INJURED)) + geom_histogram(aes(fill = CONTRIBUTING.FACTOR.VEHICLE.1), binwidth = 1, na.rm = TRUE, position = "fill") + scale_x_continuous(name = "Number of Persons Injured") + ggtitle("Total Number of Persons Injured") + ylab("Count") + labs(fill = "Contributing Factor")

ggplot(clean2, aes(NUMBER.OF.PERSONS.KILLED)) + geom_histogram(aes(fill = CONTRIBUTING.FACTOR.VEHICLE.1), binwidth = 1) + scale_x_continuous(name = "Number of Persons Killed", breaks = c(0,1,2,3,4,5,6,7,8)) + ggtitle("Total Number of Persons Killed") + ylab("Count") + labs(fill = "Contributing Factor")
ggplot(clean2, aes(NUMBER.OF.PERSONS.KILLED)) + geom_histogram(aes(fill = CONTRIBUTING.FACTOR.VEHICLE.1), binwidth = 1, na.rm = TRUE, position = "fill") + scale_x_continuous(name = "Number of Persons Killed", breaks = c(0,1,2,3,4,5,6,7,8)) + ggtitle("Total Number of Persons Killed") + ylab("Count") + labs(fill = "Contributing Factor")

ggplot(clean2, aes(Combined)) + geom_histogram(aes(fill = CONTRIBUTING.FACTOR.VEHICLE.1), binwidth = 1) + scale_x_continuous(name = "Number of Persons Hurt") + ggtitle("Total Number of Persons Hurt") + ylab("Count") + labs(fill = "Contributing Factor")
ggplot(clean2, aes(Combined)) + geom_histogram(aes(fill = CONTRIBUTING.FACTOR.VEHICLE.1), binwidth = 1, na.rm = TRUE, position = "fill") + scale_x_continuous(name = "Number of Persons Hurt") + ggtitle("Total Number of Persons Hurt") + ylab("Count") + labs(fill = "Contributing Factor")

#Contingency Tables for Clean 2
Combined_Table_Numerical <- table(clean2$Combined, clean2$CONTRIBUTING.FACTOR.VEHICLE.1)
Combined_Table_Numerical_Margins <- addmargins(A = Combined_Table_Numerical, FUN = list(total = sum), quiet = TRUE)
Killed_Table_Numerical <- table(clean2$NUMBER.OF.PERSONS.KILLED, clean2$CONTRIBUTING.FACTOR.VEHICLE.1)
Killed_Table_Numerical_Margins <- addmargins(A = Killed_Table_Numerical, FUN = list(total = sum), quiet = TRUE)
Injured_Table_Numerical <- table(clean2$NUMBER.OF.PERSONS.INJURED, clean2$CONTRIBUTING.FACTOR.VEHICLE.1)
Injured_Table_Numerical_Margins <- addmargins(A = Injured_Table_Numerical, FUN = list(total = sum), quiet = TRUE)

#Clean 3 is Injured and Killed as Categorical variables
clean3 <- read.csv("clean.csv") %>% 
  drop_na(Injured, Killed) %>% 
  select(CONTRIBUTING.FACTOR.VEHICLE.1, Injured, Killed) %>% 
  mutate(Hurt_Status = ifelse(Injured == "No" & Killed == "No", "No one hurt", "Someone Hurt"))

#Bar Graph
ggplot(clean3, aes(CONTRIBUTING.FACTOR.VEHICLE.1)) + geom_bar(aes(fill = result))
ggplot(clean3, aes(CONTRIBUTING.FACTOR.VEHICLE.1)) + geom_bar(aes(fill = result), position = "fill") #Normalized

ggplot(clean3, aes(CONTRIBUTING.FACTOR.VEHICLE.1)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") 

# Contingency Tables for Clean3
Combined_Table <- table(clean3$Hurt_Status, clean3$CONTRIBUTING.FACTOR.VEHICLE.1)
Combined_Table_Margins <- addmargins(A = Combined_Table, FUN = list(total = sum), quiet = TRUE)
Combined_Table_Prop <- round(prop.table(Combined_Table_Margins, margin = 2)*200, 1)

Killed_Table <- table(clean3$Killed, clean3$CONTRIBUTING.FACTOR.VEHICLE.1)
Killed_Table_Margins <- addmargins(A = Killed_Table, FUN = list(total = sum), quiet = TRUE)
Killed_Table_Propround(prop.table(Killed_Table_Margins, margin = 2)*200, 1)

Injured_Table <- table(clean3$Injured, clean3$CONTRIBUTING.FACTOR.VEHICLE.1)
Injured_Table_Margins <- addmargins(A = Injured_Table, FUN = list(total = sum), quiet = TRUE)
Injured_Table_Prop <- round(prop.table(Injured_Table_Margins, margin = 2)*200, 1)

#library(usethis)
#use_git_config(user.name = "c-deras", user.email = "coderasr@gmail.com")
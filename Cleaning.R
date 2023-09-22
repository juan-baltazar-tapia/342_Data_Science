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

#select variables of interests and convert
clean <- clean %>% 
  select(NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED, CONTRIBUTING.FACTOR.VEHICLE.1, CONTRIBUTING.FACTOR.VEHICLE.2,
         CONTRIBUTING.FACTOR.VEHICLE.3, CONTRIBUTING.FACTOR.VEHICLE.4, CONTRIBUTING.FACTOR.VEHICLE.5) %>% 
  drop_na(CONTRIBUTING.FACTOR.VEHICLE.1) %>% 
  mutate(Injured = ifelse(NUMBER.OF.PERSONS.INJURED > 0, "Yes", "No")) %>% 
  mutate(Killed = ifelse(NUMBER.OF.PERSONS.KILLED > 0, "Yes", "No")) %>% 
  select(-c(NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED))

#categorize
clean2 <- clean

for (i in 1:dim(clean2[1])) {
  if (clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Driver_Factors == TRUE) {
    clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] <-  "driver" 
  } else if (clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Environmental_Factors == TRUE) {
    clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "environmental"
  } else if (clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Road_Factors == TRUE) {
    clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "road"
  }else if (clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Tech_Factors == TRUE) {
    clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "tech"
  } else if (clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] %in% Vehicle_Factors == TRUE) {
    clean2$CONTRIBUTING.FACTOR.VEHICLE.1[i] <- "vehicle"
  } else {
  }
}

cleantable <- table(clean$CONTRIBUTING.FACTOR.VEHICLE.1, clean$Injured) + table(clean$CONTRIBUTING.FACTOR.VEHICLE.2, clean$Injured)
index[index == "Illness"] <- "driver"
index[index == "Outside Car Distraction"] <- "environmental"
index[index == "Passing or Lane Usage Improper"] <- "road"
index[index == "Traffic Control Device Improper/Non-Working"] <- "driver"


clean2[1] <- index

clean2 <- clean %>% 
  group_by(CONTRIBUTING.FACTOR.VEHICLE.1, Injured) %>% 
  summarise(count = n()) %>% 
  spread(Injured, count) %>% 
  select(-`<NA>`) 

library(usethis)
use_git_config(user.name = "c-deras", user.email = "coderasr@gmail.com")

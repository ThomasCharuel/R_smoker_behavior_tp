library(readxl)
library(lubridate)
library(dplyr)

# Load dataset
userdata <- read_excel("userdata.xlsx")

# Transform dataset
userdata$Type <- as.factor(userdata$Type)
# Column with corresponding weekdays
userdata$Weekday <- wday(userdata$Time, label=TRUE)
# Column with time intervals
userdata$TimeInterval[hour(userdata$Time) >= 6 & hour(userdata$Time) < 12] <- "6 to 11h59"
userdata$TimeInterval[hour(userdata$Time) >= 12 & hour(userdata$Time) < 18] <- "12 to 17h59"
userdata$TimeInterval[hour(userdata$Time) >= 18 & hour(userdata$Time) < 24] <- "18 to 23h59"
userdata$TimeInterval[hour(userdata$Time) >= 0 & hour(userdata$Time) < 6] <- "00h to 5h59"
userdata$TimeInterval <- as.factor(userdata$TimeInterval)
# Column with week number
# Get start date for each user
start_date_per_user = aggregate(userdata[, c('Time')], list(userdata$User), min)
colnames(start_date_per_user) <- c("User", "StartDate")
# Merge the two dataframes
userdata <- merge(x = userdata, y = start_date_per_user, by = "User")
# Find the week number with computing week differences with start date
userdata$WeekNumber <- as.integer(difftime(userdata$Time, userdata$StartDate, units = "weeks"))
# Observation week is supposed to be week 0, so we change every week number to 0 for observation weeks
userdata$WeekNumber[userdata$Type == "Observation week"] <- 0

### User stats

## Number of consumed cigarettes Statistics

# Total number of cigarettes (all modes)
nb_cigarettes = aggregate(userdata$User, by=list(userdata$User), FUN=length)
colnames(nb_cigarettes) <- c("User", "Total")
nb_cigarettes

# Total number of cigarettes per mode
nb_cigarettes_per_mode = aggregate(userdata$Type, by=list(userdata$User, userdata$Type), FUN=length)
colnames(nb_cigarettes_per_mode) <- c("User", "Mode", "Total")
nb_cigarettes_per_mode

# Mean and standard deviation of the number of consumed cigarettes per weekday 
# (Mondays, Tuesdays, ...) with the corresponding plot

# Find mean
# Count nb cigarettes per week day
nb_cigarettes_per_weekday_per_user <- aggregate(userdata[, c("User")], list(userdata$User, userdata$Weekday), FUN=length)
colnames(nb_cigarettes_per_weekday_per_user) <- c("User", "Weekday", "Total")

# Find the number of weeks
nb_weeks_per_user <- aggregate(userdata$WeekNumber, list(userdata$User), FUN=max)
colnames(nb_weeks_per_user) <- c("User", "NbWeeks")

# Now we divide the nb of cigarettes per week day by the number of weeks to find the mean
merge(nb_cigarettes_per_weekday_per_user, nb_weeks_per_user, by="User")

aggregate(userdata[, ], list(userdata$User, userdata$Weekday), FUN=mean)
# Find standard deviation
sd(nb_cigarettes_per_weekday$Total)

# Plot of the number of consumed cigarettes for the last seven days

# Statistics on modes
summary(userdata$Type)

# Percentage of improvement per week for week 1 and week 2


## Smoking pattern

## Smoking density per week

### General all-user stats

## Smoking intervals

## 
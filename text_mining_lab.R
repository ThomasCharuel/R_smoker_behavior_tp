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
# Columns with day, week number
# Get start date for each user
start_date_per_user = aggregate(userdata[, c('Time')], list(userdata$User), min)
colnames(start_date_per_user) <- c("User", "StartDate")
# Merge the two dataframes
userdata <- merge(x = userdata, y = start_date_per_user, by = "User")
# Find the day number with computing week differences with start date
userdata$DayNumber <- as.integer(difftime(userdata$Time, userdata$StartDate, units = "days"))
# Find the week number by looking at the day number
userdata$WeekNumber <- as.integer(userdata$DayNumber / 7)
# Observation week is supposed to be week 0, so we change every week number to 0 for observation weeks
userdata$WeekNumber[userdata$Type == "Observation week"] <- 0
# Find the last day for each user
lastDayPerUser <- summarize(group_by(userdata, User), lastDay=max(DayNumber))
userdata <- merge(userdata, lastDayPerUser)

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

# First we group by user, weekday and week number, to count the number of cigarettes per week day of each week
byUserPerWeekdayPerWeek <- group_by(userdata, User, Weekday, WeekNumber)
cigarettes_per_weekday_per_week <- summarize(byUserPerWeekdayPerWeek, count=n())
# Then we compute the mean and standard deviation of consumed cigarettes per weekday and user
byUserPerWeekday <- group_by(cigarettes_per_weekday_per_week, User, Weekday)
mean_sd_cigarettes_per_weekday <- summarize(byUserPerWeekday, mean=mean(count), sd=sd(count))

# Plot of the number of consumed cigarettes for the last seven days
# Filter to keep the last 7 days for each user
last_seven_days <- group_by(userdata[userdata$DayNumber > (userdata$lastDay - 7),], User)
nb_consumed_cigarettes_last_seven_days <- summarize(last_seven_days, count=n())

# Statistics on modes
summary(userdata$Type)

# Percentage of improvement per week for week 1 and 2
# Count number of consummed cigarettes for week 0, 1 and 2 without friend mode
a <- merge(
  x = merge(
    x = summarize(group_by(userdata[userdata$WeekNumber == 0 & userdata$Type != "Friend",], User), countWeek0 = n()),
    y = summarize(group_by(userdata[userdata$WeekNumber == 1 & userdata$Type != "Friend",], User), countWeek1 = n())
  ),
  y = summarize(group_by(userdata[userdata$WeekNumber == 2 & userdata$Type != "Friend",], User), countWeek2 = n())
)
a$ImprovementWeek1 <- ((a$countWeek0 - a$countWeek1) / a$countWeek0) * 100
a$ImprovementWeek2 <- ((a$countWeek1 - a$countWeek2) / a$countWeek1) * 100

## Smoking pattern


## Smoking density per week
# Count number of cigarettes per user per week period
cigarettesCountPerWeekPeriodPerUser <- summarize(group_by(userdata, User, Weekday, TimeInterval), count=n())
# Compute least and most smoking density per user
leastSmokingDensityPerUser <- aggregate(count ~ User, cigarettesCountPerWeekPeriodPerUser, min)
mostSmokingDensityPerUser <- aggregate(count ~ User, cigarettesCountPerWeekPeriodPerUser, max)

# Merge to link a user to his least smoking density week period
leastSmokingDensityPerUserPerWeekPeriod <- merge(cigarettesCountPerWeekPeriodPerUser, leastSmokingDensityPerUser)
mostSmokingDensityPerUserPerWeekPeriod <- merge(cigarettesCountPerWeekPeriodPerUser, mostSmokingDensityPerUser)

# remove rows with identical user and count
leastSmokingDensityPerUserPerWeekPeriod <- leastSmokingDensityPerUserPerWeekPeriod[!duplicated(leastSmokingDensityPerUserPerWeekPeriod[,c('User','count')]),]
mostSmokingDensityPerUserPerWeekPeriod <- mostSmokingDensityPerUserPerWeekPeriod[!duplicated(mostSmokingDensityPerUserPerWeekPeriod[,c('User','count')]),]

# Remove count column
leastSmokingDensityPerUserPerWeekPeriod <- leastSmokingDensityPerUserPerWeekPeriod[, c("User", "Weekday", "TimeInterval")]
mostSmokingDensityPerUserPerWeekPeriod <- mostSmokingDensityPerUserPerWeekPeriod[, c("User", "Weekday", "TimeInterv


### General all-user stats

## Smoking intervals

## 
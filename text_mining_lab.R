library(readxl)

# Load dataset
userdata <- read_excel("userdata.xlsx")

# Transform dataset
userdata$Type <- as.factor(userdata$Type)

### User stats

## Number of consumed cigarettes Statistics

# Total number of cigarettes (all modes)
nb_cigarettes = nrow(userdata)
print(paste('Total number of cigarettes: ', nb_cigarettes))
# Total number of cigarettes per mode
nb_cigarettes_per_mode = aggregate(userdata$Type, by=list(userdata$Type), FUN=length)
colnames(nb_cigarettes_per_mode) <- c("Mode", "Total")
nb_cigarettes_per_mode

# Mean and standard deviation of the number of consumed cigarettes per weekday 
# (Mondays, Tuesdays, ...) with the corresponding plot
# First we need to add a column with corresponding weekdays
userdata$Weekday <- weekdays(userdata$Time)
# Find the total number of cigarettes per weekday
nb_cigarettes_per_weekday = aggregate(userdata$Weekday, by=list(userdata$Weekday), FUN=length)
colnames(nb_cigarettes_per_weekday) <- c("Weekday", "Total")
# Find mean
mean(nb_cigarettes_per_weekday$Total)
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
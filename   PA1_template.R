## Loading and Pre-processing the Data
# Load the data
data <- read.csv("activity.csv")

# Check the structure of the data
str(data)

## Mean Total Number of Steps Taken per Day
# Calculate total number of steps per day
total_steps_per_day <- aggregate(steps ~ date, data, sum)

# Make a histogram
hist(total_steps_per_day$steps, main = "Total Number of Steps Taken Each Day", xlab = "Total Steps")

# Calculate mean and median
mean_steps <- mean(total_steps_per_day$steps)
median_steps <- median(total_steps_per_day$steps)

## Average Daily Activity Pattern
# Calculate average number of steps for each 5-minute interval
average_steps_per_interval <- aggregate(steps ~ interval, data, mean)

# Make a time series plot
plot(average_steps_per_interval$interval, average_steps_per_interval$steps, type = "l", 
     xlab = "5-Minute Interval", ylab = "Average Steps", main = "Average Daily Activity Pattern")

# Find the interval with maximum average steps
max_interval <- average_steps_per_interval$interval[which.max(average_steps_per_interval$steps)]

## Imputing Missing Values
# Calculate total number of missing values
missing_values <- sum(is.na(data$steps))

# Fill in missing values using mean or median strategy

data_imputed <- data
data_imputed$steps[is.na(data_imputed$steps)] <- ave(data_imputed$steps, 
                                                     data_imputed$interval, 
                                                     FUN = function(x) mean(x, na.rm = TRUE))

# Recalculate total steps per day with filled-in missing values
total_steps_per_day_imputed <- aggregate(steps ~ date, data_imputed, sum)

# Make histogram
hist(total_steps_per_day_imputed$steps, main = "Total Number of Steps Taken Each Day (Imputed)", 
     xlab = "Total Steps")

# Calculate mean and median
mean_steps_imputed <- mean(total_steps_per_day_imputed$steps)
median_steps_imputed <- median(total_steps_per_day_imputed$steps)

## Differences in Activity Patterns Between Weekdays and Weekends
# Create factor variable for weekdays and weekends
data_imputed$date <- as.Date(data_imputed$date)
data_imputed$day <- weekdays(data_imputed$date)
data_imputed$day_type <- ifelse(data_imputed$day %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Calculate average steps per interval for weekdays and weekends
average_steps_by_day_type <- aggregate(steps ~ interval + day_type, data_imputed, mean)

# Make panel plot
library(ggplot2)
ggplot(average_steps_by_day_type, aes(x = interval, y = steps)) +
  geom_line() +
  facet_grid(day_type ~ .) +
  labs(x = "5-Minute Interval", y = "Average Steps", 
       title = "Average Daily Activity Pattern by Day Type")


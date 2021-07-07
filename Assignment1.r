# Introduction to Data Science - 11372

# Raavin Ashwath Sundar Rajan - u3189852

# Inrtoduction of Data Science Assignment 1 consisting of Part A,B,C

# When you run the code please view the dataFrame table from the "Global Environment" and not the title table that displays/opens after running the code.

setwd("~/Desktop/u3189852_DataScience")



##################### Part A #####################



# load the tidyverse library
library(tidyverse)

# Load the dplyr
library(dplyr)

# data files (you need to specify the paths of the CSV files (e.g. relative or absolute) )

files <- c(
  "data/201808.csv",
  
  "data/201809.csv",
  
  "data/201810.csv",
  
  "data/201811.csv",
  
  "data/201812.csv",
  
  "data/201901.csv",
  
  "data/201902.csv",
  
  "data/201903.csv",
  
  "data/201904.csv",
  
  "data/201905.csv",
  
  "data/201906.csv",
  
  "data/201907.csv",
  
  "data/201908.csv"
)

dataFrame <- NULL


# read the files one by one and append the new rows, you may skip the first 7 rows as they are meta-data
for (i in 1:13) {
  dataFrame <- rbind(dataFrame, read.csv(files[i], skip = 7))
}

#Convert to tibble
dataFrame <- as.tibble(dataFrame)

# inspect the structure of data object
str(dataFrame)

# you may view the data with R studio viewer
view(dataFrame)

# Check for problems
problems(dataFrame)

# assert that there is NO problems
assertthat::assert_that(nrow(problems(data)) == 0,
                        msg = "There is still problem/s, which you need to fix first")

# Print data dimensions
dim(dataFrame)




##################### Part B #####################




# 1. Remove the variables, which have no data at all (i.e. all the records in these variables are NAs)
dataFrame <- dataFrame[colSums(!is.na(dataFrame)) > 0]

#2. Drop the variables, which have few data (i.e. NAs values are more than 90% of number of records in these variables).
dataFrame <- dataFrame[, -which(colSums(is.na(dataFrame)) > 0.9)]

#3. Change the column names to have no spaces between the words and replace these spaces with underscore the character.
names(dataFrame) <-
  gsub("\\.", "_", make.names(names(dataFrame), unique = T))

#4. Change the type of the column called Date from character to Date data type.
dataFrame$Date <- as.Date(dataFrame$Date, format = "%d/%m/%Y")

#5. Add a new column and name it Month, you may extract the contents of this column from the Date column.
dataFrame$Month <- format(dataFrame$Date, format = "%B")

#6. Change the type of ? Month column from Character to Ordinal with levels as the number of months (i.e. 13)
dataFrame$Month <- as.factor(dataFrame$Month)

#7. For all of the numeric columns, replace the remaining NAs with the median value of the values in the column.
dataFrame %>% mutate_all( ~ ifelse(is.na(.), median(., na.rm = TRUE), .)) #The result for this would make no changes as there is no numeric data available.





##################### Part C #####################



#1. Show the summary (i.e. min, 1st Qu., median, mean, 3rd Qu., max) of each of the following variables:
# a. ` Minimum_temperature`,
summary(dataFrame$Minimum_temperature)

# b. `Maximum_temperature`,
summary(dataFrame$Maximum_temperature)

# c. `9am_Temperature`,
summary(dataFrame$"X9am_Temperature")

# d. `3pm_Temperature`
summary(dataFrame$"X3pm_Temperature")

# e. ` Speed_of_maximum_wind_gust_(km/h)`.
summary(dataFrame$"Speed_of_maximum_wind_gust__km_h_")

# 2. Extract the mean of minimum temperature by month
group_by(dataFrame, Month) %>% summarize(Mean = mean(Minimum_temperature))

# 3. Extract the mean of maximum temperature by month
group_by(dataFrame, Month) %>% summarize(Mean = mean(Maximum_temperature))

# 4. Extract the mean of speed of maximum wind gust by direction of maximum wind gust
group_by(dataFrame, Direction_of_maximum_wind_gust) %>% summarize(Mean = mean(Speed_of_maximum_wind_gust__km_h_))

# 5. Which month has the highest rain fall quantity?
dataFrame[which.max(dataFrame$Rainfall__mm_), "Month"]

# 6. Which months were dry, if any, (i.e. no rainfall at all)?

count <- dataFrame[(dataFrame$Rainfall__mm_) == 0, "Month"] %>%
    lapply(table) %>%
    lapply(as.data.frame)
count


# 7. What about the humidity, which month in the ACT has the highest humidity level in the last year?
dataFrame$Humidity <- dataFrame$X9am_relative_humidity____ + dataFrame$X3pm_relative_humidity____
as.data.frame(summarise(group_by(dataFrame , Month), "Humidity" = sum(Humidity)))


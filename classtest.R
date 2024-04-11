#q1
london_crime<-read.csv("london-crime-data.csv")
View(london_crime)
str(london_crime)

# Amalgamate the day, month, and year variables into a new variable called Date
london_crime$day
london_crime$Date <- paste(london_crime$day, london_crime$month, london_crime$year)
london_crime$Date
# Optionally, you can convert the Date variable to the Date format
london_crime$Date <- as.Date(london_crime$Date, format = "%Y-%d-%m")
london_crime$Date

#q2
names(london_crime)
library(data.table)


# Renaming variables
names(london_crime) <- c("Borough", "MajorCategory", "SubCategory", "Value", "CrimeDate")

# Dropping unrequired variables
london_crime <- subset(london_crime, select = c("Borough", "MajorCategory", "SubCategory", "Value", "CrimeDate"))

str(london_crime)

#q3
london_crime$CrimeDate<-as.Date(london_crime$CrimeDate, format = "%Y-%d-%m" )
str(london_crime)

#q4
summary_borough <- table(london_crime$Borough)
summary_borough
barplot(summary_borough, main = "Crime Summary by Borough", xlab = "Borough", ylab = "Number of Crimes")

# Finding the borough with the highest level of crime
# Add a comment to indicate which borough has the highest level of crime
# The borough with the highest level of crime is the one with the maximum count in the summary
max_crime_borough <- names(summary_borough)[which.max(summary_borough)]
max_crime_borough

# Finding the area with the lowest level of crime
# Add a comment to indicate which area has the lowest level of crime
# The area with the lowest level of crime is the one with the minimum count in the summary
min_crime_borough <- names(summary_borough)[which.min(summary_borough)]
min_crime_borough

#q5

summary_major_category <- table(london_crime$MajorCategory)


pie(summary_major_category, main = "Major Categories of Crime in London")

# Finding the MajorCategory with the highest level of crimes

max_crime_major_category <- names(summary_major_category)[which.max(summary_major_category)]
max_crime_major_category

# Finding the MajorCategory with the lowest level of crimes

min_crime_major_category <- names(summary_major_category)[which.min(summary_major_category)]
min_crime_major_category

#q6
region_lookup <- data.frame(
  Borough = c("Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "Croydon", "Ealing", "Enfield", 
              "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey", "Harrow", "Havering", "Hillingdon", "Hounslow", 
              "Islington", "Kensington and Chelsea", "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", 
              "Redbridge", "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", "Wandsworth", 
              "Westminster"),
  Region = c("East", "North", "East", "West", "South", "North", "South", "West", "North", "East", "North", "West", "North",
             "West", "East", "West", "West", "Central", "Central", "East", "Central", "Central", "South", "East", "East", 
             "West", "Central", "South", "Central", "Central", "East", "Central")
)
region_lookup

# Merge the region_lookup with London_crime dataset based on Borough
london_crime <- merge(london_crime, region_lookup, by = "Borough", all.x = TRUE)
london_crime

# Check for NA values in the Region column
na_regions <- sum(is.na(london_crime$Region))
na_regions

# If there are NA values, replace them with a suitable Region
if (na_regions > 0) {
  london_crime$Region[is.na(london_crime$Region)] <- "Unknown"
}

# Check again for NA values in the Region column
na_regions_after_replace <- sum(is.na(london_crime$Region))
na_regions_after_replace



#q7
crime_by_region <- table(london_crime$Region)
crime_by_region

# Plot the number of reported crimes by region
plot(crime_by_region, type = "h", main = "Reported Crimes by Region in London", xlab = "Region", ylab = "Number of Crimes")

# Finding the region with the highest number of crimes
max_crime_region <- names(crime_by_region)[which.max(crime_by_region)]
max_crime_count <- max(crime_by_region)
max_crime_count
max_crime_region
# Finding the region with the lowest number of crimes
min_crime_region <- names(crime_by_region)[which.min(crime_by_region)]
min_crime_count <- min(crime_by_region)
min_crime_region
min_crime_count


#q8
#subset of data with highest number of crimes
highest_crime_subset <- subset(london_crime, Region == max_crime_region)
highest_crime_subset
# Subset of data with the lowest number of crimes
lowest_crime_subset <- subset(london_crime, Region == min_crime_region)
lowest_crime_subset

# Comment the major crime category of both regions
# For the highest crime region
table(highest_crime_subset$MajorCategory)

# For the lowest crime region
table(lowest_crime_subset$MajorCategory)

#q9
par(mfrow=c(1,2)) # Set up the plotting layout with 1 row and 2 columns

# Plotting the highest crime region subset
barplot(table(highest_crime_subset$MajorCategory), main = paste("Major Categories of Crime in", max_crime_region),
        xlab = "Major Category", ylab = "Number of Crimes", las = 2)

# Plotting the lowest crime region subset
barplot(table(lowest_crime_subset$MajorCategory), main = paste("Major Categories of Crime in", min_crime_region),
        xlab = "MajorCategory", ylab = "Number of Crimes", las = 2)

#q10
write.csv(london_crime, file = "london-crime-modified.csv", row.names = FALSE)



#q1
# Read the dataset into a data frame called london_crime
london_crime <- read.csv("london_crime_dataset.csv")  # Replace "london_crime_dataset.csv" with the actual file name and path

# Show the structure of the dataset
str(london_crime)

# Amalgamate the day, month, and year variables into a new variable called Date
london_crime$Date <- paste(london_crime$Day, london_crime$Month, london_crime$Year, sep = "-")

# Optionally, you can convert the Date variable to the Date format
london_crime$Date <- as.Date(london_crime$Date, format = "%d-%B-%Y")











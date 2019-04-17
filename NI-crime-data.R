#Reading the data
Postcodedata <- read.csv("NIPostcodes.csv", header = FALSE)
Postcodedata

#Generating first 10 rows of the data
head(Postcodedata, 10)

#Showing the total number of rows
nrow(Postcodedata)

#preview the structure of the data frame
str(Postcodedata)

#Adding title attributes for the data by creating vector
title <- c("OrganisationName", "SubBuildingName", "BuildingName", "Number", "PrimaryThorfare", "AltThorfare", 
          "SecondaryThorfare", "Locality", "Townland", "Town", "County", "Postcode", "x-coordinates", "y-coordinates",
          "PrimaryKey")
title
#assigning column header by colnames function
colnames(Postcodedata) <- title
Postcodedata
head(Postcodedata, 10)

#Replacing the null values as NA 
Postcodedata[Postcodedata == ""] <- NA
#Finding the total number of missing values in "Postcodedata"
sum(is.na(Postcodedata))

#finding the mean of missing values in all the columns
colMeans(is.na(Postcodedata))

#Finding the number of missing values in all columns
sapply(Postcodedata, function(x) sum(is.na(x)))

#Categorizing the county attribute 
Postcodedata$County <- as.factor(Postcodedata$County)
Postcodedata$County
str(Postcodedata)

#Moving the Primary key column to the start of the dataset 
Postcodedata <- Postcodedata[c(15, 1:14)]
Postcodedata

#importing the dplyr library 
library(dplyr)

#creating a new dataset "Limavady_data" and storing the neccessary information in a new csv file.
Limavady_data <- subset(Postcodedata, grepl("LIMAVADY",Locality) & grepl("LIMAVADY",Townland) & grepl("LIMAVADY",Town)) 
Limavady_data

View(Limavady_data)
write.csv (Limavady_data, file = "Limavady.csv", row.names = FALSE)

#Saving the modified dataset in a csv file "CleanNIPostcodeData".
write.csv(Postcodedata, file = "CleanNIPostcodeData.csv", row.names = FALSE)

#Importing the data.table library and listing the files into a new dataframe
library(data.table)
data_listing <- dir("NI Crime Data", pattern = '*.csv$', full.names = T, recursive = TRUE)
data_listing

#binding the dataset
AllNICrimeData <- Reduce(rbind, lapply(data_listing, read.csv))

#Counting the number of rows in AllNICrimeData
nrow(AllNICrimeData)

#Modifying the structure of "AllNICrimeData" as requested.
AllNICrimeData <- AllNICrimeData[c(2,5,6,7,10)]
str(AllNICrimeData)

#Factorising the crime type attribute and showing the modified structure of "AllNICrimeData"
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
str(AllNICrimeData)

#Removing the improper location attribute values 
AllNICrimeData$Location <- as.factor(gsub(".*On or near ","",AllNICrimeData$Location))
AllNICrimeData$Location <- as.factor(gsub(".*No Location","",AllNICrimeData$Location)) 
 
AllNICrimeData

#Modifying the location attribute by assigning the "NA" to the empty attribute values.
AllNICrimeData$Location [AllNICrimeData$Location == ""] <- NA

AllNICrimeData

#Selecting all the data excluding the NA identifier 
data <- AllNICrimeData[complete.cases(AllNICrimeData$Location), ]
 
data

#choosing the 1000 random sample of crime data from the "AllNICrimeData" 
random_crime_sample <- data[sample(1:nrow(data), 1000, replace = F),]
random_crime_sample

#Reading the data
CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv") 

#Changing the Location column attributes to uppercase in order to match it with the PrimaryThorfare,
#to find a suitable post code
random_crime_sample$Location <- toupper(random_crime_sample$Location)
random_crime_sample

#Selecting only PrimaryThorfare and Postcode columns and omitting the remaining one's.
df1 <- tbl_df(CleanNIPostcodeData[c(6,13)])
df1 <- na.omit(df1) 

df1
str(random_crime_sample)

#creating a function as "find_a_postcode" to compare PrimaryThorfare from df1 dataframe
#and Location from random_crime_sample dataframe to select the maximun occurence of the Postcode
#by passing the Location attribute in the function
find_a_postcode <- function(Location) { 
  link <- filter(df1, PrimaryThorfare == Location)
  result <- names(which.max(table(link$Postcode)))
  return(result)
} 

#applying lapply function to the Location attribute from random_crime_sample 
#and the find_a_postcode funtion and storing it as a list 
find_a_postcode1 <- lapply(random_crime_sample$Location, find_a_postcode)
str(find_a_postcode1)

find_a_postcode1

#converting the find_a_postcode1 list to a dataframe using sapply function
find_a_postcode1 <- sapply(find_a_postcode1, paste, collaspse= "")
random_crime_sample$postcode <- find_a_postcode1 

random_crime_sample

#saving the modified random crime sample data frame as "random_crime_sample.csv" 
write.csv(random_crime_sample, file = "random_crime_sample.csv", row.names = FALSE)

#Extracting the data into a new dataframe "updated_random_sample" 
updated_random_sample <- copy(random_crime_sample)
names(updated_random_sample)

#creating the dataframe "chart_data" and sorting it using the grepl function as requested 
#and showing the summary statistics for the crime type.
chart_data <- filter(random_crime_sample, grepl("BT1", postcode))
chart_data <- chart_data[with(chart_data, order(postcode, Crime.type)), ]
chart_data

summary(chart_data$Crime.type)

#creating a table to plot bar graph which contains the crime type attribute
sample_table <- table(chart_data$Crime.type)

#plotting tha bar graph with the properties shown below.
barplot(sample_table, las = 2, ylab = "Incidents", main="Type of Crimes vs Incidents")

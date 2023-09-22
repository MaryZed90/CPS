
#Define the directory where all the data sets are
csv_dir <- "C:/Users/MaRal/Desktop/Maral-New Dataset/Data setsssss"

# Set the directory 
setwd(csv_dir)
#List all CSV files in the directory:
csv_files <- list.files(csv_dir, pattern = ".csv", full.names = TRUE)

#Create an empty list to store the data frames:
data_frames <- list()

#Loop through each CSV file, read it, and store it in the list of data frames:
for (file in csv_files) {
  data <- read.csv(file = file, fill = TRUE, encoding = "UTF-8")
  data_frames[[file]] <- data
}

#Determine the common column names:
common_column_names <- intersect(names(data_frames[[1]]), names(data_frames[[2]]))

for (i in 3:length(data_frames)) {
  common_column_names <- intersect(common_column_names, names(data_frames[[i]]))
}

#Loop through each data frame, rename the columns to the common names:
for (i in 1:length(data_frames)) {
  colnames(data_frames[[i]]) <- common_column_names
}
#Merge the data frames into on and Write the merged data frame to a CSV file:
merged_data <- do.call(rbind, data_frames)
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)

# Read the merged data from the CSV file and print the column names:
data <- read.csv("merged_data.csv")


#Change the last column name to Year

colnames(data)[53] <-"Year"

str(data)
colnames(data)
getwd()
#overwrite to change the Year column
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)


# Read the CSV file into a data frame

head(data$Year)
# Convert the "Year" column to numeric
data$Year <- as.numeric(data$Year)

# Sort the data frame based on the "Year" column in ascending order
sorted_data <- data[order(data$Year), ]

# Write the sorted data frame to a CSV file
write.csv(sorted_data, file = "sorted_merged_data.csv", row.names = FALSE)

data <- sorted_data
View(data)

#---------------------------------------------------
# Load 
# install.packages("readr")
library(readr)
library(tidyverse)

#To begin, we are going to run the head function,
#which allows us to see the first 6 rows by default. We are going to override the default and ask to 
#preview the first 10 rows.

head(data)
dim(data)

#Displays the type and a preview of all columns as a row using glimpse from “dplyr” library
# install.packages("dplyr")
library(dplyr)
glimpse(data)
summary(data)

#SKIMR SKIM FUNCTION 
#Next we run the skim function from the skimr package.
#The skim function is a good addition to the summary function. 
#It displays most of the numerical attributes from summary, but it also displays missing values, more quantile information and an inline histogram for each variable! install.

# install.packages("skimr")
library(skimr)
skim(data)

#VISDAT VIS_DAT FUNCTION
#The Vis_dat() function is a great way to visualize the data type and missing data within a data frame.

# install.packages("devtools")
library(devtools)
# devtools::install_github("ropensci/visdat")
library(visdat)
vis_miss(data)

vis_dat(data)

#Data Cleaning

#1. Removal of Irrelevant or Incomplete Data
#Exclude irrelevant columns
colnames(data)
View(data)
#Here I've sorted out to show more columns in Rstudio
# rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

irrelevant_columns <- c("Percentage.of.L.Motoring.Offences.Unsuccessful")
data <- data[, !(colnames(data) %in% irrelevant_columns)]

# check if it's worked
colnames(data)


object.size(data)

#Initial look at the data frame
str(data)
summary(data)


#For better analysis I remove percentage columns 

# Specify the word you want to search for in column names
word_to_remove <- "Percentage"

# Identify the columns that contain the specified word
columns_to_remove <- grep(word_to_remove, colnames(data), ignore.case = TRUE)

# Remove the identified columns from the dataset
cleaned_data <- data[, -columns_to_remove]
str(cleaned_data)




#names of columns are too long, so we make it short(**)
new_column_names<-c("City","Homicide","Homicide_Unsuccessful","Against_Person","Against_Person_Unsuccessful","Sexual_Offences","Sexual_offences_Unsuccessful",
                    "Burglary","Burglary_Unsuccessful","Robbery","Robbery_Unsuccessful","Theft_and_Handling","Theft_and_Handling_Unsuccessful","Fraud_and_Forgery","Fraud_and_Forgery_unsuccessful",
                    "Criminal_Damage", "Criminal_Damage_Unsuccessful","Drugs_Offences", "Drug_Offences_Unsuccessful","Public_Order","Public_Order_Unsuccessful",
                    "Other","Other_unsuccessful","Motoring","Motoring_Unsuccessful","Admin_Unsuccessful","Month","Year")

colnames(cleaned_data) <- new_column_names                    
colnames(cleaned_data)
#Remove National rows, because it was irrelevant 
cleaned_data <- cleaned_data[cleaned_data$City != "National", ]
View(cleaned_data)


# Check for missing values in the dataset
missing_values <- is.na(cleaned_data)

# Count the number of missing values in each column
missing_counts <- colSums(missing_values)

missing_columns <- names(missing_counts[missing_counts > 0])

missing_counts[missing_counts > 0]

#Changing data types into integer 

# Identify the columns to convert
convert_columns <- 2:(ncol(cleaned_data) - 2)  # Columns to convert: excluding first column and last two columns

# Loop through the identified columns and convert character to integer
for (i in convert_columns) {
  cleaned_data[, i] <- as.integer(cleaned_data[, i])
}


str(cleaned_data)

# Check for duplicate rows in the dataset

duplicates <- duplicated(cleaned_data)

# Check if there are any duplicate rows
if (any(duplicates)) {
  # Duplicates exist, perform cleaning
  cleaned_data <- cleaned_data[!duplicates, ]
  message("Duplicates removed from the dataset.")
} else {
  # No duplicates found
  message("No duplicates found in the dataset.")
}

#in this part After checking the NA numbers in summary I've decided that it is better to remove the metropolitan and city row 

# Filter rows with a specific name using filter

cleaned_data <- cleaned_data[cleaned_data$City != "Metropolitan and City", ]
summary(cleaned_data)

write.csv(cleaned_data, file = "cleaned_data.csv", row.names = FALSE)


#c. Descriptive Analytics
#DATA VISUALISATION



# Convert Month and Year columns to Date format

month_lookup <- c("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5, "June" = 6,
                  "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12)

cleaned_data$MonthNumeric <- month_lookup[cleaned_data$Month]

cleaned_data$Month <- sprintf("%02d", cleaned_data$MonthNumeric)  # Convert month to two digits format
cleaned_data$Date <- as.Date(paste(cleaned_data$Year, cleaned_data$Month, "01", sep = "-"))  # Use "01" as the day value

write.csv(cleaned_data, file = "cleaned_data.csv", row.names = FALSE)

str(cleaned_data)


#EDA for Hypothesis one:
#1-Calculate the summary statistics

str(cleaned_data$Drugs_Offences)
str(cleaned_data$Homicide)

# Summary statistics for Drug_Offenses
summary(cleaned_data$Drugs_Offences)

# Summary statistics for Homicide_Convictions
summary(cleaned_data$Homicide)

# Mean of Drug_Offenses
mean(cleaned_data$Drugs_Offences)

# Median of Homicide_Convictions
median(cleaned_data$Homicide)

# Standard deviation of Drug_Offenses
sd(cleaned_data$Drugs_Offences)

#2-Calculate the correlation coefficient:


# Pearson correlation coefficient between Drug_Offenses and Homicide_Convictions
cor(cleaned_data$Drugs_Offences, cleaned_data$Homicide, method = "pearson")

# Spearman correlation coefficient between Drug_Offenses and Homicide_Convictions
cor(cleaned_data$Drugs_Offences, cleaned_data$Homicide, method = "spearman")



#3- Data visualisation for Hypothesis 1:


library(ggplot2)


cleaned_data %>%
  ggplot(aes(x = Formatted_Date, y = Homicide)) +
  geom_line(color = "#69b3a2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# Create a SCATTER PLOT
ggplot(cleaned_data, aes(x = Drugs_Offences, y = Homicide)) +
  geom_point() +
  xlab("Number of Drugs Offences") +
  ylab("Number of Homicide Convictions") +
  ggtitle("Relationship between Drug Offenses and Homicide Convictions")


#BARPLOT

# Calculate the average or total number of drug offenses and homicide convictions by city
average_drugs_offences <- aggregate(Drugs_Offences ~ City, cleaned_data, mean)
total_homicide <- aggregate(Homicide ~ City, cleaned_data, sum)

# Create a bar plot for average drug offenses
# Create a bar plot for average drug offenses
barplot(average_drugs_offences$Drugs_Offences, names.arg = average_drugs_offences$City,
        xlab = "City", ylab = "Average Drug Offenses",
        main = "Average Drug Offenses by City",
        las = 2)  # Rotate labels vertically



# Calculate the average  number of drug offenses and homicide convictions by city
cleaned_data$Formatted_Date <- format(as.Date(cleaned_data$Date), "%Y-%b")
write.csv(cleaned_data, file = "cleaned_data.csv", row.names = FALSE)


date_avg_drugs_offences <- aggregate(Drugs_Offences ~ Formatted_Date, cleaned_data, mean)
date_total_homicide <- aggregate(Homicide ~ Formatted_Date, cleaned_data, sum)

# Create a data frame or tibble
plot_data <- data.frame(Formatted_Date = date_avg_drugs_offences$Formatted_Date,
                        Average_Drugs_Offences = date_avg_drugs_offences$Drugs_Offences,
                       total_Homicide_Convictions = date_total_homicide$Homicide)

#Creating BARPLOT

# Set the width of the bars
bar_width <- 0.35

# Creating grouped bar plot using ggplot2
ggplot(plot_data, aes(x = Formatted_Date)) +
  geom_bar(aes(y = Average_Drugs_Offences, fill = "Average Drugs Offences"),
           position = "dodge", stat = "identity", width = bar_width) +
  geom_bar(aes(y = total_Homicide_Convictions, fill = "Total Homicide Convictions"),
           position = "dodge", stat = "identity", width = bar_width) +
  xlab("Date") +
  ylab("Average Count") +
  ggtitle("Average Drug Offenses and Total Homicide Convictions by Date") +
  scale_fill_manual(values = c("Average Drugs Offences" = "blue", "Total Homicide Convictions" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#EDA for Hypothesis two:

# Calculate mean conviction for each month across all cities

library(ggplot2)
library(dplyr)

# Get unique months and years from the dataset
months <- unique(cleaned_data$Month)
years <- unique(cleaned_data$Year)

mean_values <- list()

for (year in years) {
  for (month in months) {
    mean_value <- cleaned_data %>%
      filter(Month == month, Year == year) %>%
      summarise(mean_value = mean(Fraud_and_Forgery, na.rm = TRUE))
    
    mean_values[[paste(month, year, sep = "_")]] <- mean_value
  }
}


#Data visualisation for Hypothesis 2:

cleaned_data %>%
  ggplot(aes(x = Formatted_Date, y = Fraud_and_Forgery, group = City, color = City)) +
  geom_line(stat = "summary", fun = "mean") +
  ggtitle("Mean Fraud  by City Over Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = rainbow(length(unique(cleaned_data$City))))

cleaned_data %>%
  ggplot(aes(x = Year, y = Fraud_and_Forgery, group = City, fill = City)) +
  geom_area() +
  ggtitle("Mean Fraud by City Over Time") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  facet_wrap(~City)




# Convert year-month values to Date class
formatted_dates <- as.Date(paste0(month_list, "-01"), format = "%Y-%b-%d")

# Order the formatted dates
ordered_dates <-month_list[order(formatted_dates)]

# Print the ordered dates
print(ordered_dates)

o = 1
for (i in ordered_dates)
{
  mean_df[o] = mean(cleaned_data[cleaned_data$Formatted_Date == i, 'Fraud_and_Forgery'])
  o = o + 1
}

# barplot(unlist(mean_df),unlist(ordered_dates))

barplot(unlist(mean_df), names.arg = unlist(ordered_dates),
        xlab = "Date", ylab = "Average Count",las=2)


sum_df = list()
l = 1
for (i in ordered_dates)
{
  sum_df[l] = sum(subset(cleaned_data, Formatted_Date == i)$'Fraud_and_Forgery')
  l = l + 1
}


barplot(unlist(sum_df), names.arg = unlist(ordered_dates),
        xlab = "Date", ylab = "Average Count",las=2)



# ------------------------------------CORRELATION AND COVARIANCE 

# Exclude columns City, Year, Month, Date, MonthNumeric, Formatted_Date
excluded_columns <- c("City", "Year", "Month", "Date", "MonthNumeric", "Formatted_Date")
selected_columns <- setdiff(names(cleaned_data), excluded_columns)

# Create a new data frame with the selected columns
new_data <- cleaned_data[, selected_columns]

data_cov=cov(new_data, method='pearson')


#correlation matrix



library(Hmisc)
data_cor=cor(new_data, method= 'pearson')
res<-cor(new_data)
# Extract the correlation coefficients
rcorr(cor(new_data), type="pearson")
res2<-rcorr(as.matrix(new_data)) #Significance Level
res2$r
#Extract p-values
res2$p
# Visualize correlation matrix
library(corrplot)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.7)


# Close the plotting device
dev.off()

plot.new()
dev.off()


#correlation coeficient

library(Hmisc)
cor_p=rcorr(as.matrix(new_data))

#corrplot
library(corrplot)

corrplot(data_cor, type= "upper", width=plot_Size, height=plot_Size, tlcol="blue", tl.srt=75, diag=T)





# ----------------------------------Prediction Model Implementation
#Linear regression
library(ggplot2)
library(dplyr)
library(broom)
# install.packages("ggpubr")

library(ggpubr)
library(tidyverse)

#scatterplot
plot(Homicide~Drugs_Offences, data=cleaned_data)
# Simple regression: Drugs  and Homicide

Drugs.Homicide.lm<- lm(Homicide~Drugs_Offences, data=cleaned_data)
summary(Drugs.Homicide.lm)

#Multiple regression
#Let's see if there is a linear relationship between Sexual offences, Against_Person and drugs

Drugs.Offences.lm<- lm(Drugs_Offences~Sexual_Offences+Against_Person, data=cleaned_data)

summary(Drugs.Offences.lm)

# Visualize the results with a graph
#simple regression
Drugs.Homicide.graph<- ggplot(cleaned_data, aes(x=Drugs_Offences, y=Homicide))+geom_point()
#Add the linear regression line to the plotted data
Drugs.Homicide.graph<- Drugs.Homicide.graph+geom_smooth(method="lm",col="black")
Drugs.Homicide.graph
#Add the equation for the regression line

Drugs.Homicide.graph<-Drugs.Homicide.graph+ stat_regline_equation(label.x=3, label.y=7)
Drugs.Homicide.graph

#We will try a different method: plotting the relationship between Sexual Offences and  Drugs at different levels of Against In this example, Drugs  will be treated as a factor with three levels, just for the purposes of displaying the relationships in our data.

#MULTIPLE REGRESSION
plotting.data <- expand.grid(Sexual_Offences = seq(min(cleaned_data$Sexual_Offences),
                                                   max(cleaned_data$Sexual_Offences),
                                                   length.out = 30),
                             Against_Person = c(min(cleaned_data$Against_Person),
                                                mean(cleaned_data$Against_Person),
                                                max(cleaned_data$Against_Person)))

#Next we will save our ‘predicted y’ values as a new column in the dataset we just created.


plotting.data$predicted.y <- predict.lm(Drugs.Offences.lm, newdata=plotting.data)


# Round the Against person numbers to two decimals
plotting.data$Against_Person <- round(plotting.data$Against_Person, digits = 2)
# Change the Against person variable into a factor
plotting.data$Against_Person<- as.factor(plotting.data$Against_Person)
#PLOT the original data

Drugs.plot<- ggplot(cleaned_data, aes(x= Sexual_Offences, y= Drugs_Offences))+ geom_point()


Drugs.plot

#Add the regression line 

Drugs.plot<- Drugs.plot +geom_line(data=plot_data, aes(x=Sexual_Offences,
                                                       y=predicted.y, color=Against_Person), size=1.25)
Drugs.plot

# --------------CLUSTERING

#Applying the Elbow method to find the optimal number of clusters

library(ggplot2)

wcss <- vector()
k_values <- 1:10

# Perform K-means clustering for different cluster numbers
for (k in k_values) {
  model <- kmeans(new_data, centers = k)
  wcss[k] <- model$tot.withinss
}

# Plot the elbow curve
elbow_curve <- data.frame(k = k_values, WCSS = wcss)
ggplot(elbow_curve, aes(x = k, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters", y = "Within-Cluster Sum of Squares") +
  ggtitle("Elbow Curve")



#compute k-means with k=5


library(ggpubr)
library(factoextra)

set.seed(123)#Applying seed value make reproducible results, so that everyone obtain exactly the same results.
res.km <- kmeans(scale(new_data), 5, nstart = 25)#new_data was the name excluded data
print(res.km)

# K-means clusters showing the group of each individual
res.km$cluster

# Plotting the cluster using the function fviz_cluster() 
fviz_cluster(res.km, data = new_data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF0000", "#990099"), # Add more colors
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)







# ----------------------CLASSIFICATION
#1)KNN
##Generate a random number that is 90% of the total number of rows in dataset.
ran<- sample(1:nrow(cleaned_data),0.9*nrow(cleaned_data))
length(ran)

#Because different values have different scaling units,we normalize each one of the variables using this formula:
nor<- function(x){x-min((x))/(max(x)-min(x))}

#Run normalization on columns 2,4,6 because they are the predictors 
cleaned_data_norm<- as.data.frame(lapply(cleaned_data[,c(2,4,6,18)],nor))
summary(cleaned_data_norm)
#Now we split our data into training and testing sets

cleaned_data_train<- cleaned_data_norm[ran,] #this one extract training set
cleaned_data_test<- cleaned_data_norm[-ran,]#rhis one extract testing set

#extract the 16th column of train dataset because it will be used as 'cl' argument in knn function.

cleaned_data_target_category<- cleaned_data[ran,16]
#extract the 16th column if test dataset to measure the accuracy
cleaned_data_test_category<- cleaned_data[-ran,16]

#load the package class
library(class)
#run knn function
pr<- knn(cleaned_data_train, cleaned_data_test, cl=cleaned_data_target_category, k=13)
#create confusion matrix
tab<- table(pr, cleaned_data_test_category)

#this function divides the correct predictions by total number of predictions that tell us how acuurate the model is.
accuracy<- function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab)




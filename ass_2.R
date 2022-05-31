# Include.packages
#install.packages('readr')
#install.packages('stringr')
#install.packages('dplyr')
#install.packages('jpeg')
#install.packages("e1071")
#install.packages("caret")

# Load packages 
library(readr)
library(stringr)
library(dplyr)
library(jpeg)
library(e1071)
library(caret)

# Read CSV file
ds <- read.csv("appstore_games.csv")

# Task 2 ------------------------------------------------------------------

# Remove URL and Name columns
ds$URL <- NULL
ds$Name <- NULL

# Subtitle
#   Add new column that contains 1 if game has review, 0 if it does not
ds$hasSub <- ifelse(ds$Subtitle == '', 0, 1)

#   Remove original subtitle column
ds$Subtitle <- NULL

# In-app Purchases (IAP) as numeric variables
#  Number of different possible In-App purchases in a game
ds$IAP.values <- ifelse(ds$In.app.Purchases == '', 0, 
                        lengths(str_split(ds$In.app.Purchases, ',')))


#   Create dataframe that splits the different IAPs into columns for every game
IAP.df <- as.data.frame(str_split_fixed(ds$In.app.Purchases, 
                                        ',', n = max(ds$IAP.values)))
#   Convert dataframe to numeric
IAP.df[] <- lapply(IAP.df, as.numeric)

#   Calculate sum of IAP per game in a separate vector
ds$IAP.sum <- apply(IAP.df, FUN = sum, na.rm = TRUE, MARGIN = 1)

#   Calculate mean of IAP per game in separate vector
ds$IAP.mean <- apply(IAP.df, mean, na.rm = TRUE, MARGIN = 1)
ds$IAP.mean[which(is.nan(ds$IAP.mean))] <- 0

#   Calculate max. of IAP per game in separate vector
ds$IAP.max <- apply(IAP.df, FUN = max, na.rm = TRUE, MARGIN = 1)
ds$IAP.max[which(is.infinite(ds$IAP.max))] <- 0

#   Calculate min. of IAP per game in separate vector
ds$IAP.min <- apply(IAP.df, FUN = min, na.rm = TRUE, MARGIN = 1)
ds$IAP.min[which(is.infinite(ds$IAP.min))] <- 0

#   Remove dataframe
remove(IAP.df)

# Description
#   Add word count of description
ds$descriptionCount <- sapply(str_split(ds$Description, " "), length)
ds$Description <- NULL

# Developer
ds <- transform(ds,number_of_games = ave(seq(nrow(ds)),Developer,FUN=length))
ds$developer_category <- ifelse(ds$number_of_games >=4, "Professional", "Newbie")

ds$number_of_games < NULL

# Age Rating
#   Transform age rating to binary 4+ and 9+
ds$Age.RatingBinary <- ifelse(ds$Age.Rating == "4+", "4+", "9+")

# Languages
#   assign English Language to games with missing values (Task 3)
ds$Languages <- sub("^$", "EN", ds$Languages)

#   create new attribute for a single language or many languages
ds$Num_of_languages <- ifelse(ds$Languages == '', 0, 
                              lengths(str_split(ds$Languages, ',')))

ds$Languages_amount = ifelse(ds$Num_of_languages == 1, 'single','many')

ds$Num_of_languages < NULL

#   Create new attribute if language is english
ds$Is_Available_in_English <- ifelse(sapply(str_split(ds$Languages, ', '), 
                                            `%in%`, x = 'EN'), "Yes", "No")

# Primary genre
ds$Primary.Genre <- NULL

# Genres

#   Count number of commas and add one to find number of genres per game
ds$number_of_genres <- str_count(ds$Genres, ',') + 1

# Dates

#   Release Month
#   Assign Original Release Date column to a 'date' format (day/month/year)
Original.Release.Date <- as.Date(ds$Original.Release.Date)

#   Extract the month from original release date column into a new column
ds$releasemonth <- months(Original.Release.Date)


#   Elapsed Months
#   Function that takes 03/08/2019 and the current version date as input,
#   and calculates the difference in  months
elapsed_months <- function(end_date, start_date) {
  ed_year <-as.numeric(format(as.Date(end_date, format="%d/%m/%Y"),"%Y"))
  sd_year <- as.numeric(format(as.Date(start_date, format="%d/%m/%Y"),"%Y"))
  sd_mon <- as.numeric(format(as.Date(start_date, format="%d/%m/%Y"),"%m"))
  ed_mon <- as.numeric(format(as.Date(end_date, format="%d/%m/%Y"),"%m"))
  12 * (ed_year - sd_year) + (ed_mon - sd_mon)
}

Current.Version <- as.Date(ds$Current.Version.Release.Date, format="%d/%m/%Y")

#   Use function to create the new column
ds$elapsed_months = elapsed_months(as.Date("03/08/2019",format="%d/%m/%Y"),  Current.Version)

#   Set negative values to zero. This happens if the current release date is past 03/08/2019
ds$elapsed_months <- ifelse(ds$elapsed_months < 0, 0, ds$elapsed_months)

# Game free

#   Create binary column. '1' if no IAP, '0'otherwise
ds$game_free <- ifelse(ds$In.app.Purchases <= "", "1", "0")

# Categorical rating count

#   Store value of median in median_rating, excluding NA
median_rating <- median(ds$User.Rating.Count, na.rm = TRUE)

#   Create column for categorical rating count,  to label high if user rating count is lower than median (5)
ds$categorical_rating_count<- ifelse(is.na(ds$User.Rating.Count), "Low", 
                                     ifelse(ds$User.Rating.Count >= median_rating, "High", "Low"))

# Task 3 ----------------------------------------------------------
# Subtitles
table(is.na(ds$hasSub))
ds$hasSub <- factor(ds$hasSub)

# Icon URL
table(is.na(ds$Icon.URL))

# Average User Rating
ds$Average.User.Rating[which(is.na(ds$Average.User.Rating))] <- 0

# Price
ds <- ds[which(!is.na(ds$Price)),]

# Description
table(is.na(ds$description))

# Age rating
table(is.na(ds$Age.Rating))
ds$Age.Rating <- factor(ds$Age.Rating)
ds$Age.RatingBinary <- factor(ds$Age.RatingBinary)

# Categorical user rating count
#   Assign rating count 5 to games with null values, based on the article
ds$User.Rating.Count[is.na(ds$User.Rating.Count)] <- 5
ds$categorical_rating_count <- factor(ds$categorical_rating_count)

# Task 4 ------------------------------------------------------------------

icon_ds <- ds[c("ID", "Icon.URL", "Average.User.Rating")]

# Lowest rated 200 games
icon_ds_asc <- icon_ds %>% arrange(Average.User.Rating)
icon_ds_lowest <- icon_ds_asc[1:200, 1:2]

# Highest rated 200 games
icon_ds_dsc <- icon_ds %>% arrange(desc(Average.User.Rating))
icon_ds_highest <- icon_ds_dsc[1:200, 1:2]

# Combine two vectors
icon_complete <- full_join(icon_ds_highest, icon_ds_lowest)

# Remove unneeded dataframes
remove(icon_ds, icon_ds_asc, icon_ds_dsc, icon_ds_highest, icon_ds_lowest)

# Download 10 pictures as example
#for (i in 1:10) {
# download.file(icon_complete[i,2], 
#               paste0(icon_complete[i,1],".jpeg"), 
#              mode = 'wb')
#}

# Remove Icon Url Column
ds$Icon.URL <- NULL

# Task 5 ------------------------------------------------------------------

# Identify unneeded columns from dataset
dsCleanCols <- colnames(ds)[which(!colnames(ds) 
                                  %in% 
                                    c("ID","In.app.Purchases",'Developer','Languages','Genres',
                                      'Original.Release.Date',"Current.Version.Release.Date"
                                    ))]
# Remove unneeded columns
dsClean <- ds[dsCleanCols]
# Remove original dataset
remove(ds)

# Split training and test data
pTraining <- 0.8
nTraining <- round(nrow(dsClean) * pTraining)

obsTrain <- sample(1:nrow(dsClean),
                   nTraining)

dsTrainURC <- dsClean[obsTrain,]
dsTestURC <- dsClean[-obsTrain,]

# Dataset without User.Rating.Count
dsTrainNoURC <- dsTrainURC[which(!colnames(dsTrainURC) == "User.Rating.Count")]
dsTestNoURC <- dsTestURC[which(!colnames(dsTestURC) == "User.Rating.Count")]


# Calculation of dumb baseline

#   Option 1
#   A dumb baseline would be an accuracy of 0.5, 
#   because there are two possible values for target variable
1 / length(table(dsTestURC$categorical_rating_count))

#   Option 2
#   ZeroR (always predict the most common value in your dataset)
zeroR <- max(table(dsTestURC$categorical_rating_count)) / 
  sum(table(dsTestURC$categorical_rating_count))

print(paste("ZeroR accuracy:", round(zeroR, 4)))


# Task 6 ------------------------------------------------------------------
# Identify numeric variables
numCols <- c("Average.User.Rating", "User.Rating.Count", 
             "Price", "Size", "IAP.values", "descriptionCount")

# Calc. average and std. deviation of numeric variables in train dataset
trainAvg <- colMeans(dsTrainURC[numCols])
trainStd <- apply(dsTrainURC[numCols], 2, sd)

# Scale train dataset
dsTrainURCScaled <- dsTrainURC
dsTrainURCScaled[numCols] <- scale(dsTrainURCScaled[numCols])

# Scale test dataset using mean and sd from train dataset
dsTestURCScaled <- dsTestURC
dsTestURCScaled[numCols] <- scale(dsTestURCScaled[numCols],
                                  center = trainAvg[numCols],
                                  scale = trainStd[numCols])

# Scale dataset without User.Rating.Count
dsTrainNoURCScaled <- dsTrainURCScaled[which(!colnames(dsTrainURCScaled) == "User.Rating.Count")]
dsTestNoURCScaled <- dsTestURCScaled[which(!colnames(dsTestURCScaled) == "User.Rating.Count")]

# Task 7 ----------------------------------------------------------------------

# Naive Bayes Model

set.seed(120)

#     Training 
Naive_Bayes <- naiveBayes(categorical_rating_count ~ ., data = dsTrainNoURCScaled)

#     Testing
y_pred <- predict(Naive_Bayes, newdata = dsTestNoURCScaled)

#     Reporting results
cm <- table(dsTestNoURC$categorical_rating_count, y_pred)
confusionMatrix(cm)

# Each model is evaluated against the performance of baseline option 2 (see task 5)
# This baseline is chosen as it the better performer among the two baselines.
# Furthermore, it makes sense from a decision-making point of view, to classify an unseen observation as the most frequent class.
# this could be a common decision-making process for simple decisions.

# The option 2 baseline has an accuracy of 0.781.
# The Naive Bayes model has an accuracy of 0.849, therefore it significantly outperforms the baseline model.
# For instance, out of 100 unseen observations, the baseline would misclassify 22 observations, whereas the naive bayes would misclassify only 16.
# As such, the model is positively evaluated and can be used to aid in classification/decision-making.

# K-NN

set.seed(120)

#     Training 
KNN <- gknn(categorical_rating_count ~ ., dsTrainNoURCScaled, k = 3, method = "Manhattan")
KNN

#     Testing
pred<-predict(KNN, dsTestNoURCScaled)

#     Reporting results
cm <- table(dsTestNoURC$categorical_rating_count, pred)
confusionMatrix(cm)

# The K-Nearest Neighbour has an accuracy of 0.859, slighty higher than the  Naive Bayes model.
# Therefore, this models receives a slightly more positive evaluation and is slightly better suited to classify the user rating count of a game.

# SUPPORT VECTOR MACHINE

#     Training
support_vector_machine <- svm(categorical_rating_count ~ ., data = dsTrainNoURCScaled, kernel="linear", type = "C")

#     Testing
pred <- predict(support_vector_machine, newdata = dsTestNoURCScaled)

#     Reporting results
cm <- table(dsTestNoURC$categorical_rating_count, pred)
confusionMatrix(cm)

# The support Vector Machine has an accuracy of 0.8608. 
# it significantly outperforms the baseline predictions, slightly better than the naive bayes model, and equal to the K-Nearest Neighbour model.

#  In conclusion, all three models significantly outperform the baseline model and are suited for this classification task.
#  That being said, the K-Nearest Neighbour and Support Vector Machine both slightly outperformed the naive bayes model.
#  Thus, either of these models are the model of choice for this classification task.

# Save dataframes ---------------------------------------------------------------

save(dsClean,file="data.Rda")
save(dsTrainNoURCScaled, file="training.Rda")
save(dsTestNoURCScaled, file="testing.Rda")

save(dsClean, file ="mydata.Rda") 
load(file= "mydata.Rda") 

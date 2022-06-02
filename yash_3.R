# INCLUDES

#install.packages("mlbench")
#install.packages("tensorflow")
#library(tensorflow)
#install_tensorflow()

library(keras)
library(mlbench)
library(dplyr)
library(neuralnet)
library(caret)
require(tidyr)
library(tensorflow)
#tf$constant("Hellow Tensorflow")


#-------------------------------------------------------
# DATA PREPARATION


load(file= "mydata.Rda")
data <- dsClean
summary(data)
str(data)
head(data)

data <- data %>% 
  mutate(high = ifelse(categorical_rating_count=='High', 1, 0),
         low = ifelse(categorical_rating_count=='Low', 1, 0))


data <- data %>% 
  mutate(Languages_amount = ifelse(Languages_amount=='single', 1, 0))
     

data <- data %>% 
  mutate(Is_Available_in_English = ifelse(Is_Available_in_English=='Yes', 1, 0))

data <- data %>% 
  mutate(developer_category = ifelse(developer_category=='Professor', 1, 0))
         

data$releasemonth <- NULL
data$IAP.values <- NULL
data$IAP.max <- NULL
data$IAP.mean <- NULL
data$IAP.min <- NULL
data$IAP.sum <- NULL



data2 <- mutate_if(data, is.factor,as.numeric)  
data3 <- lapply(data2, function(x) as.numeric(as.character(x)))
data <- data.frame(data3)
data <- na.omit(data)



summary(data)
head(data)
str(data)

#-----------------------------------#
#-------------------------------------------------------
# VISUALIZATION OF THE NEURAL NETWORK

n <- neuralnet(categorical_rating_count ~Num_of_languages+number_of_genres+elapsed_months+game_free+hasSub+IAP.values,
               data = data[700, ],
               hidden = c(10,5),
               startweights = NULL,
               linear.output = F,
               lifesign = 'full',
               rep=1)

plot(n, col.hidden = "darkgreen", 
     col.hidden.synapse = 'darkgreen',
     show.weights = F, 
     information = F, 
     fill = "lightblue")
#------------------------------------------------




#-------------------------------------------------------

# Matrix
data <- as.matrix(data)
dimnames(data) <- NULL 

# Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob=c(0.7, 0.3))
training <- data[ind==1, c(1:19)]
test <- data[ind==2, 1:19]
trainingtarget <- data[ind==1, 19]
testtarget <- data[ind==2, 19]

#-------------------------------------------------------
# FEATURE SCALING

m    <- colMeans(training)
sd   <- apply(training, 2, sd)
training <- scale(training, center = m, scale = sd)
test <- scale(test, center = m, scale = sd)


#------------------------------------------------------

# MODEL CREATION



model <- keras_model_sequential()
model %>%  layer_dense(units = 10, activation = 'relu', input_shape = ncol(data)) %>%    
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dense(units = 1)  
summary(model)



#-------------------------------------------------------
# MODEL BACKPROPAGATION - DESCRIBE HOW MODEL UPDATES ITSELF

model %>% compile(loss = 'binary_crossentropy',  
                  optimizer = 'rmsprop',  
                  metrics = 'mae')

#-------------------------------------------------------
# MODEL TRAINING
mymodel <- model %>%   
  fit(training, 
      trainingtarget, 
      epochs = 50,
      batch_size = 32, #samples per gradient update default 32
      validation_split = 0.2)

plot(mymodel)

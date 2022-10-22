

library(tidyverse)
library(caret)
library(nnet)
library(anyflights)
library(Sherlock)

data("flights")

flights <- flights %>%
  group_by(origin, month, day) %>%
  mutate(Num.Flights.That.Day = n()) %>%
  mutate(Dep.Flight.Status = case_when(dep_delay > 0 ~ "Late",
                                       dep_delay < 0 ~ "Early",
                                       dep_delay == 0 ~ "On Time")) %>%
  mutate(Arr.Flight.Status = case_when(arr_delay > 0 ~ "Late",
                                       arr_delay < 0 ~ "Early",
                                       arr_delay == 0 ~ "On Time"))

trimmed <- flights[,c("Dep.Flight.Status", "Arr.Flight.Status", "dep_time", "arr_time", "sched_dep_time", "sched_arr_time", "carrier", "origin", "dest", "distance", "Num.Flights.That.Day")]  

airlines <- as.data.frame(anyflights::get_airlines())

trimmed <- as.data.frame(trimmed)

trimmed <- left_join(trimmed, airlines, by = "carrier")

trimmed$Dep.Flight.Status <- as.factor(trimmed$Dep.Flight.Status)
trimmed$Arr.Flight.Status <- as.factor(trimmed$Arr.Flight.Status)

trimmed$carrier <- trimws(trimmed$carrier)
trimmed$origin <- trimws(trimmed$origin)
trimmed$dest <- trimws(trimmed$dest)

trimmed$carrier <- as.factor(trimmed$carrier)
trimmed$origin <- as.factor(trimmed$origin)
trimmed$dest <- as.factor(trimmed$dest)
trimmed$name <- as.factor(trimmed$name)

trimmed <- as.data.frame(trimmed)

trimmed <- na.omit(trimmed)

anndata <- trimmed

anndata <- anndata %>%
  mutate_if(is.factor, function(x) as.numeric(x))

# levels(trimmed$Dep.Flight.Status)
# 
# anndata$Dep.Flight.Status_Early <- ifelse(anndata$Dep.Flight.Status == 1, 1, 0)
# anndata$Dep.Flight.Status_Late <- ifelse(anndata$Dep.Flight.Status == 2, 1, 0)
# anndata$Dep.Flight.Status_OnTime <- ifelse(anndata$Dep.Flight.Status == 3, 1, 0)


#--------------------------------------------------------------------------------------------------
#Split data into training and testing sets
#--------------------------------------------------------------------------------------------------

seed1 <- runif(1, 1, 100)
seed1 <- round(seed1, 0)

set.seed(seed = seed1)

trimmed$isTrain <- rbinom(nrow(trimmed), 1, 0.9)

train <- subset(trimmed, trimmed$isTrain == 1)

test <- subset(trimmed, trimmed$isTrain == 0)

anndata$isTrain <- rbinom(nrow(anndata), 1, 0.9)

anntrain <- subset(anndata, anndata$isTrain == 1)

anntest <- subset(anndata, anndata$isTrain == 0)

#--------------------------------------------------------------------------------------------------
#Multinomial logistic regression
#--------------------------------------------------------------------------------------------------

model <- multinom(formula = Dep.Flight.Status ~ sched_dep_time + sched_arr_time + carrier + origin + dest + distance + Num.Flights.That.Day + name, data = train)

summary(model)

test$predictions <- predict(model, newdata = test)

test$Counter <- ifelse(test$Dep.Flight.Status == test$predictions, 1, 0)

test <- test[!is.na(test$Counter),]

accuracy <- as.numeric(sum(test$Counter))/as.numeric(nrow(test))

avflights <- test %>%
  group_by(origin) %>%
  summarise(Num.Flights.That.Day = mean(Num.Flights.That.Day))

train <- train[,-c(11)]

train <- left_join(train, avflights, by="origin")


model1 <- multinom(formula = Arr.Flight.Status ~ sched_dep_time + sched_arr_time + carrier + origin + dest + distance + Num.Flights.That.Day + name, data = train)

test$predictions <- predict(model1, newdata = test)

test$Counter <- ifelse(test$Arr.Flight.Status == test$predictions, 1, 0)

test <- test[!is.na(test$Counter),]

accuracy <- as.numeric(sum(test$Counter))/as.numeric(nrow(test))

saveRDS(train, "C:/Users/thigg/Desktop/Flight App/App Directory/train.RDS")
saveRDS(model, "C:/Users/thigg/Desktop/Flight App/App Directory/Depart Model.RDS")
saveRDS(model1, "C:/Users/thigg/Desktop/Flight App/App Directory/Arrive Model.RDS")


#--------------------------------------------------------------------------------------------------
#Build Artificial Neurel Net for survival time prediction
#--------------------------------------------------------------------------------------------------

anntrain <- anntrain[!is.na(anntrain$Dep.Flight.Status),]
anntest <- anntest[!is.na(anntest$Dep.Flight.Status),]

#-------------
#Load tensor flow packages
#https://tensorflow.rstudio.com/
#-------------

library(tensorflow)
library(keras)
library(tfdatasets)
library(reticulate)

#tensorflow::install_tensorflow(version="2.8.0")

#-------------
#Set aside ID info for joining during testing later on
#-------------

y_train <- as.data.frame(anntrain$Dep.Flight.Status)
colnames(y_train) <- "Dep.Flight.Status"

anntrain1 <- subset(anntrain, select = -c(Dep.Flight.Status, isTrain, Arr.Flight.Status, arr_time, distance))

#-------------
#Create initial input layer for the ANN
#-------------

spec <- feature_spec(anntrain1, label ~ . ) %>% 
  step_numeric_column(all_numeric()) %>% 
  fit()

#this line of code is part of the tensorflow tutorial but causes issues when saving the model using h5 method
#normalizer_fn = scaler_standard()


spec

layer_dense_features(
  feature_columns = dense_features(spec), 
  dtype = tf$float32
)


input <- layer_input_from_dataset(anntrain1)


#-------------
#Model Building
#-------------

output <- input %>% 
  layer_dense_features(dense_features(spec)) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'softmax')

annmodel <- keras_model(input, output)

summary(annmodel)

annmodel %>% 
  compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(learning_rate = 0.01),
    metrics = list('acc')
  )

#-------------
#Model Learning
#-------------

history <- annmodel %>% fit(
  x = anntrain1,
  y = y_train$Dep.Flight.Status,
  epochs = 25,
  validation_split = 0.2,
  verbose = 1,
  view_metrics = TRUE,
  callbacks = list(
    callback_reduce_lr_on_plateau(monitor = "val_acc", factor = 0.1, patience = 3),
    callback_csv_logger("C:/Users/thigg/Desktop/Flight App/App Directory/epoch.csv", separator = ",", append = FALSE))
)

min(history$metrics$val_loss)/30
min(history$metrics$loss)/30

plot(history)

#-------------
#Initial ANN testing for survival time prediction accuracy
#-------------

anntest$Predictions <- annmodel %>% predict(anntest)

anntest$Counter <- ifelse(anntest$Dep.Flight.Status == anntest$Predictions, 1, 0)

anntest <- anntest[!is.na(anntest$Counter),]

accuracy <- as.numeric(sum(anntest$Counter))/as.numeric(nrow(anntest))


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#Save For Later If Needed

# pit <- readRDS("PITT 2020 Flights.RDS")
# buf <- readRDS("BUFF 2020 Flights.RDS")
# cle <- readRDS("CLE 2020 Flights.RDS")
# roc <- readRDS("ROC 2020 Flights.RDS")
# 
# flights <- rbind(pit, buf, cle, roc)

# library(ggmap)
# library(ggnetwork)
# 
# airports <- as.data.frame(anyflights::get_airports())
# 
# colnames(airports)[1] <- "dest"
# 
# 
# flights[nrow(flights)+1,"origin"] <- "PIT"
# flights[nrow(flights),"dest"] <- "PIT"
# flights[nrow(flights)+1,"origin"] <- "BUF"
# flights[nrow(flights),"dest"] <- "BUF"
# flights[nrow(flights)+1,"origin"] <- "CLE"
# flights[nrow(flights),"dest"] <- "CLE"
# flights[nrow(flights)+1,"origin"] <- "ROC"
# flights[nrow(flights),"dest"] <- "ROC"
# 
# flights1 <- left_join(flights, airports, by = "dest")
# 
# flights1 <- as.data.frame(flights1)
# 
# lookup <- flights1[flights1$origin == flights1$dest,]
# lookup <- lookup %>%
#   select(origin, lon, lat)
# 
# colnames(lookup) <- c("origin", "lonend", "latend")
# 
# flights1 <- left_join(flights1, lookup, by = "origin")
# 
# flightssim <- flights1 %>%
#   mutate(combo = paste(origin, dest, sep = "-")) %>%
#   # mutate(lonend = lon) %>%
#   # mutate(latend = lat) %>%
#   select(combo, origin, dest, lon, lat, lonend, latend) %>%
#   distinct(combo, .keep_all = TRUE) %>%
#   group_by(origin) %>%
#   mutate(Total.Dest = n())
# # mutate(lon = jitter(lon, amount = .1)) %>%
# # mutate(lat = jitter(lat, amount = .1))
# 
# pitt <- flightssim %>%
#   filter(origin == "PIT") %>%
#   slice(1) %>%
#   slice(rep(1:n(), each = 5))
# 
# buf <- flightssim %>%
#   filter(origin == "BUF")
# 
# cle <- flightssim %>%
#   filter(origin == "CLE")
# 
# roc <- flightssim %>%
#   filter(origin == "ROC")
# 
# 
# qmplot(data = pitt, lon, lat, zoom = 18) +
#   geom_segment(aes(xend=lonend, yend=latend), colour = "yellow", size = 2) +
#   theme_minimal() +
#   labs(
#     title = "Pittsburgh Airport Destination Flight Network",
#     subtitle = "2020 - Oct. 2021",
#     x = "Longitutde",
#     y = "Latitude"
#   ) +
#   theme(
#     plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(face = "bold", hjust = 0.5),
#     axis.title.x = element_text(size = 10),
#     axis.title.y = element_text( size = 10),
#   )
# 
# saveRDS(flightssim, "C:/Users/thigg/Desktop/Flight App/App Directory/Geo Data.RDS")

##############################################################################
# 1) Loading Libraries
##############################################################################

library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(VIM)
library(fivethirtyeight)
library(dplyr)
library(ggplot2)
library(reshape2)
library(factoextra)

##############################################################################
# 2.1 loading data from the web
# apply the column names using the colnames function
##############################################################################

wine_data <- read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                        delim = ",", col_names = FALSE)
wine_data

colnames(wine_data) <- c(
  "Type", "Alcohol", "Malic", "Ash",
  "Alcalinity", "Magnesium", "Phenols",
  "Flavanoids", "Nonflavanoids",
  "Proanthocyanins", "Color", "Hue",
  "Dilution", "Proline"
)
wine_data

##############################################################################
# 2.2 import rollingsales_manhattanwithdup.xlsx dataset
##############################################################################

setwd("C:\\Users\\bradn\\OneDrive\\Desktop\\MBA 739\\Datasets\\")

manhattan_sales_data <- read_excel("rollingsales_manhattanwithdup.xlsx")
manhattan_sales_data

manhattan_sales_data <- read_excel("rollingsales_manhattanwithdup.xlsx")%>%
  remove_empty()
manhattan_sales_data

daily_show_guests <- read_csv("Daily_show.csv")
daily_show_guests 

##############################################################################
# 3.a - Data Exploration
##############################################################################

summary(manhattan_sales_data)
head(manhattan_sales_data)
glimpse(manhattan_sales_data)
skim(manhattan_sales_data)

##############################################################################
# 3.b - Renaming Variables
##############################################################################

manhattan_sales_data

manhattan_sales_data %>%
  rename(borough = BOROUGH)

manhattan_sales_data <- manhattan_sales_data %>%
  clean_names()

manhattan_sales_data

##############################################################################
# 3.c - Resolving Duplicates
##############################################################################

manhattan_sales_data

manhattan_sales_data <- manhattan_sales_data %>%
  distinct()

manhattan_sales_data

##############################################################################
# 4.a - remove NAs
##############################################################################

manhattan_sales_data
manhattan_sales_data_na <- na.omit(manhattan_sales_data)
manhattan_sales_data_na

##############################################################################
# 4.b - Impute missing values with KNN
##############################################################################

manhattan_sales_data
manhattan_imputed_missing_values <- manhattan_sales_data %>%
  kNN(variable = c("sale_price"), k=5)
manhattan_imputed_missing_values

##############################################################################
# 5.a - Selecting Columns and extracting rows
##############################################################################

x1 <- daily_show_guests %>%
  slice(1:3)

x1 <- daily_show_guests %>%
  arrange(Group)

x1 <- daily_show_guests %>%
  arrange(desc(YEAR))

x1 <- daily_show_guests %>%
  filter(GoogleKnowlege_Occupation == "Comedian")

x1 <- daily_show_guests %>%
  select(Show, Raw_Guest_List)

x1 <- daily_show_guests %>%
  select(-Show)


##############################################################################
# 6.a - Dimension Reduction - Correlation Matrix
##############################################################################

housing_df <- read.csv("BostonHousing.csv")

correlation_mat <- round(cor(housing_df),2) 
correlation_mat

meltedcorrelation_mat <- melt(correlation_mat)
meltedcorrelation_mat

# Generate the heat map
ggplot(meltedcorrelation_mat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = Var1, y = Var2, label = value))


##############################################################################
# 6.b - Dimension Reduction - PCA
##############################################################################

cereals <- read.csv("Cereals.csv") %>%
  na.omit()
cereals

# compute PCs for all variables except for first 3 and rating
cereals_short <- cereals %>%
  select(-c(1:3,16))%>%
  na.omit()
pcs <- cereals %>%
  select(-c(1:3,16)) %>%
  na.omit() %>%
  prcomp()

summary(pcs)

# rotation
pcs$rotation [,1:5]

# regress
cereals_reg <- cbind(cereals,as.data.frame(pcs$x))
x1 <- lm(rating~PC1+PC2+PC3, data = cereals_reg)
summary(x1)

# Q1 - load packages. Isolate Delta
# load the packages
# strip out non delta flights
library(nycflights13)
library(tidyverse)

Delta_flights <- flights %>% 
  filter(carrier == "DL")

Delta_flights
summary(Delta_flights)


# Q2
# Relationship between departure delay and arrival delay using scatterplot
# note that there are two ways to do this. Caret gives you some flexibility
Delta_flights %>%
  ggplot(mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_point()

ggplot(data = Delta_flights,
       mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_point()


# Weather Delays
# Q3a
# Create a new dataset for last 15 days of dec'13 out of Newark
# I could do this in one shot but I am an old man and like to 
# go step by step
late_dec_weather <- weather 

late_dec_weather <- late_dec_weather%>%
  filter(origin == "EWR" & month == 12 & day >= 16)

# Q3b - line chart between temperature and hour
ggplot(data = late_dec_weather,
       mapping = aes(x = time_hour, y = temp)) +
  geom_line()

# Q3c - did it rain on christmas
ggplot(data = late_dec_weather,
       mapping = aes(x = time_hour, y = precip)) +
  geom_line()
# no rain on Christmas

#4 Modifications of Color
# 4a # histogram to depict the temperature distribution

ggplot(data = weather, 
       mapping = aes(x = temp)) +
  geom_histogram(binwidth = 10, color = "white", fill = "red")
# looks like the 35-45 is the most 


#4b - Adding rows using facet_wrap
# histogram with usage of facet
ggplot(data = weather, 
       mapping = aes(x = temp)) +
  geom_histogram(binwidth = 10, color = "white", fill = "red") +
  facet_wrap(~ month, nrow = 4)
#this gives me a histogram of the temperatures in each month


#5 Making Boxplots
# again, two ways to do this
# Boxplot to depict weather ranges for all months. convert the month to factors

ggplot(data = weather, 
       mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot()

weather2 <- weather
weather2$month <- factor(weather$month)

ggplot(data = weather2, 
       mapping = aes(x = month, y = temp)) +
  geom_boxplot()
# lowest median temp looks like february
# highest recorded temp looks like july

# 6A charting by distribution
# Bar chart to provide the distribution of outgoing flight by carrier and origin airport
ggplot(data = flights,
       mapping = aes(x = carrier, fill = origin)) +
  geom_bar(stat = "count") 


ggplot(data = flights,
       mapping = aes(x = carrier, fill = origin)) +
  geom_bar(stat = "count") + 
  labs(title ="Flights Distribution to NYC", x ="Carrier", y ="Flights Count")
# Delta beats American
# EWR, then LGA, then JFK, 

# 6b - stacking the chart side by side
# Stacking bar chart
ggplot(data = flights,
       mapping = aes(x = carrier, fill = origin)) +
  geom_bar(stat = "count", position = "dodge") + 
  labs(title ="Flights Distribution from NYC", x ="Carrier", y ="Flights Count")


# Load libraries
library(tidyverse)
library(arules)
library(arulesViz)
library(janitor)

course_df <- read_csv("C:\\Users\\bradn\\Dropbox\\MBA 739\\Week 3\\Coursetopics.csv")

## Generate a set of association rules 
# Recast incidence matrix into transactions list  
## Generate an item frequency plot
## Generate the rules
course_mat <- as.matrix(course_df)
head(course_mat)

course_trans <- as(course_mat, "transactions")
course_trans@itemInfo
itemFrequencyPlot(course_trans)

course_rules <- apriori(course_trans, parameter = list(supp = 0.001, conf = 0.7, target = "rules"))
summary(course_rules)


## Get the top rules based on the lift ratio and interpret the top 2 rules. 
course_rules <- sort(course_rules, by = "lift", decreasing=TRUE)
inspect(course_rules[1:2])

## Rule 1:
## "If data_mining, survey, regression, doe courses are taken, then forecast course is taken as well." 
## Rule 1 has a confidence of 100% and a lift of 7.1. 
## Rule 2:
## "If intro, data_mining, survey, regression courses are taken, then forecast course is taken as well." 
## Rule 2 has a confidence of 100% and a lift of 7.1.


## Specify a concise rule with maxlen of 4
# Generate a set of concise rules
# Get the top rules based on the lift ratio and interpret the top 2 rules. 
concise_rules <- apriori(course_trans, parameter = list(supp = 0.001, conf = 0.7, maxlen = 4))
concise_rules <- sort(concise_rules, by = "lift", decreasing=TRUE)
inspect(concise_rules[1:2])

## Rule 1:
## "If survey, regression, sw courses are taken, then doe course is taken as well" 
## Rule 1 has a confidence of 100% and a lift of 5.79. 
## Rule 2:
## "If survey, regression, forecast courses are taken, then data_mining course is taken as well." 
## Rule 2 has a confidence of 100% and a lift of 5.6.


## Targeting rule
# Identify the courses that needs to be taken before DataMining is taken. Use the same parameters as before.  

target_rules<- apriori(course_trans, parameter = list(supp = 0.001, conf = 0.7),
                       appearance = list(default = "lhs", rhs = "DataMining"))
inspect(target_rules)

# There are 13 rules (paths) of courses prior to data_mining being taken. 
# the only courses which are universally taken are "Regression and Forecast
# Load libraries
library(tidyverse)
library(janitor)
library(rsample)
library(forecast)

#clean names
#remove cat_medv
#run a correlation table
housing_df <- read_csv("C:/Users/28mac/Documents/Graduate School/Advanced DM MBA 739/Datasets/BostonHousing.csv") %>%
  clean_names
housing_df

housing_df <- housing_df %>%
  select(-cat_medv)

cor(housing_df)
round(cor(housing_df),2) 

reduced_housing_df <- housing_df %>%
  select(-c(rad))

#split the sample into training and validation
# Specify the set.seed option to reproduce the random sampling
set.seed(739)
housing_df_split <- initial_split(reduced_housing_df, prop = 0.60)

housing_training <- housing_df_split %>% 
  training() 
housing_valid <- housing_df_split %>% 
  testing()

# Linear regression model
# run linear regression model
# Apply the model to the validation/testing dataset, and check for the prediction measures.
# use predict function to apply to validation
## Evaluate the performance

housing_reg <- lm(medv~., data=housing_training,na.action=na.exclude)
summary(housing_reg)

housing_pred_v <- predict(housing_reg, newdata=housing_valid)

forecast::accuracy(housing_pred_v,housing_valid$medv)


# Kmeans Clustering
# load libraries
# Load the Framingham dataset
library(tidyverse)
library(skimr)
library(factoextra)

FraminghamData <- read.csv("C:/Users/28mac/Documents/Graduate School/Advanced DM MBA 739/Datasets/FraminghamData.csv") %>%
  clean_names()

# assess the data visually
# remove missing values
# scale the data for use

skim(FraminghamData)

FraminghamScaled <- FraminghamData %>%
  drop_na()

FraminghamScaled <- FraminghamScaled %>%
  scale()

FraminghamScaled <- FraminghamData %>%
  drop_na()  %>%
  scale()

# execute the k-Means clustering using kmeans(). Set the seed to 12345 and nstart = 25
# k-means creates 2 clusters of sizes 1176, 2482
set.seed(12345)
km.Framingham=kmeans(FraminghamScaled,3,nstart=25)
km.Framingham
dist(km.Framingham$centers)

# visualize the clusters, have some fun with color
fviz_cluster(km.Framingham, data = FraminghamScaled,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_cluster(km.Framingham, data = FraminghamScaled,
             palette = c("Blue", "Red"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# Use Silhoutte Method to determine K
fviz_nbclust(FraminghamScaled, kmeans, method = "silhouette")

fviz_nbclust(FraminghamScaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

km.Framingham=kmeans(FraminghamScaled,3,nstart=25)

fviz_cluster(km.Framingham, data = FraminghamScaled,
             palette = c("Blue", "Red", "Green"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

## Heirarchical Clustering
# Dissimilarity matrix
# Create Hierarchical clusters using Complete Linkage and Wards Method
d <- dist(FraminghamScaled, method = "euclidean")

hc1 <- hclust(d, method = "ward.D2" )
plot(hc1)
groups <- cutree(hc1, k=2)
rect.hclust(hc1, k=2, border = "red")

hc2 <- hclust(d, method = "complete")
plot(hc2)
groups <- cutree(hc2, k=2)
rect.hclust(hc2, k=2, border = "red")




# load the required libraries and load data

library(rpart)
library(rpart.plot)
library(ipred)
library(caret)
library(tidyverse)
library(rsample) 
library(fastDummies)

setwd("C:/Users/28mac/Documents/Graduate School/Advanced DM MBA 739/Datasets/")
ds_heart <- read_csv("Heart.csv") 


# prepare and partition the data
# Data preparation
# convert Heart Disease into a zero one
# convert other categorical variables into dummies
ds_heart
ds_heart2 <- ds_heart %>%
  mutate(HeartDisease = as.factor(ifelse(HeartDisease == "Yes",1,0)))

ds_heart2 <- ds_heart2 %>%
  dummy_cols(select_columns = c('Gender','ChestPain','ThalliumStressTest'), 
             remove_first_dummy = TRUE,remove_selected_columns = TRUE)

# partition the data
# sample 60% training data
set.seed(739)

# classification tree
class_tree <- rpart(HeartDisease ~ ., data = ds_train, 
                    method = "class",
                    control = rpart.control(cp = 0.01, maxdepth = 3, minsplit = 6))
# plot tree
prp(class_tree)
prp(class_tree, type = 2)
prp(class_tree, type = 2, extra = 101)
prp(class_tree, type = 2, extra = 101, under = TRUE, split.font = 1, varlen = 0)


# apply the model to the validation data
# set argument type = "class" in predict() to generate predicted class membership.
tree_valid <- predict(class_tree, ds_valid, type = "class")
confusionMatrix(tree_valid, as.factor(ds_valid$HeartDisease))

# The formula below will extract the optimal for that tree
# prune the tree
# generate confusion matrix
mincp <- class_tree$cptable[which.min(class_tree$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(class_tree, cp = mincp )
pruned_tree_valid <- predict(pruned_tree, ds_valid, type = "class")
confusionMatrix(pruned_tree_valid, as.factor(ds_valid$HeartDisease))


# Ensembles
# we need to reload the data at this point because there are some tricky things
# going on. the big one is that we need to scale our data and you cant scale
# factor variables

library(randomForest)
library(adabag)
library(ipred)

ds_heart <- read_csv("Heart.csv") 

ds_heart2 <- ds_heart %>%
  mutate(HeartDisease = ifelse(HeartDisease == "Yes",1,0)) 

ds_heart2 <- ds_heart2%>%
  dummy_cols(select_columns = c('Gender','ChestPain','ThalliumStressTest'), remove_first_dummy = TRUE,remove_selected_columns = TRUE)

maxs <- apply(ds_heart2, 2, max)
mins <- apply(ds_heart2, 2, min)

ds_clean<- ds_heart2 %>%
  scale(center = mins, scale = maxs - mins) %>%
  as.data.frame() %>%
  mutate(HeartDisease=as.factor(HeartDisease))

# partition the data
# sample 60% training data
set.seed(739)

ds_split <- initial_split(ds_clean, prop = 0.60)

ds_train <- ds_split %>% 
  training()
ds_valid <- ds_split %>% 
  testing()

# execute the random forest
# variable importance plot
# produce a confusion matrix 

# ntree - number of trees
# mtry - number of variables selected
# nodesize - minimum nodes
set.seed(739)

rf <- randomForest(HeartDisease ~ ., data = ds_train, ntree = 500 , 
                   mtry = 4, nodesize = 5 , importance = TRUE)  

varImpPlot(rf, type = 1)

rf_pred <- predict(rf, ds_valid,type = "class")
confusionMatrix(as.factor(rf_pred), ds_valid$HeartDisease)

# bagging
# bagging from ipred package works better than one in adabag, so use this package. 
bag <- ipred::bagging(HeartDisease ~ ., data = ds_train)
pred <- predict(bag, ds_valid, type = "class")
confusionMatrix(as.factor(pred), ds_valid$HeartDisease)

# boosting
boost <- boosting(HeartDisease ~ ., data = ds_train)
pred <- predict(boost, ds_valid, type = "class")
confusionMatrix(as.factor(pred$class), ds_valid$HeartDisease)


#load libraries and data
library(neuralnet)
library(tidyverse)
library(nnet)
library(caret)
library(e1071)
library(rsample)

setwd("C:\\Users\\bradn\\Dropbox\\MBA 739\\Week 6\\")
bank_df <- read.csv("UniversalBank.csv")


# trim data
# scale data
# set personal loan as factor
bank_df <- bank_df %>%
  select(-c("ID","ZIP.Code"))

maxs <- apply(bank_df, 2, max)
mins <- apply(bank_df, 2, min)

bank_df <- bank_df %>%
  scale(center = mins, scale = maxs - mins) %>%
  as.data.frame() %>%
  mutate(Personal.Loan = as.factor(Personal.Loan)) 

# partition the data
# sample 60% training data
set.seed(739)

bank_split <- initial_split(bank_df, prop = 0.60)

bank_training <- bank_split %>% 
  training()
bank_valid <- bank_split %>% 
  testing()

# * Use neuralnet for classification. Generate the neuralnets with following 
# options and compare the models using classification performance metrics.
# * 1 hidden layer - 3 nodes
# * 2 hidden layers - 2 nodes each
# * 2 hidden layers - 3 nodes each

### 1 hidden layer - 3 nodes
# run nn with 3 hidden nodes 
# use hidden= with a vector of integers specifying number of hidden nodes in each layer
# stepmax = maximum number of steps to train the model
# linear.output

set.seed(4030)
nn_1 <- neuralnet(Personal.Loan ~ . , data = bank_training, hidden = 3, stepmax = 1e7, linear.output = F)
plot(nn_1, rep="best")

validation.prediction <- neuralnet::compute(nn_1, bank_valid)
validation.class <-apply(validation.prediction$net.result,1,which.max)-1
confusionMatrix(table(validation.class, bank_valid$Personal.Loan))

### 2 hidden layers - 2 nodes each
# run nn with 2 hidden nodes 
# use hidden= with a vector of integers specifying number of hidden nodes in each layer

set.seed(4030)
nn_2 <- neuralnet(Personal.Loan ~ . , data = bank_training, hidden = c(2,2), stepmax = 1e7)
plot (nn_2, rep = "best")

validation.prediction <- neuralnet::compute(nn_2, bank_valid)
validation.class <-apply(validation.prediction$net.result,1,which.max)-1
confusionMatrix(table(validation.class, bank_valid$Personal.Loan))

### 2 hidden layers - 3 nodes each
# run nn with 2 hidden nodes 
# use hidden= with a vector of integers specifying number of hidden nodes in each layer

set.seed(4030)
nn_3 <- neuralnet(Personal.Loan ~ . , data = bank_training, hidden = c(3,3), stepmax = 1e7)
plot (nn_3, rep = "best")

validation.prediction <- neuralnet::compute(nn_3, bank_valid)
validation.class <-apply(validation.prediction$net.result,1,which.max)-1
confusionMatrix(table(validation.class, bank_valid$Personal.Loan))

### 4 hidden layers - 4 nodes each
# run nn with 4 hidden nodes 
# use hidden= with a vector of integers specifying number of hidden nodes in each layer

set.seed(4030)
nn_4 <- neuralnet(Personal.Loan ~ . , data = bank_training, hidden = c(4,4,4,4), stepmax = 1e7)
plot (nn_4, rep = "best")

validation.prediction <- neuralnet::compute(nn_4, bank_valid)
validation.class <-apply(validation.prediction$net.result,1,which.max)-1
confusionMatrix(table(validation.class, bank_valid$Personal.Loan))

library(tidyverse)
library(tm)
library(SnowballC)
library(lsa)
library(rsample)
library(caret)

setwd("C:/Users/28mac/Documents/Graduate School/Advanced DM MBA 739/Datasets/")
clothing_review <- read_csv("clothing_review.csv")

# first thing I will do is select the recommendation out
# Create the corpus - This is applicable only to vector source and not data frames, so we choose only the review
#1 is recc, 0 is not recc
clothing_label <- clothing_review %>%
  select(recommendation)

clothing_review_corp <- Corpus(VectorSource(clothing_review$review))

#Preprocess text - Tokenization, remove punctuation, remove numbers, remove stopwords, and stemthe document. 
clothing_review_corp <- clothing_review_corp %>%
  tm_map(stripWhitespace) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) 

#create term document matrix, calculate the tfidf, and run everything through an lsa. 
clothing_review_tdm <- TermDocumentMatrix(clothing_review_corp)
clothing_review_tfidf <- weightTfIdf(clothing_review_tdm)
clothing_review_lsa_tfidf <- lsa(clothing_review_tfidf, dims=20)


# merge the concepts with the original labels and split the sample

clothing_review_words <- as.data.frame(as.matrix(clothing_review_lsa_tfidf$dk)) %>%
  cbind(clothing_label) %>%
  mutate(recommendation = as.factor(recommendation))

set.seed(739)

clothing_split <- initial_split(clothing_review_words, prop = 0.60)

clothing_training <- clothing_split %>% 
  training()
clothing_valid <- clothing_split %>% 
  testing()


# run a Logistic regression to predict the recommendation using your concepts
# determine how accurate your model is at predicting a complaint
# explore various cutoffs to see if you can improve the classification
clothing_logit_reg <- glm(recommendation ~ ., data = clothing_training, family = "binomial") 
clothing_logit_pred <- predict(clothing_logit_reg, clothing_valid, type = "response")

#when logit predicts 1 vs 0- cutoff threshold
#if prediction is more than .3, then it is 1 or a reccomendation
confusionMatrix(table(ifelse(clothing_logit_pred > 0.01, 1, 0), clothing_valid$recommendation))
confusionMatrix(table(ifelse(clothing_logit_pred > 0.3, 1, 0), clothing_valid$recommendation))
confusionMatrix(table(ifelse(clothing_logit_pred > 0.4, 1, 0), clothing_valid$recommendation))
confusionMatrix(table(ifelse(clothing_logit_pred > 0.5, 1, 0), clothing_valid$recommendation))
confusionMatrix(table(ifelse(clothing_logit_pred > 0.6, 1, 0), clothing_valid$recommendation))
confusionMatrix(table(ifelse(clothing_logit_pred > 0.7, 1, 0), clothing_valid$recommendation))


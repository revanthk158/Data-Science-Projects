getwd()

#setwd()

setwd("I:/NEU/Data_Mining/Assignment1 and Use cases")

library(tidyverse)
library(car)
library(stringr)
library("gvlma")
library(leaps) 
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

cars <- read_csv("autos.csv")

# Data Cleaning:  

View(cars)

range(cars$price)

# Removing Outliers: 
# considering used cars above 20000 USD and Less than 1000 USD as outliers.

# Based on domain knowledge, considering only the following predictors for predicting 
# Price of a used car:  

# Predictors considered for analysis: 
# brand,kilometer, yearOfRegistration, gearbox, fuelType, price 

d1 <- cars %>%
  select(brand,kilometer, yearOfRegistration, gearbox, fuelType, price, 
         powerPS, notRepairedDamage)

unique(d1$brand)

v1 <- d1 %>%
  group_by(brand) %>%
  summarise(brandcount = n()) %>%
  arrange(desc(brandcount))  

# Considering Top Volvo Brand Cars For Analysis:  

d2 <- filter(d1, brand == "volvo" | brand == "honda" )

range(d2$price)  

ggplot(data = d2)+
  geom_boxplot(mapping = aes(x = brand, y = price))

ggplot(data = d2)+
  geom_point(mapping = aes(x = brand, y = price))

d3 <- d2 %>%
  filter(price >= 1000000)

count(d3)  


# Here, we can see that one volvo car price is greater than $10,000,000
# Considering it as an outlier and removing it, we get
# Removing the outlier.  

# Removing the outlier.  

d4 <- filter(d2, price <= 1000000, price > 500)

range(d4$price)

ggplot(data = d4)+
  geom_point(mapping = aes(x = brand, y = price))

ggplot(data = d4)+
  geom_point(mapping = aes(y = price, x = kilometer))

# Here we can see that majority of cars are in a price range of $30,000  

# Removing years that are greater than the current year, we get:

ggplot(data = d4)+
  geom_point(mapping = aes(x = yearOfRegistration, y = price))

# Here, we can see that for few cars, year of registration is greater than current year, 
# Considering them as outliers and removing them from data, we get

range(d4$yearOfRegistration)

d5 <- filter(d4, yearOfRegistration <= 2017)
  
d6 <- mutate(d5, yearsold = 2017 - yearOfRegistration)

ggplot(data = d6)+
  geom_smooth(mapping = aes(x = price, y = yearsold), se = "false")

# No Pattern 

ggplot(data = d6)+
  geom_smooth(mapping = aes(x = price, y = kilometer), se = FALSE)

# We can see that lesser the car travels, higher the prices will be.  
  
unique(d6$gearbox)

# removing NA values from the gear box and creating a dummy variables for each gear box type  
# we get:  

d7 <- filter(d6, gearbox != "NA", fuelType != "NA", notRepairedDamage != "NA")  

d8 <- mutate(d7, autogear = str_detect(gearbox, "automatik")) 

ggplot(data = d8)+
  geom_point(mapping = aes(x = autogear, y = price))

# a1 <- select(d8, kilometer, yearsold,autogear ,price)

# scatterplotMatrix(a1)
#d8 <- str_replace_all(d, "TRUE", "1")
#d8 <- str_replace_all(d8, "False", "0")

# View(d8)
# Added dummy column, 
unique(d8$fuelType)
  
# Creating a Dummy Column for Fuel Type  
  
d8 %>%
  group_by(fuelType) %>%
  summarise(fueltypes = n())

d9 <- mutate(d8, petrol = str_detect(fuelType, "benzin"))
d10 <- mutate(d9, diesel = str_detect(fuelType, "diesel"))
d11 <- mutate(d10, lpg = str_detect(fuelType, "lpg"))
d12 <- mutate(d11, cng = str_detect(fuelType, "cng"))
d13 <- mutate(d12, hybrid = str_detect(fuelType, "hybrid"))  
d14 <- mutate(d13, Volvo = str_detect(brand, "volvo"))

d14$autogear <-  d14$autogear %>%
  str_replace_all( "TRUE", "1") %>%
  str_replace_all("FALSE", "0")

d14$petrol <- d14$petrol %>%
  str_replace_all("TRUE", "1") %>%
  str_replace_all("FALSE", "0")

d14$diesel <- d14$diesel %>%
  str_replace_all("TRUE", "1") %>%
  str_replace_all("FALSE", "0")

d14$lpg <- d14$lpg %>%
  str_replace_all("TRUE", "1") %>%
  str_replace_all("FALSE", "0")

d14$cng <- d14$cng %>%
  str_replace_all("TRUE", "1") %>%
  str_replace_all("FALSE", "0")

d14$hybrid <- d14$hybrid %>%
  str_replace_all("TRUE", "1") %>%
  str_replace_all("FALSE", "0") 

d14$Volvo <- d14$Volvo %>%
  str_replace_all("TRUE", "1") %>%
  str_replace_all("FALSE", "0")

d14$notRepairedDamage <- d14$notRepairedDamage %>%
  str_replace_all("nein", "0") %>%
  str_replace_all("ja","1")


# scatterplotMatrix(a2)  


# Data Preprocesing:  
# Splitting 60% data to training and 40% to validation, we get:

finaldata <- select(d14, price ,kilometer, yearsold, powerPS , notRepairedDamage, 
                    autogear, petrol, diesel, lpg, cng, hybrid, Volvo)

set.seed(55)
train.index <- sample(c(1:dim(finaldata)[1]), dim(finaldata)[1]*0.6)
carstrain.df <- finaldata[train.index, ]
carsvalid.df <- finaldata[-train.index, ]

names(finaldata)


# Interpretations:  (To write 
# about linear relationship between the preditor variables with the output response variable)  
  
# Linear Regression Model:  

# mod1 <- lm(price ~ kilometer + yearsold, data = d13)

# summary(mod1)

mod1 <- lm(price ~ kilometer + yearsold+ powerPS+ notRepairedDamage+ autogear+
             petrol+ diesel + lpg + cng + hybrid + Volvo, data = carstrain.df)

summary(mod1)  


mod2 <- lm(price ~ kilometer + yearsold + autogear + powerPS+ notRepairedDamage
           , data = carstrain.df)  

summary(mod2)

# From summary, we can see that Kilometer, yearsold, autogear, petrol, diesel, lpg cng
# as variables important for analysis and selecting best regression model, we get:  


# Selecting Best regression Model:  
  
AIC(mod1,mod2)

# Fine Tune Variable Selection:  
  
finalvariables <- regsubsets(price ~ kilometer + yearsold + autogear + petrol + 
                                       diesel + lpg + cng + hybrid + powerPS+  
                               notRepairedDamage + Volvo , data = carstrain.df, nbest = 10) 

plot(finalvariables, scale = "adjr2"  )  
  
# Here from the plot, we can see that Except for CNG, all other variables can be used for
# Analysis

# Final model using best predictor variables:

finalmodel <- lm(price ~ kilometer + yearsold + autogear + petrol+ diesel + 
                   powerPS + notRepairedDamage+ Volvo,  data = carstrain.df)

# Performance evaluation of all the models:  

p1 <- predict(mod1, carsvalid.df)
p2 <- predict(mod1, carstrain.df)

RMSE(p1, carsvalid.df$price)
RMSE(p2, carstrain.df$price)


p3 <- predict(mod2, carstrain.df)
p4 <- predict(mod2, carsvalid.df)

RMSE(p3, carstrain.df$price)
RMSE(p4, carsvalid.df$price)  

# Final Model:  

p5 <- predict(finalmodel, carstrain.df)

p6 <- predict(finalmodel, carsvalid.df)

RMSE(p5, carstrain.df$price)
RMSE(p6, carsvalid.df$price)

# From the above, Comparing root mean square errors of all the models, we can say that 
# Final model which is built by selecting best predictor variables is the best 
# As it has the least RMSE value.


## Using Classification Trees to Predict Used Cars Prices :

cars_ct <- d7

# Regression Tree to predict used cars prices:  

cars_tree <- rpart(price ~ . , data = cars_ct, cp = 0, minsplit = 5, xval = 5)

prp(cars_tree,type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(cars_tree$frame$var == "<leaf>", 'gray', 'white'))

#printcp(cars_tree)

pruned.ct <- prune(cars_tree,
                   cp = cars_tree$cptable[which.min(cars_tree$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

cars_tree2 <- rpart(price~. , data = cars_ct, 
                    control = rpart.control(maxdepth = 7, minbucket = 30, cp = 0.001))

prp(cars_tree2,type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10,
    box.col=ifelse(cars_tree2$frame$var == "<leaf>", 'gray', 'white') )

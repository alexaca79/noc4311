#Library Load 

library(tidyverse)
library(data.table)
library(TStools)
library(caret)
library(doParallel)
library(lattice)


#Use more cores 

registerDoParallel(cores=4)

#Set Working Directory 
setwd("D:/Drive/MCSCS-Interview/Presentation/NOC431_LMA")
x35100076 <- read.csv("35100076.csv")
Population <- read.csv("book2.csv")
forecast <- read.csv("forecast.csv")



#Data Clean 
x35100076 <- read_csv("35100076.csv")

x35100076on<- x35100076 %>%
  filter( grepl('ontario|Ontario', GEO))

x35100076on$StatisticsUOM <- paste(x35100076on$Statistics,x35100076on$UOM,sep = "_")

x35100076on$UOM_ID <- NULL
x35100076on$SCALAR_FACTOR <- NULL
x35100076on$SCALAR_ID <- NULL
x35100076on$VECTOR <- NULL
x35100076on$COORDINATE <- NULL
x35100076on$STATUS <- NULL
x35100076on$SYMBOL <- NULL
x35100076on$DECIMALS <- NULL
x35100076on$Statistics<- NULL
x35100076on$UOM  <- NULL
x35100076on$DGUID  <- NULL
x35100076on$TERMINATED  <- NULL
x35100076on$GEO  <- NULL

#Spread Data

Data_spread <- x35100076on %>%
  spread(StatisticsUOM,VALUE)

#Delete NA 
Data_spread$`Per capita cost_Dollars` <- NULL
Data_spread$`Population per police officer_Rate`<- NULL
Data_spread$`Total expenditures on policing_Dollars`<- NULL
Data_spread$`Crime severity index_Index`<- NULL
Data_spread$`Weighted clearance rate_Rate` <-NULL
Data_spreaddropna <- Data_spread%>%drop_na()


#Add population data 

Data_spreaddropna$pop <- Population$pop

#Train Control 
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 20,
                              horizon = 5,
                              fixedWindow = TRUE)


#Different Models 

##Glmnet - Lasso and Elastic-Net Regularized Generalized Linear Model
glmnet.ontario <- train(`Authorized police officer strength per 100,000 population_Rate`~ REF_DATE + pop ,
                    data = Data_spreaddropna,
                    method = "glmnet",
                    family = "gaussian",
                    preProc = c("center", "scale"),
                    trControl = myTimeControl)



##Elm - Extreme Learning Machine 
elm.ontario <- train(`Authorized police officer strength per 100,000 population_Rate`~  REF_DATE + pop  ,
                         data = Data_spreaddropna,
                         method = "elm",
                         preProc = c("center", "scale"),
                         trControl = myTimeControl)


##Partial Least Squares 
pls.ontario <- train(`Authorized police officer strength per 100,000 population_Rate`~  REF_DATE + pop  ,
                    data = Data_spreaddropna,
                    method = "pls",
                    preProc = c("center", "scale"),
                    trControl = myTimeControl)

## Neural Network
nnnet.ontario <- train(`Authorized police officer strength per 100,000 population_Rate`~  REF_DATE + pop  ,
                    data = Data_spreaddropna,
                    method = "nnet",
                    trControl = myTimeControl)

## Random Forest 
ranger.ontario <- train(`Authorized police officer strength per 100,000 population_Rate`~  REF_DATE + pop  ,
                       data = Data_spreaddropna,
                       method = "ranger",
                       preProc = c("center", "scale"),
                       trControl = myTimeControl)

## Linear Model 
lm.ontario <- train(`Authorized police officer strength per 100,000 population_Rate`~  REF_DATE + pop  ,
                        data = Data_spreaddropna,
                        method = "lm",
                        preProc = c("center", "scale"),
                        trControl = myTimeControl)
               
## Generalized Linear MOdel 
randomGLM.ontario <- train(`Authorized police officer strength per 100,000 population_Rate`~  REF_DATE + pop  ,
                    data = Data_spreaddropna,
                    method = "glm",
                    preProc = c("center", "scale"),
                    trControl = myTimeControl )



#Compare Models 
resamps <- resamples(list(glmnet = glmnet.ontario,
                          elm = elm.ontario,
                          pls = pls.ontario,
                          nnnet=nnnet.ontario,
                          ranger = ranger.ontario,
                          lm = lm.ontario,
                          glm = randomGLM.ontario))


summary.ontario <-summary(resamps)
print <- knitr::kable(summary.ontario[[3]]$Rsquared)
trellis.par.set(caretTheme())

write.csv(summary.ontario,"errors.csv")

dotplot(resamps, metric = "RMSE")
dotplot(resamps, metric = "Rsquared")




#LM is tne best 

coefficients <- coef(glmnet.ontario$finalModel, glmnet.ontario$bestTune$lambda)
predict <- predict(glmnet.ontario, forecast,interval = "confidence")
write.csv(forecast,"forecast.csv")





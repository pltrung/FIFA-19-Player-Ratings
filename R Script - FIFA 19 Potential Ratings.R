#libaries 

library(readr)
install.packages('rlang')
library(rlang)
library(dplyr)
library(tidyr)
install.packages('lazyeval')
library(lazyeval)
library(readxl)
library(ggplot2)
library(Hmisc)


#data cleaning

getwd()
setwd("C:/Users/pltru/Google Drive")

fifa19 <- read_csv("Masters/Soccer Project/data.csv")
fifa19 <- data.frame(fifa19)

head(fifa19) 
str(fifa19)

fifa19 <- fifa19 %>%
  select(Overall, Potential,Special, Preferred.Foot, International.Reputation, 
         Weak.Foot, Skill.Moves,Work.Rate, Crossing,
         Finishing, HeadingAccuracy, ShortPassing, Volleys, Dribbling, Curve, FKAccuracy, LongPassing, BallControl,Acceleration,
         SprintSpeed, Agility, Reactions, Balance, ShotPower, Jumping, Stamina, Strength, LongShots, Aggression, Interceptions,Positioning,
         Vision, Penalties, Composure, Marking, StandingTackle, SlidingTackle, GKDiving, GKHandling, GKKicking, GKPositioning,
         GKReflexes)

f19 <- fifa19[complete.cases(fifa19),]

f19 <- f19[-8]

f19$Work.Rate <- as.factor(f19$Work.Rate)
f19$Preferred.Foot <- as.factor(f19$Preferred.Foot)
f19$Potential <- as.ordered(f19$Potential) 
f19$Overall <- as.ordered((f19$Overall))

summary(f19)
str(f19)

f19 <- f19[-c(7,8,16,17,18,19)]

summary(f19$Special) 

#DATA VISUALIZATION
#Draw the histogram of players Potential score and overall score.
G1 <- ggplot(f19, aes(Potential)) + 
  geom_histogram()
G1


G2 <- ggplot(f19, aes(Overall)) +
  geom_histogram()
G2


#Correlation coefficient of their age, overall score and potential score
f19 <- data.frame(f19)
colnames(f19)
avp <- f19[, c ("Age","Overall","Potential")]
avp
cm_avp <- cor(avp)
cm_avp <-round(cm_avp,2)
cm_avp
heatmap(cor(avp))
colnames(f19)

#Try to apply the function to all variables
cm_total <- cor(rgm)
cm_total <- round(cm_total, 2)
cm_total
heatmap (cm_total)

#Correlation Matrix with significanec levels (P-value)
cms <- rcorr(as.matrix(rgm))
cms$r #correlation coefficients
cms$P #The p-values corresponding to the significance levels of correlations.

##Format the cprrelation matrix 

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Formatting
flattenCorrMatrix(cms$r, cms$P)

#Draw a correlogram
install.packages("corrplot")
library("corrplot")
corrplot (cm_total, type = 'upper', order = 'hclust', 
          tl.col = 'black', tl.srt= 45 )
corrplot (cms$r, type = 'upper', order = 'hclust',
          tl.col = 'black', tl.srt= 90, tl.cex = 0.5,
          p.mat = cms$P, sig.level = 0.01, insig = 'blank')


#REGRESSION ANALYSIS
#data partioning

set.seed(1234)
ind <- sample(2,nrow(f19), replace = T, prob = c(0.7,0.3))

training <- f19[ind == 1,]
test <- f19[ind == 2,]

#linear model 1
lm1 <- lm(Potential~., data = training)

lm1$coefficients
lm1$residuals
summary(lm1)

#predict 

lm1pred <- predict(lm1, test)

actual_preds <- data.frame(cbind(actuals = test$Potential, predicteds = lm1pred))

accuracy <- cor(actual_preds)

AIC(lm1)
BIC(lm1)

#random forest model

library(randomForest)
install.packages("caret")
library(caret)

rf1 <- randomForest(Potential~., data = training)


print(rf1)
attributes(rf1)
mean(rf1$rsq)
#predict 

p1 <- predict(rf1, test)

actual_preds2 <- data.frame(cbind(actuals = test$Potential, predicteds = p1))

accuracy2 <- cor(actual_preds2)

varImpPlot(rf1, sort = T, n.var = 15, main = "Top 15 - Variable Importance")

plot(rf1)
varUsed(rf1)

#tuneR
tuneRF(training[,-3],training[,3], stepFactor = 0.5, plot = T, ntreeTry = 500, trace = T)

#after tuning
rf2 <- randomForest(Potential~., data = training, mtry = 28)

print(rf2)

p2 <- predict(rf2, test)

actual_preds3 <- data.frame(cbind(actuals = test$Potential, predicteds = p2))

accuracy3 <- cor(actual_preds3)

?randomForest


#KNN model
# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = 'repeatedcv',
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = multiClassSummary)

model1 <- train(Potential~. , data = train, method = 'knn',
                preProcess = c('center','scale'),
                trControl = x,
                metric = 'ROC',
                tuneLength = tunel)

# Summary of model
model1
plot(model1)


# Validation
#valid_pred <- predict(model1,validation, type = 'prob')

valid_pred <- predict(model1, newdata = validation)
confusionMatrix(valid_pred, validation$Potential)


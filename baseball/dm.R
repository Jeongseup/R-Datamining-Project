
#data loading 
getwd()
setwd("C:/Projects/R_2020_businessanalytics")
getwd()

data <- read.csv("kbo_winners.csv" ,header = T)

View(data)
str(data)
head(data)

df <- data
# seed설정
set.seed(1)  

# 데이터 train과 valid나누기 train 60% / validation 40%
train.index <- sample(c(0:dim(df)[1]), dim(df)[1]*0.6)  

train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

# 데이터 파티션 확인
str(train.df) #272개
str(valid.df) #183개

library(rpart)
library(rpart.plot)

# decision tree making // train의 10%정도인 20으로 minbucket 설정, 오류 20개 이상일 때 가지를 침 
#최대 depth = 7 // 우승에 영향력 있는 주 원인만 고려하기 위함

default.ct <- rpart(우승여부 ~ ., data = train.df, control = rpart.control(maxdepth = 7), method = "class", minbucket = 20 )

# plot tree
prp(default.ct, type =0, extra = 1, under = TRUE, split.font = 2, varlen = -10,
    box.col=ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white'))

# Main Value ; 희비 , 볼넷, 장타, 삼진허용, 3루타, 2루타, 승

#Making Confusion Matrix 
#install.packages("e1071")

library(caret)
## confusion matrix for train set
train.pred <- predict(default.ct, train.df, type = "class")
#factor form
train_factor.df <- as.factor(train.df$우승여부)
train_factor.pred <- as.factor(train.pred)
confusionMatrix(train_factor.pred, train_factor.df)

# Train Accuracy : 0.9118 

## confusion matrix for valid set
valid.pred <- predict(default.ct, newdata = valid.df, type="class")
#factor form
valid_factor.pred <- as.factor(valid.pred)
valid_factor.df <- as.factor(valid.df$우승여부)
confusionMatrix(valid_factor.pred, valid_factor.df)

# Validation Accuracy : 0.7869  

library(randomForest)

#install.packages("randomForest")    
# mtry= 20, 기존 변수가 많으므로 약 50% 20개로 Bagging 
# ntree = 1000 , 47C2 는 약 1000개의 경우의 수
# nodesize = maxdepth

rf <- randomForest(as.factor(우승여부)~. , data = train.df, ntree = 1000,
                   mtry = 20, nodesize = 7 , importance = T,proximity=T)


importance(rf) #중요도
varImpPlot(rf, type = 1) #MeanDecrease Plotting

# Main Value : 삼진허용 희비 OPS 타율 장타  홈런 볼넷 ..


rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$우승여부)
# Random Forest Accuracy : 0.8852 

# Result Interpret
# Decision Tree -> Random Forest 10%정도 상승

# DT Value ; 희비 , 볼넷, 장타, 삼진허용, 3루타, 2루타, 승 -> # RF Value : 삼진허용 희비 OPS 타율 장타  홈런 볼넷 ..
# *Important Value(just Considering to both value) ; 희비 삼진허용 장타 볼넷

  
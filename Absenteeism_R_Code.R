rm(list = ls())
setwd("D:/DS_New/Project")
getwd()

#Loading Libraries
library(xlsx)
library(stats)
library(usdm)
library(DMwR)
library(utils)
library(corrgram)
library(randomForest)
library(psych)
library(usdm)
library(reshape2)
library(utils)
library(rpart)

#Reading Data
df = read.xlsx("Absenteeism_at_work_Project.xls", sheetIndex = 1)
str(df)

#Univariate Analysis
#Converting 0 in a few columns to NA 
df$Month.of.absence[df$Month.of.absence == 0] = NA
df$Reason.for.absence[df$Reason.for.absence == 0] = NA

#Converting relevant predictors to factor
for (i in c(1,2,3,4,5,12,13,15,16))
{
  df[,i] = as.factor(df[,i])
}


#Missing Value Analysis
miss_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
miss_val$Predictorsr = row.names(miss_val)
names(miss_val)[1] =  "Missing_Percentage"
miss_val$Missing_Percentage = (miss_val$Missing_Percentage/nrow(df)) * 100
miss_val = miss_val[order(-miss_val$Missing_Percentage),]
row.names(miss_val) = NULL
miss_val = miss_val[,c(2,1)]

write.xlsx(miss_val, "Missing_Values.xlsx", row.names=F)

#Imputation
df_test = df
df_test$Work.load.Average.day.[20] 
df_test$Work.load.Average.day.[20] = NA

#Test
df_test$Work.load.Average.day.[is.na(df_test$Work.load.Average.day.)] =median(df_test$Work.load.Average.day., na.rm = T)
df_test$Work.load.Average.day.[is.na(df_test$Work.load.Average.day.)] =mean(df_test$Work.load.Average.day., na.rm = T) 
df_test = knnImputation(df_test)

#Freezing KNN Imputation
df = knnImputation(df)

#Clean the environment
rmExcept("df")

#Creating separate Data Frames for numeric and categorical variables
numeric_index = sapply(df, is.numeric)
num_dt = df[,numeric_index]
num_nm = colnames(num_dt)

cat_index = sapply(df, is.factor)
cat_dt = df[,cat_index]
cat_name = colnames(cat_dt)


#Outlier Analysis
#Visualization

colnames(df)
par(mar = c(2,2,2,2))
hist(df$Height)
boxplot(df$Height,data = df, xlab = "Boxplot for Height")

#Replacing Outliers with NA
for (i in num_nm)
{
  print(i)
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(val)
  df[,i][df[,i] %in% val] = NA
}

#Complete Cases
df = df[complete.cases(df),]

#Variable Importance
rm(PredImp)
PredImp = randomForest(Absenteeism.time.in.hours ~ ., data = df, ntree = 1000, keep.forest = FALSE, importance = TRUE)
importance(PredImp, type = 1)

#Correlation
corrgram(df[,numeric_index], order = F,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

symnum(cor(df[,numeric_index]))

#Dimensionality Reduction
df = subset(df, select = -c(Social.smoker,Weight,Day.of.the.week,Education,Height))

#Feature Scaling
hist(df$Age)

#Normalization
numeric_index = sapply(df[,-17], is.numeric)
num_dt = df[,numeric_index]
num_nm = colnames(num_dt)

cat_index = sapply(df, is.factor)
cat_dt = df[,cat_index]
cat_name = colnames(cat_dt)

for(i in num_nm){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/(max(df[,i]) - min(df[,i]))
}


#Standardisation
for(i in num_nm)
{
  print(i)
  df[,i] = (df[,i] - mean(df[,i]))/sd(df[,i])
}

rm(i)
write.xlsx(df, "Absenteeism_Clean.xlsx", row.names=F)

#Error Metrics
mae = function(y,yhat){
  mean(abs(y-yhat))
}

mse = function(y,yhat) {
  mean((y-yhat)^2)
}


#Multiple Linear Regressio n
lr_model = lm(Absenteeism.time.in.hours ~., data = df)
summary(lr_model)

pred_lr = predict(lr_model , df[,-16])

mae(df[,16],pred_lr)
mse(df[,16],pred_lr)

anova(lr_model)
plot(lr_model, col = 'blue')

#Regression Tree
rt_model = rpart(Absenteeism.time.in.hours ~., data = df, method = 'anova')

summary(rt_model)

pred_rt = predict(rt_model , df[,-16])
mae(df[,16],pred_rt)
mse(df[,16],pred_rt)


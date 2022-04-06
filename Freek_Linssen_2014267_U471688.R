###Final exam Freek Linssen 


dev.off(dev.list()["RStudioGD"])
rm(list = ls())
cat("\014")


library(psych)
library(corrplot)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(ISLR)
library(glmnet)
library(tidyr)
library(ranger)
library(ipred)
library(data.table)
library(ggdark)
library(writexl)
library(DescTools)
library(stargazer)
library(rpart.plot)
library(rattle)
library(RColorBrewer)




filepath = rstudioapi::getSourceEditorContext()$path
dirpath = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirpath)


set.seed(1)
## Loading data ##
data <- readRDS("crsp_data_exam2022.rds")



## 1.1 ##
data = data %>% as_tibble() 

data = data %>%
  arrange(permno)


data = data %>%
  group_by(permno) %>%
  mutate(prc = lag(prc))%>%
  mutate(shrout = lag(shrout))%>%
  mutate(mom1 = lag(ret))


data = data %>%
  group_by(permno) %>%
  mutate(mom_2_1 = (1 + lag(ret, n = 2L)) - 1)


data = data %>%
  group_by(permno) %>%
  mutate(mom_3_1 = (1 + lag(ret, n = 2L))
          *(1 + lag(ret, n = 3L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_4_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_5_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L))
         *(1 + lag(ret, n = 5L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_6_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L))
         *(1 + lag(ret, n = 5L))
         *(1 + lag(ret, n = 6L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_7_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L))
         *(1 + lag(ret, n = 5L))
         *(1 + lag(ret, n = 6L))
         *(1 + lag(ret, n = 7L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_8_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L))
         *(1 + lag(ret, n = 5L))
         *(1 + lag(ret, n = 6L))
         *(1 + lag(ret, n = 7L))
         *(1 + lag(ret, n = 8L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_9_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L))
         *(1 + lag(ret, n = 5L))
         *(1 + lag(ret, n = 6L))
         *(1 + lag(ret, n = 7L))
         *(1 + lag(ret, n = 8L)) 
         *(1 + lag(ret, n = 9L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_10_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L))
         *(1 + lag(ret, n = 5L))
         *(1 + lag(ret, n = 6L))
         *(1 + lag(ret, n = 7L))
         *(1 + lag(ret, n = 8L)) 
         *(1 + lag(ret, n = 9L))
         *(1 + lag(ret, n = 10L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_11_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L))
         *(1 + lag(ret, n = 5L))
         *(1 + lag(ret, n = 6L))
         *(1 + lag(ret, n = 7L))
         *(1 + lag(ret, n = 8L)) 
         *(1 + lag(ret, n = 9L))
         *(1 + lag(ret, n = 10L))
         *(1 + lag(ret, n = 11L)) - 1)

data = data %>%
  group_by(permno) %>%
  mutate(mom_12_1 = (1 + lag(ret, n = 2L))
         *(1 + lag(ret, n = 3L))
         *(1 + lag(ret, n = 4L))
         *(1 + lag(ret, n = 5L))
         *(1 + lag(ret, n = 6L))
         *(1 + lag(ret, n = 7L))
         *(1 + lag(ret, n = 8L)) 
         *(1 + lag(ret, n = 9L))
         *(1 + lag(ret, n = 10L))
         *(1 + lag(ret, n = 11L))
         *(1 + lag(ret, n = 12L)) - 1)



des <- describe(data)
print(des,digits=5)

tmp = seq()
for (i in colnames(data)){
  tmp[i] = sum(is.na(data[[i]]))
}
tmp


##1.2 

data = data %>%
  group_by(date)%>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))


tmp = seq()
for (i in colnames(data)){
  tmp[i] = sum(is.na(data[[i]]))
}
tmp

des <- describe(data)
print(des,digits=5)


##1.3

data = data %>%
  replace(is.na(.), 0.0)

tmp = seq()
for (i in colnames(data)){
  tmp[i] = sum(is.na(data[[i]]))
}
tmp

des <- describe(data)
print(des,digits=5)



## 1.4

data = data %>%
  group_by(date)%>%
  mutate(test_max = max(mom1))%>%
  mutate(test_min = min(mom1))%>%
  mutate(mom1 = ((2 * (mom1 - min(mom1))) / (max(mom1) - min(mom1))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_2_1 = ((2 * (mom_2_1 - min(mom_2_1))) / (max(mom_2_1) - min(mom_2_1))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_3_1 = ((2 * (mom_3_1 - min(mom_3_1))) / (max(mom_3_1) - min(mom_3_1))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_4_1 = ((2 * (mom_4_1 - min(mom_4_1))) / (max(mom_4_1) - min(mom_4_1))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_5_1 = ((2 * (mom_5_1 - min(mom_5_1))) / (max(mom_5_1) - min(mom_5_1))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_6_1 = ((2 * (mom_6_1 - min(mom_6_1))) / (max(mom_6_1) - min(mom_6_1))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_7_1 = ((2 * (mom_7_1 - min(mom_7_1))) / (max(mom_7_1) - min(mom_7_1))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_8_1 = ((2 * (mom_8_1  - min(mom_8_1 ))) / (max(mom_8_1 ) - min(mom_8_1 ))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_9_1 = ((2 * (mom_9_1  - min(mom_9_1 ))) / (max(mom_9_1 ) - min(mom_9_1 ))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_10_1 = ((2 * (mom_10_1  - min(mom_10_1 ))) / (max(mom_10_1 ) - min(mom_10_1 ))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_11_1 = ((2 * (mom_11_1  - min(mom_11_1 ))) / (max(mom_11_1 ) - min(mom_11_1 ))) -1 )

data = data %>%
  group_by(date)%>%
  mutate(mom_12_1 = ((2 * (mom_12_1  - min(mom_12_1 ))) / (max(mom_12_1 ) - min(mom_12_1 ))) -1 )



data = data %>%
  replace(is.na(.), 0.0)


des <- describe(data)
print(des,digits=5)


##1.5

train_data = data %>%
  filter(date <= '1999-12-31')

train_data = train_data %>%
  filter(date  >= '1990-01-01')


val_data = data %>%
  filter(date <= '2007-06-30')

val_data = val_data %>%
  filter(date >= '2000-01-01')


test_including_fc_data = data %>%
  filter(date <= '2021-12-31')

test_including_fc_data = test_including_fc_data %>%
  filter(date >= '2007-07-01')


test_excluding_fc_data = data %>%
  filter(date <= '2021-12-31')

test_excluding_fc_data = test_including_fc_data %>%
  filter(date >= '2010-01-01')


des <- describe(train_data)
print(des,digits=5)
des <- describe(val_data)
print(des,digits=5)
des <- describe(test_including_fc_data)
print(des,digits=5)
des <- describe(test_excluding_fc_data)
print(des,digits=5)




#1.6

train_data_cor_matrix = train_data%>%
  ungroup()%>%
  select(-date , -permno, -prc, -shrout, -test_max, -test_min)


train_matrix <- cor(train_data_cor_matrix)


corrplot( train_matrix, method="number", col=brewer.pal(n=4, name= "RdYlBu"))



#######################################2
 
 
lm1 = lm(ret ~ mom1 + mom_2_1 + mom_3_1 + mom_4_1 + mom_5_1 + mom_6_1 + mom_7_1 + mom_8_1
         + mom_9_1 + mom_10_1 + mom_11_1 + mom_12_1, data= train_data)

summary(lm1)

out <- capture.output(summary(lm1))

cat("Linear Regression Training Data", out, file="2.1.txt", sep="\n", append=TRUE)



###################### R square on test data
train_data$predict = predict(lm1, 
                            newdata= train_data)


Kelly_Gu_R_2 <- function( true, predicted) {
      R_Squared <- 1 - ((sum((true - predicted)^2))/(sum(true^2)))
     return(R_Squared)
    }
  
   Kelly_Gu_R_2(train_data$ret, train_data$predict)
  
   
    
#################################### R square on validation data
   
val_data$predict = predict(lm1, newdata= val_data)
   
Kelly_Gu_R_2(val_data$ret, val_data$predict)

##################################### test_including_fc_data:

test_including_fc_data$predict = predict(lm1, newdata = test_including_fc_data)
Kelly_Gu_R_2(test_including_fc_data$ret, test_including_fc_data$predict)


####################################test_excluding_fc_data:

test_excluding_fc_data$predict = predict(lm1, newdata = test_excluding_fc_data)
Kelly_Gu_R_2(test_excluding_fc_data$ret, test_excluding_fc_data$predict)






   
##########################################make a matrix for all variables
   
tmp = train_data%>%
     ungroup()%>%
     select(-permno, -date, -prc, -shrout, -test_max, -test_min, -predict) 
   
x_train= model.matrix( ret~., tmp)[, -1]
y_train= train_data$ret
   
   
   
tmp = val_data%>%
  ungroup()%>%
  select(-permno, -date, -prc, -shrout, -test_max, -test_min, -predict)
   
x_val = model.matrix( ret~., tmp)[, -1]
y_val = val_data$ret
   
   
   
tmp = test_including_fc_data%>%
     ungroup()%>%
     select(-permno, -date, -prc, -shrout, -test_max, -test_min, -predict)
   
x_test_including_fc = model.matrix( ret~., tmp)[, -1]
y_test_including_fc = test_including_fc_data$ret

   
   
tmp = test_excluding_fc_data%>%
     ungroup()%>%
     select(-permno, -date, -prc, -shrout, -test_max, -test_min, -predict)
   
x_test_excluding_fc = model.matrix( ret~., tmp)[, -1]
y_test_excluding_fc =test_excluding_fc_data$ret
   
   
   
###########2.2

lambda <- 10^seq(-5, 1, length = 200)

Kelly_Gu_R_2_vector <- vector()

for (i in 1:length(lambda)) {
  cat(paste0("iteration: ", i, "\n"))
  
  model_lasso = glmnet(x_train,           # predictors (use train sample)
                        y_train,          # y variable (use train sample)
                        alpha= 1,         # alpha = 1--> lasso
                        lambda = lambda[i]) 

  
  val_data$pred_lasso = predict(model_lasso, x_val)
  val_r_2 = Kelly_Gu_R_2(val_data$ret, val_data$pred_lasso)
  
  Kelly_Gu_R_2_vector[i] = val_r_2
  
}

max = max(Kelly_Gu_R_2_vector)
max
max_index  = which.max(Kelly_Gu_R_2_vector)
max_index
lambda[max_index]


##optimal lambda value is 0.004198707, reached at iteration 88 with a R-squared 0.005356456. 

log_lambda = log(lambda)

plot(log_lambda, Kelly_Gu_R_2_vector, type= "b", lwd=2, ylab= "Kelly Gu R-Squared (validation data)", xlab= "log lambda")


############### check the model with another approach 

check_lasso = cv.glmnet(x_train,               # predictors (use train sample)
                      y_train,    # y variable (use train sample)
                      alpha= 1,                 # alpha = 1--> lasso  
                      nfolds = 5,
                      lambda = lambda) 

check_lasso$lambda.min
coef(check_lasso, 
     s= check_lasso$lambda.min) 


train_data$pred_lasso = predict(check_lasso, x_train)
Kelly_Gu_R_2(train_data$ret, train_data$pred_lasso)

val_data$pred_lasso = predict(check_lasso, x_val)
Kelly_Gu_R_2(val_data$ret, val_data$pred_lasso)


## the earlier method gives indeed a higher accuracy on the validation set, so I use thatone 

####2.3

# decided to build the final model with cross-validation

final_model = glmnet(x_train,               # predictors (use train sample)
                      y_train,    # y variable (use train sample)  
                      alpha = 1,
                      lambda = lambda[max_index])   


out <- capture.output(coef(final_model))
coef(final_model)
final_model


cat("Optimal Lasso Regression", out, file="2.1.txt", sep="\n", append=TRUE)



######train_data
train_data$pred_lasso = predict(final_model, x_train)
Kelly_Gu_R_2(train_data$ret, train_data$pred_lasso)

####test_data
val_data$pred_lasso = predict(final_model, x_val)
Kelly_Gu_R_2(val_data$ret, val_data$pred_lasso)


#############################2.4

# test_including_fc_data:
test_including_fc_data$pred_lasso = predict(final_model, x_test_including_fc)
Kelly_Gu_R_2(test_including_fc_data$ret, test_including_fc_data$pred_lasso)

#test_excluding_fc_data:

test_excluding_fc_data$pred_lasso = predict(final_model, x_test_excluding_fc)
Kelly_Gu_R_2(test_excluding_fc_data$ret, test_excluding_fc_data$pred_lasso)







#########################################3


###################excluding crisis
test_excluding_fc_data = test_excluding_fc_data %>%
  group_by(date)%>%
   mutate(quintiles= ntile(pred_lasso, 5))


#market return for later
test_excluding_fc_data = test_excluding_fc_data %>%
  as_tibble() %>%
  mutate(market_cap = shrout * prc )%>%
  group_by(date)%>%
  mutate(tot_mcap = sum(market_cap))%>%
  mutate(contribution = ret *(market_cap/tot_mcap))%>%
  mutate(market_ret  = sum(contribution))
#
     
test_excluding_fc_data_long = test_excluding_fc_data%>%
  filter(quintiles == 5 )%>%
  group_by(date)%>%
  summarise(long_return = mean(ret))

test_excluding_fc_data_short = test_excluding_fc_data%>%
  filter(quintiles == 1 )%>%
  group_by(date)%>%
  summarise(short_return = mean(ret))

test_excluding_fc_data_market = test_excluding_fc_data%>%
  group_by(date)%>%
  summarise(market_return = mean(market_ret ))


result_test_excluding_fc_3.1 = merge(test_excluding_fc_data_long,
                 test_excluding_fc_data_short,
                 by = c("date"))

result_test_excluding_fc_3.1 = merge(result_test_excluding_fc_3.1,
                                     test_excluding_fc_data_market,
                                     by = c("date"))

summary(result_test_excluding_fc_3.1)



######################### including crisis
test_including_fc_data = test_including_fc_data %>%
  group_by(date)%>%
  mutate(quintiles= ntile(pred_lasso, 5))

#market return for later
test_including_fc_data = test_including_fc_data %>%
  as_tibble() %>%
  mutate(market_cap = shrout * prc )%>%
  group_by(date)%>%
  mutate(tot_mcap = sum(market_cap))%>%
  mutate(contribution = ret *(market_cap/tot_mcap))%>%
  mutate(market_ret  = sum(contribution))
#

test_including_fc_data_long = test_including_fc_data%>%
  filter(quintiles == 5 )%>%
  group_by(date)%>%
  summarise(long_return = mean(ret))

test_including_fc_data_short = test_including_fc_data%>%
  filter(quintiles == 1 )%>%
  group_by(date)%>%
  summarise(short_return = mean(ret))

test_including_fc_data_market = test_including_fc_data%>%
  group_by(date)%>%
  summarise(market_return = mean(market_ret ))


result_test_including_fc_3.1 = merge(test_including_fc_data_long,
                                     test_including_fc_data_short,
                                     by = c("date"))

result_test_including_fc_3.1 = merge(result_test_including_fc_3.1,
                                     test_including_fc_data_market,
                                     by = c("date"))


summary(result_test_including_fc_3.1)



############## 3.2

######################### excluding crisis

result_test_excluding_fc_3.1 =result_test_excluding_fc_3.1 %>%
  mutate(Momentum_Factor = long_return - short_return)

des <- describe(result_test_excluding_fc_3.1)
print(des,digits=5)

######################### including crisis


result_test_including_fc_3.1 =result_test_including_fc_3.1 %>%
  mutate(Momentum_Factor = long_return - short_return)

des <- describe(result_test_including_fc_3.1)
print(des,digits=5)

###3.3

CAPM_including_fc =  lm(Momentum_Factor ~ market_return, data = result_test_including_fc_3.1)

summary(CAPM_including_fc)

stargazer(CAPM_including_fc, type = "html", title = "CAPM regression test set including crisis", out = "3.3_inc.htm")


CAPM_excluding_fc =  lm(Momentum_Factor ~ market_return, data = result_test_excluding_fc_3.1)

summary(CAPM_excluding_fc)

stargazer(CAPM_excluding_fc, type = "html", title = "CAPM regression test set excluding crisis", out = "3.3_exc.htm")






############################ 4

library(keras)
library(ISLR)
library(tensorflow)

tensorflow::tf$random$set_seed(1)


##4.1 + 4.2 

dim(x_train)[2]

model <- keras_model_sequential() %>% 
  layer_dense(units = 4, activation = "relu", input_shape = dim(x_train)[2]) %>% 
  layer_dense(units = 1, activation = "linear")


model %>% compile(
  optimizer = "rmsprop",
  loss = "MeanSquaredError",
  metrics = c("accuracy")
)


history <- model %>% fit(
  x_train,
  y_train,
  epochs = 5,
  batch_size = 200,
  validation_data = list(x_val, y_val),
)

###############################train_data

train_data$DL_predictions1 <- model %>% predict(x_train)

Kelly_Gu_R_2(train_data$ret, train_data$DL_predictions1)

###############################val_data

val_data$DL_predictions1 <- model %>% predict(x_val)

Kelly_Gu_R_2(val_data$ret, val_data$DL_predictions1)


###############################x_test_excluding_fc
result <- model %>% evaluate(x_test_excluding_fc, y_test_excluding_fc)
 
test_excluding_fc_data$DL_predictions1 <- model %>% predict(x_test_excluding_fc)

Kelly_Gu_R_2(test_excluding_fc_data$ret, test_excluding_fc_data$DL_predictions1)



#################################x_test_including_fc
result <- model %>% evaluate(x_test_including_fc, y_test_including_fc)

test_including_fc_data$DL_predictions1 <- model %>% predict(x_test_including_fc)

Kelly_Gu_R_2(test_including_fc_data$ret, test_including_fc_data$DL_predictions1)




##4.3



model2 <- keras_model_sequential() %>% 
  layer_dense(units = 4, activation = "relu", input_shape = dim(x_train)[2], kernel_regularizer = regularizer_l2(0.001)) %>% 
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = "linear")


model2 %>% compile(
  optimizer = "rmsprop",
  loss = "MeanSquaredError",
  metrics = c("accuracy")
)


history <- model2 %>% fit(
  x_train,
  y_train,
  epochs = 5,
  batch_size = 200,
  validation_data = list(x_val, y_val),
)

###############################train_data

train_data$DL_predictions2 <- model2 %>% predict(x_train)

Kelly_Gu_R_2(train_data$ret, train_data$DL_predictions2)


###############################val_data

val_data$DL_predictions2 <- model2 %>% predict(x_val)

Kelly_Gu_R_2(val_data$ret, val_data$DL_predictions2)


###############################x_test_excluding_fc
result <- model2 %>% evaluate(x_test_excluding_fc, y_test_excluding_fc)

test_excluding_fc_data$DL_predictions2 <- model2 %>% predict(x_test_excluding_fc)

Kelly_Gu_R_2(test_excluding_fc_data$ret, test_excluding_fc_data$DL_predictions2)



#################################x_test_including_fc
result <- model2 %>% evaluate(x_test_including_fc, y_test_including_fc)

test_including_fc_data$DL_predictions2 <- model2 %>% predict(x_test_including_fc)

Kelly_Gu_R_2(test_including_fc_data$ret, test_including_fc_data$DL_predictions2)




## 4.4

# I will build the hedge portfolio based on the second model


########################### excluding financial crisis 
test_excluding_fc_data = test_excluding_fc_data %>%
  group_by(date)%>%
  mutate(quintiles_DL = ntile(DL_predictions2, 5))


test_excluding_fc_data_long = test_excluding_fc_data%>%
  filter(quintiles_DL == 5 )%>%
  group_by(date)%>%
  summarise(long_return_DL = mean(ret))

test_excluding_fc_data_short = test_excluding_fc_data%>%
  filter(quintiles_DL == 1 )%>%
  group_by(date)%>%
  summarise(short_return_DL = mean(ret))


result_test_excluding_fc_3.1 = merge(test_excluding_fc_data_long,
                                     result_test_excluding_fc_3.1,
                                     by = c("date"))

result_test_excluding_fc_3.1 = merge(result_test_excluding_fc_3.1,
                                     test_excluding_fc_data_short,
                                     by = c("date"))


result_test_excluding_fc_3.1 = result_test_excluding_fc_3.1%>%
mutate(factor_DL = long_return_DL- short_return_DL)

des <- describe(result_test_excluding_fc_3.1)
print(des,digits=5)




########################## including financial crisis
test_including_fc_data = test_including_fc_data %>%
  group_by(date)%>%
  mutate(quintiles_DL= ntile(DL_predictions2, 5))


test_including_fc_data_long = test_including_fc_data%>%
  filter(quintiles_DL == 5 )%>%
  group_by(date)%>%
  summarise(long_return_DL = mean(ret))

test_including_fc_data_short = test_including_fc_data%>%
  filter(quintiles_DL == 1 )%>%
  group_by(date)%>%
  summarise(short_return_DL = mean(ret))


result_test_including_fc_3.1 = merge(test_including_fc_data_long,
                                     result_test_including_fc_3.1,
                                     by = c("date"))

result_test_including_fc_3.1 = merge(result_test_including_fc_3.1,
                                     test_including_fc_data_short,
                                     by = c("date"))


result_test_including_fc_3.1 = result_test_including_fc_3.1%>%
  mutate(factor_DL = long_return_DL- short_return_DL)

des <- describe(result_test_including_fc_3.1)
print(des,digits=5)


##regression

CAPM_including_fc =  lm(factor_DL ~ market_return, data = result_test_including_fc_3.1)

summary(CAPM_including_fc)

stargazer(CAPM_including_fc, type = "html", title = "Deep Learning CAPM regression test set including crisis", out = "4.4_inc.htm")


CAPM_excluding_fc =  lm(factor_DL ~ market_return, data = result_test_excluding_fc_3.1)

summary(CAPM_excluding_fc)

stargazer(CAPM_excluding_fc, type = "html", title = "Deep Learning CAPM regression test set excluding crisis", out = "4.4_exc.htm")


## some extra plots:

for_plot =result_test_including_fc_3.1%>%
  mutate(market_return = market_return +1)%>%
  mutate(market_return = cumprod(market_return))%>%
  mutate(long_return = long_return +1)%>%
  mutate(long_return = cumprod(long_return))%>%
  mutate(short_return = short_return +1)%>%
  mutate(short_return = cumprod(short_return ))%>%
  select( date, market_return, long_return, short_return)



for_plot = for_plot[,c("date", "market_return", "long_return", "short_return")]
for_plot= pivot_longer(for_plot, !date, names_to = "ID")
plot =ggplot(data = for_plot, mapping = aes(y = value, x = as.Date(date), group = ID, color = ID)) +
  geom_line() +
  scale_colour_manual(name = "Return long and short portfolio",
                      breaks = c("market_return", "long_return", "short_return"),
                      values = c(brewer.pal(3, "Greens")),
                      labels = c("market_return" = "Market Return", "long_return" = "Long Return", "short_return" ="Short Return")) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", name = "Date") +
  scale_y_continuous(name = "Cumulative return") +
  ggtitle("Long and short momentum portfolio returns, test set including crisis") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot


for_plot =result_test_including_fc_3.1%>%
  mutate(market_return = market_return +1)%>%
  mutate(market_return = cumprod(market_return))%>%
  mutate(long_return = long_return +1)%>%
  mutate(long_return = cumprod(long_return))%>%
  mutate(short_return = short_return +1)%>%
  mutate(short_return = cumprod(short_return ))%>%
  mutate(Momentum_Factor = Momentum_Factor +1)%>%
  mutate(Momentum_Factor = cumprod(Momentum_Factor))%>%
  select( date, market_return, long_return, short_return, Momentum_Factor)


for_plot = for_plot[,c("date", "market_return", "long_return", "short_return", "Momentum_Factor")]
for_plot= pivot_longer(for_plot, !date, names_to = "ID")
plot =ggplot(data = for_plot, mapping = aes(y = value, x = as.Date(date), group = ID, color = ID)) +
  geom_line() +
  scale_colour_manual(name = "Return Momentum Factor",
                      breaks = c("market_return", "long_return", "short_return", "Momentum_Factor"),
                      values = c(brewer.pal(4, "Greens")),
                      labels = c("market_return" = "Market Return", "long_return" = "Long Return", "short_return" ="Short Return", "Momentum_Factor" = "Momentum Factor")) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", name = "Date") +
  scale_y_continuous(name = "Cumulative return") +
  ggtitle("Momentum factor returns, test set including crisis") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot


for_plot = result_test_including_fc_3.1%>%
  mutate(market_return = market_return +1)%>%
  mutate(market_return = cumprod(market_return))%>%
  mutate(factor_DL = factor_DL +1)%>%
  mutate(factor_DL = cumprod(factor_DL))%>%
  mutate(Momentum_Factor = Momentum_Factor +1)%>%
  mutate(Momentum_Factor = cumprod(Momentum_Factor))%>%
  select( date, market_return, factor_DL, Momentum_Factor)


for_plot = for_plot[,c("date", "market_return", "factor_DL", "Momentum_Factor")]
for_plot= pivot_longer(for_plot, !date, names_to = "ID")
plot =ggplot(data = for_plot, mapping = aes(y = value, x = as.Date(date), group = ID, color = ID)) +
  geom_line() +
  scale_colour_manual(name = "Return Momentum Factors",
                      breaks = c("market_return", "factor_DL", "Momentum_Factor"),
                      values = c(brewer.pal(3, "Greens")),
                      labels = c("market_return" = "Market Return", "factor_DL" ="Momentum factor DL", "Momentum_Factor" = "Momentum Factor Lasso")) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", name = "Date") +
  scale_y_continuous(name = "Cumulative return") +
  ggtitle("Momentum factor returns, test set including crisis") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot


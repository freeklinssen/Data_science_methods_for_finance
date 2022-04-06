#####################################################################################################
## Group Assignment Data Science Methods in Finance                                                ##
## Group Members:                                                                                  ##
## Freek Linssen          - SNR: 2015267 - ANR: U471688 - f.c.w.linssen@tilburguniversity.edu      ##
## Willem van der Meiden  - SNR: 2014236 - ANR: U800370 - w.f.m.vdrmeiden@tilburguniversity.edu    ##
## Sven Meijers           - SNR: 2002736 - ANR: U407995 - s.j.p.meijers@tilburguniversity.edu      ##
## Zeger Vugts            - SNR: 2008041 - ANR: U671613 - z.f.vugts@tilburguniversity.edu          ##
#####################################################################################################

# Clearing 
dev.off(dev.list()["RStudioGD"])
rm(list = ls())  ## libs
cat("\014")

# Maximizing storage
max_memory <- memory.size(max = TRUE)
memory.limit(99999999)

# Install packages
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("zoo")
# install.packages("rollRegres")
# install.packages("roll")
# install.packages("ISLR")
# install.packages("tree")
# install.packages("caret")
# install.packages("tidyr")
# install.packages("ranger")
# install.packages("randomForest")
# install.packages("leaps")
# install.packages("ipred")
# install.packages("data.table")


# Loading in libaries
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(rollRegres)
library(roll)
library(ISLR)
library(tree)
library(caret)
library(tidyr)
library(ranger)
library(randomForest)
library(leaps)
library(ipred)
library(data.table)
library(ggdark)
library(writexl)
library(lubridate)
library(magrittr)
library(DescTools)
library(stargazer)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

#Path of working directory
filepath = rstudioapi::getSourceEditorContext()$path
dirpath  = dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dirpath)

# Loading the data
CRSP_Monthly = read.csv("CRSP_Monthly.csv",
                        header = TRUE,
                        sep = ",")

################
## Question 1 ##
################

DataQ1 = CRSP_Monthly
summary(DataQ1) #Negative share prices (prc) should be solved --> absolute value

# Create seperate year and month variables
DataQ1 = DataQ1 %>%
  mutate(year = substr(date,1,4)) %>%
  mutate(month = substr(date,6,7))

DataQ1 = DataQ1 %>%
  mutate(date_for_plot = date)

DataQ1$date_for_plot = as.Date(DataQ1$date_for_plot, format="%Y-%m-%d")

# Create the same date variable as Fama and French to easily merge the datasets later in the assignment
DataQ1 = DataQ1 %>% 
  mutate(FF_date = paste(year, month, sep =""))

# Cleaning the data (solve the problem of negative share prices (prc)) and create new variables, besides filter on share code (10 & 11) and exchange code (1, 2 & 3)
DataQ1 = DataQ1 %>%
  mutate(prc = abs(prc)) %>%
  mutate(m_cap = prc * shrout) %>%
  mutate(log_m_cap = log(m_cap)) %>%
  arrange(permno, date) %>%
  filter(shrcd == 10 | shrcd == 11) %>%
  filter(exchcd == 1 | exchcd == 2 | exchcd == 3)

# Check the summary of the data and compare number of missing values of share price (prc) with raw dataset
summary(DataQ1)
summary(CRSP_Monthly)

# Create variable missing is 1 if return is missing and 0 otherwise
DataQ1 = DataQ1 %>%
  mutate(missing = ifelse(is.na(ret),1, 0))

# Performing analysis Question 1
DataQ1.1 <- DataQ1 %>%
  group_by(year) %>%
  summarise(count = n())
sum(DataQ1.1$count)

# Count number of missing values per year
tmp <- DataQ1 %>%
  filter(missing == 1) %>%
  group_by(year) %>%
  summarise(missing = n())
sum(DataQ1$missing)
sum(tmp$missing)

# Merging the datasets of total return observations (non-missing and missing values) with the missing value dataset
DataQ1.1 = merge(DataQ1.1,
                 tmp,
                 by = c("year"))  
sum(DataQ1.1$count)
sum(DataQ1$missing)

# Performing analysis on the merged datasets of Question 1
DataQ1.1 = DataQ1.1 %>%
  mutate(non_missing = count - missing) %>%
  mutate(missing_percentage = missing / count) %>%
  mutate(non_missing_percentage = non_missing / count) %>%
  mutate(control_total_percentage = missing_percentage + non_missing_percentage)

# Create plot
DataQ1.1 = DataQ1.1 %>% pivot_longer(!year, names_to = "ID")
data = DataQ1.1 %>% filter(ID != "count" & ID!= "control_total_percentage")
data[data$ID == "missing_percentage",] %<>% mutate(value = value * 4571428.5)# = data %>% mutate(value = value * 457142.85 * (ID == "missing_percentage"))
plot = ggplot(data = data %>% filter(ID != "missing_percentage" & ID != "non_missing_percentage"), mapping = aes(fill = ID, y = value, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(name = "Legend", breaks = c("non_missing", "missing", "missing_percentage"),
                    values = c("missing" = "light green", "non_missing" = "dark green", "missing_percentage" = "white"),
                    labels = c("Non-missing observations", "Missing observations", "Percentage missing observations")) +
  geom_line(data = data %>% filter(ID == "missing_percentage"), aes(x = year, y = value, group = 1)) +
  scale_y_continuous(name = "Count",sec.axis = sec_axis(~./4571428.5, name = "Missing percentage")) +
  scale_x_discrete(name = "Date") +
  ggtitle("Overview of missing, non missing and the percentage missing observations in each year") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot

# Construct the required values (mean returns, standard deviation of returns, mean log market cap, standard deviation of log market cap)
DataQ1.2 = DataQ1 %>%
  filter(!is.na(ret))
summary(DataQ1.2)

# Mean return (reported monthly in percentage points)
Data_mean_return_yearly = DataQ1.2 %>%
  group_by(year) %>%
  summarise(mean(ret)*100)

# Standard deviation return (reported monthly in percentage points)
Data_sd_return_yearly = DataQ1.2 %>%
  group_by(year) %>%
  summarise(sd(ret)*100)

# Mean log market cap (reported monthly)
Data_mean_log_market_cap_yearly = DataQ1.2 %>%
  group_by(year) %>%
  summarise(mean(log_m_cap))

# Standard Deviation log market cap (reported monthly in percentage points)
Data_sd_log_market_cap_yearly = DataQ1.2 %>%
  group_by(year) %>%
  summarise(sd(log_m_cap))

# Generating the table output
table_year = cbind(Data_mean_return_yearly, Data_sd_return_yearly, Data_mean_log_market_cap_yearly, Data_sd_log_market_cap_yearly)
write_xlsx(table, "Question 1 table yearly.xlsx")


## Do this the same only for monthly returns
Data_mean_return_monthly = DataQ1.2 %>%
  group_by(month) %>%
  summarise(mean(ret)*100)

# Standard deviation return (reported monthly in percentage points)
Data_sd_return_monthly = DataQ1.2 %>%
  group_by(month) %>%
  summarise(sd(ret)*100)

# Mean log market cap (reported monthly)
Data_mean_log_market_cap_monthly = DataQ1.2 %>%
  group_by(month) %>%
  summarise(mean(log_m_cap))

# Standard Deviation log market cap (reported monthly in percentage points)
Data_sd_log_market_cap_monthly = DataQ1.2 %>%
  group_by(month) %>%
  summarise(sd(log_m_cap))

# Generating the table output
table = cbind(Data_mean_return_monthly, Data_sd_return_monthly, Data_mean_log_market_cap_monthly, Data_sd_log_market_cap_monthly)
write_xlsx(table, "Question 1 table monthly.xlsx")

DataQ1.3 = DataQ1.2 %>%
  summarise(mean(ret), mean(log_m_cap), sd(ret), sd(log_m_cap)) 

################
## Question 2 ##
################

# Lag the market cap, since the price is given on the last date of the month, therefore the market cap belongs to the month after previous month
DataQ2 = DataQ1.2 %>%
  group_by(permno) %>%
  arrange(permno, date) %>%
  mutate(m_cap = lag(m_cap)) %>%
  mutate(log_m_cap = lag(log_m_cap))
summary(DataQ2)

# Calculate the total market cap of all companies by date
DataQ2 = DataQ2 %>%
  group_by(date) %>%
  mutate(missing = ifelse(is.na(m_cap),1, 0)) %>%
  filter(missing == 0) %>%
  mutate(tot_mcap = sum(m_cap, na.rm = TRUE)) %>%
  arrange(date)

summary(DataQ2)

# Number of companies with a market cap 
DataQ2 = DataQ2 %>%
  group_by(date) %>%
  mutate(n_companies = n())

# Weight for value weighted portfolio and contribution to total return of value portfolio
DataQ2 = DataQ2 %>%
  mutate(weight_value_portfolio = m_cap / tot_mcap) %>%
  mutate(value_portfolio_contribution = weight_value_portfolio * ret)

# Weight for equal weighted portfolio and contribution to total return of equal portfolio
DataQ2 = DataQ2 %>%
  mutate(weight_equal_portfolio = 1/n_companies) %>%
  mutate(equal_portfolio_contribution = weight_equal_portfolio * ret)

# Total return value portfolio
DataQ2 = DataQ2 %>%
  group_by(date) %>%
  mutate(value_weighted_ret = sum(value_portfolio_contribution, na.rm = TRUE))

# Total return equal portfolio
DataQ2 = DataQ2 %>%
  group_by(date) %>%
  mutate(equally_weighted_ret = sum(equal_portfolio_contribution, na.rm = TRUE))

# Summarize in single data frame
DataQ2.1 = DataQ2 %>%
  group_by(FF_date) %>%
  summarise(value_weighted_ret  = mean(value_weighted_ret ), equally_weighted_ret  = mean(equally_weighted_ret))

# Construct the cumulative return over the period for both portfolios (equal weighted and value weighted)
DataQ2.1 = DataQ2.1 %>%
  mutate(value_weighted_ret= value_weighted_ret + 1) %>%
  mutate(equally_weighted_ret = equally_weighted_ret + 1)
DataQ2.1 = DataQ2.1 %>%
  mutate (cum_value_weighted_ret = cumprod(value_weighted_ret))%>%
  mutate (cum_equally_weighted_ret = cumprod(equally_weighted_ret))

# Import the market data
FF_data <- data.table::fread("F-F_Research_Data_Factors-2.csv",
                             header = TRUE,
                             sep = ",")

# Understanding the market data
summary(FF_data)

# Constructing the market return (= Market Risk Premium + Risk Free Rate)
FF_data = FF_data %>%
  mutate(`Mkt-RF` = `Mkt-RF`/100)%>%
  mutate(RF = RF/100)%>%
  mutate(Mkt_ret = RF + `Mkt-RF`)

Mkt_ret_Q5 = FF_data[,c("V1", "Mkt_ret")]

# Construct the cumulative return over the period for the market 
FF_data = FF_data %>%
  mutate(Mkt_ret = Mkt_ret + 1)%>%
  rename (FF_date = V1) %>%
  filter(FF_date <= 202012) %>%
  filter(FF_date >= 200002) %>%
  mutate (cum_Mkt_ret = cumprod(Mkt_ret))

# Merge the data of the portfolios and the market
DataQ2.2 = merge(DataQ2.1,
                 FF_data,
                 by = c("FF_date")) 

# Plot the cumulative return
DataQ2.3 = DataQ2.2 %>%
  select(FF_date, cum_value_weighted_ret, cum_equally_weighted_ret, cum_Mkt_ret) 

DataQ2.3[nrow(DataQ2.3)+1,]=c(200001,1,1,1)

DataQ2.3 = DataQ2.3 %>%
  arrange(FF_date) %>%
  mutate(FF_date = paste0(FF_date, "01"))
DataQ2.3 = DataQ2.3 %>% pivot_longer(!FF_date, names_to = "ID")
DataQ2.3$FF_date = as.Date(DataQ2.3$FF_date, "%Y%m%d")
DataQ2.3$FF_date = ceiling_date(DataQ2.3$FF_date, "month") - days(1)

plot = ggplot(data = DataQ2.3, mapping = aes(y = value, x = FF_date, group = ID, color = ID, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = c("cum_value_weighted_ret" = "dashed", "cum_equally_weighted_ret" = "solid", "cum_Mkt_ret" = "solid"),
                        name = "Legend", breaks = c("cum_value_weighted_ret", "cum_equally_weighted_ret", "cum_Mkt_ret"),
                        labels = c("Value weighted portfolio", "Equally weighted portfollio", "Market factor")) +
  scale_colour_manual(name = "Legend", breaks = c("cum_value_weighted_ret", "cum_equally_weighted_ret", "cum_Mkt_ret"),
                      values = c("cum_value_weighted_ret" = "yellow", "cum_equally_weighted_ret" = "white", "cum_Mkt_ret" = "dark green"),
                      labels = c("Value weighted portfolio", "Equally weighted portfollio", "Market factor")) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", name = "Date") +
  scale_y_continuous(name = "Cumulative return") +
  ggtitle("Cumulative return of the equally weighted portfolio, value weighted portfolio and the market factor") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot

################
## Question 3 ##
################

# Merge the cleaned data with the Fama and French data
DataQ3 = merge(DataQ2,
               FF_data,
               by = c("FF_date")) 
summary(DataQ3)

# Clean the data frame
DataQ3 = DataQ3 %>%
  filter(!is.na(ret))%>%
  select(FF_date, permno, date, prc, ret, shrout, vol, exchcd, shrcd, year, month, `Mkt-RF`, RF) %>%
  mutate(excess_return = ret - RF)
summary(DataQ3)

# Arrange the data
DataQ3 = DataQ3 %>%
  arrange(permno, date)

# Count number of observations per permno
DataQ3.1 = DataQ3 %>%
  group_by(permno) %>%
  summarise(count=n())
summary(DataQ3.1)

# Merge number of permno with original data
DataQ3 = merge(DataQ3, DataQ3.1,by="permno")

# Delete observations that do not cover a 60 month window
tmp = count(distinct(DataQ3, permno)) 

DataQ3_Control= DataQ3 %>%
  summarise(permno, count) %>%
  distinct(permno,count)
sum(DataQ3_Control$count < 60)

DataQ3 = DataQ3 %>%
  filter(!count < 60)
summary(DataQ3)
tmp = count(distinct(DataQ3, permno))

rm(DataQ3_Control)
rm(DataQ3.1)

# Check if their is a difference of more than one month between the subsequent data points per permno (important for subsequent 60 month rolling window)
DataQ3 = DataQ3 %>%
  group_by(permno) %>%
  mutate(months = as.numeric(difftime(date, lag(date), units="days"))/(365.25/12)) 

DataQ3 = DataQ3 %>%
  mutate(months = round(months))

# Check which companies have a gap in the rolling window
summary(DataQ3$months)
tmp = DataQ3 %>%
  group_by(permno) %>%
  summarise(months) %>%
  filter(months>1) %>%
  distinct(permno)

# Keep only the data that have no gap
DataQ3 = DataQ3[!(DataQ3$permno %in% tmp$permno),]

summary(DataQ3$months)

# Which firms are suited for rolling regression
tmp = DataQ3 %>%
  summarise(permno) %>%
  distinct(permno)

# Rolling beta window 60 months
DataQ3 = DataQ3 %>%
  arrange(permno, date)

# Rolling beta window 60 months
Rolling_output = roll_regres(excess_return ~ `Mkt-RF`, DataQ3, width = 60L)

tmp = Rolling_output[["coefs"]]
DataQ3 = cbind(DataQ3, tmp[ , 2])

# Change column name
colnames(DataQ3)
DataQ3 = DataQ3 %>%
  rename(beta = ...17)


# Delete all first 59 observations of each seperate permno
DataQ3 = DataQ3 %>%
  mutate(ones = 1) %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(n = cumsum(ones)) %>%
  select(-ones)%>%
  mutate(beta = ifelse(n < 60, NA, beta))

DataQ4 = DataQ3
DataQ3 = DataQ3 %>%
  filter(n >= 60)

# Split into 100 groups
DataQ3.1 = DataQ3 %>%
  group_by(date) %>%
  mutate(hunderdtiles = ntile(beta, 100)) %>%
  arrange(hunderdtiles, date, beta)

summary(DataQ3.1$hunderdtiles)


# Select only the useful variables
DataQ3.1 = DataQ3.1 %>%
  select(permno, date, excess_return, RF, hunderdtiles, beta)%>%
  group_by(date, hunderdtiles)%>%
  mutate(count = n())%>%
  filter(!is.na(beta))

DataQ3.1 = DataQ3.1 %>%
  arrange(date, hunderdtiles, beta)

# Create portfolio
DataQ3.1 = DataQ3.1 %>%
  mutate(weight_return = 1 / count) %>%
  mutate(excess_ret_contribution = excess_return * weight_return) %>%
  group_by(date, hunderdtiles)%>%
  mutate(excess_equally_weighted_return = sum(excess_ret_contribution))%>%
  mutate(equally_weighted_return = excess_equally_weighted_return + RF)

# Create required figures
DataQ3.1 = DataQ3.1 %>%
  group_by(date, hunderdtiles)%>%
  summarize(mean_equally_weighted_return   = mean(equally_weighted_return), count = mean(count),  beta =  mean(beta))

DataQ3.1 = DataQ3.1 %>%
  arrange(date, hunderdtiles, beta)


# Plot
# Return = y, beta = x
DataQ3.2 = aggregate(DataQ3.1[,c("mean_equally_weighted_return", "beta")], list(DataQ3.1$hunderdtiles), mean)
DataQ3.2 = DataQ3.2 %>% mutate(Quantile = ifelse(Group.1 <= 25, "1-25", ifelse(Group.1 <= 50 & Group.1 >25, "26-50" , ifelse(Group.1<=75 & Group.1 > 50, "51-75", "76-100"))))
plot = ggplot(data = DataQ3.2, aes(y = mean_equally_weighted_return, x = beta)) +
  geom_point(aes(color = Quantile)) +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  scale_colour_manual(name = "Portfolio",
                      values = c("1-25" = "dark green", "26-50" = "green", "51-75" = "light green", "76-100" = "white")) +
  scale_x_continuous(name = "60-month rolling forward beta") +
  scale_y_continuous(name = "Mean equally weighted return") +
  ggtitle("Scatterplot of average weighted return and average beta for 100 portfolios") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot

#Winsorizing
DataQ3.1$mean_equally_weighted_return = Winsorize(DataQ3.1$mean_equally_weighted_return)
DataQ3.2 = aggregate(DataQ3.1[,c("mean_equally_weighted_return", "beta")], list(DataQ3.1$hunderdtiles), mean)
DataQ3.2 = DataQ3.2 %>% mutate(Quantile = ifelse(Group.1 <= 25, "1-25", ifelse(Group.1 <= 50 & Group.1 >25, "26-50" , ifelse(Group.1<=75 & Group.1 > 50, "51-75", "76-100"))))
plot = ggplot(data = DataQ3.2, aes(y = mean_equally_weighted_return, x = beta)) +
  geom_point(aes(color = Quantile)) +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  scale_colour_manual(name = "Portfolio",
                      values = c("1-25" = "dark green", "26-50" = "green", "51-75" = "light green", "76-100" = "white")) +
  scale_x_continuous(name = "60-month rolling forward beta") +
  scale_y_continuous(name = "Mean equally weighted return") +
  ggtitle("Scatterplot of average weighted return and average beta for 100 portfolios") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot

################
## Question 4 ##
################

# Loading the data
oil_data = read.csv("WTISPLC.csv",
                    header = TRUE,
                    sep = ",")


# Preparing the data in order that it can be merged with the other beta dataset from the previous questions
oil_data = oil_data %>%
  rename(date = DATE)

oil_data = oil_data %>%
  mutate(year = substr(date,1,4)) %>%
  mutate(month = substr(date,6,7))

# Create returns
oil_data = oil_data %>%
  rename(oil_price = WTISPLC) %>%
  mutate(oil_ret = (oil_price - lag(oil_price))/ lag(oil_price)) %>%
  mutate(oil_ret = lead(oil_ret))

# Keeping only the relevant variables
oil_data = oil_data %>%
  mutate(date1 = paste(year, month, sep ="")) %>%
  select(-year, -month, -date) %>%
  rename(FF_date = date1) %>%
  mutate(FF_date = as.integer(FF_date))


# Merge the oil data set with the dataset of all beta's
DataQ4 = merge(DataQ2,
               oil_data,
               by = c("FF_date")) 

DataQ4 = DataQ4 %>%
  arrange(permno, FF_date)

DataQ4 = DataQ4 %>%
  select(-tot_mcap, -weight_equal_portfolio, -missing, -n_companies, -weight_value_portfolio, -value_portfolio_contribution, -equal_portfolio_contribution, -value_weighted_ret, -equally_weighted_ret)

summary(DataQ4)


# Arrange the data
DataQ4 = DataQ4 %>%
  arrange(permno, date)

# Count number of observations per permno
DataQ4.1 = DataQ4 %>%
  group_by(permno) %>%
  summarise(count=n())
summary(DataQ4.1)

# Merge number of permno with original data
DataQ4 = merge(DataQ4, DataQ4.1,by="permno")

# Delete observations that do not cover a 60 month window
tmp = count(distinct(DataQ4, permno)) 

DataQ4_Control= DataQ4 %>%
  summarise(permno, count) %>%
  distinct(permno,count)
sum(DataQ4_Control$count < 60)

DataQ4 = DataQ4 %>%
  filter(!count < 60)
summary(DataQ4)
tmp = count(distinct(DataQ4, permno))

rm(DataQ4_Control)
rm(DataQ4.1)

# Check if their is a difference of more than one month between the subsequent data points per permno (important for subsequent 60 month rolling window)
DataQ4 = DataQ4 %>%
  group_by(permno) %>%
  mutate(months = as.numeric(difftime(date, lag(date), units="days"))/(365.25/12))

DataQ4 = DataQ4 %>%
  mutate(months = round(months))

# Check which companies have a gap in the rolling window
summary(DataQ4$months)
tmp = DataQ4 %>%
  group_by(permno) %>%
  summarise(months) %>%
  filter(months>1) %>%
  distinct(permno)

# Keep only the data that have no gap
DataQ4 = DataQ4[!(DataQ4$permno %in% tmp$permno),]

summary(DataQ4$months)

# Which firms are suited for rolling regression
tmp = DataQ4 %>%
  summarise(permno) %>%
  distinct(permno)

DataQ4 = DataQ4 %>%
  arrange(permno, FF_date)

summary(DataQ4)
# Delete the observation where there is no oil return
DataQ4 = DataQ4 %>%
  filter(!is.na(oil_ret)) 

# Merge with FF dataframe
DataQ4 = merge(DataQ4, 
               FF_data,
               by="FF_date")


# Create new variable
DataQ4 = DataQ4 %>%
  mutate(excess_oil_return = oil_ret - RF) %>%
  mutate(excess_return = ret - RF)

DataQ4 = DataQ4 %>%
  arrange(permno, FF_date)

# Possibility 2
# # Rolling beta window 60 months
Rolling_output = roll_regres(excess_return ~ excess_oil_return, DataQ4, width = 60L)

tmp <- Rolling_output$coefs

DataQ4 = DataQ4 %>%
  mutate(beta_oil = tmp[ ,2])


# Delete all first 59 observations of each separate permno
DataQ4 = DataQ4 %>%
  mutate(ones = 1) %>%
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(n = cumsum(ones)) %>%
  select(-ones)%>%
  mutate(beta_oil = ifelse(n < 60, NA, beta_oil))

DataQ4 = DataQ4 %>%
  filter(n >= 60)


# Arrange the data
DataQ4 = DataQ4 %>%
  arrange(permno, FF_date)

summary(DataQ4)

DataQ4 = DataQ4 %>%
  filter(!is.na(beta_oil))

# Create 10 deciles
DataQ4 = DataQ4 %>%
  group_by(date) %>%
  mutate(deciles = ntile(beta_oil, 10)) 

DataQ4 = DataQ4 %>%
  arrange(FF_date, deciles, beta_oil)

DataQ4 = DataQ4 %>%
  group_by(date, deciles) %>%
  mutate(count = n())

DataQ4 = DataQ4 %>%
  arrange(FF_date, deciles, beta_oil)

# Add market beta to dataframe
DataQ4.1 = DataQ3 %>%
  select(FF_date, permno, beta)

DataQ4 =  merge(DataQ4, 
                DataQ4.1,
                by= c("FF_date", "permno"))


DataQ4 = DataQ4 %>%
  arrange(permno, FF_date)

# Create training and test data frames
train_oil = DataQ4 %>%
  filter(date < '2012-01-31')

test_oil = DataQ4 %>%
  filter(date >= '2012-01-31')

# Second stage Fama McBeth regression
lambda_oilQ4 <- train_oil %>% group_by(date) %>%
  do(reg_lambda = lm(data = ., formula = excess_return ~ beta_oil)) %>%
  mutate(lambdaQ4 = coef(reg_lambda)[2]) %>%
  select(-reg_lambda)

train_oil <- merge(train_oil, lambda_oilQ4)

## Calculating t-stat
mean(lambda_oilQ4$lambdaQ4)/(sd(lambda_oilQ4$lambdaQ4)/sqrt(nrow(lambda_oilQ4)))

FamaMcBeth <- plm::pmg(data=train_oil, excess_return ~ beta_oil, index=c("date", "permno"))

summary(FamaMcBeth)
stargazer(FamaMcBeth, type = "html", title = "Two stage Fama-McBeth regression", out = "FamaReg.htm")


################
## Question 5 ##
################

# Create value weighted return portfolio
train_oil = train_oil %>%
  arrange(date, deciles) 

summary(train_oil)
train_oil = train_oil %>%
  group_by(deciles, date) %>%
  mutate(total_mcap = sum(m_cap, na.rm = TRUE))

train_oil = train_oil %>%
  mutate(weight_value_ret = m_cap / total_mcap)

train_oil = train_oil %>%
  mutate(value_ret_contribution = weight_value_ret * ret)

# Create top decile
top_10 =  train_oil %>%
  filter(deciles == 10) %>%
  group_by(date) %>%
  summarize(top_10_returns = sum(value_ret_contribution), log_oil_price = log(mean(oil_price)))

top_10 = top_10 %>%
  arrange(date)

# Create oil index
first_obs = as.double(top_10[1,3])

top_10 = top_10 %>%
  mutate(oil2 = first_obs) %>%
  mutate(index = (log_oil_price / oil2))

# Create bottom decile
bottom_10 =  train_oil %>%
  filter(deciles == 1) %>%
  group_by(date) %>%
  summarize(bottom_10_returns = sum(value_ret_contribution))

# Merge both datasets
bottom_top_oil_return =  merge(top_10 ,
                               bottom_10,
                               by = c("date")) 
Mkt_ret_Q5$date = paste0(Mkt_ret_Q5$V1, "01")
Mkt_ret_Q5$date = as.Date(Mkt_ret_Q5$date, format = "%Y%m%d")
Mkt_ret_Q5$date = ceiling_date(Mkt_ret_Q5$date, "month") - days(1)
Mkt_ret_Q5$date = as.character(Mkt_ret_Q5$date)
bottom_top_oil_return =  merge(bottom_top_oil_return ,
                               Mkt_ret_Q5,
                               by = c("date")) 
bottom_top_oil_return = bottom_top_oil_return %>%
  select(date, index, top_10_returns, bottom_10_returns, Mkt_ret)

bottom_top_oil_return[1, c(3:4)] = 0

# Create cumulative returns
bottom_top_oil_return = bottom_top_oil_return %>%
  mutate(Mkt_ret = 1 + Mkt_ret) %>%
  mutate(top_10_returns = 1 + top_10_returns) %>%
  mutate(bottom_10_returns = 1 + bottom_10_returns) %>%
  mutate(cum_Mkt_ret = cumprod(Mkt_ret)) %>%
  mutate(cum_top_10_returns = cumprod(top_10_returns)) %>%
  mutate(cum_bottom_10_returns = cumprod(bottom_10_returns))

summary(DataQ4$beta)
train_oil = train_oil %>%
  rename(beta_oil_60 = beta_oil) %>%
  rename(beta_60 = beta)
test_oil = test_oil %>%
  rename(beta_oil_60 = beta_oil) %>%
  rename(beta_60 = beta)
train_oil = train_oil %>%
  select(-SMB, -HML, -weight_value_ret, -value_ret_contribution)
test_oil = test_oil %>%
  select(-SMB, -HML)

# Plot

DataQ5 = bottom_top_oil_return[,c(1,2,6,7,8)] %>% pivot_longer(!date, names_to = "ID")
plot = ggplot(data = DataQ5, mapping = aes(y = value, x = as.Date(date), group = ID, color = ID)) +
  geom_line() +
  scale_colour_manual(name = "Legend", breaks = c("index", "cum_Mkt_ret", "cum_top_10_returns", "cum_bottom_10_returns"),
                      values = brewer.pal(4, "Greens"),#c("index" = "white", "Mkt_ret" = "green", "cum_top_10_returns" = "dark green", "cum_bottom_10_returns" = "light green"),
                      labels = c("Oil price index", "Market return","Top 10% (high beta)", "Bottom 10% (low beta)")) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", name = "Date") +
  scale_y_continuous(name = "Cumulative return") +
  ggtitle("Cumulative return of the oil price index, a high beta portfolio and a low beta portfolio") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot

################
## Question 6 ##
################

# Loading in the data

firm_char <- data.table::fread("datashare.csv",
                               header = TRUE,
                               sep = ",") 


# Adjusting and filtering some vriables
firm_char = firm_char %>%
  rename(date = DATE)


firm_char = firm_char %>%
  filter(date > 19991231)

firm_char$date <- as.character(firm_char$date)
firm_char$date <- as.Date(firm_char$date, format="%Y%m%d")
firm_char$date <- format(as.Date(firm_char$date, format="%Y-%m-%d"),"%Y/%m")



tmp = seq()
for (i in colnames(firm_char)){
  tmp[i] = sum(is.na(firm_char[[i]]))
}
tmp



# Selecting the useful variables
firm_char = firm_char %>%
  select(date, permno, idiovol, bm, age, currat, ep, lev, quick, roic, sin, retvol, sic2, cashdebt, cash, ear, invest, grcapx)

# Constructing a variable that is reflects industries that are sensible for oil prices
firm_char = firm_char %>%
  mutate(oil_sector= ifelse(sic2 == 13, 1, 0))%>%
  mutate(air_transport_sector= ifelse( sic2 == 44 , 1, 0))%>%    #sic2 == 45 | sic2 == 40 | sic2 == 42 | sic2 == 43|
  mutate(consumer_goods= ifelse(sic2 == 50 | sic2 == 51 | sic2 == 55, 1, 0))%>%
  select( -sic2)


# Replacing missing values with the median value
firm_char = firm_char%>%
  group_by(date)%>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))


# Create seperate year and month variables
firm_char = firm_char %>%
  mutate(year = substr(date,1,4)) %>%
  mutate(month = substr(date,6,7))

firm_char = firm_char %>% 
  mutate(FF_date = paste(year, month, sep =""))

# Merge with dataframes of Q5
train_oil_firm_char = merge(train_oil,
                            firm_char,
                            by = c("FF_date", "permno")) 


test_oil_firm_char = merge(test_oil,
                           firm_char,
                           by = c("FF_date", "permno")) 



# Normal regression
lr1 = lm(beta_oil_60 ~idiovol+ bm+ age+ currat+ ep+ lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector,
         data = train_oil_firm_char)
summary(lr1)

# Also did this forward stepwise regression to see which variables aree important  
regfit.full = regsubsets(beta_oil_60~idiovol+ bm+ age+ currat+ ep+ lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector
                         , train_oil_firm_char , nvmax = 10, method= "forward")
reg.summary = summary(regfit.full)
reg.summary


# Tree
tree.oil_6 = tree(beta_oil_60 ~idiovol+ bm+ age+ currat+ ep+ lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector, data = train_oil_firm_char)
plot(tree.oil_6)
text(tree.oil_6, pretty = F, cex=0.75)

## Willem
m1 <- rpart(
  formula = beta_oil_60 ~idiovol+ bm+ age+ currat+ ep+ lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector,
  data    = train_oil_firm_char
)

fancyRpartPlot(m1,type = 4, sub = "", palettes = "Greens", digits = -3, main = "Regression tree predicting Oil Beta")
## Results

test_oil_firm_char$pred = predict(tree.oil_6, test_oil_firm_char)

MSE_tst = mean((test_oil_firm_char$beta_oil_60 - test_oil_firm_char$pred)^2)
MSE_tst

test_oil_firm_char = test_oil_firm_char%>%
  select(-pred)

#pruning is not possible but we va still predict and test on the validation set

#set.seed(2)
#cv.tree.oil_6 = cv.tree(tree.oil_6, FUN = prune.misclass)
## Should we do more here?
#par(mfrow=c(1,2))
## size:= # terminal nodes
#plot(cv.tree.oil_6$size, cv.tree.oil_6$dev, type= "b", ylab= "CV Err Rate", xlab= "Terminal Nodes")
## Terminal Nodes are already 5, so first tree is the best one!

################
## Question 7 ##
################

# We will estimate on oil_sector, idiovol, lev, age, retvol, cash, grcapx 
train_oil_firm_char_7 = train_oil_firm_char %>%
  filter(deciles == 1 | deciles == 10)%>%
  mutate(decile_ten_one= ifelse(deciles == 1, "Yes", "No")) %>%
  mutate(decile_ten_one= factor(decile_ten_one, levels = c("Yes", "No")))
#tried this because the bagging model didn't work but even without data it does not work 

test_oil_firm_char_7 = test_oil_firm_char%>%
  filter(deciles == 1 | deciles == 10)%>%
  mutate(decile_ten_one= ifelse(deciles == 1, "Yes", "No")) %>%
  mutate(decile_ten_one= factor(decile_ten_one, levels = c("Yes", "No")))
# Tried this because the bagging model didn't work but even without data it does not work 


################
## Classic tree
################

## training sample
tree.oil_7_rpart= rpart(decile_ten_one ~ idiovol+ bm+ age+ currat+ ep+ lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector
                 ,train_oil_firm_char_7 )
tree.oil_7= tree(decile_ten_one ~ idiovol+ bm+ age+ currat+ ep+ lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector
                        ,train_oil_firm_char_7 )

df <- data.frame(imp = tree.oil_7_rpart$variable.importance)
imp <- df %>% 
  rownames_to_column() %>% 
  rename("variable" = rowname) %>% 
  arrange(imp) %>%
  mutate(variable = forcats::fct_inorder(variable))

#Variable importacne plot
plot = ggplot(imp) +
  geom_col(aes(x = variable, y = imp), show.legend = F, fill = "dark green") +
  scale_y_continuous(name = "Importance") +
  scale_x_discrete(name = "Variable") +
  scale_fill_manual(values = "Green") +
  ggtitle("Tree") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
plot

plot(tree.oil_7)
text(tree.oil_7, pretty = F, cex=0.75)

## Predict
tree.pred = predict(tree.oil_7, test_oil_firm_char_7 , type= "class")
test_oil_firm_char_7$pred_tree = predict(tree.oil_7, test_oil_firm_char_7 , type= "class")
## Results
out = table(tree.pred, test_oil_firm_char_7$decile_ten_one)
out


hitRate_clasic_tree = sum(diag(out))/nrow(test_oil_firm_char_7)
hitRate_clasic_tree

test_oil_firm_char_7 = test_oil_firm_char_7 %>%
  mutate(Err_tree= ifelse(pred_tree != decile_ten_one, 1, 0))

cat("OOS misclassification error: ", mean(test_oil_firm_char_7$Err_tree))  


## Can we do better with CV? 
set.seed(2)
cv.tree.oil_7 = cv.tree(tree.oil_7, FUN = prune.misclass)

## Should we do more here?
par(mfrow=c(1,2))
## size:= # terminal nodes 
plot(cv.tree.oil_7$size, cv.tree.oil_7$dev, type= "b", ylab= "CV Err Rate", xlab= "Terminal Nodes")
abline(v=9, col= "blue")
## k:= alpha 
plot(cv.tree.oil_7$k, cv.tree.oil_7$dev, type= "b", ylab= "CV Err Rate", xlab= "Alpha")

## prune tree to 5 nodes and alpha = 0
prune.tree.oil_7= prune.misclass(tree.oil_7, best= 5)
plot(prune.tree.oil_7)
text(prune.tree.oil_7, pretty = F, cex=0.75)

## Error rate is same as calculated above!
## AMount of FALSONO = 5286 of the total of 12737 NO

################
## Bagging
################

model_b = bagging(formula = decile_ten_one ~  idiovol+ bm+ age+ currat+ ep+lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector,
                  data    =  train_oil_firm_char_7,
                  coob    = TRUE,
                  nbagg   = 200)

print(model_b)

imp = varImp(model_b)
imp = as.data.frame(imp)
imp$varnames = rownames(imp)
rownames(imp) = NULL

#Variable importacne plot
plot = ggplot(imp, aes(x=reorder(varnames, Overall), y = Overall)) + 
  geom_bar(stat = "identity", fill = "dark green") +
  ylab("Importance") +
  xlab("Variable") + 
  ggtitle("Bagging") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
plot

## 
test_oil_firm_char_7$pred_b = predict(model_b, 
                                      newdata= test_oil_firm_char_7,
                                      type= "class")

## Merge with actual 
test_oil_firm_char_7 = test_oil_firm_char_7 %>%
  mutate(Err_b= ifelse(pred_b != decile_ten_one, 1, 0))

cat("OOS misclassification error: ", mean(test_oil_firm_char_7$Err_b))  


################
## Random Forest
################

model_rf = randomForest(formula = decile_ten_one ~ idiovol+ bm+ age+ currat+ ep+lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector, 
                        data    = train_oil_firm_char_7,
                        mtry    = floor(sqrt(ncol(train_oil_firm_char_7))),
                        ntree   = 200)

print(model_rf)

imp = varImpPlot(model_rf, type = 2)
imp = as.data.frame(imp)
imp$varnames = rownames(imp)
rownames(imp) = NULL

plot = ggplot(imp, aes(x=reorder(varnames, MeanDecreaseGini), y = MeanDecreaseGini)) + 
  geom_bar(stat = "identity", fill = "dark green") +
  scale_fill_discrete(name="Variable Group") +
  ylab("Importance") +
  xlab("Variable") + 
  ggtitle("Random Forest") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
plot

## Predictions
test_oil_firm_char_7$pred_rf = predict(model_rf, 
                                       newdata = test_oil_firm_char_7,
                                       type= "class")

## Merge with actual 
test_oil_firm_char_7 = test_oil_firm_char_7 %>%
  mutate(Err_rf = ifelse(pred_rf != decile_ten_one, 1, 0))

cat("OOS misclassification error: ", mean(test_oil_firm_char_7$Err_rf))  


################
## Lasso
################

############ substituted the lasso with a elastic net regression because it chooses automaticcaly between lasso and ridge

trn.Cnt  = trainControl(
  method = "cv", 
  number = 5,
  verboseIter = TRUE # Print training log
) # Define the train object 

set.seed(1)
model_lasso <- train(
  decile_ten_one ~ idiovol+ bm+ age+ currat+ ep+lev+ quick+ roic+ sin+ retvol+ cashdebt+ cash+ ear+ invest+ grcapx + oil_sector +air_transport_sector, 
  train_oil_firm_char_7,
  method = "glmnet",
  tuneLength = 10,
  trControl= trn.Cnt   
)

imp = varImp(model_lasso, lambda = model_lasso$lambda.min)
imp = as.data.frame(imp["importance"])
imp$varnames = rownames(imp)
rownames(imp) = NULL

# Variable importance plot
ggplot(imp, aes(x=reorder(varnames, Overall), y = Overall)) + 
  geom_bar(stat = "identity", fill = "dark green") +
  scale_fill_discrete(name="Variable Group") +
  ylab("Importance") +
  xlab("Variable Name") + 
  ggtitle("Lasso") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

model_lasso # Print model 
test_oil_firm_char_7$pred_lasso = predict(model_lasso, test_oil_firm_char_7)

test_oil_firm_char_7 = test_oil_firm_char_7 %>%
  mutate(Err_lasso= ifelse(pred_lasso != decile_ten_one, 1, 0))


cat("OOS misclassification error: ", mean(test_oil_firm_char_7$Err_lasso))  

## Let's calculate the true Yes, true No, False Yes, False No. Decile 1 = YES = 1
# Classical Tree
test_oil_firm_char_7$decile_ten_one = as.integer(test_oil_firm_char_7$decile_ten_one)
test_oil_firm_char_7$pred_tree = as.integer(test_oil_firm_char_7$pred_tree)
test_oil_firm_char_7$pred_b = as.integer(test_oil_firm_char_7$pred_b)
test_oil_firm_char_7$pred_rf = as.integer(test_oil_firm_char_7$pred_rf)
test_oil_firm_char_7$pred_lasso = as.integer(test_oil_firm_char_7$pred_lasso)

test_oil_firm_char_FALSEYES = test_oil_firm_char_7 %>%
  filter(decile_ten_one == 1) %>%
  mutate(FALSEYES_tree = ifelse(decile_ten_one != pred_tree, 1, 0)) %>%
  mutate(FALSEYES_b = ifelse(decile_ten_one != pred_b, 1, 0)) %>%
  mutate(FALSEYES_rf = ifelse(decile_ten_one != pred_rf, 1, 0)) %>%
  mutate(FALSEYES_lasso = ifelse(decile_ten_one != pred_lasso, 1, 0))

sum(test_oil_firm_char_FALSEYES$FALSEYES_tree)
sum(test_oil_firm_char_FALSEYES$FALSEYES_b)
sum(test_oil_firm_char_FALSEYES$FALSEYES_rf)
sum(test_oil_firm_char_FALSEYES$FALSEYES_lasso)

## Do the same for FALSE No
test_oil_firm_char_FALSENO = test_oil_firm_char_7 %>%
  filter(decile_ten_one == 2) %>%
  mutate(FALSENO_tree = ifelse(decile_ten_one != pred_tree, 1, 0)) %>%
  mutate(FALSENO_b = ifelse(decile_ten_one != pred_b, 1, 0)) %>%
  mutate(FALSENO_rf = ifelse(decile_ten_one != pred_rf, 1, 0)) %>%
  mutate(FALSENO_lasso = ifelse(decile_ten_one != pred_lasso, 1, 0))

sum(test_oil_firm_char_FALSENO$FALSENO_tree)
sum(test_oil_firm_char_FALSENO$FALSENO_b)
sum(test_oil_firm_char_FALSENO$FALSENO_rf)
sum(test_oil_firm_char_FALSENO$FALSENO_lasso)

################
## Question 8 ##
################

##### Want to try a lasso and ridge and random forest
# Filtering out the right periods needs to be done first and it think we should fit a lasso.

test_oil_firm_char = test_oil_firm_char %>%
  arrange(permno, FF_date)

train_oil_firm_char_8 = train_oil_firm_char %>%
  filter( "200806" <= FF_date)%>%
  filter(FF_date <= "200901") 

tmp = test_oil_firm_char%>%
  filter( "201406" <= FF_date)%>%
  filter(FF_date <= "201501") 


train_oil_firm_char_8 = bind_rows(train_oil_firm_char_8, tmp)

train_oil_firm_char_8 =train_oil_firm_char_8%>%
  arrange(FF_date)

test_oil_firm_char_8 = test_oil_firm_char %>%
  filter( "201506" <= FF_date)%>%
  filter(FF_date <= "201602") 

################
## Lasso
################

lambda <- 10^seq(-3, 3, length = 140)

trn.Cnt  = trainControl(
  method = "cv", 
  number = 5,
  verboseIter = TRUE # Print training log
) # Define the train object 

set.seed(1)
model_lasso <- train(
  ret ~ beta_oil_60 +  beta_60 + bm + oil_sector + idiovol + age +lev + cash + air_transport_sector, 
  train_oil_firm_char_8,
  method = "glmnet",
  tuneLength = 25,
  trControl= trn.Cnt,
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

test_oil_firm_char_8$pred_lasso = predict(model_lasso, test_oil_firm_char_8)

# Test returns lasso
test_oil_firm_char_8_tmp = test_oil_firm_char_8 %>%
  mutate(deciles_pred_lasso = ntile(pred_lasso, 10))%>%
  filter(deciles_pred_lasso == 10)%>%
  group_by(FF_date)%>%
  mutate(tot_mcap = sum(m_cap))%>%
  mutate(weight = m_cap / tot_mcap) %>%
  mutate(ret_contribution = weight * ret)%>%
  mutate(oilbeta_contribution = weight * beta_oil_60) %>%
  mutate(beta_contribution = weight * beta_60) %>%
  group_by(FF_date)%>%
  mutate(value_weighted_ret_decile = sum(ret_contribution))%>%
  mutate(value_weighted_oilbeta_decile = sum(oilbeta_contribution)) %>%
  mutate(value_weighted_beta_decile = sum(beta_contribution)) %>%
  select(-ret_contribution, -oilbeta_contribution, -beta_contribution)

test_oil_firm_char_8_tmp = test_oil_firm_char_8_tmp %>%
  arrange(FF_date)

result_lasso_8 = test_oil_firm_char_8_tmp%>%
  group_by(date_for_plot)%>%
  summarise(ret_oil_hedge_lasso = mean(value_weighted_ret_decile), ret_market = mean(Mkt_ret), beta_oil_hedge_lasso = mean(value_weighted_oilbeta_decile), beta_hedge_lasso = mean(value_weighted_beta_decile))

################
## Ridge
################

set.seed(1)
model_ridge <- train(
  ret ~ beta_oil_60 +  beta_60 + bm + oil_sector + idiovol + age +lev + cash + air_transport_sector, 
  train_oil_firm_char_8,
  method = "glmnet",
  tuneLength = 25,
  trControl= trn.Cnt,
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

test_oil_firm_char_8$pred_ridge = predict(model_ridge, test_oil_firm_char_8)

# Test returns ridge
test_oil_firm_char_8_tmp = test_oil_firm_char_8 %>%
  mutate(deciles_pred_ridge = ntile(pred_ridge, 10))%>%
  filter(deciles_pred_ridge == 10)%>%
  group_by(FF_date)%>%
  mutate(tot_mcap = sum(m_cap))%>%
  mutate(weight = m_cap / tot_mcap) %>%
  mutate(ret_contribution = weight * ret)%>%
  mutate(oilbeta_contribution = weight * beta_oil_60) %>%
  mutate(beta_contribution = weight * beta_60) %>%
  group_by(FF_date)%>%
  mutate(value_weighted_ret_decile = sum(ret_contribution))%>%
  mutate(value_weighted_oilbeta_decile = sum(oilbeta_contribution)) %>%
  mutate(value_weighted_beta_decile = sum(beta_contribution)) %>%
  select(-ret_contribution)

test_oil_firm_char_8_tmp = test_oil_firm_char_8_tmp %>%
  arrange(FF_date)

result_ridge_8  = test_oil_firm_char_8_tmp%>%
  group_by(date_for_plot)%>%
  summarise(ret_oil_hedge_ridge = mean(value_weighted_ret_decile), oilbeta_hedge_ridge = mean(value_weighted_oilbeta_decile), beta_hedge_ridge = mean(value_weighted_beta_decile))

################
## Elastic net regression
################

set.seed(1)
model_enr <- train(
  ret ~ beta_oil_60 +  beta_60 + bm + oil_sector + idiovol + age +lev + cash + air_transport_sector, 
  train_oil_firm_char_8,
  method = "glmnet",
  tuneLength = 25,
  trControl= trn.Cnt
)

test_oil_firm_char_8$pred_enr = predict(model_enr, test_oil_firm_char_8)

# Test returns enr
test_oil_firm_char_8_tmp = test_oil_firm_char_8 %>%
  mutate(deciles_pred_enr = ntile(pred_enr, 10))%>%
  filter(deciles_pred_enr == 10)%>%
  group_by(FF_date)%>%
  mutate(tot_mcap = sum(m_cap))%>%
  mutate(weight = m_cap / tot_mcap) %>%
  mutate(ret_contribution = weight * ret)%>%
  mutate(oilbeta_contribution = weight * beta_oil_60) %>%
  mutate(beta_contribution = weight * beta_60) %>%
  group_by(FF_date) %>%
  mutate(value_weighted_ret_decile = sum(ret_contribution))%>%
  mutate(value_weighted_oilbeta_decile = sum(oilbeta_contribution)) %>%
  mutate(value_weighted_beta_decile = sum(beta_contribution)) %>%
  select(-ret_contribution, -oilbeta_contribution, -beta_contribution)

test_oil_firm_char_8_ridge = test_oil_firm_char_8_tmp %>%
  arrange(FF_date)

result_enr_8 = test_oil_firm_char_8_tmp%>%
  group_by(date_for_plot)%>%
  summarise(ret_oil_hedge_enr = mean(value_weighted_ret_decile), beta_oil_hedge_enr = mean(value_weighted_oilbeta_decile), beta_hedge_enr = mean(value_weighted_beta_decile))

################
## Random forest
################

model_rf = randomForest(formula = ret ~ beta_oil_60 +  beta_60 + bm + oil_sector + idiovol + age +lev + cash + air_transport_sector, 
                        data    = train_oil_firm_char_8,
                        mtry    = floor(sqrt(ncol(train_oil_firm_char_8))),
                        ntree   = 100)

print(model_rf)

## Predictions
test_oil_firm_char_8$pred_rf = predict(model_rf, 
                                       newdata = test_oil_firm_char_8,
                                       type= "class")

# Test returns random forrest
test_oil_firm_char_8_tmp = test_oil_firm_char_8 %>%
  mutate(deciles_pred_rf = ntile(pred_rf, 10))%>%
  filter(deciles_pred_rf == 10)%>%
  group_by(FF_date)%>%
  mutate(tot_mcap = sum(m_cap))%>%
  mutate(weight = m_cap / tot_mcap) %>%
  mutate(ret_contribution = weight * ret)%>%
  mutate(oilbeta_contribution = weight * beta_oil_60) %>%
  mutate(beta_contribution = weight * beta_60) %>%
  group_by(FF_date)%>%
  mutate(value_weighted_ret_decile = sum(ret_contribution))%>%
  mutate(value_weighted_oilbeta_decile = sum(oilbeta_contribution)) %>%
  mutate(value_weighted_beta_decile = sum(beta_contribution)) %>%
  select(-ret_contribution)

test_oil_firm_char_8_tmp = test_oil_firm_char_8_tmp %>%
  arrange(FF_date)

result_random_forest_8= test_oil_firm_char_8_tmp%>%
  group_by(date_for_plot)%>%
  summarise(ret_oil_hedge_random_forest = mean(value_weighted_ret_decile), oil_ret = mean(oil_ret), oilbeta_hedge_rf = mean(value_weighted_oilbeta_decile), beta_hedge_rf = mean(value_weighted_beta_decile))

result_8 = merge(result_random_forest_8,
                 result_lasso_8,
                 by = c("date_for_plot"))

result_8 = merge(result_8,
                 result_enr_8 ,
                 by = c("date_for_plot"))

result_8 = merge(result_8,
                 result_ridge_8 ,
                 by = c("date_for_plot"))

result_8 = result_8 %>%
  mutate(ret_market = ret_market)%>%
  mutate(cum_market_ret = cumprod(ret_market))%>%
  
  mutate(ret_oil_hedge_random_forest = ret_oil_hedge_random_forest +1)%>%
  mutate(cum_ret_oil_hedge_random_forest = cumprod(ret_oil_hedge_random_forest))%>%
  
  mutate(ret_oil_hedge_enr = ret_oil_hedge_enr +1)%>%
  mutate(cum_ret_oil_hedge_enr = cumprod(ret_oil_hedge_enr))%>%
  
  mutate(ret_oil_hedge_ridge = ret_oil_hedge_ridge +1)%>%
  mutate(cum_ret_oil_hedge_ridge = cumprod(ret_oil_hedge_ridge))%>%
  
  mutate(ret_oil_hedge_lasso = ret_oil_hedge_lasso +1)%>%
  mutate(cum_ret_oil_hedge_lasso = cumprod(ret_oil_hedge_lasso))%>%
  
  mutate(oil_ret = oil_ret +1)%>%
  mutate(cum_oil_ret = cumprod(oil_ret))


## Let's summarize the data per model
mean = sapply(result_8, mean)
sd = sapply(result_8, sd)
min = sapply(result_8, min)
max = sapply(result_8, max)
descriptives = rbind(mean, sd, min, max)

result_8 = result_8 %>%
  arrange(date_for_plot)

fix = c("2015-05-31", as.double(rep(1, 12)))
result_8 = rbind(fix, result_8)
result_8$date_for_plot = as.Date(result_8$date_for_plot)

lines = c("cum_oil_ret","ret_market", "cum_ret_oil_hedge_random_forest", "cum_ret_oil_hedge_enr", "cum_ret_oil_hedge_ridge", "cum_ret_oil_hedge_lasso")
result_8 = pivot_longer(result_8[,c("date_for_plot", lines)], !date_for_plot, names_to = "ID")
result_8$value = as.double(result_8$value)

plot = ggplot(data = result_8, mapping = aes(y = value, x = as.Date(date_for_plot), group = ID, color = ID)) +
  geom_line() +
  scale_colour_manual(name = "Oil hedge model", breaks = lines,
                      values = brewer.pal(6, "Greens"),
                      labels = c("cum_oil_ret" = "Return on oil", "ret_market" = "return on market", "cum_ret_oil_hedge_random_forest" = "Rondom Forest", "cum_ret_oil_hedge_enr" ="Elastic net regression", "cum_ret_oil_hedge_ridge" = "Ridge", "cum_ret_oil_hedge_lasso" = "Lasso")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", name = "Date") +
  scale_y_continuous(name = "Cumulative return") +
  ggtitle("Performance of oil hedging portfolios based on different prediction methods") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot

################
## Question 9
################

# the result from the elastic net regression method from 2016 onwards

data_9 = test_oil_firm_char %>%
  filter( "201601" <= FF_date)


####### Use elastic net regression
data_9$pred = predict(model_enr, data_9)

model_returns = data_9 %>%
  mutate(deciles_pred = ntile(pred, 10))%>%
  filter(deciles_pred == 10)%>%
  group_by(FF_date)%>%
  mutate(tot_mcap = sum(m_cap))%>%
  mutate(weight = m_cap / tot_mcap) %>%
  mutate(ret_contribution = weight * ret)%>%
  group_by(FF_date)%>%
  mutate(value_weighted_ret_decile = sum(ret_contribution))%>%
  select(-ret_contribution)

model_returns = model_returns %>%
  arrange(FF_date)

model_returns = model_returns%>%
  group_by(date_for_plot)%>%
  summarise(return_hedge_portfolio = mean(value_weighted_ret_decile), market_return = mean(Mkt_ret))

decile_1 = data_9 %>%
  filter(deciles == 1)%>%
  group_by(FF_date)%>%
  mutate(tot_mcap = sum(m_cap))%>%
  mutate(weight = m_cap / tot_mcap) %>%
  mutate(ret_contribution = weight * ret)%>%
  group_by(FF_date)%>%
  mutate(value_weighted_ret_decile = sum(ret_contribution))%>%
  select(-ret_contribution)

decile_1 = decile_1 %>%
  arrange(FF_date)

decile_1 = decile_1%>%
  group_by(date_for_plot)%>%
  summarise(decile_1_oil_betas = mean(value_weighted_ret_decile), oil_ret = mean(oil_ret))

result_9 = merge(decile_1,
                 model_returns,
                 by = c("date_for_plot"))

result_9 =result_9 %>%
  mutate(cum_market_ret = cumprod(market_return))%>%
  mutate(oil_ret = oil_ret +1)%>%
  mutate(cum_oil_ret = cumprod(oil_ret))%>%
  mutate(return_hedge_portfolio = return_hedge_portfolio +1)%>%
  mutate(cum_ret_hedge_portfolio = cumprod(return_hedge_portfolio ))%>%
  mutate(decile_1_oil_betas = decile_1_oil_betas +1)%>%
  mutate(cum_decile_1_oil_betas = cumprod(decile_1_oil_betas ))

result_9 = result_9[,c("date_for_plot", "cum_ret_hedge_portfolio", "cum_market_ret", "cum_oil_ret", "cum_decile_1_oil_betas")]
result_9 = pivot_longer(result_9, !date_for_plot, names_to = "ID")
plot =ggplot(data = result_9, mapping = aes(y = value, x = as.Date(date_for_plot), group = ID, color = ID)) +
  geom_line() +
  scale_colour_manual(name = "Oil hedge model",
                      breaks = c("cum_ret_hedge_portfolio", "cum_decile_1_oil_betas", "cum_market_ret", "cum_oil_ret"),
                      values = c(brewer.pal(4, "Greens")),
                      labels = c("cum_oil_ret" = "Return on oil", "cum_ret_hedge_portfolio" = "Return on hedged portfolio", "cum_market_ret" ="Return on market", "cum_decile_1_oil_betas" = "Return on bottom 10% (low beta)")) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", name = "Date") +
  scale_y_continuous(name = "Cumulative return") +
  ggtitle("Performance of oil hedging portfolios based on different prediction methods") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot

################
## Question 10
################

test_oil = test_oil %>%
  group_by(FF_date)%>%
  mutate(tot_mcap = sum(m_cap))%>%
  mutate(pasive_weight = m_cap / tot_mcap)

test_oil = test_oil %>%
  mutate(reverse_oil_beta = -beta_oil_60)

theta = 1.25
#N has to be defined by month we guess

test_oil = test_oil %>%
  group_by(FF_date)%>%
  mutate(standardized_reverse_oil_beta = (reverse_oil_beta - mean(reverse_oil_beta))/ sd(reverse_oil_beta))

test_oil = test_oil %>%
  group_by(FF_date)%>% 
  mutate(active_weight = (theta/ n()) * standardized_reverse_oil_beta )%>%
  mutate(total_weight = pasive_weight + active_weight )

test_oil = test_oil %>%
  mutate(ret_contribution = total_weight * ret)%>%
  group_by(FF_date) %>%
  mutate(ret_with_active_part= sum(ret_contribution))%>%
  select(-ret_contribution)

result_10 = test_oil %>%
  group_by(FF_date) %>%
  summarize(market_ret  = mean(Mkt_ret), ret_with_active_part  = mean(ret_with_active_part), oil_ret = mean(oil_ret))

result_10 = result_10 %>%
  rename(date = FF_date)%>%
  filter( "201601" <= date)

result_10 =result_10 %>%
  mutate(cum_market_ret = cumprod(market_ret))%>%
  mutate(oil_ret = oil_ret +1)%>%
  mutate(cum_oil_ret = cumprod(oil_ret))%>%
  mutate(ret_with_active_part = ret_with_active_part +1)%>%
  mutate(cum_ret_with_active_part = cumprod(ret_with_active_part ))

result_10$date = paste0(result_10$date, "01")
result_10$date = as.Date(result_10$date, "%Y%m%d")
result_10$date = ceiling_date(result_10$date, "month") - days(1)

result_10 = result_10[, c("date","cum_ret_with_active_part", "cum_market_ret", "cum_oil_ret")]
result_10 = pivot_longer(result_10, !date, names_to = "ID")

plot = ggplot(data = result_10, mapping = aes(y = value, x = as.Date(date), group = ID, color = ID)) +
  geom_line() +
  scale_colour_manual(name = "Legend",
                      breaks = c("cum_ret_with_active_part", "cum_market_ret", "cum_oil_ret"),
                      values = c(brewer.pal(3, "Greens")),
                      labels = c("cum_oil_ret" = "Return on oil", "cum_market_ret" ="Return on market", "cum_ret_with_active_part" = "Optimal portfolio")) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", name = "Date") +
  scale_y_continuous(name = "Cumulative return") +
  ggtitle("Performance of oil hedging portfolios based on different prediction methods") +
  dark_theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))
plot

#Econometrics Research Paper
#Implements a VAR model trying to undestand the relatio between
#portuguse sovereing yield and the evolution/perfomance of
#the Banking sector. The main portuguese banks are

#1.BPI
#2.BES
#3.BCP
#IDEA: limit the sample to the date of BES bankruptcy
library("vars")
library('ggplot2')
library("dynlm")
library("DataCombine")
library(stargazer)
library(dyn)



#Load the data series

BPI <- read.csv("Data/BPI.csv")
BES <- read.csv("Data/BES.csv")
BCP <- read.csv("Data/BCP.csv")
YIELDS <- read.csv("Data/Yields_quotes.csv")



#Manipulating the data series (for BPI)
colnames(BPI)[2] <- "Open_BPI"
colnames(BPI)[3] <- "High_BPI"
colnames(BPI)[4] <- "Low_BPI"
colnames(BPI)[5] <- "Close_BPI"
colnames(BPI)[6] <- "Volume_BPI"
colnames(BPI)[7] <- "AdjClose_BPI"
colnames(BPI)[8] <- "Var_BPI"
colnames(BPI)[9] <- "Lag1_BPI"
colnames(BPI)[10] <- "Lag2_BPI"
colnames(BPI)[11] <- "Lag3_BPI"
colnames(BPI)[12] <- "Lag4_BPI"



BPI$BPI_traded_volume <- BPI$Close_BPI * BPI$Volume_BPI

#Manipulating the data series (for BES)
colnames(BES)[2] <- "Open_BES"
colnames(BES)[3] <- "High_BES"
colnames(BES)[4] <- "Low_BES"
colnames(BES)[5] <- "Close_BES"
colnames(BES)[6] <- "Volume_BES"
colnames(BES)[7] <- "AdjClose_BES"
colnames(BES)[8] <- "Var_BES"
colnames(BES)[9] <- "Lag1_BES"
colnames(BES)[10] <- "Lag2_BES"
colnames(BES)[11] <- "Lag3_BES"
colnames(BES)[12] <- "Lag4_BES"
BES$BES_traded_volume <- BES$Close_BES * BES$Volume_BES

#Manipulating the data series (for BCP)
colnames(BCP)[2] <- "Open_BCP"
colnames(BCP)[3] <- "High_BCP"
colnames(BCP)[4] <- "Low_BCP"
colnames(BCP)[5] <- "Close_BCP"
colnames(BCP)[6] <- "Volume_BCP"
colnames(BCP)[7] <- "AdjClose_BCP"
colnames(BCP)[8] <- "Var_BCP"
colnames(BCP)[9] <- "Lag1_BCP"
colnames(BCP)[10] <- "Lag2_BCP"
colnames(BCP)[11] <- "Lag3_BCP"
colnames(BCP)[12] <- "Lag4_BCP"



#Merging the four files
Banks_quotes <- merge(x = BPI, y = BES, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = BCP, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = YIELDS, y = Banks_quotes, by = "Date", all.y = TRUE)



#Decomposing the variations into positive and negative for one of the models
Banks_quotes$BCP_Positive = ifelse(Banks_quotes$Var_BCP > 0, 0, Banks_quotes$Var_BCP)
Banks_quotes$BCP_Negative = ifelse(Banks_quotes$Var_BCP < 0, 0, Banks_quotes$Var_BCP)

Banks_quotes$BPI_Positive = ifelse(Banks_quotes$Var_BPI > 0, 0, Banks_quotes$Var_BPI)
Banks_quotes$BPI_Negative = ifelse(Banks_quotes$Var_BPI < 0, 0, Banks_quotes$Var_BPI)

Banks_quotes$BES_Positive = ifelse(Banks_quotes$Var_BES > 0, 0, Banks_quotes$Var_BES)
Banks_quotes$BES_Negative = ifelse(Banks_quotes$Var_BES < 0, 0, Banks_quotes$Var_BES)

Banks_quotes <-Banks_quotes[order(Banks_quotes$Numeric_date),]
Banks_quotes <- Banks_quotes[2:3911,]
Banks_quotes <- Banks_quotes[complete.cases(Banks_quotes),]


#Subset a dataset for the VAR Model
VAR_dataset <- Banks_quotes[,c("PT_var", "Var_BES","Var_BPI","Var_BCP")]
VAR_dataset <- VAR_dataset[2:3795,]
VAR_dataset <- VAR_dataset[complete.cases(VAR_dataset),]


#BaselineModel
BaseLineModel <- lm(Banks_quotes$PT_var ~ Banks_quotes$Var_BCP + Banks_quotes$Var_BES + Banks_quotes$Var_BPI)

summary(BaseLineModel)

stargazer(BaseLineModel, title="Results", align=TRUE)

cov <- vcovHC(BaseLineModel, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(BaseLineModel, BaseLineModel, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)

#Model with different coefficients for positive and negative variations
Different_coefficients <- lm(Banks_quotes$PT_var ~ Banks_quotes$BCP_Negative + Banks_quotes$BCP_Positive + Banks_quotes$BPI_Negative +
                     Banks_quotes$BPI_Positive + Banks_quotes$BES_Negative +
                     Banks_quotes$BES_Positive)

summary(Different_coefficients)

stargazer(Different_coefficients, title="Results", align=TRUE)

cov <- vcovHC(Different_coefficients, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(Different_coefficients, Different_coefficients, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)



#ModelWithLags
Model_with_lags <- lm(Banks_quotes$PT_var ~ Banks_quotes$Lag1_BCP + Banks_quotes$Lag2_BCP+
                        Banks_quotes$Lag1_BES + Banks_quotes$Lag2_BES +
                        Banks_quotes$Lag1_BPI + Banks_quotes$Lag2_BPI)



cov <- vcovHC(Model_with_lags, type = "HC")
robust.se <- sqrt(diag(cov))

stargazer(Model_with_lags, Model_with_lags, se=list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE)

#For the trading simulation

#Initializing the variables and dataframes
sum_profit <- 0
sum_short <- 0
sum_long <- 0
profit <- 0
profit_average <- 0
cumulative_profit <- data.frame()
cumulative_short <- data.frame()
cumulative_long <- data.frame()
average_profit <- data.frame()
period_profit <- data.frame()

for(i in 50:3532)
{
  #slice the data into a train period and estimates the model
  train <- Banks_quotes[1:i,]
  fit <- lm(train$PT_var ~ train$Lag1_BCP + train$Lag2_BCP+
                               train$Lag1_BES + train$Lag2_BES +
                               train$Lag1_BPI + train$Lag2_BPI)
  
  #runs the model and computes the errors
  prediction <- summary(fit)$coefficients[1] + summary(fit)$coefficients[2] * Banks_quotes$Lag1_BCP[i+1] +
    summary(fit)$coefficients[3] *Banks_quotes$Lag2_BCP[i+1] + 
    summary(fit)$coefficients[4] *Banks_quotes$Lag1_BES[i+1] +
    summary(fit)$coefficients[5] *Banks_quotes$Lag2_BES[i+1] +
    summary(fit)$coefficients[6] *Banks_quotes$Lag1_BPI[i+1] +
    summary(fit)$coefficients[7] *Banks_quotes$Lag2_BPI[i+1]
  
  
  actual <- Banks_quotes$PT_var[i + 1]
  profit <- ifelse(prediction > 0, actual, -actual)
  sum_profit <- sum_profit + profit
  sum_short <- sum_short - actual
  sum_long <- sum_long + actual
  profit_average <- sum_profit/ (i-1499)
  
  #saves the results into a dataframe
  cumulative_profit <- rbind(cumulative_profit, sum_profit)
  cumulative_short <- rbind(cumulative_short, sum_short)
  cumulative_long <- rbind(cumulative_long, sum_long)
  average_profit <- rbind(average_profit, profit_average)
  period_profit <- rbind(period_profit, profit)
  
}


Chart_data <- data.frame()

Chart_data$Model_Profit <- cumulative_profit[1]


ts.plot(cumulative_profit)
ts.plot(cumulative_short)
ts.plot(cumulative_long)


#VAR Model

VARselect(VAR_dataset, lag.max = 8, type = "both")
p1ct <- VAR(VAR_dataset, p = 4, type = "both")

p1ct.irf <- irf(p1ct, response = "PT_var", n.ahead = 28, cumulative = TRUE, boot = TRUE)
plot(p1ct.irf)


#Econometrics Research Paper
#Implements a VAR model trying to undestand the relatio between
#portuguse sovereing yield and the evolution/perfomance of
#the Banking sector. The main portuguese banks are
#1.BPI
#2.BES
#3.BCP
#4.BANIF

library("vars")
library('ggplot2')
library("dynlm")
library("DataCombine")


#Load the data series

BPI <- read.csv("Data/BPI_quotes.csv")
BES <- read.csv("Data/BES_quotes.csv")
BCP <- read.csv("Data/BCP_quotes.csv")
BANIF <- read.csv("Data/BANIF_quotes.csv")
EDP <- read.csv("Data/EDP_quotes.csv")
JM <- read.csv("Data/JM_quotes.csv")
GALP <- read.csv("Data/GALP_quotes.csv")
YIELDS <- read.csv("Data/Yields_quotes.csv")
STOXX <- read.csv("Data/STOXX_quotes.csv")
STOXXGENERAL <- read.csv("Data/STOXXGENERAL_quotes.csv")

#Manipulating the data series (for Yields)
YIELDS <- YIELDS[order(YIELDS$Date),]
YIELDS$PT_var <- c(NA, diff(YIELDS$PT)/ YIELDS$PT[-1])

#Manipulating the data series (for BPI)
BPI <- BPI[order(BPI$Date),]
colnames(BPI)[2] <- "Open_BPI"
colnames(BPI)[3] <- "High_BPI"
colnames(BPI)[4] <- "Low_BPI"
colnames(BPI)[5] <- "Close_BPI"
colnames(BPI)[6] <- "Volume_BPI"
colnames(BPI)[7] <- "AdjClose_BPI"
BPI$BPI_var <- c(NA, diff(BPI$AdjClose_BPI)/ BPI$AdjClose_BPI[-1])
BPI$BPI_traded_volume <- BPI$Close_BPI * BPI$Volume_BPI

#Manipulating the data series (for BES)
BES <- BES[order(BES$Date),]
colnames(BES)[2] <- "Open_BES"
colnames(BES)[3] <- "High_BES"
colnames(BES)[4] <- "Low_BES"
colnames(BES)[5] <- "Close_BES"
colnames(BES)[6] <- "Volume_BES"
colnames(BES)[7] <- "AdjClose_BES"
BES$BES_var <- c(NA, diff(BES$AdjClose_BES)/ BES$AdjClose_BES[-1])
BES$BES_traded_volume <- BES$Close_BES * BES$Volume_BES

#Manipulating the data series (for BCP)
BCP <- BCP[order(BCP$Date),]
colnames(BCP)[2] <- "Open_BCP"
colnames(BCP)[3] <- "High_BCP"
colnames(BCP)[4] <- "Low_BCP"
colnames(BCP)[5] <- "Close_BCP"
colnames(BCP)[6] <- "Volume_BCP"
colnames(BCP)[7] <- "AdjClose_BCP"
BCP$BCP_var <- c(NA, diff(BCP$AdjClose_BCP)/ BCP$AdjClose_BCP[-1])
BCP$BCP_traded_volume <- BCP$Close_BCP * BCP$Volume_BCP

#Manipulating the data series (for BANIF)
BANIF <- BANIF[order(BANIF$Date),]
colnames(BANIF)[2] <- "Open_BANIF"
colnames(BANIF)[3] <- "High_BANIF"
colnames(BANIF)[4] <- "Low_BANIF"
colnames(BANIF)[5] <- "Close_BANIF"
colnames(BANIF)[6] <- "Volume_BANIF"
colnames(BANIF)[7] <- "AdjClose_BANIF"
BANIF$BANIF_var <- c(NA, diff(BANIF$AdjClose_BANIF)/ BANIF$AdjClose_BANIF[-1])
BANIF$BANIF_traded_volume <- BANIF$Close_BANIF * BANIF$Volume_BANIF

#Manipulating the data series (for EDP)
EDP <- EDP[order(EDP$Date),]
colnames(EDP)[2] <- "Open_EDP"
colnames(EDP)[3] <- "High_EDP"
colnames(EDP)[4] <- "Low_EDP"
colnames(EDP)[5] <- "Close_EDP"
colnames(EDP)[6] <- "Volume_EDP"
colnames(EDP)[7] <- "AdjClose_EDP"
EDP$EDP_var <- c(NA, diff(EDP$AdjClose_EDP)/ EDP$AdjClose_EDP[-1])
EDP$EDP_traded_volume <- EDP$Close_EDP * EDP$Volume_EDP

#Manipulating the data series (forJM)
JM <-JM[order(JM$Date),]
colnames(JM)[2] <- "Open_JM"
colnames(JM)[3] <- "High_JM"
colnames(JM)[4] <- "Low_JM"
colnames(JM)[5] <- "Close_JM"
colnames(JM)[6] <- "Volume_JM"
colnames(JM)[7] <- "AdjClose_JM"
JM$JM_var <- c(NA, diff(JM$AdjClose_JM)/JM$AdjClose_JM[-1])
JM$JM_traded_volume <-JM$Close_JM *JM$Volume_JM

#Manipulating the data series (forGALP)
GALP <-GALP[order(GALP$Date),]
colnames(GALP)[2] <- "Open_GALP"
colnames(GALP)[3] <- "High_GALP"
colnames(GALP)[4] <- "Low_GALP"
colnames(GALP)[5] <- "Close_GALP"
colnames(GALP)[6] <- "Volume_GALP"
colnames(GALP)[7] <- "AdjClose_GALP"
GALP$GALP_var <- c(NA, diff(GALP$AdjClose_GALP)/GALP$AdjClose_GALP[-1])
GALP$GALP_traded_volume <-GALP$Close_GALP *GALP$Volume_GALP

#Manipulating the data series (forSTOXX)
STOXX <-STOXX[order(STOXX$Date),]
colnames(STOXX)[2] <- "Open_STOXX"
colnames(STOXX)[3] <- "High_STOXX"
colnames(STOXX)[4] <- "Low_STOXX"
colnames(STOXX)[5] <- "Close_STOXX"
colnames(STOXX)[6] <- "Volume_STOXX"
colnames(STOXX)[7] <- "AdjClose_STOXX"
STOXX$STOXX_var <- c(NA, diff(STOXX$AdjClose_STOXX)/STOXX$AdjClose_STOXX[-1])
STOXX$STOXX_traded_volume <-STOXX$Close_STOXX *STOXX$Volume_STOXX

#Manipulating the data series (forSTOXXGENERAL)
STOXXGENERAL <-STOXXGENERAL[order(STOXXGENERAL$Date),]
colnames(STOXXGENERAL)[2] <- "Open_STOXXGENERAL"
colnames(STOXXGENERAL)[3] <- "High_STOXXGENERAL"
colnames(STOXXGENERAL)[4] <- "Low_STOXXGENERAL"
colnames(STOXXGENERAL)[5] <- "Close_STOXXGENERAL"
colnames(STOXXGENERAL)[6] <- "Volume_STOXXGENERAL"
colnames(STOXXGENERAL)[7] <- "AdjClose_STOXXGENERAL"
STOXXGENERAL$STOXXGENERAL_var <- c(NA, diff(STOXXGENERAL$AdjClose_STOXXGENERAL)/STOXXGENERAL$AdjClose_STOXXGENERAL[-1])
STOXXGENERAL$STOXXGENERAL_traded_volume <-STOXXGENERAL$Close_STOXXGENERAL *STOXXGENERAL$Volume_STOXXGENERAL





#Creating the master dataframe for the VAR model
Banks_quotes <- merge(x = BPI, y = BES, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = BCP, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = BANIF, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = EDP, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = YIELDS, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = JM, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = GALP, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = STOXX, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = STOXXGENERAL, y = Banks_quotes, by = "Date", all.y = TRUE)



#creating the excess return variables
Banks_quotes$BPI_ex_return <- Banks_quotes$BPI_var - Banks_quotes$STOXX_var
Banks_quotes$BES_ex_return <- Banks_quotes$BES_var - Banks_quotes$STOXX_var
Banks_quotes$BCP_ex_return <- Banks_quotes$BCP_var - Banks_quotes$STOXX_var
Banks_quotes$BANIF_ex_return <- Banks_quotes$BANIF_var - Banks_quotes$STOXX_var
Banks_quotes$EDP_ex_return <- Banks_quotes$EDP_var - Banks_quotes$STOXXGENERAL_var
Banks_quotes$JM_ex_return <- Banks_quotes$JM_var - Banks_quotes$STOXXGENERAL_var
Banks_quotes$GALP_ex_return <- Banks_quotes$GALP_var - Banks_quotes$STOXXGENERAL_var

Banks_quotes[is.na(Banks_quotes)] <- 0
Banks_quotes <- Banks_quotes[Banks_quotes$Date > 36528, ]

#charting

ggplot(Banks_quotes$BANIF_var) + geomline(aes(x=u,y=v))
#lags (NOT WORKING)

Banks_quotes <-Banks_quotes[order(Banks_quotes$Date),]
Banks_quotes <- slide(Banks_quotes, Var = "BCP_ex_return", slideBy = -1)
Banks_quotes <- slide(Banks_quotes, Var = "BCP_ex_return", slideBy = -2)
Banks_quotes <- slide(Banks_quotes, Var = "BCP_ex_return", slideBy = -3)
Banks_quotes <- slide(Banks_quotes, Var = "BPI_ex_return", slideBy = -1)
Banks_quotes <- slide(Banks_quotes, Var = "BPI_ex_return", slideBy = -2)
Banks_quotes <- slide(Banks_quotes, Var = "BPI_ex_return", slideBy = -3)
Banks_quotes <- slide(Banks_quotes, Var = "PT_var", slideBy = -1)
Banks_quotes <- slide(Banks_quotes, Var = "PT_var", slideBy = -2)
Banks_quotes <- slide(Banks_quotes, Var = "PT_var", slideBy = -3)


Banks_quotes$BCP_Negative = ifelse(Banks_quotes$BCP_ex_return > 0, 0, -Banks_quotes$BCP_ex_return)
Banks_quotes$BCP_Positive = ifelse(Banks_quotes$BCP_ex_return < 0, 0, Banks_quotes$BCP_ex_return)

Banks_quotes$BPI_Negative = ifelse(Banks_quotes$BPI_ex_return > 0, 0, -Banks_quotes$BPI_ex_return)
Banks_quotes$BPI_Positive = ifelse(Banks_quotes$BPI_ex_return < 0, 0, Banks_quotes$BPI_ex_return)



#VAR model
Linear_Model <- lm(Banks_quotes$PT_var ~ Banks_quotes$BPI_Negative + Banks_quotes$BPI_Positive + Banks_quotes$BES_ex_return +
                   Banks_quotes$BCP_Negative + Banks_quotes$BCP_Positive + Banks_quotes$BANIF_ex_return +
                   Banks_quotes$EDP_ex_return + Banks_quotes$GALP_ex_return +
                   Banks_quotes$JM_ex_return + Banks_quotes$"BCP_ex_return-1" +
                   Banks_quotes$"BCP_ex_return-2" + Banks_quotes$"BCP_ex_return-3" +
                   Banks_quotes$"BPI_ex_return-1" + Banks_quotes$"BPI_ex_return-2" +
                   Banks_quotes$"BPI_ex_return-3" + Banks_quotes$"PT_var-1" +
                   Banks_quotes$"PT_var-2" + Banks_quotes$"PT_var-3")

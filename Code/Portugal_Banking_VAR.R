#Econometrics Research Paper
#Implements a VAR model trying to undestand the relatio between
#portuguse sovereing yield and the evolution/perfomance of
#the Banking sector. The main portuguese banks are
#1.BPI
#2.BES
#3.BCP
#4.BANIF

library("vars")


#Load the data series

BPI <- read.csv("Data/BPI_quotes.csv")
BES <- read.csv("Data/BES_quotes.csv")
BCP <- read.csv("Data/BCP_quotes.csv")
BANIF <- read.csv("Data/BANIF_quotes.csv")


#Manipulating the data series (for BPI)
BPI$Date <- as.Date(BPI$Date)
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
BES$Date <- as.Date(BES$Date, format = "%Y-%m-%d")
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
BCP$Date <- as.Date(BCP$Date)
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
BANIF$Date <- as.Date(BANIF$Date)
BANIF <- BANIF[order(BANIF$Date),]
colnames(BANIF)[2] <- "Open_BANIF"
colnames(BANIF)[3] <- "High_BANIF"
colnames(BANIF)[4] <- "Low_BANIF"
colnames(BANIF)[5] <- "Close_BANIF"
colnames(BANIF)[6] <- "Volume_BANIF"
colnames(BANIF)[7] <- "AdjClose_BANIF"
BANIF$BANIF_var <- c(NA, diff(BANIF$AdjClose_BANIF)/ BANIF$AdjClose_BANIF[-1])
BANIF$BANIF_traded_volume <- BANIF$Close_BANIF * BANIF$Volume_BANIF


#Creating the master dataframe for the VAR model
Banks_quotes <- merge(x = BPI, y = BES, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = BCP, y = Banks_quotes, by = "Date", all.y = TRUE)
Banks_quotes <- merge(x = BANIF, y = Banks_quotes, by = "Date", all.y = TRUE)


#charting



#diagnostic tests



#VAR model

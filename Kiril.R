#ThisIsKirooooo
#install.packages('quantmod')
library(quantmod)
#install.packages('dplyr')
library(dplyr)
#install.packages('infotheo')
library(infotheo)
#install.packages('caret')
library(caret)

# get market data
getSymbols(c("^GSPC", "BRK-B", "HPE", "MSFT"))
BRK <- data.frame(`BRK-B`)
HPE <- data.frame(HPE)
MSFT <- data.frame(MSFT)
rm(`BRK-B`)
# transfer market data to a simple data frame
GSPC <- data.frame(GSPC)

# extract the date row name into a date column




# take random sets of sequential rows 
new_set <- c()
for (row_set in seq(10000)) {
  row_quant <- sample(10:30, 1)
  print(row_quant)
  row_start <- sample(1:(nrow(GSPC) - row_quant), 1)
  market_subset <- GSPC[row_start:(row_start + row_quant),]
  market_subset <- dplyr::mutate(market_subset, 
                                 Close_Date = max(market_subset$Close.Date),
                                 Close_Gap=(GSPC.Close - lag(GSPC.Close))/lag(GSPC.Close) ,
                                 High_Gap=(GSPC.High - lag(GSPC.High))/lag(GSPC.High) ,
                                 Low_Gap=(GSPC.Low - lag(GSPC.Low))/lag(GSPC.Low),
                                 Volume_Gap=(GSPC.Volume - lag(GSPC.Volume))/lag(GSPC.Volume),
                                 Daily_Change=(GSPC.Close - GSPC.Open)/GSPC.Open,
                                 Outcome_Next_Day_Direction= (lead(GSPC.Volume)-GSPC.Volume)) %>%
    dplyr::select(-GSPC.Open, -GSPC.High, -GSPC.Low, -GSPC.Close, -GSPC.Volume, -GSPC.Adjusted, -Close.Date) %>%
    na.omit
  market_subset$Sequence_ID <- row_set
  new_set <- rbind(new_set, market_subset)
}
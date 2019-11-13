

setwd("P:/Malte/01 - Research/R/Momentum Tool/")

rm(list = ls())

library(Rblpapi)
library(tidyr)
library(xts)
library(qpcR)
library(gdata)
library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(data.table)

# Bloomberg Formula for Returns in EQS
# (($RT100:M-3)+($RT100:M-6))/2


#For loop mit Datum in Dates von Beispiel Datenreihe mit Apple beispiel
#Mit merge mit all x = true


# Generate Vector of Months

sector <- c("S5INFT US Equity")

start_date <- as.Date("2019-04-01")
end_date <- as.Date("2019-11-01")

blpConnect()
opt <- c("periodicitySelection"="MONTHLY")
Closing_Example <- bdh(securities = sector,
                      fields = c("PX_Last"),
                      start.date = start_date,
                      end.date = end_date,
                      options = opt)


dates <- as.Date(Closing_Example$date[-1])
Tickers <- t(as.data.frame(beqs(sector, date = Closing_Example$date[1])[,c("Ticker")]))


i <- 1
while (i <= length(dates) ) {
  
  Tickers <- qpcR:::rbind.na(Tickers,t(as.data.frame(beqs("S5INFT Example", date = dates[i])[,c("Ticker")])))
  
  i = i +1
}


vector_data<- unmatrix(Tickers,byrow=T)
tick <- unique(vector_data)
tick <- tick[!is.na(tick)]


Closing_Prices <- bdh(securities = tick,
                      fields = c("PX_Last"),
                      start.date = start_date-200, ## -200 as we need data points up to 6 months before our period
                      end.date = end_date,
                      options = opt)


datxts <- lapply(Closing_Prices, function(d) xts(d[,-1], order.by=as.Date(d[,1])))
res <- do.call(merge, datxts)  
colnames(res) <- names(Closing_Prices) 
rownames(Tickers) <- as.character(Closing_Example$date)

#save(list = ls(), file = "P:/Malte/01 - Research/R/Momentum Tool/return_data.rda")

###### HIER res ABGESPEICHERT

load(file = c("P:/Malte/01 - Research/R/Momentum Tool/return_data.rda"))

res <- na.locf(res)

One_M_Ret <- ROC(res, type = "discrete")
Three_M_Ret <- ROC(res, type = "discrete", n = 3)
Six_M_Ret <- ROC(res, type = "discrete", n = 6)


# Set up 1-Month Return per Security after EQS - Filter
one_M_Return <- as.data.frame(matrix(nrow = nrow(Tickers),ncol = length(tick)))
colnames(one_M_Return) <- tick
rownames(one_M_Return) <- rownames(Tickers)

for (MM in as.character(Closing_Example$date)) {
  
  for (stock in as.character(tick)) {
  
    one_M_Return[MM,stock] <- One_M_Ret[MM,stock]

  }
}

# Set up 3-Month Return per Security after EQS - Filter - Recalculation after 3 Months
Three_M_Return <- as.data.frame(matrix(nrow = nrow(Tickers),ncol = length(tick)))
colnames(Three_M_Return) <- tick
rownames(Three_M_Return) <- rownames(Tickers)

Three_M_Dates <-Closing_Example$date[seq(1, length(Closing_Example$date), 3)]

for (MM in as.character(Three_M_Dates)) {
  
  for (stock in as.character(tick)) {
    
    Three_M_Return[MM,stock] <- Three_M_Ret[MM,stock]
    Three_M_Return[which( rownames(Three_M_Return)== as.character(MM) )+1,stock] <- Three_M_Ret[MM,stock]
    Three_M_Return[which( rownames(Three_M_Return)== as.character(MM) )+2,stock] <- Three_M_Ret[MM,stock]
    
  }
}

Three_M_Return <- Three_M_Return[rownames(Tickers),]
  

# Set up 6-Month Return per Security after EQS - Filter - Recalculation after 6 Months
Six_M_Return <- as.data.frame(matrix(nrow = nrow(Tickers),ncol = length(tick)))
colnames(Six_M_Return) <- tick
rownames(Six_M_Return) <- rownames(Tickers)

Six_M_Dates <-Closing_Example$date[seq(1, length(Closing_Example$date), 6)]

for (MM in as.character(Six_M_Dates)) {
  
  for (stock in as.character(tick)) {
    
    Six_M_Return[MM,stock] <- Six_M_Ret[MM,stock]
    Six_M_Return[which( rownames(Six_M_Return)== as.character(MM) )+1,stock] <- Six_M_Ret[MM,stock]
    Six_M_Return[which( rownames(Six_M_Return)== as.character(MM) )+2,stock] <- Six_M_Ret[MM,stock]
    Six_M_Return[which( rownames(Six_M_Return)== as.character(MM) )+3,stock] <- Six_M_Ret[MM,stock]
    Six_M_Return[which( rownames(Six_M_Return)== as.character(MM) )+4,stock] <- Six_M_Ret[MM,stock]
    Six_M_Return[which( rownames(Six_M_Return)== as.character(MM) )+5,stock] <- Six_M_Ret[MM,stock]
    
  }
}

Six_M_Return <- Six_M_Return[rownames(Tickers),]

# Set up Total Return Strength
Total_Return_Strength <- 0.6 * Six_M_Return + 0.25 * Three_M_Return + 0.15 * one_M_Return

# Fill EQS Filter with Total Return Strength
Tot_Ret_1_3_6 <- as.data.frame(matrix(nrow = nrow(Tickers),ncol = length(tick)))
colnames(Tot_Ret_1_3_6) <- tick
rownames(Tot_Ret_1_3_6) <- rownames(Tickers)

for (MM in as.character(Closing_Example$date)) {
  
  for (stock in na.omit(Tickers[MM,])) {
    
    Tot_Ret_1_3_6[MM,stock] <- Total_Return_Strength[MM,stock]

  }
}

# Must be equal to numbers of Securities of EQS
apply(Tot_Ret_1_3_6, 1, function(x) sum(!is.na(x)))



# Create List to delete NAs

xy.list <- split(Tot_Ret_1_3_6, seq(nrow(Tot_Ret_1_3_6)))
xy.list <- setNames(split(Tot_Ret_1_3_6, seq(nrow(Tot_Ret_1_3_6))), rownames(Tot_Ret_1_3_6))


i <- 1
while (i <= length(xy.list)) {
  
  xy.list[[i]] <- xy.list[[i]][, colSums(is.na(xy.list[[i]])) != nrow(xy.list[[i]])]
  
  i = i+1
  
}

# Define 0.6 Quantile Cuts per Date

cuts <- as.data.frame(matrix(ncol = 1, nrow = length(xy.list)))

i <- 1
while (i <= length(xy.list)) {
  
  cuts[i,] = quantile(t(xy.list[[i]][1,]), probs = 0.6)
  
  i = i+1
}

# Assign -1 to NAs in EQS Screens
Tot_Ret_1_3_6[is.na(Tot_Ret_1_3_6)] <- -1

# If Total Return Value > Cut, then 1/n weight, else 0

weights_1_3_6 <- Tot_Ret_1_3_6
  
i <- 1
while (i <= nrow(weights_1_3_6)) {
  
  weights_1_3_6[i,] <- ifelse(Tot_Ret_1_3_6[i,] > cuts[i,],yes = 1/sum(Tot_Ret_1_3_6[i,] > cuts[i,]), no = 0)
  
  i = i+1
}


# Check if Weights sum up to 1

rowSums(weights_1_3_6)


# Build Returns Portfolio
portfolioReturn_1_3_6M <- Return.portfolio(One_M_Ret, weights = weights_1_3_6)



####################################################################################################
####################################################################################################
####################################################################################################


# Build Weights for 3/6 Month Return Strength

# Set up 1-Month Return per Security after EQS - Filter
Three_M_Return <- as.data.frame(matrix(nrow = nrow(Tickers),ncol = length(tick)))
colnames(Three_M_Return) <- tick
rownames(Three_M_Return) <- rownames(Tickers)

for (MM in as.character(Closing_Example$date)) {
  
  for (stock in as.character(tick)) {
    
    Three_M_Return[MM,stock] <- Three_M_Ret[MM,stock]
    
  }
}

Six_M_Return <- as.data.frame(matrix(nrow = nrow(Tickers),ncol = length(tick)))
colnames(Six_M_Return) <- tick
rownames(Six_M_Return) <- rownames(Tickers)

for (MM in as.character(Closing_Example$date)) {
  
  for (stock in as.character(tick)) {
    
    Six_M_Return[MM,stock] <- Six_M_Ret[MM,stock]
    
  }
}


# Set up Total Return Strength
Total_Return_Strength <- 0.5 * Six_M_Return + 5 * Three_M_Return

# Fill EQS Filter with Total Return Strength
Tot_Ret_3_6 <- as.data.frame(matrix(nrow = nrow(Tickers),ncol = length(tick)))
colnames(Tot_Ret_3_6) <- tick
rownames(Tot_Ret_3_6) <- rownames(Tickers)

for (MM in as.character(Closing_Example$date)) {
  
  for (stock in na.omit(Tickers[MM,])) {
    
    Tot_Ret_3_6[MM,stock] <- Total_Return_Strength[MM,stock]
    
  }
}

# Must be equal to numbers of Securities of EQS
apply(Tot_Ret_3_6, 1, function(x) sum(!is.na(x)))



# Create List to delete NAs

xy.list <- split(Tot_Ret_3_6, seq(nrow(Tot_Ret_3_6)))
xy.list <- setNames(split(Tot_Ret_3_6, seq(nrow(Tot_Ret_3_6))), rownames(Tot_Ret_3_6))


i <- 1
while (i <= length(xy.list)) {
  
  xy.list[[i]] <- xy.list[[i]][, colSums(is.na(xy.list[[i]])) != nrow(xy.list[[i]])]
  
  i = i+1
  
}

# Define 0.6 Quantile Cuts per Date

cuts <- as.data.frame(matrix(ncol = 1, nrow = length(xy.list)))

i <- 1
while (i <= length(xy.list)) {
  
  cuts[i,] = quantile(t(xy.list[[i]][1,]), probs = 0.6)
  
  i = i+1
}

# Assign -1 to NAs in EQS Screens
Tot_Ret_3_6[is.na(Tot_Ret_3_6)] <- -1

# If Total Return Value > Cut, then 1/n weight, else 0

weights_3_6 <- Tot_Ret_3_6

i <- 1
while (i <= nrow(weights_3_6)) {
  
  weights_3_6[i,] <- ifelse(Tot_Ret_3_6[i,] > cuts[i,],yes = 1/sum(Tot_Ret_3_6[i,] > cuts[i,]), no = 0)
  
  i = i+1
}


# Check if Weights sum up to 1

rowSums(weights_3_6)

############### Ab hier am testen ############### 



# Build Returns Portfolio
#weights_3_6 <- weights_3_6[-nrow(weights_3_6),]
portfolioReturn_3_6M <- Return.portfolio(one_M_Return, weights = weights_3_6)

r <- as.matrix(one_M_Return[i,])+1
w <- as.matrix(weights_3_6[i-1,])

w %*% t(r) - 1

# Build Return Series of Portfolio

PF_Return_3_6 <- as.data.frame(matrix(nrow = nrow(one_M_Return)))
colnames(PF_Return_3_6) <- c("PF 0.5x3M 0.5x6M - Monthly Rebal")
rownames(PF_Return_3_6) <- rownames(one_M_Return)
i <- 2
while (i <= nrow(one_M_Return)) {
  
  PF_Return_3_6[i,] = as.matrix(weights_3_6[i,]) %*% t(as.matrix(one_M_Return[i,])+1)-1
  
  i = i+1
}

PF_Return_3_6 <- as.xts(PF_Return_3_6)

  
  



# Build Benchmark and create Backtesting
rownames(as.data.frame(portfolioReturn_3_6M))[1]




bm <- bdh(securities = sector,
                       fields = c("PX_Last"),
                       start.date = start_date,
                       end.date = end_date,
                       options = opt)



bm_vergleich <- cbind(portfolioReturn_1_3_6M, portfolioReturn_3_6M)
colnames(bm_vergleich) <- c("portfolioReturn_1_3_6M","portfolioReturn_3_6M")
charts.PerformanceSummary(bm_vergleich)








table.AnnualizedReturns(PF_Return_3_6)
table.CalendarReturns(PF_Return_3_6)

table.AnnualizedReturns(portfolioReturn_3_6M)
table.CalendarReturns(portfolioReturn_3_6M)


table.Stats(portfolioReturn_1_3_6M)



chartSeries(Return.cumulative(portfolioReturn_3_6M))


sp500prices <- getSymbols.yahoo("SPY", from = start_date, periodicity = "monthly", auto.assign = FALSE)[,4]
sp500Rets <- na.omit(ROC(sp500prices))
sp500Rets <- as.xts(sp500Rets)

BM_Rets <- ROC(Closing_Example$PX_Last)




chart.Weights(weights, main = "Weights")


fb_Rets <- na.omit(ROC(portfolioPrices$FB.Close))
aapl_Rets <- na.omit(ROC(portfolioPrices$AAPL.Close))

rets_new <- cbind(fb_Rets, aapl_Rets, sp500Rets)
charts.PerformanceSummary(rets_new, main = "Maltes Test")





### Test ob ältere Datenreihen überhaupt gehen - sieht gut aus!


t <- beqs("US Financials Momentum", date = as.Date("2005-01-01"))[,c("Ticker")]
heut <- beqs("US Financials Momentum")[,c("Ticker")]






## special treatments to action-direction
## Shirley 2016-06-21

library(data.table)
library(gftUtils)
library(RMySQL)
library(TTR)
library(zoo)
library(xts)


# The Aroon Oscillator signals an upward trend is underway when it is above zero and a downward trend is underway when it falls below zero.
futures_aroon <- function(Table){
  aroonResult <- aroon(xts(Table[,list(HighPrice, LowPrice)], as.POSIXct(Table[,datetime])))
  aroonResult <- as.data.table(aroonResult)
  return(aroonResult)
} 

# The CCI usually falls in a channel of -100 to 100. The conventional CCI trading system works as follows. When it rises above 100, buy and hold until CCI falls back below 100. When CCI falls below -100, sell short and cover the short when it rises above the -100 line. 
future_CCI<- function(Table, n){
  CCIResult<- CCI(xts(Table[,list(HighPrice, LowPrice, ClosePrice)], as.POSIXct(Table[,datetime])), n)
  return(CCIResult)
}

# The RSI indicator ranges in value from 0 to 100, with numbers above 70 indicating overbought conditions and under 30 indicating oversold. If the RSI rises above 30, it is considered bullish, while if the RSI falls below 70, it is considered bearish. 
future_RSI<- function(Table){
  RSIResult<- RSI(xts(Table[,ClosePrice], as.POSIXct(Table[,datetime])))
  return(RSIResult)
}

future_SMA20 <- function(Table){
  SMA20 <- SMA(xts(Table[,ClosePrice], as.POSIXct(Table[,datetime])), n = 20)
  return(SMA20)
}

future_SMA60 <- function(Table){
  SMA60 <- SMA(xts(Table[,ClosePrice], as.POSIXct(Table[,datetime])), n = 60)
  return(SMA60)
}

future_OBV <- function(Table){
  OBVResult <- OBV(xts(Table[,ClosePrice], as.POSIXct(Table[,datetime])), xts(Table[,Volume], as.POSIXct(Table[,datetime])))
  return(OBVResult)
}

future_CMF <- function(Table){
  OBVResult <- CMF(xts(Table[,list(HighPrice, LowPrice, ClosePrice)], as.POSIXct(Table[,datetime])), xts(Table[,Volume], as.POSIXct(Table[,datetime])))
  return(OBVResult)
}


# generate keep long or keep short and No-op
opencloseAction <- function(futuresTechnical, signalCol){
  result <- list(long=NA, short=NA)
  Table <- copy(futuresTechnical)
  
  Table[,actionCol:=c(NA,diff(get(signalCol), lag=1)), by=list(symbol, date)]
  Table[!actionCol==0, actionType:=ifelse(sign(actionCol)>0, "Open_Long", "Open_Short")]
  LongTable <- copy(Table)
  LongTable[sign(actionCol)<0, actionType:="Close_Long"]
  ShortTable <- copy(Table)
  ShortTable[sign(actionCol)>0, actionType:="Close_Short"]
  
  # generate keep action direction
  LongTable[actionCol==0, actionCol:=NA]
  LongTable[,actionCol:=na.locf(actionCol, na.rm = F), by=list(symbol, date)]
  LongTable[is.na(actionCol), actionCol:=0]
  LongTable[actionCol>0 & is.na(actionType), actionType:="Keep_Long"]
  LongTable[actionCol<=0 & is.na(actionType), actionType:="No_Op"]
  
  ShortTable[actionCol==0, actionCol:=NA]
  ShortTable[,actionCol:=na.locf(actionCol, na.rm = F), by=list(symbol, date)]
  ShortTable[is.na(actionCol), actionCol:=0]
  ShortTable[actionCol<0 & is.na(actionType), actionType:="Keep_Short"]
  ShortTable[actionCol>=0 & is.na(actionType), actionType:="No_Op"]
  
  LongTable[,mindatetime:=min(datetime), by=list(symbol, date)]
  LongTable[datetime==mindatetime, actionType:="No_Op"]
  LongTable[,minLongDatetime:=min(.SD[actionType=="Open_Long", datetime]), by=list(symbol, date)]
  #LongTable <- LongTable[datetime>=minLongDatetime]
  ShortTable[,mindatetime:=min(datetime), by=list(symbol, date)]
  ShortTable[datetime==mindatetime, actionType:="No_Op"]
  ShortTable[,minShortDatetime:=min(.SD[actionType=="Open_Short", datetime]), by=list(symbol, date)]
  #ShortTable <- ShortTable[datetime>=minShortDatetime]
  
  # in order to get rid of the relative incomplete data
  LongTable[,actionCount:=nrow(.SD), by=list(symbol, date)]
  LongTable <- LongTable[actionCount>=80]
  ShortTable[,actionCount:=nrow(.SD), by=list(symbol, date)]
  ShortTable <- ShortTable[actionCount>=80]
  
  result <- list(long=LongTable[,mindatetime:=NULL], short=ShortTable[,mindatetime:=NULL])
  
  return(result)
}


ProductionConn <- util.connectMySQL("Product")
hfconn <- util.connectMySQL("hf")

CommodityFutures <- dbGetQuery(ProductionConn, paste("SELECT b.TRADEPRODUCTCODE,a.TRADEDATE,a.FUTURESCODE FROM V_MAIN_FUTURES_VOLUME a LEFT JOIN FFM_FUTURES_STANDARD_CONTRACT b on a.SCID=b.gid ORDER BY a.TRADEDATE", seq = ""))
setDT(CommodityFutures)
setnames(CommodityFutures, c("code", "date", "symbol"))
FuturesInfo <- dbGetQuery(hfconn, paste("SELECT FuturesCode as symbol, TradeDate AS date, TradeDateTime as datetime, OpenPrc AS OpenPrice, ClosPrc as ClosePrice, HighPrc AS HighPrice, LowPrc AS LowPrice, Volume FROM MKT_Future1min WHERE TradeDate>='2016-05-01' order by  TradeDate", sep = ""))
setDT(FuturesInfo)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

## we do not have hf data for commodity futures currently
setkey(CommodityFutures,symbol,date)
setkey(FuturesInfo, symbol, date)
futuresTable<-CommodityFutures[FuturesInfo]
futuresTable[,date:=util.asdate(date)]
futuresTable[,code:=NULL]

# signals generation
futuresTable[,c("OpenPrice", "ClosePrice"):=list(na.locf(OpenPrice, na.rm = F), na.locf(ClosePrice, na.rm = F)), by=list(symbol, date)]
futuresTable[,c("HighPrice", "LowPrice"):=list(ifelse(is.na(HighPrice), OpenPrice, HighPrice), ifelse(is.na(LowPrice), OpenPrice, LowPrice)), by=list(symbol, date)]

futuresTable[,aroonIndex:=ifelse(futures_aroon(.SD)[,oscillator]>0, 1, -1), by=list(symbol, date)]
futuresTable[,CCIIndex:=sign(future_CCI(.SD, 25)-100), by=list(symbol, date)]
futuresTable[,RSIIndex:= ifelse(future_RSI(.SD)<30, 1, ifelse(future_RSI(.SD)> 70 , -1, 0)), by=list(symbol, date)]
futuresTable[,SMA20:=as.numeric(future_SMA20(.SD)), by=list(symbol, date)]
futuresTable[,SMA60:=as.numeric(future_SMA60(.SD)), by=list(symbol, date)]
futuresTable[,OBVIndex:=(future_OBV(.SD)/sum(Volume))*100, by=list(symbol, date)]
futuresTable[,CMFIndex:=ifelse(future_CMF(.SD)>0.5, 1, -1), by=list(symbol, date)]


# transform to buysell longshort signals
#futuresTesta <- futuresTable[!is.na(aroonIndex) & !is.na(CCIIndex) & !is.na(RSIIndex) & !is.na(SMA20) & !is.na(SMA60) & !is.na(OBV) & !is.na(CMFIndex)]
futuresTesta[,SMAIndex:=SMA20-SMA60]
futuresTesta[,SMAIndex:=sign(SMAIndex)]
futuresTesta[,OBVIndex:=as.numeric(OBVIndex)]
futuresTesta[,OBVIndex:=ifelse(OBVIndex>0, 1, -1)]
futuresTesta[,aroonNcci:=ifelse(aroonIndex>0 & CCIIndex>0, 1, ifelse(aroonIndex>0 & CCIIndex>0, -1, 0))]
futuresTesta[,obvNrsi:=ifelse(OBVIndex>0.5 & RSIIndex>0, 1, ifelse(OBVIndex<0.5 & RSIIndex<0, -1, 0))]
futuresTesta[,KDJ:=ifelse(fastK>fastD, 1, ifelse(fastK<fastD, -1, 0))]

futuresTesta[,c("CCIIndex", "RSIIndex", "SMAIndex", "OBVIndex", "CMFIndex"):=list(as.numeric(CCIIndex), as.numeric(RSIIndex), as.numeric(SMAIndex), as.numeric(OBVIndex), as.numeric(CMFIndex))]

aroon_Long <- opencloseAction(futuresTesta, "aroonIndex")$long
aroon_Short <- opencloseAction(futuresTesta, "aroonIndex")$short
CCI_Long <- opencloseAction(futuresTesta, "CCIIndex")$long
CCI_Short <- opencloseAction(futuresTesta, "CCIIndex")$short
RSI_Long <- opencloseAction(futuresTesta, "RSIIndex")$long
RSI_Short <- opencloseAction(futuresTesta, "RSIIndex")$short
SMA_Long <- opencloseAction(futuresTesta, "SMAIndex")$long
SMA_Short <- opencloseAction(futuresTesta, "SMAIndex")$short
OBV_Long <- opencloseAction(futuresTesta, "OBVIndex")$long
OBV_Short <- opencloseAction(futuresTesta, "OBVIndex")$short
CMF_Long <- opencloseAction(futuresTesta, "CMFIndex")$long
CMF_Short <- opencloseAction(futuresTesta, "CMFIndex")$short

aroonNcci_Long <- opencloseAction(futuresTesta, "aroonNcci")$long
aroonNcci_Short <- opencloseAction(futuresTesta, "aroonNcci")$short
obvNrsi_Long <- opencloseAction(futuresTesta, "obvNrsi")$long
obvNrsi_Short <- opencloseAction(futuresTesta, "obvNrsi")$short
KDJ_Long <- opencloseAction(futuresTesta, "KDJ")$long
KDJ_Short <- opencloseAction(futuresTesta, "KDJ")$short
TDIsignals_Long <- opencloseAction(futuresTesta, "TDIsignals")$long
TDIsignals_Short <- opencloseAction(futuresTesta, "TDIsignals")$short


####################################################################################
## signals processing
## Shirley 2016-06-22

library(data.table)
library(gftUtils)
library(zoo)

Action_Direction <- function(signalTable){
  result <- list(error="", value=NA)
  
  if(!"aroonIndex"%in%colnames(futuresTesta)){
    result$error <- "technical index aroon must be included!"
    return(result)
  }
  
  if(!"CCIIndex"%in%colnames(futuresTesta)){
    result$error <- "technical index CCI must be included!"
    return(result)
  }
  
  if(!"RSIIndex"%in%colnames(futuresTesta)){
    result$error <- "technical index RSI must be included!"
    return(result)
  }
  
  if(!"SMAIndex"%in%colnames(futuresTesta)){
    result$error <- "technical index SMA must be included!"
    return(result)
  }
  
  if(!"OBVIndex"%in%colnames(futuresTesta)){
    result$error <- "technical index OBV must be included!"
    return(result)
  }
  
  aroonTable <- generate_singleSignal(signalTable, "aroonIndex")
  CCITable <- generate_singleSignal(signalTable, "CCIIndex")
  RSITable <- generate_singleSignal(signalTable, "RSIIndex")
  SMATable <- generate_singleSignal(signalTable, "SMAIndex")
  OBVTable <- generate_singleSignal(signalTable, "OBVIndex")
  CMFTable <- generate_singleSignal(signalTable, "CMFIndex")
  MFITable <- generate_singleSignal(signalTable, "MFIIndex")
  TDITable <- generate_singleSignal(signalTable, "TDIsignals")
  KDJTable <- generate_singleSignal(signalTable, "KDJsignals")
  
  result$value <- list(aroon=aroonTable, CCI=CCITable, RSI=RSITable, SMA=SMATable, CMF=CMFTable, MFI=MFITable, TDI=TDITable)
  return(result)
}


generate_singleSignal <-function(signals, signalCol){
  open_long <- signals[get(signalCol)>0, list(symbol, date, datetime, OpenPrice, ClosePrice, openclose=1, longshort=1)]
  open_short <- signals[get(signalCol)<0, list(symbol, date, datetime, OpenPrice, ClosePrice, openclose=1, longshort=-1)]
  tradingTable <- rbind(open_long, open_short)
  setorder(tradingTable, symbol, date, datetime)
  tradingTable[,idx:=1:.N]
  close_long <- tradingTable[longshort==1,list(openclose=-1, longshort, idx=idx+1)]
  close_short <- tradingTable[longshort==-1,list(openclose=-1, longshort, idx=idx+1)]
  setkey(close_long, idx)
  setkey(close_short, idx)
  setkey(tradingTable, idx)
  close_long <- tradingTable[,list(symbol, date, datetime, OpenPrice, ClosePrice, idx)][close_long][idx%in%tradingTable[,idx]]
  close_short <- tradingTable[,list(symbol, date, datetime, OpenPrice, ClosePrice, idx)][close_short][idx%in%tradingTable[,idx]]
  
  tradingTable <- rbind(tradingTable, close_long)
  tradingTable <- rbind(tradingTable, close_short)
  setorder(tradingTable, symbol, date, datetime)
  
  setkey(signals, symbol, date, datetime)
  setkey(tradingTable, symbol, date, datetime)
  completeTable <- tradingTable[,list(symbol, date, datetime, openclose, longshort)][signals]
  completeTable[is.na(openclose),openclose:=0]
  completeTable[is.na(longshort),longshort:=0]
  
  return(completeTable[,list(symbol, date, datetime, openclose, longshort, get(signalCol))])
}

source("C:/Users/gft0071/Desktop/futuresSignals.R")
signalTable <- copy(futuresTesta)
outputTable <- Action_Direction(signalTable)



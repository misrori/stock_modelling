library(data.table)
library(rtsdata)
library(TTR)
library(pracma)
library(plotly)

get_one_ticker  <- function(my_ticker, start_date = "1900-01-01", end_date = Sys.Date(),  mas=NULL, emas=c(3, 5, 10, 20, 30), keepma=F, keepema=T, addmacd=T, addrsi=T, remove_nas = T, calc_diff = F, add_macd_cross=T ) {
  tryCatch({
    adatom <- data.frame(ds.getSymbol.yahoo(my_ticker, from = start_date, to =end_date ))
    names(adatom) <- tolower(sapply(strsplit(names(adatom), '.', fixed = T), '[[', 2))
    adatom$date <- as.Date(row.names(adatom)) 
    row.names(adatom) <- 1:nrow(adatom)
    adatom <- data.table(adatom)
    
    if( !identical(names(adatom) , c("open","high","low", "close","volume",  "adjusted","date"))) {
      text<- paste0('Error: ', my_ticker, ' # problem: names of dataframe is bad ', ' time: ', Sys.time())
      stop(text)
    }
    
    if ( nrow(adatom[complete.cases(adatom)==F,])> 0)  {
      adatom <- adatom[complete.cases(adatom),]
      if(nrow(adatom)==0){
        text<- paste0('Error: ', my_ticker, ' # problem: empty dataframe ', ' time: ', Sys.time())
        stop(text)
      }
    }
    
  }, error=function(x) {
    print(x)
    stop('No ticker')
  })
  
  
  for (simple_mas in mas) {
    adatom[[paste0('ma_', simple_mas, '_value')]] <- movavg(adatom[['close']], simple_mas, type="s")
    if (calc_diff==T) {
      
      adatom[[paste0('diff_',simple_mas,'_ma_value')]] <-  (( adatom[["close"]]  /adatom[[paste0('ma_', simple_mas, '_value')]] )-1)*100
      if (keepma==F) {
        adatom[[paste0('ma_', simple_mas, '_value')]] <- NULL
      }
      
    }
  }
  
  for (exp_mas in emas) {
    adatom[[paste0('ma_', exp_mas, '_exp_value')]] <- movavg(adatom[['close']], exp_mas, type="e")
    if (calc_diff==T) {
      adatom[[paste0('diff_',exp_mas,'_exp_ma_value')]] <-  (( adatom[["close"]]  / adatom[[paste0('ma_', exp_mas, '_exp_value')]] )-1)*100
      if (keepema==F) {
        adatom[[paste0('ma_', exp_mas, '_exp_value')]]<- NULL
      }
    }
  }
  
  if (addmacd==T) {
    adatom <- cbind(adatom, data.table(MACD(adatom[['close']], nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)))
  }  
  
  if (addrsi==T) {
    adatom$rsi <- RSI(adatom[['close']])
  }
  if (add_macd_cross==T) {
    adatom$macd_cross <- F
    for (i in 2:nrow(adatom)) {
      adatom$macd_cross[i] <- (adatom$macd[(i-1)]< adatom$signal[(i-1)]) & (adatom$macd[i]> adatom$signal[i])
      
    }
    
    adatom$macd_cross <- ifelse(adatom$macd_cross, 1, 0)
  }
  
  
  adatom <- adatom[complete.cases(adatom)]
  
  
  return(adatom)  
  
} # fg vege
adat <- get_one_ticker('SNY', start_date = '2010-01-01', end_date = '2019-01-01')
ttt <- adat


adat <- ttt
ema_values <- sort(as.numeric(sapply(strsplit(names(adat)[which(grepl('exp_value', names(adat)))], '_', fixed = T), '[[', 2)))
adat$ema_signal <- 0

for (i in c(1:(length(ema_values)-1))) {
  print(i)
  adat[[paste0('help_', ema_values[i])]] <- adat[[paste0('ma_', ema_values[i], '_exp_value')]] > adat[[paste0('ma_', ema_values[(i+1)], '_exp_value')]]
}

for (my_row in 1:nrow(adat)) {
  my_sum <- 0
  for (i in c(1:(length(ema_values)-1))) {
    my_sum  <- my_sum + as.numeric(adat[[paste0('help_', ema_values[i])]][my_row])
  }
  adat$ema_signal[my_row] <- my_sum
}

adat$run_mean_ema_signal <- runSum(adat$ema_signal, 3)


#adat$run_mean_ema_signal  <- adat$run_mean_ema_signal * adat$rsi

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)
p <- plot_ly(data = adat) %>%
  add_lines(x = ~date, y = ~close, name = "slope of 10") %>%
  add_lines(x = ~date, y = ~run_mean_ema_signal, name = "slope of 1", yaxis = "y2") %>%
  layout(
    title = "Double Y Axis", yaxis2 = ay,
    xaxis = list(title="x")
  )

p


adat$trade_id <- 0
adat <- data.table(adat[complete.cases(adat)])






trade_id <-1

for (i in 1:nrow(adat)) {

  if(adat$run_mean_ema_signal[i]>15){
    adat$trade_id[i]<-trade_id 
  }else{
    trade_id <-trade_id+1
  }

  
}


final_trades <- data.table()
for (my_trade_id in unique(adat$trade_id)) {
  if (my_trade_id==0) {
    next
  }
  temp_trade <- adat[trade_id==my_trade_id]
  temp_trade$sell_signal <- temp_trade$ma_3_exp_value < temp_trade$ma_20_exp_value
  
  last_row <- which(temp_trade$sell_signal)[1]
  if(is.na(last_row)){
    last_row<- nrow(temp_trade)
  }
  temp_trade <- temp_trade[1:last_row ]
  
  final_trades <- rbind(final_trades, temp_trade)
  
  
}







tradek <- final_trades[,.('trade_open'= head(close, 1), 'trade_close'= tail(close, 1), 'number_of_days'= .N),by=trade_id]
tradek[,win:=trade_close>trade_open ]

start_money <- 2000
for (i in 1:nrow(tradek)) {
  print('--------------------------------------------')
  print(i)
  trade_res <- round(((tradek$trade_close[i]/tradek$trade_open[i])-1)*100, 2)
  print(paste0('trade res: ', trade_res, ' Number of days: ', tradek$number_of_days[i]))
  start_money <- start_money* (tradek$trade_close[i]/tradek$trade_open[i])
  start_money <- start_money -16
  print(paste0('your current result ', start_money))
  print('--------------------------------------------')

}




# 
# i <- 1
# 
# 
# 
# 
# 
# table(tradek$win)
# 
# 
# 
# 
# 
library(plotly)
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)
p <- plot_ly(data = adat) %>%
  add_lines(x = ~date, y = ~close, name = "slope of 10") %>%
  add_lines(x = ~date, y = ~run_mean_ema_signal, name = "slope of 1", yaxis = "y2") %>%
  layout(
    title = "Double Y Axis", yaxis2 = ay,
    xaxis = list(title="x")
  )

p

# x <- replace(x, seq_along(x) >= which(r < cummax(r))[1], NA)
# x
# 
# # 
# # t[[paste0('ma_', ema_values[1], '_exp_value')]] > t[[paste0('ma_', ema_values[2], '_exp_value')]]  &
# # t[[paste0('ma_', ema_values[2], '_exp_value')]] > t[[paste0('ma_', ema_values[3], '_exp_value')]] 
# # 
# # 
# # > t[[paste0('ma_', ema_values[4], '_exp_value')]]
# 
# 
# t[]
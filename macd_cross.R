library(data.table)
library(rtsdata)
library(TTR)
library(pracma)


#get_one_ticker  <- function(my_ticker, start_date = "1900-01-01", end_date = Sys.Date(),  mas=NULL, emas=c(3, 5, 10, 20, 30), keepma=F, keepema=T, addmacd=T, addrsi=T, remove_nas = T, calc_diff = F, add_macd_cross=T ) {
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

adat <- get_one_ticker('TOT', start_date = '2000-01-01')


adat$start_signal <- adat$macd_cross & adat$rsi<40 
sum(adat$start_signal)




library(plotly)
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)
p <- plot_ly(data = adat) %>%
  add_lines(x = ~date, y = ~macd, name = "slope of 10") %>%
  add_lines(x = ~date, y = ~signal, name = "slope of 1", yaxis = "y2") %>%
  layout(
    title = "Double Y Axis", yaxis2 = ay,
    xaxis = list(title="x")
  )

p

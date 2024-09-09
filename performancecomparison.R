# install.packages('tidyquant')
library(dplyr)
library(tidyquant)
library(stringr)
library(ggplot2)
library(reshape2)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

# ^IXIC : 나스닥, ^GSPC : S&P500

symbols = c('PODD', '^IXIC')
from = "2024-06-01"
to = "2024-09-06"

result.df <- data.frame(Date=as.Date(character()))
filename = ""

getChangePct <- function(df, symbol) {
  pct.df <- data.frame(index=index(df), coredata(df[, 6])) %>%
    mutate(changeRate=((.[, 2]/first(.[, 2]) - 1)))
  
  pct.df$changeRate <- round(pct.df$changeRate, 2)
  colnames(pct.df) <- c('date', 'adjusted', symbol)
  return (pct.df[, c(1, 3)])
}

for(symbol in symbols) {
  getSymbols(symbol, from = from,
             to = to,warnings = FALSE,
             auto.assign = TRUE)
  symbol <- str_replace(symbol, fixed("^"), "")
  filename <- paste0(filename, symbol, "_")
  if(nrow(result.df) == 0 || symbol %in% colnames(result.df)) {
    result.df <- getChangePct(get(symbol), symbol)
  } else {
    result.df <- left_join(result.df, getChangePct(get(symbol), symbol), by='date')
  }
}

write.csv(result.df, fileEncoding = 'UTF-8', row.names = F,
          file=paste0(filename, "from", from, "to", to, ".csv"))

colnames(result.df) <- ifelse(colnames(result.df) %in% c('GSPC', '^GSPC'), 'S&P500', colnames(result.df))
symbols <- ifelse(symbols %in% c('GSPC', '^GSPC'), 'S&P500', symbols)
colnames(result.df) <- ifelse(colnames(result.df) %in% c('IXIC', '^IXIC'), '나스닥', colnames(result.df))
symbols <- ifelse(symbols %in% c('IXIC', '^IXIC'), '나스닥', symbols)

result.long <- melt(result.df, id="date", measure = str_replace(symbols, fixed("^"), ""))

getTitle <- function (symbols) {
  title <- ""
  for(symbol in symbols) {
    if(symbols[length(symbols)] != symbol) {
      title <- paste0(title, getIndexName(symbol), ", ")
    } else {
      title <- paste0(title, getIndexName(symbol), "의 상대 주가 흐름")
    }
  }
  return(title)
}

title <- getTitle(str_replace(symbols, fixed("^"), ""))

result.long %>%
  ggplot(aes(x = date, y = value, colour = variable)) + geom_line(size = 1.25) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  xlab("날짜") + ylab('수익률') + 
  labs(title = title, colour="티커") + 
  theme_light() + theme(legend.position = "top", legend.justification = c(0, 0))


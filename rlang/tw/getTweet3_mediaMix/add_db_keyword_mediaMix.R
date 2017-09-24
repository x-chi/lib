wd1 <- "/home/aaaa/Documents/R_/"
wd2 <- paste0(wd1,"getTweet3_mediaMix") 

setwd( wd2 )






setwd( wd2 )

#httrライブラリの読み込み
library(tidyverse)
library(RSQLite)
library(DBI)
library(readr)


dat1 <- read_tsv("tw_keywords.tsv", col_names = TRUE, col_types = NULL,
  locale = default_locale(), na = c("", "NA"), quoted_na = TRUE,
 )

dat1$time_stamp <- NA
dat1$start_time_stamp <-  format(Sys.time(), format="%Y-%m-%d %H:%M:%S")




dbname="tw_keywords1_mediaMix.db"
driver=dbDriver("SQLite")
con=dbConnect(driver,dbname)




i5=0
for ( i5 in seq( length(dat1$keyword ) )   ){

	tryCatch({
	rs=dbWriteTable(con,"search_keyword",dat1[i5,],
		row.names=F,append = TRUE)

	},
	error=function(e){
		cat("データ挿入時のエラー")
	}
	)

}# for owari


dbDisconnect(con)

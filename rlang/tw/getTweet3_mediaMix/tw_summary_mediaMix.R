wd1 <- "/home/aaaa/Documents/R_/"
wd2 <- paste0(wd1,"getTweet3_mediaMix") 

loadDB1 <- "tw_search_keyword_mediaMix.db"



setwd( wd2 )





#httrライブラリの読み込み
library(httr)
library(tidyverse)
library(RSQLite)
library(DBI)
library(rtweet)
library(stringr)





setwd( wd2 )
dbname <- loadDB1
driver=dbDriver("SQLite")
con1 =dbConnect(driver,dbname)


data1 <- dbGetQuery(con1, "SELECT * FROM tw")

dbDisconnect(con1)






setwd( wd2 )
dbname <- "tw_keywords1_mediaMix.db"
driver=dbDriver("SQLite")
con2 =dbConnect(driver,dbname)
lookupTable1 <- dbGetQuery(con2, "SELECT * FROM search_keyword")
dbDisconnect(con2)

data3 <- dplyr::left_join( data1 ,lookupTable1,by="keyword" )


data4 <- data3 %>%
filter(keyword_group=="実写映画メディアミックス") %>% 
select( created_at ,text,keyword,screen_name,status_id,is_retweet) %>% 
arrange(created_at,text)


data4$deleteFlag <- 0


ngWord_vec1 <- c("メディアミックスα")


for( i4 in seq(ngWord_vec1) ){


for( i5 in seq(data4$keyword) ){
	ifelse(	str_detect(data4$text[i5], ngWord_vec1[i4] )==TRUE,
	data4$deleteFlag[i5] <- 1,
	data4$deleteFlag[i5] <- data4$deleteFlag[i5])
	}
}


data4 <- data4 %>%
filter(deleteFlag!="1")


data4$url <- paste0( "https://twitter.com/",data4$screen_name,"/status/",data4$status_id )




write_tsv(data4,
	paste0(wd2,"/","output/nvsovno.tsv"),
	 na = "NA", append = FALSE, col_names = T)




str1 <- "<html><head></head><body>"



for( i5 in seq(data4$keyword) ){

	str1 <- 
	paste0( str1,data4$text[i5] ,'<a href="',
		data4$url[i5] ,'" target="_blank">',"リンク",'</a><br><br>'
	)

}

	str1 <- 
	paste0( str1,
		"</body></html>"
	)






setwd( paste0( wd2,"/output"  ) )

write(str1, file = "data.html",
		sep=" ")




#考えないといけないこと
# →配信タイミング
# →一日1回で問題なし？？

# →過去何日間のデータが必要？
# →運用し始めると、どれくらい堅牢性が必要か？


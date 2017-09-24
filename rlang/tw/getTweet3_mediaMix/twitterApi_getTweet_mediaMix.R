wd1 <- "/home/aaaa/Documents/R_/"
wd2 <- paste0(wd1,"getTweet3_mediaMix") 

loadKeywordDB1 <- "tw_keywords1_mediaMix.db"
writeTweetDB1 <- "tw_search_keyword_mediaMix.db"



setwd( wd2 )





#httrライブラリの読み込み
library(httr)
library(tidyverse)
library(RSQLite)
library(DBI)
library(rtweet)
library(stringr)








##キーワードを選ぶ作業
  #リストが将来的に大量になる可能性が高いため
  #Rのメモリー上に置かない

setwd( wd2 )
dbname <- loadKeywordDB1
driver=dbDriver("SQLite")
con1 =dbConnect(driver,dbname)


#NULL値のものを取得
#＜参考＞dbReadTable(con1, "search_keyword")
timestampNull_1 <- dbGetQuery(con1, "SELECT * FROM search_keyword WHERE time_stamp IS NULL")
#<参考>これなら動く aa <- dbGetQuery(con1, "SELECT * FROM search_keyword WHERE keyword = '逃げ恥'")



#NULL値のものに加え、time_stamp が古いものを優先
keywords1 <- dbGetQuery(con1,
	"SELECT * FROM search_keyword WHERE time_stamp >0   order by time_stamp limit 300")
dbDisconnect(con1)



keywords2 <- rbind(timestampNull_1,keywords1)




#SQlite用のエスケープ

keywords2_escape <- keywords2

# for( i7 in  seq(keywords2$keyword)  ){ 
# 	keywords2_escape$keyword[i7] <- str_replace_all( keywords2_escape$keyword[i7],"'","''" )
# }





twGet1 <- function( keywords2=keywords2){

	# キーワード を取得するループ
	for( i8 in  1: length(keywords2$keyword)    ){

	setwd(  paste0(wd2,"/OAuth_")  )
		temp1 <- "TwitterOAuth2.Oauth"
	load( temp1 )


	# api 15 分間に 180 回まで
	# 検索結果を取得するAPIに対する1時間あたりの回数制限。IPアドレス単位でカウントされるらしいです
	tw1 <- search_tweets(q = URLencode(iconv( keywords2$keyword[i8] ,to = "UTF-8")),n = 10000, include_rts = TRUE)





#別ファイルで、
#これを使って、特定のユーザーのツイートを
#延々と拾っていくように改造する
#	tw1 <- get_timeline("Yuji_48", n = 100, max_id = NULL, home = FALSE, parse = TRUE,
#check = TRUE, usr = TRUE)





	keywords2_escape_str <- str_replace_all( keywords2$keyword[i8],"'","''" )




str(tw1)
cat(  length(tw1$user_id[1])  )


if( is.na(tw1$user_id[1]) ==FALSE  ){



	tw1$created_at <-  format( (tw1$created_at + 32400 ),
	 format="%Y-%m-%d %H:%M:%S")
	i5=0






	for ( i5 in seq( length(tw1$user_id ) )   ){
	tw1$keyword <- keywords2$keyword[i8]

	#半角スペース区切りで、先頭の単語（漫画名）が
	#含まれているかチェック		
	str2<- str_split(keywords2$keyword[i8]," ")




		#status_id かつ keyword が同じ場合は重複カット
		#（別キーワードで、同一status_idのツイートを拾いたいため）


	if( length(tw1$keyword)!= 0 ){
cat("tw1$keywordが0でないとき","\n") 

		str6 <- paste0(
			'SELECT * FROM tw WHERE keyword=',
			"'",
			keywords2_escape_str,
			"'",
			' and status_id=',
			tw1$status_id[i5]
			)


		setwd( wd2 )
		dbname <- writeTweetDB1
		driver=dbDriver("SQLite")
		con2=dbConnect(driver,dbname)


		distinctCheckStr <- dbGetQuery(con2,str6 )

		dbDisconnect(con2)


cat("distinctCheckStr","\n") 
str(distinctCheckStr)

			flag_distinct_or_na <- 0
			if( is.na(distinctCheckStr$user_id[1] ) == FALSE ){
				flag_distinct_or_na <- 1
			}

		
cat("flag_distinct_or_na→",flag_distinct_or_na,"\n")
cat("tw1$text[i5]→",tw1$text[i5],"\n") 

			tryCatch({

				cat("ddddddd")


				setwd( wd2 )
				dbname <- writeTweetDB1
				driver=dbDriver("SQLite")
				con2=dbConnect(driver,dbname)


				flag_match_str <- str_detect(tw1$text[i5] , pattern= str2[[1]][1]	 )
				cat("flag_match_str→",flag_match_str,"\n")

				if(flag_match_str == TRUE && flag_distinct_or_na == 0 ){

					cat("aaaaaa")
					rs=dbWriteTable(con2,"tw",tw1[i5,],
						row.names=F,append = TRUE)
					cat("bbbbbb")

				dbDisconnect(con2)

				}

			},
			error=function(e){
				#cat("データ挿入時のエラー")
			}

			) # tryCatch   owari

  }else{
	cat("ツイート取得の長さが0","\n") 
		}


	}# for i5 owari



}






	keywords2_escape_str <- str_replace_all( keywords2$keyword[i8],"'","''" )


	#文字列をtw api検索したあと、dbにタイムスタンプを残す
	now_unixtime1 <- as.numeric( Sys.time() )

	setwd( wd2 )
	dbname <- loadKeywordDB1
	driver=dbDriver("SQLite")
	con1 =dbConnect(driver,dbname)

	str1 <- paste0(
		"UPDATE search_keyword SET time_stamp = '",
		now_unixtime1,"'",
		" WHERE  keyword = '",keywords2_escape_str,"'"
	)

	keywords1 <- dbGetQuery(con1,str1)
	dbDisconnect(con1)


	} # for i8 owari




} # twGet1 owari



aa <- twGet1( keywords2 )




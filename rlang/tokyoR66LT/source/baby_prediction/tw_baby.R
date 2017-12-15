library(tidyr)
library(dplyr)
library(RSQLite)
library(DBI)
library(readr)
library(httr)
library(rtweet)
library(stringr)



#バッチ処理ではこれは使えない
#Rファイルを置いている場所から、保存場所を決定
#currentfileDir <- (function() {
#	path <- (function() attr(body(sys.function(-1)), "srcfile"))()$filename
#	dirname(path)
#})()



#ファイルとして実行するまで下記で代用
currentfileDir <-  "/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction"
wd2 <- currentfileDir



setwd( currentfileDir )









#キーワードでツイートを取得し、
#DBへ蓄積

setwd( paste0(currentfileDir,"/","add_keyword") )

dat1 <- read_tsv("tw_keywords.tsv", col_names = TRUE, col_types = NULL,
	locale = default_locale(), na = c("", "NA"), quoted_na = TRUE,
	)

dat1$time_stamp <- NA
dat1$start_time_stamp <-  format(Sys.time(), format="%Y-%m-%d %H:%M:%S")




setwd( currentfileDir )
dbname="tw_search_keyword_baby2.db"
driver=dbDriver("SQLite")
con=dbConnect(driver,dbname)



i5=0
for ( i5 in seq( dat1$keyword )   ){

	tryCatch({
		rs=dbWriteTable(con,"search_keyword",dat1[i5,],
			row.names=F,append = TRUE)

	},
	error=function(e){
		cat("データ挿入時のエラー_キーワード")
	}
	)

}# for owari


dbDisconnect(con)













#httrライブラリの読み込み
library(httr)
library(RSQLite)
library(DBI)
library(rtweet)
library(stringr)















##キーワードを選ぶ作業
  #リストが将来的に大量になる可能性が高いため
  #Rのメモリー上に置かない



setwd( wd2 )
loadKeywordDB1 <- "tw_search_keyword_baby2.db"

dbname <- loadKeywordDB1
driver=dbDriver("SQLite")
con1 =dbConnect(driver,dbname)


#NULL値のものを取得
timestampNull_1 <- dbGetQuery(con1, "SELECT * FROM search_keyword WHERE time_stamp IS NULL")



#NULL値のものに加え、time_stamp が古いものを優先
keywords1 <- dbGetQuery(con1,
	"SELECT * FROM search_keyword WHERE time_stamp >0   order by time_stamp limit 300")
dbDisconnect(con1)



keywords2 <- rbind(timestampNull_1,keywords1)















































twGet1 <- function( keywords2=keywords2,currentfileDir,writeTweetDB1, codingTestFlag ){
cat("twGet1開始")


	if(codingTestFlag==1){
#テスト時の引数
		keywords2 = keywords2
		writeTweetDB1 = "twData_search_keyword_baby2.db"
	}



	# キーワード を取得するループ
	for( i8 in  1: length(keywords2$keyword)    ){

		setwd(  paste0(currentfileDir,"/OAuth_")  )
		temp1 <- "TwitterOAuth2.Oauth"
		load( temp1 )


		if(codingTestFlag == 0){

	# api 15 分間に 180 回まで
	# 検索結果を取得するAPIに対する1時間あたりの回数制限。IPアドレス単位でカウントされるらしいです
			tw1 <- search_tweets(q = URLencode(iconv( keywords2$keyword[i8] ,to = "UTF-8")),n = 10000, include_rts = TRUE)


		}else{
			tw1 <- search_tweets(q = URLencode(iconv( keywords2$keyword[i8] ,to = "UTF-8")),n = 1, include_rts = TRUE)
		}


		twUserData1 <- users_data(tw1)


		twUserData2 <- twUserData1 %>%
		select(  name,location,description,protected,
			followers_count,friends_count,listed_count,
			favourites_count,utc_offset,time_zone,geo_enabled,
			verified,statuses_count,contributors_enabled,
			is_translator,is_translation_enabled,profile_background_color,
			profile_background_image_url,profile_background_image_url_https,
			profile_background_tile,profile_image_url,profile_image_url_https,
			profile_link_color,profile_sidebar_border_color,profile_sidebar_fill_color,
			profile_text_color,profile_use_background_image,default_profile,
			default_profile_image,profile_banner_url )



		tw1$keyword <- keywords2$keyword[i8]
		tw1 <- cbind(tw1,twUserData2)

		#デバッグ確認用
		tw1_ <<- tw1













		library(stringr)


		tw_select_pregnancyUser <- tw1 %>%
		select(screen_name,user_id,
			text,description,name,
			keyword)  %>%
		distinct( user_id, .keep_all = TRUE )


#NG検索キーワードが入っているもの（ここは追加はほぼないので、
#外部データとして読み込まない）
		tw_select_pregnancyUser$deleteFlag  <-  0
		vc_NG_word <- c("流産","妊娠検査薬","早期流産","切迫流産")

		for( i7  in seq(tw_select_pregnancyUser$screen_name) ){
			ifelse(tw_select_pregnancyUser$keyword[i7] %in% vc_NG_word,
				tw_select_pregnancyUser$deleteFlag <- 1, aaaa <- 1 )
		}




		tw_select_pregnancyUser$priorityValue <- 0 




		for( i7  in seq(tw_select_pregnancyUser$screen_name) ){
			ifelse(tw_select_pregnancyUser$keyword[i7] %in% vc_NG_word,
				tw_select_pregnancyUser$deleteFlag <- 1, aaaa <- 1 )
		}




		tw_select_pregnancyUser$priorityValue <- 0



		setwd( wd2 )
		keyword_baby_decision <- read.table("keyword_baby_decision.tsv", header=T,
			sep = "\t", stringsAsFactors = FALSE )





		profile_ng_keyword1 <- keyword_baby_decision %>%
		select(profile_ng_keyword) %>%
		filter(  profile_ng_keyword =="" )


		fnc1 <- function( arg1 ){
			code1 <- paste0(
				"dat1 <- keyword_baby_decision %>%  select(",arg1,") %>% filter(  ",
				arg1 , " != ",'"" )'
				)

			eval( parse(text = code1
				))
			return(dat1)
		}
		profile_ng_keyword1 <- fnc1("profile_ng_keyword")
		tweet_ng_keyword1 <- fnc1("tweet_ng_keyword")
		ng_acount1 <- fnc1("ng_acount")

		txt_importance_keyword1 <- fnc1("txt_importance_keyword")
		profile_importance_keyword1 <- fnc1("profile_importance_keyword")














#プロフィールなどにNGの文言が入っている場合には
#削除のフラグを立てる
#（調整しながら、実際に使えるか確かめてみる。
# 使えなそうなら、常に0を入れるようにする	 ）

		tweet_ng_keyword1 <- tweet_ng_keyword1$tweet_ng_keyword
		ng_acount1 <- ng_acount1$ng_acount



		for( i6 in  seq(tw_select_pregnancyUser[,1]) ){
			for( i5 in  seq(profile_ng_keyword1) ){



				ifelse(  str_detect(tw_select_pregnancyUser$description[i6],  profile_ng_keyword1$profile_ng_keyword[i5]),
					tw_select_pregnancyUser$deleteFlag[i6] <- 1 , aaa <- 1
					)
			}
		}


		for( i6 in  seq(tw_select_pregnancyUser[,1]) ){
			for( i5 in  seq(tweet_ng_keyword1) ){

				ifelse(  str_detect(tw_select_pregnancyUser$text[i6],  tweet_ng_keyword1[i5]),
					tw_select_pregnancyUser$deleteFlag[i6] <- 1 , aaa <- 1
					)
			}
		}



		for( i6 in  seq(tw_select_pregnancyUser[,1]) ){
			for( i5 in  seq(ng_acount1) ){

				ifelse(  tw_select_pregnancyUser$ng_acount1[i6]  == ng_acount1[i5],
					tw_select_pregnancyUser$deleteFlag[i6] <- 1 , aaa <- 1
					)
			}
		}





#ツイートの重要度（より妊婦らしい振る舞いをするアカウント）を示す値を追加する


#ifelseの高速化のために、何も使わない変数を作る →代入
		dmy1 <-""

		for(i3 in seq(tw_select_pregnancyUser[,1]) ){
			for(i2 in seq(profile_importance_keyword1) ){

				str1 <- tw_select_pregnancyUser$description[i3]
				temp_word1 <- profile_importance_keyword1$profile_importance_keyword[i2]
				ifelse(  str_detect(str1,  temp_word1),
					tw_select_pregnancyUser$priorityValue[i3] <-  tw_select_pregnancyUser$priorityValue[i3] + 1 ,
					dmy1 <- 0
					)

			}
		}


		for(i3 in seq(tw_select_pregnancyUser[,1]) ){
			for(i2 in seq(txt_importance_keyword1) ){

				str1 <- tw_select_pregnancyUser$text[i3]
				temp_word1 <- txt_importance_keyword1$txt_importance_keyword[i2]
				ifelse(  str_detect( str1 ,  temp_word1  ),
					tw_select_pregnancyUser$priorityValue[i3] <-  tw_select_pregnancyUser$priorityValue[i3] + 1 ,
					dmy1 <- 0
					)

			}
		}




		start_time_stamp <-  format(Sys.time(), format="%Y-%m-%d %H:%M:%S")



		tw_select_pregnancyUser2 <- tw_select_pregnancyUser %>%
		select(
			screen_name,
			user_id,
			description,
			name,
			deleteFlag,
			priorityValue
			) %>%
		mutate(
			time_stamp="",
			start_time_stamp= start_time_stamp,
			name_past="",
			birth_1="",
			birth_2="",
			birth_3="",
			birth_4="",
			birth_5="",
			pregnancyFlag="",
			Expected_date_of_birth=""
			)




# 子どもが2名目（複数）の人の出産タイミングはどう扱うか？
# ↑子どもの出産タイミングが後からわかることもある
# DBの持ち方として、5人目までの子どもの誕生日（誕生月）を格納しておく方向にする



#ユーザーの情報をDBへ格納していく(各種ユーザー情報の更新も兼ねる)
#重複したときの処理も書くこと




		for ( i5 in seq( tw_select_pregnancyUser2$user_id )   ){




			if(currentfileDir == "/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction"){
#x201 の場合

				dbname <- "tw_search_acount_baby2.db"


				tryCatch({

					setwd( wd2 )
					driver=dbDriver("SQLite")
					con4=dbConnect(driver,dbname)
					pre_existing_acount <- dbGetQuery(con4, "select distinct user_id from search_acount")


					existing_acount <- pre_existing_acount$user_id
					if(tw_select_pregnancyUser2$user_id[i5] %in% existing_acount ){


			#DBから同じuser_idを探してきて、ユーザー名を分解、ユーザー名を追加、
			#priorityValue を追加する 
						userId1 <- tw_select_pregnancyUser2$user_id[i5]

						sql1 <- paste0( "select * FROM search_acount WHERE user_id=", userId1 ,";" )
						temp_db_data1 <- dbGetQuery(con4, sql1)


				#user_idでユニークかけているため、
				#1行目だけ指定で問題なし
						rowNum1 <- temp_db_data1$rowNum[1] 
						priorityValue_db1 <- temp_db_data1$priorityValue[1] 

						priorityValue_input <- priorityValue_db1 + tw_select_pregnancyUser2$priorityValue[i5]
						sql2 <- paste0(  "update search_acount set priorityValue = ", priorityValue_input ,
							" where rowNum =", rowNum1 , ";"
							) 
						dbSendQuery(con4,sql2 ) 

						dateStr1<- format(( Sys.time() ),"%Y%m%d" )

			#description が更新されていたら入れる
			#name が更新されていたら入れる
			#screen_name が更新されていたら入れる
			#yyyy-mm-dd で日付入れてから中身を更新する
			# 区切りがしやすいように独特の区切り文字を使う sep_απΣ


						if(temp_db_data1$screen_name[1] != tw_select_pregnancyUser2$screen_name[i5] ){

							value1 = paste0(temp_db_data1$screen_name[1],"Σω",dateStr1,"_",tw_select_pregnancyUser2$screen_name[i5])
							sql2 <- paste0(  "update search_acount set screen_name = ", value1 ,
								" where rowNum =", rowNum1 , ";"
								) 
							dbSendQuery(con4,sql2 ) 
						}

						if(temp_db_data1$description[1] != tw_select_pregnancyUser2$description[i5] ){

							value1 = paste0(temp_db_data1$description[1],"Σω",dateStr1,"_",tw_select_pregnancyUser2$description[i5])
							sql2 <- paste0(  "update search_acount set description = ", value1 ,
								" where rowNum =", rowNum1 , ";"
								) 
							dbSendQuery(con4,sql2 ) 
						}

						if(temp_db_data1$name[1] != tw_select_pregnancyUser2$name[i5] ){

							value1 = paste0(temp_db_data1$name[1],"Σω",dateStr1,"_",tw_select_pregnancyUser2$name[i5])
							sql2 <- paste0(  "update search_acount set name = ", value1 ,
								" where rowNum =", rowNum1 , ";"
								) 
							dbSendQuery(con4,sql2 ) 
						}


					}else{


						rs=dbWriteTable(con4,"search_acount",tw_select_pregnancyUser2[i5,],
							row.names=F,append = TRUE)



					}





					rs=dbWriteTable(con2,"tw",tw1[i5,],
						row.names=F,append = TRUE)

					dbDisconnect(con2)

				},
				error=function(e){
				#cat("データ挿入時のエラー")
				}

				) # tryCatch   owari





			}else{
#R500 の場合


			}


		}




















		tryCatch({

			# cat("ddddddd")


			setwd( wd2 )
			dbname <- writeTweetDB1
			driver=dbDriver("SQLite")
			con2=dbConnect(driver,dbname)


			flag_match_str <- str_detect(tw1$text[i5] , pattern= str2[[1]][1]	 )
			# cat("flag_match_str→",flag_match_str,"\n")

			if(flag_match_str == TRUE && flag_distinct_or_na == 0 ){

				# cat("aaaaaa")
				rs=dbWriteTable(con2,"tw",tw1[i5,],
					row.names=F,append = TRUE)
				# cat("bbbbbb")

				dbDisconnect(con2)

			}

		},
		error=function(e){
				#cat("データ挿入時のエラー")
		}

		) # tryCatch   owari








#検索キーワードを参照するDBへの処理

		keywords2_escape_str <- str_replace_all( keywords2$keyword[i8],"'","''" )




		# str(tw1)
		# cat(  length(tw1$user_id[1])  )


		if( is.na(tw1$user_id[1]) ==FALSE  ){



			tw1$created_at <-  format( (tw1$created_at + 32400 ),
				format="%Y-%m-%d %H:%M:%S")
			i5=0






			for ( i5 in seq( tw1$user_id )   ){
				tw1$keyword <- keywords2$keyword[i8]

    #複数キーワードの場合、最初の単語は必須にする
    #仕様が入っています
	#半角スペース区切りで、先頭の単語（漫画名）が
	#含まれているかチェック		
				str2<- str_split(keywords2$keyword[i8]," ")


		#status_id かつ keyword が同じ場合は重複カット
		#（別キーワードで、同一status_idのツイートを拾いたいため）
		#status_idはtwitter apiから出力されるデータ

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


					# cat("flag_distinct_or_na→",flag_distinct_or_na,"\n")
					# cat("tw1$text[i5]→",tw1$text[i5],"\n") 

					tryCatch({

						# cat("ddddddd")


						setwd( wd2 )
						dbname <- writeTweetDB1
						driver=dbDriver("SQLite")
						con2=dbConnect(driver,dbname)


						flag_match_str <- str_detect(tw1$text[i5] , pattern= str2[[1]][1]	 )
						# cat("flag_match_str→",flag_match_str,"\n")

						if(flag_match_str == TRUE && flag_distinct_or_na == 0 ){

							# cat("aaaaaa")
							rs=dbWriteTable(con2,"tw",tw1[i5,],
								row.names=F,append = TRUE)
							# cat("bbbbbb")

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



twData1 <- twGet1( keywords2 , currentfileDir,writeTweetDB1 = "twData_search_keyword_baby2.db" ,codingTestFlag=1)









































































##アカウントを選ぶ作業
#※SQLite上でうまく IS NULLが動かなかったため、dplyrで絞る

setwd( wd2 )
load_acountDB1 <- "tw_search_acount_baby2.db"

dbname <- load_acountDB1
driver=dbDriver("SQLite")
con1 =dbConnect(driver,dbname)



temp1 <- dbGetQuery(con1, "SELECT * FROM search_acount")
temp2 <- temp1 %>%
filter(time_stamp=="")




#全ての行にタイムスタンプがあった場合には、
#過去にタイムスタンプを押したものから選ぶ
temp3 <-data.frame()
if(length(temp2$user_id) ==0 ){
	temp3 <- temp1 %>%
	arrange( (time_stamp))
}

acounts2 <- rbind(temp2,temp3)



#エラーを起こして取れないTLをユーザーもいるため、
#ランダムに並べ替えて、膠着してしまうことを防ぐ
acounts2 <- acounts2 %>%
sample_n(size = 10000)


#テスト用に少量だけ試す場合には下記を使用
#   acounts2<- timestampNull_1 %>%
#   head(3)









































acount_tw_Get1 <- function( acounts2=acounts2,currentfileDir,writeTweetDB1, codingTestFlag ){


	if(codingTestFlag==1){
	#テスト時の引数
	}



# 各ユーザーのツイートを取得するループ
	for( i8 in  1: length( acounts2$user_id)    ){


acounts2_debug <<- acounts2
i8_debug <<- i8

		setwd(  paste0(currentfileDir,"/OAuth_")  )
		temp1 <- "TwitterOAuth2.Oauth"
		load( temp1 )


#万が一、screen_nameがNAのときを回避 例：user_id:186552155
if( is.na(acounts2$screen_name[i8]) ==  TRUE){
	next
}





#なぜか取得できないユーザーのタイムラインが存在するため、
#それをあらかじめ除く処理
tryCatch({
	tw1 <-  get_timeline( acounts2$screen_name[i8] , n = 2000,
		max_id = NULL, home = FALSE, parse = TRUE,
		check = TRUE, usr = TRUE)

	},
	error=function(e){
	cat("get_timelineが正常に取得できなかった")
	next
	}

) # tryCatch   owari





tw1 <- data.frame()


		if(codingTestFlag == 0){
	# 検索結果を取得するAPIに対する回数制限があるので注意。IPアドレス単位でカウントされるらしいです 

			tw1 <-  get_timeline( acounts2$screen_name[i8] , n = 2000,
				max_id = NULL, home = FALSE, parse = TRUE,
				check = TRUE, usr = TRUE)

		}else{
			tw1 <-  get_timeline( acounts2$screen_name[i8] , n = 1,
				max_id = NULL, home = FALSE, parse = TRUE,
				check = TRUE, usr = TRUE)
		}




#デバッグ確認用
tw1_debug <<- tw1




#検索結果に何もなかったときには飛ばす処理
if( length(tw1[1,]) == 0){
	next
}




#検索アカウント を参照するDBへの処理


		# str(tw1)
		# cat(  length(tw1$user_id[1])  )


		if( is.na(tw1$user_id[1]) ==FALSE ){


	#twitterの時間と日本の時間で補正
			tw1$created_at <-  format( ( tw1$created_at + 32400 ),
				format="%Y-%m-%d %H:%M:%S")
			i5=0



			setwd( wd2 ) 	
			dbname <- writeTweetDB1 	
			driver=dbDriver("SQLite") 	
			con2=dbConnect(driver,dbname) 	
				
			userAcount_tw1 <- dbReadTable(con2, "userAcount_tw") 	
			vc_1 <- userAcount_tw1$status_id 	
			dbDisconnect(con2) 




			tryCatch({

				#1行ごとにDBへデータを挿入
				for ( i5 in seq( tw1$user_id )   ){

				if( tw1$status_id[i5]  %in% vc_1){
						cat("重複のため、保存なし")
cyouhuku_hozonnasi_debug <<- tw1[i5,]
					}else{

					setwd( wd2 )
					dbname <- writeTweetDB1
					driver=dbDriver("SQLite")
					con2=dbConnect(driver,dbname)
					rs=dbWriteTable(con2,"userAcount_tw",tw1[i5,],
					  row.names=F,append = TRUE)

					dbDisconnect(con2)

				 }

				}

			},
			error=function(e){
			cat("データ挿入時のエラー_userAcount_tw")
			}

			) # tryCatch   owari



fgmfmp  <<- tw1$user_id[1]


#文字列をtw api検索したあと、dbにタイムスタンプを残す
			now_unixtime1 <- as.character( Sys.time()  )


			tryCatch({


			setwd( currentfileDir )
			dbname <- load_acountDB1
			driver=dbDriver("SQLite")
			con1 =dbConnect(driver,dbname)

			str1 <- paste0(
				"UPDATE  search_acount  SET time_stamp = '",
				now_unixtime1,"'",
				" WHERE  user_id = '",tw1$user_id[1],"'"
				)

			keywords1 <- dbExecute(con1,str1)
			dbDisconnect(con1)


				},
				error=function(e){
				cat("データ挿入時のエラー_アカウントタイムスタンプ")
				}

				) # tryCatch   owari




		} 
	}# 

}







twData1 <- acount_tw_Get1( acounts2=acounts2,
	currentfileDir,writeTweetDB1 = "tw_search_acount_baby2.db",
	codingTestFlag=0)















################
################
################
#参考
#SQLiteがトラブルを起こした場合

#【SQLite】"database is locked"というエラーが出たときの原因と対処
#https://qiita.com/fantm21/items/f9711c333b70999f7ab8
#
#  おそらくロックしているだけなら、Rを落とせばロックが解除されるはず



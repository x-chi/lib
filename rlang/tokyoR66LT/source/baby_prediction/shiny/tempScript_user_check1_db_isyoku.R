library(readr)
library(dplyr)
library(tidyr)
library(RSQLite)
library(DBI)
library(httr)
library(rtweet)
library(stringr)



		setwd("/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction/shiny")
		temp_df1 <- read_tsv("user_check1.txt")




#		temp_df1 <- data.frame( user_id  =  uniqueUserid1[i8] ,
#			pregnancyFlag = pregnancyFlag,
#			Expected_date_of_birth = birth_baby,
#			birth_1 = birth_1,
#			birth_2 = birth_2,
#			birth_3 = birth_3,
#			birth_4 = birth_4,
#			birth_5 = birth_5,
#			time_stamp =  format(Sys.time() + 60*60*9, format="%Y-%m-%d %H:%M:%S")
#		)
# shinyのタイムゾーンは簡単に変更できなかったため、9時間足して対応		
# shiny上ではタイムゾーンが日本に設定できない様子
# → https://stackoverflow.com/questions/24842229/how-to-retrieve-the-clients-current-time-and-time-zone-when-using-shiny

# tz="Asia/Tokyo" は機能しない、tz="Japan" は機能しない
# →tz="JST" だとうまく 9時間さらに時間が戻る（別のtz が先に設定されるぽい）
# cat(file=stderr(),"debug=tzは",Sys.timezone(),"\n") で調べるとJapanになる。謎？？






for(  i4  in  seq(temp_df1$user_id)  ){


tryCatch({

		setwd("/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction")
		load_acountDB1 <- "tw_search_acount_baby2.db"
		dbname <- load_acountDB1
		driver=dbDriver("SQLite")
		con1 =dbConnect(driver,dbname)

		str1 <- paste0(
			"UPDATE  search_acount  SET pregnancyFlag = '",
			temp_df1$pregnancyFlag[i4],"'",
			" WHERE  user_id = '",temp_df1$user_id[i4],"'"
			)
		a1 <- dbExecute(con1,str1)


		str1 <- paste0(
			"UPDATE  search_acount  SET Expected_date_of_birth = '",
			temp_df1$Expected_date_of_birth[i4],"'",
			" WHERE  user_id = '",temp_df1$user_id[i4],"'"
			)
		a1 <- dbExecute(con1,str1)



		str1 <- paste0(
			"UPDATE  search_acount  SET birth_1 = '",
			temp_df1$birth_1[i4],"'",
			" WHERE  user_id = '",temp_df1$user_id[i4],"'"
			)
		a1 <- dbExecute(con1,str1)


		str1 <- paste0(
			"UPDATE  search_acount  SET birth_2 = '",
			temp_df1$birth_2[i4],"'",
			" WHERE  user_id = '",temp_df1$user_id[i4],"'"
			)
		a1 <- dbExecute(con1,str1)


		str1 <- paste0(
			"UPDATE  search_acount  SET birth_3 = '",
			temp_df1$birth_3[i4],"'",
			" WHERE  user_id = '",temp_df1$user_id[i4],"'"
			)
		a1 <- dbExecute(con1,str1)


		str1 <- paste0(
			"UPDATE  search_acount  SET birth_4 = '",
			temp_df1$birth_4[i4],"'",
			" WHERE  user_id = '",temp_df1$user_id[i4],"'"
			)
		a1 <- dbExecute(con1,str1)


		str1 <- paste0(
			"UPDATE  search_acount  SET birth_5 = '",
			temp_df1$birth_5[i4],"'",
			" WHERE  user_id = '",temp_df1$user_id[i4],"'"
			)
		a1 <- dbExecute(con1,str1)


		dbDisconnect(con1)


	},
	error=function(e){
	cat("妊娠状態が正しく挿入できなかった")
	}

	) # tryCatch   owari


}



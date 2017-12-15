## 動作環境メモ
## R version 3.4.2 (2017-09-28)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 16.04.3 LTS

library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(

	dashboardHeader(
		title = "妊婦_目視確認"

		),
	dashboardSidebar(

		sidebarMenu(
			menuItem("tweet1", tabName = "tweet1", icon = icon("dashboard")),
			menuItem("tweet2", tabName = "tweet2", icon = icon("dashboard")),
			menuItem("check1", tabName = "check1", icon = icon("dashboard")),

			selectInput("pregnancy", "pregnancy",
				choices = c("未選択","妊娠している", "妊娠していない", "以後無視する"), multiple=FALSE, selectize=TRUE,
				width = '98%'),


			textInput("birth_date", "予定年月日（YYYYMMDD）", value = "", width = "100%", placeholder = NULL),
			textInput("child_birth_date_first", "第一子誕生（YYYYMMDD）", value = "", width = "100%", placeholder = NULL),
			textInput("child_birth_date_second", "第二子誕生（YYYYMMDD）", value = "", width = "100%", placeholder = NULL),
			textInput("child_birth_date_third", "第三子誕生（YYYYMMDD）", value = "", width = "100%", placeholder = NULL),
			textInput("child_birth_4th", "第四子誕生（YYYYMMDD）", value = "", width = "100%", placeholder = NULL),
			textInput("child_birth_5th", "第五子誕生（YYYYMMDD）", value = "", width = "100%", placeholder = NULL),

			textInput("check_acount1", "アカウントチェック", value = "", width = "100%", placeholder = NULL),

			actionButton("button_fix", "決定")
			)

		),
	dashboardBody(
		tabItems(

			tabItem(tabName = "tweet1",
				h4("既存のフラグ"),
				DT::dataTableOutput('table5'),

				h4("ユーザープロフィール"),
				DT::dataTableOutput('table1'),

				h4("ユーザーツイート"),
				DT::dataTableOutput('table2')
				) ,


			tabItem(tabName = "tweet2",
				h4("ユーザープロフィール"),
				DT::dataTableOutput('table3'),

				h4("ユーザーツイート2"),
				DT::dataTableOutput('table4')
				),


			tabItem(tabName = "check1",
				h4("aaaa"),
				DT::dataTableOutput('table6'),

				h4("bbbb"),
				DT::dataTableOutput('table7'),

				h4("cccc"),
				DT::dataTableOutput('table8')
				)

			)
		)

	)


































########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################

server <- function(input, output , session ) {


library(readr)


#デバッグプリントを出力
	cat(file=stderr(),"debug=","server開始\n")










# https://shiny.rstudio.com/articles/action-buttons.html
	v <- reactiveValues(do1 = FALSE)

	observeEvent(input$button_fix, {


		cat(file=stderr(),"button_fix発火" ,"\n" )

		pregnancyFlag  = NA


		check_acount1 <- isolate({ input$check_acount1 })



if(check_acount1 !=""){
		cat(file=stderr(),"check_acount1_開始" ,"\n" )


		dbname="tw_search_acount_baby2.db"
		driver=dbDriver("SQLite")
		con5=dbConnect(driver,dbname)

		temp_tw_df1 <- dbGetQuery(con5,
			paste0("SELECT * FROM userAcount_tw WHERE screen_name ='",check_acount1,"'")
			)

		dbDisconnect(con5)

		temp_tw_df1 <- temp_tw_df1 %>%
		select( screen_name,created_at,text)





#ユーザーのプロフィールを取得
		dbname="tw_search_acount_baby2.db"
		driver=dbDriver("SQLite")
		con2=dbConnect(driver,dbname)

		user_profile1 <- dbGetQuery(con2,
			paste0("SELECT * FROM search_acount WHERE screen_name = '", check_acount1,"'"
				)
			)

		dbDisconnect(con2)


		user_profile2 <- user_profile1 %>%
		select(screen_name,description,name,deleteFlag,priorityValue)

		user_profile3 <- user_profile1 %>%
		select(pregnancyFlag,Expected_date_of_birth,birth_1,birth_2,birth_3,birth_4,birth_5 )


		observe({
			output$table6 <- renderDataTable({user_profile3},options = list(lengthChange = FALSE))
			output$table7 <- renderDataTable({user_profile2},options = list(lengthChange = FALSE))
			output$table8 <- renderDataTable({temp_tw_df1},options = list(pageLength = 200))
		})


























}else{



			answer1 <- isolate({ input$pregnancy   })


			if(answer1 =="妊娠している"){
				pregnancyFlag  = 1

			}
			if(answer1 =="妊娠していない"){
				pregnancyFlag  = -1

			}
			if(answer1 =="不明"){
				pregnancyFlag  = NA
			}
			if(answer1 =="以後無視する"){
				pregnancyFlag  = "ignore_acount"
			}


			birth_baby <- isolate({ input$birth_date   })
			birth_1 <- isolate({ input$child_birth_date_first   })
			birth_2 <- isolate({ input$child_birth_date_second   })
			birth_3 <- isolate({ input$child_birth_date_third   })
			birth_4 <- isolate({ input$child_birth_4th   })
			birth_5 <- isolate({ input$child_birth_5th   })





			if(is.na(pregnancyFlag) ==FALSE){

				temp_df1 <- data.frame( user_id  =  uniqueUserid1,
					pregnancyFlag = pregnancyFlag,
					Expected_date_of_birth = birth_baby,
					birth_1 = birth_1,
					birth_2 = birth_2,
					birth_3 = birth_3,
					birth_4 = birth_4,
					birth_5 = birth_5,
					time_stamp =  format(Sys.time() + 60*60*9, format="%Y-%m-%d %H:%M:%S")
					)
	# shinyのタイムゾーンは簡単に変更できなかったため、9時間足して対応		
	# shiny上ではタイムゾーンが日本に設定できない様子
	# → https://stackoverflow.com/questions/24842229/how-to-retrieve-the-clients-current-time-and-time-zone-when-using-shiny

	# tz="Asia/Tokyo" は機能しない、tz="Japan" は機能しない
	# →tz="JST" だとうまく 9時間さらに時間が戻る（別のtz が先に設定されるぽい）
	# cat(file=stderr(),"debug=tzは",Sys.timezone(),"\n") で調べるとJapanになる。謎？？





				setwd("/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction/shiny")
				user_check1_df <- read_tsv("user_check1.txt")


				user_check1_df <- rbind( user_check1_df, temp_df1 )


				write.table(user_check1_df, file = "user_check1.txt", append = FALSE, quote = FALSE, sep = "\t",
					eol = "\n", na = "NA", dec = ".", row.names = FALSE,
					col.names = TRUE)

				cat(file=stderr(),"書込完了" ,"\n" )








				tryCatch({

					setwd("/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction")
					load_acountDB1 <- "tw_search_acount_baby2.db"
					dbname <- load_acountDB1
					driver=dbDriver("SQLite")
					con1 =dbConnect(driver,dbname)

					str1 <- paste0(
						"UPDATE  search_acount  SET pregnancyFlag = '",
						temp_df1$pregnancyFlag[1],"'",
						" WHERE  user_id = '",temp_df1$user_id[1],"'"
						)
					a1 <- dbExecute(con1,str1)


					str1 <- paste0(
						"UPDATE  search_acount  SET Expected_date_of_birth = '",
						temp_df1$Expected_date_of_birth[1],"'",
						" WHERE  user_id = '",temp_df1$user_id[1],"'"
						)
					a1 <- dbExecute(con1,str1)



					str1 <- paste0(
						"UPDATE  search_acount  SET birth_1 = '",
						temp_df1$birth_1[1],"'",
						" WHERE  user_id = '",temp_df1$user_id[1],"'"
						)
					a1 <- dbExecute(con1,str1)


					str1 <- paste0(
						"UPDATE  search_acount  SET birth_2 = '",
						temp_df1$birth_2[1],"'",
						" WHERE  user_id = '",temp_df1$user_id[1],"'"
						)
					a1 <- dbExecute(con1,str1)


					str1 <- paste0(
						"UPDATE  search_acount  SET birth_3 = '",
						temp_df1$birth_3[1],"'",
						" WHERE  user_id = '",temp_df1$user_id[1],"'"
						)
					a1 <- dbExecute(con1,str1)


					str1 <- paste0(
						"UPDATE  search_acount  SET birth_4 = '",
						temp_df1$birth_4[1],"'",
						" WHERE  user_id = '",temp_df1$user_id[1],"'"
						)
					a1 <- dbExecute(con1,str1)


					str1 <- paste0(
						"UPDATE  search_acount  SET birth_5 = '",
						temp_df1$birth_5[1],"'",
						" WHERE  user_id = '",temp_df1$user_id[1],"'"
						)
					a1 <- dbExecute(con1,str1)


					dbDisconnect(con1)


				},
				error=function(e){
					cat("妊娠状態が正しく挿入できなかった")
				}

				) # tryCatch   owari

		}


		}

	})


















	d<-shiny::reactiveValues()

	library(tidyr)
	library(dplyr)
	library(RSQLite)
	library(DBI)
	library(readr)
	library(DT)



	dir1 <- "/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction"
	setwd(dir1)


	t<-proc.time()


	d<-shiny::reactiveValues()


	output$vTO1<-shiny::renderText({
		d$clicked<-"1"
		str(d$clicked)
		return(d$clicked)
	})






















































	acountSelect  <- function(){



	dir1 <- "/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction"
	setwd(dir1)

		dbname="tw_search_acount_baby2.db"
		driver=dbDriver("SQLite")
		con2=dbConnect(driver,dbname)

		acount1 <- dbGetQuery(con2,
			paste0("SELECT DISTINCT user_id FROM userAcount_tw"
				)
			)

		dbDisconnect(con2)



		acount1 <- acount1 %>%
		sample_n(size=1)


		tw_id1 <- acount1$user_id[1]
		uniqueUserid_1 <- tw_id1







		dbname="tw_search_acount_baby2.db"
		driver=dbDriver("SQLite")
		con5=dbConnect(driver,dbname)

		temp_tw_df1 <- dbGetQuery(con5,
			paste0("SELECT * FROM userAcount_tw WHERE user_id ='",tw_id1,"'")
			)

		dbDisconnect(con5)

		temp_tw_df1 <- temp_tw_df1 %>%
		select( screen_name,created_at,text)








#ユーザーのプロフィールを取得
		dbname="tw_search_acount_baby2.db"
		driver=dbDriver("SQLite")
		con2=dbConnect(driver,dbname)

		user_profile1 <- dbGetQuery(con2,
			paste0("SELECT * FROM search_acount WHERE user_id = '", tw_id1,"'"
				)
			)

		dbDisconnect(con2)


		user_profile2 <- user_profile1 %>%
		select(screen_name,description,name,deleteFlag,priorityValue)


		user_profile3 <- user_profile1 %>%
		select(pregnancyFlag,Expected_date_of_birth,birth_1,birth_2,birth_3,birth_4,birth_5 )


#デバッグプリントを出力
		cat(file=stderr(),"uniqueUserid_1=",uniqueUserid_1,"\n" )


















#関連単語が含まれるツイートのみを表示させる側
		include_keyword_tw <- function(tw_df,db_fileDir){
			library(stringr)


			setwd( db_fileDir )
			dbname="tw_search_keyword_baby2.db"
			driver=dbDriver("SQLite")
			con=dbConnect(driver,dbname)

			keywords1 <- dbGetQuery(con,
				"SELECT * FROM search_keyword")
			dbDisconnect(con)


			vc_kw1 <- keywords1$keyword


#ヒントになりそうな文言を追加
			vc_kw1 <- c(vc_kw1,"予定日","予定",
				#"0w","1w","2w","3w","4w","5w","6w","7w","8w","9w",
				"2018","ヶ月","ヵ月","出産","産まれ","生まれ")

			df_temp1 <- data.frame()

			for( i9 in seq(tw_df$text) ){
				for( i10 in seq(vc_kw1) ){
					if(str_detect( tw_df$text[i9] ,vc_kw1[i10] )){
						df_temp1 <- rbind( df_temp1 ,tw_df[i9,])
					}
				}
			}

			return(df_temp1)
		}


		db_fileDir <- "/home/aaaa/Documents/repo_bitbucket/R_tw/baby_prediction"
		include_keyword_tw1 <- include_keyword_tw(temp_tw_df1,db_fileDir)


		include_keyword_tw1 <- include_keyword_tw1 %>% distinct(created_at,.keep_all = TRUE)





		time1 <- proc.time()-t
#デバッグプリントを出力
		cat(file=stderr(),"経過_", time1 ,"\n" )

		observe({


			output$table5 <- renderDataTable({user_profile3},options = list(lengthChange = FALSE))
			output$table1 <- renderDataTable({user_profile2},options = list(lengthChange = FALSE))
			output$table2 <- renderDataTable({include_keyword_tw1},options = list(pageLength = 200))

			output$table3 <- renderDataTable({user_profile1},options = list(lengthChange = FALSE))
			output$table4 <- renderDataTable({temp_tw_df1},options = list(pageLength = 200))

		})


return(tw_id1)


#鍵ユーザーも取得できるため、注意すること
#このダッシュボードでは、鍵ユーザーも別け隔てなく扱う
	}



uniqueUserid1 <- acountSelect()




}



shinyApp(ui, server)


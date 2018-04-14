##' union of picture,especially adbanner
##'
##' @param df dataframe contains image file path
##' @param textColor assignment of picture file name and picture width,height
##' @param textBoxColor assignment of backgournd color with textColor(param)

##' @param captionType assignment of image caption type,"outside" or "inside".in most case, "inside" is better.
##' @param captionOutside_backgroundColor when the captionType is "outside",assignment of backgroud color of rect 
##' @param captionOutside_rectSize when the captionType is "outside",assignment of size of rect
##' @param caption_textSize when the captionType is "outside",assignment of text size of rect
##' @param imageIdentifier_textColor assignment of color of picture identifier
##' @param imageIdentifier_textBoxColor assignment of backgroud color with imageIdentifier_textColor(param)
##' @param imageIdentifier_textSize assignment of text size of picture identifier

##' @author Kazuyoshi Nakakita
##'
##' @importFrom dplyr select mutate filter  arrange %>% 
##' @importFrom magick image_annotate image_montage image_mosaic image_info image_read image_rotate image_scale image_blank

##' @examples in preparation
##' @examples in preparation
##' @examples in preparation



bannerMerge <- function(
    df,
    textColor="#FFFFFF",
    textBoxColor="#00000070",

    imageIdentifier_textColor="#FFFFFF",
    imageIdentifier_textBoxColor="#000000200",
    imageIdentifier_textSize=30,

    captionType="outside", #inside
    captionOutside_backgroundColor = "#cccccc",
    captionOutside_rectSize = 100,
    caption_textSize=30
    ){


    library(magick)
    library(dplyr)


    #順番を並びかえておく
    imgInfos_temp <- image_info(image = image_read(as.character( df$imgPath) ) )

    imgInfos_temp2 <- imgInfos_temp %>%
    select(width,height,filesize) %>%
    mutate( longSide = ifelse( width > height,"W","H" ) )  %>%
    mutate( whRatio = width / height )
    
    for(  i8  in  seq(imgInfos_temp2) ){
        imgInfos_temp2$group[i8] <- "1_standard"
        imgInfos_temp2$group[i8] <- ifelse( imgInfos_temp2$whRatio[i8] >3 , "2_W" ,imgInfos_temp2$group[i8] ) 
        imgInfos_temp2$group[i8] <- ifelse( imgInfos_temp2$whRatio[i8] <0.33 , "3_H" , imgInfos_temp2$group[i8] ) 
    }


    imgInfos_temp2 <- imgInfos_temp2 %>%
    select(group,filesize)
    
    df2 <- cbind( df,imgInfos_temp2 ) %>%
    arrange(group, desc(filesize) )  %>%
    select(-group, -filesize)

    df2_debug <<- df2




    imgName <- data.frame( 
        imgName = sub( as.character( df2$imgPath) ,pattern="^.*/",replacement=""),
        stringsAsFactors= FALSE        
        )

    imgs <- NA
    imgs <- image_read(as.character( df2$imgPath) )
    imgInfos <- image_info(image = imgs)

    imgInfos <- cbind(imgInfos, imgName )



    #識別子をつける
    imgInfos$imageIdentifier <- paste0("-",1:nrow(imgInfos),"-")


    imgInfos$longSide <- ifelse( imgInfos$width > imgInfos$height,"W","H" )
    imgInfos$longSideSize <- ifelse( imgInfos$width > imgInfos$height,imgInfos$width ,imgInfos$height )
    imgInfos$whRatio <- imgInfos$width/imgInfos$height

    #対角線の長さを求め、その平均価も出しておく
    imgInfos$diagonal1 <- sqrt(imgInfos$width^2 + imgInfos$height^2 )
    ave_diagonal <- ave(imgInfos$diagonal1 )[1]








    if(captionType=="inside"){

    #画像名と、画像サイズを入れる
    for( i9 in  seq(imgs)){

        sizeCaptionValue <- paste0( "W:",imgInfos$width[i9]," H:",imgInfos$height[i9] ) 

        if(imgInfos$longSide[i9] == "W"){

            imgs[i9] <- imgs[i9] %>% 
            image_annotate(., sizeCaptionValue  ,size= caption_textSize * imgInfos$diagonal1[i9] /ave_diagonal,
                gravity='northeast',color=textColor,
                boxcolor = textBoxColor )  %>%

            image_annotate(.,imgInfos$imgName[i9],size= caption_textSize *  imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='southwest',color=textColor,
                boxcolor = textBoxColor )  %>% 

            image_annotate(.,imgInfos$imageIdentifier[i9],size= imageIdentifier_textSize *  imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='northwest',color=imageIdentifier_textColor,
                boxcolor = imageIdentifier_textBoxColor )
        }

        if(imgInfos$longSide[i9] == "H"){
            imgs[i9] <- image_rotate(imgs[i9], 90)

            imgs[i9] <- imgs[i9] %>% 
            image_annotate(.,sizeCaptionValue ,size= caption_textSize * imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='southeast',color=textColor,
                boxcolor = textBoxColor ) %>%

            image_annotate(.,imgInfos$imgName[i9],size= caption_textSize * imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='northwest',color=textColor,
                boxcolor = textBoxColor ) 

            imgs[i9] <- image_rotate(imgs[i9], 270)

            imgs[i9] <- imgs[i9] %>%
            image_annotate(.,imgInfos$imageIdentifier[i9],
                size= imageIdentifier_textSize *  imgInfos$diagonal1[i9] / ave_diagonal ,
                gravity='northwest',color=imageIdentifier_textColor,
                boxcolor = imageIdentifier_textBoxColor )
        }
    }


    imgs2 <- imgs
    for( i2  in  seq(imgs2) ){
        info_x <- image_info(imgs2[i2] )
        info_x$longSide <- ifelse( info_x$width > info_x$height,"W","H" )
        info_x$longSideSize <- ifelse( info_x$width > info_x$height,info_x$width ,info_x$height )
        info_x$diagonal1 <- sqrt(info_x$width^2 + info_x$height^2 )

        if(info_x$longSide == "W"){
            w1 <- info_x$width * ave_diagonal / info_x$diagonal1
            imgs2[i2] <- image_scale(imgs2[i2],geometry = w1)
        }

        if(info_x$longSide == "H"){
            h1 <- info_x$height * ave_diagonal / info_x$diagonal1
            imgs2[i2] <- image_scale(imgs2[i2],geometry =  paste0("x",h1 ) )
        }
    }

    montage2 <- image_montage(imgs2)
    }




if(captionType=="outside"){

    #画像名と、画像サイズを入れる
    i9 <- 0
    for( i9 in  seq(imgs)){


        if(imgInfos$longSide[i9] == "W"){
            #背景を追加する
            bg1 <- NA

            bg1 <- image_blank(
                width = imgInfos$width[i9],
                height = imgInfos$height[i9] + captionOutside_rectSize * imgInfos$diagonal1[i9] / ave_diagonal,
                color = captionOutside_backgroundColor )
            
            imgs[i9] <- image_mosaic( c(bg1,imgs[i9]) )



            location1 <- paste0("+0+",imgInfos$height[i9])
            location2 <- paste0("+0+", imgInfos$height[i9] + captionOutside_rectSize * imgInfos$diagonal1[i9] / ave_diagonal /3)
            location3 <- paste0("+0+", imgInfos$height[i9] + captionOutside_rectSize * imgInfos$diagonal1[i9] / ave_diagonal /3 *2)


            imgs[i9] <- imgs[i9] %>% 
            image_annotate(.,imgInfos$imageIdentifier[i9],size= imageIdentifier_textSize * imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='northwest',color=imageIdentifier_textColor,
                boxcolor = imageIdentifier_textBoxColor,
                location = location1 )  %>%

            image_annotate(., sizeCaptionValue  ,size= caption_textSize * imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='northwest',color=textColor,
                boxcolor = textBoxColor,
                location = location2 )  %>%

            image_annotate(.,imgInfos$imgName[i9],size= caption_textSize * imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='northwest',color=textColor,
                boxcolor = textBoxColor,
                location = location3 )
        }


        if(imgInfos$longSide[i9] == "H"){
            #背景を追加する
            bg1 <- image_blank(
                width = imgInfos$width[i9]+ captionOutside_rectSize * imgInfos$diagonal1[i9] / ave_diagonal,
                height = imgInfos$height[i9],
                color = captionOutside_backgroundColor )
            
            imgs[i9] <- image_mosaic( c(bg1,imgs[i9]) )


            imgs[i9] <- image_rotate(imgs[i9], 270)


            location1 <- paste0("+0+","0")
            location2 <- paste0("+0+", captionOutside_rectSize * imgInfos$diagonal1[i9] / ave_diagonal /3)
            location3 <- paste0("+0+", captionOutside_rectSize * imgInfos$diagonal1[i9] / ave_diagonal /3 *2)


            imgs[i9] <- imgs[i9] %>% 

            image_annotate(.,imgInfos$imageIdentifier[i9],size= imageIdentifier_textSize * imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='northwest',color=imageIdentifier_textColor,
                boxcolor = imageIdentifier_textBoxColor,
                location = location1 )  %>%


            image_annotate(.,imgInfos$imgName[i9],size= caption_textSize * imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='northwest',color=textColor,
                boxcolor = textBoxColor,
                location = location2 )  %>%

            image_annotate(., sizeCaptionValue  ,size= caption_textSize * imgInfos$diagonal1[i9] / ave_diagonal,
                gravity='northwest',color=textColor,
                boxcolor = textBoxColor,
                location = location3 )


            imgs[i9] <- image_rotate(imgs[i9], 90)
        }
    }


    imgs2 <- imgs
    for( i2  in  seq(imgs2) ){
        info_x <- image_info(imgs2[i2] )
        info_x$longSide <- ifelse( info_x$width > info_x$height,"W","H" )
        info_x$longSideSize <- ifelse( info_x$width > info_x$height,info_x$width ,info_x$height )
        info_x$diagonal1 <- sqrt(info_x$width^2 + info_x$height^2 )

        if(info_x$longSide == "W"){
            w1 <- info_x$width * ave_diagonal / info_x$diagonal1
            imgs2[i2] <- image_scale(imgs2[i2],geometry = w1)
        }

        if(info_x$longSide == "H"){
            h1 <- info_x$height * ave_diagonal / info_x$diagonal1
            imgs2[i2] <- image_scale(imgs2[i2],geometry =  paste0("x",h1 ) )
        }
    }



    montage2 <- image_montage(imgs2)
    }


return(montage2)
}

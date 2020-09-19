library(tidyverse)
library(jsonlite)
library(stringr)
library("jpeg")
library(tidyr)
library(utf8)
library(rvest)

#install.packages("jsonlite")

#devtools::install_github("hadley/emo")
#---------------------------------------------------------
#Download JSON File from Instagram for a specific Hashtag
#---------------------------------------------------------
#fromJSON()
hashtag <- "bbnaija"
url_start <- str_glue("http://instagram.com/explore/tags/{hashtag}/?__a=1")
json <- jsonlite::fromJSON(url_start)
edge_hashtag_to_media <- json$graphql$hashtag$edge_hashtag_to_media
end_cursor <- edge_hashtag_to_media$page_info$end_cursor
posts <- edge_hashtag_to_media$edges$node

fromJSON(url_start)
#-----------------------------
#Extract Information per Post
#-----------------------------
index <- 1
post_id <- list()
post_url <- list()
post_text <- list()
post_time <- list()
post_likes <- list()
post_owner <- list()
post_img_url <- list()

extractInfo <- function(index){
  print("extractInfo function called")
  maxrows <- nrow(posts)
  for(i in 1:maxrows){
    if(i == maxrows){
      assign("index", index, envir = .GlobalEnv)
      assign("post_id", post_id, envir = .GlobalEnv)
      assign("post_text", post_text, envir = .GlobalEnv)
      assign("post_time", post_time, envir = .GlobalEnv)
      assign("post_img_url", post_img_url, envir = .GlobalEnv)
      assign("post_url", post_url, envir = .GlobalEnv)
      assign("post_likes", post_likes, envir = .GlobalEnv)
      assign("post_owner", post_owner, envir = .GlobalEnv)
      getNewPosts(index)
    } else {
      if(length(posts$edge_media_to_caption$edges[[i]][["node"]][["text"]])==0){
        post_text[index] <- "no-text"
        print("no text in post")
      } else {
        temp <- posts$edge_media_to_caption$edges[[i]][["node"]][["text"]]
        post_text[index] <- gsub("\n", " ", temp)
      }
      
      post_id_temp <- posts[i,5]
      post_url[index] <-  str_glue("http://instagram.com/p/{post_id_temp}")
      post_id[index] <- post_id_temp
      post_time[index] <- toString(as.POSIXct(posts[i,7], origin="1970-01-01"))
      post_img_url[index] <- posts[i,9]
      post_likes[index] <- posts[i,11]
      post_owner[index] <- posts[i,12]
      
      #optional: download image
      #img_dir <- str_glue("images/{index}_{hashtag}_post_img.jpg")
      #download.file(posts[i,8], img_dir, mode = 'wb')
      
      index <- index + 1
    }
  }    
}

#------------------------------
#Get New Posts from Instagram
#------------------------------
getNewPosts <- function(index){
  print("getNewPosts function called")
  url_next <- str_glue("{url_start}&max_id={end_cursor}")
  json <- fromJSON(url_next)
  edge_hashtag_to_media <- json$graphql$hashtag$edge_hashtag_to_media
  end_cursor <- edge_hashtag_to_media$page_info$end_cursor
  posts <- edge_hashtag_to_media$edges$node
  assign("end_cursor", end_cursor, envir = .GlobalEnv)
  assign("posts", posts, envir = .GlobalEnv)
  print(index)
  Sys.sleep(1)
  extractInfo(index)
}

#library(R.utils)
#Start the Madness but time out after 2020 seconds

# while(TRUE)
# {
# withTimeout(
#   extractInfo(index),
#   timeout = 10
# )    
#   Sys.sleep(5)          
# }         



getNewPosts(index)

#-----------------------------
#Export Dataframe to CSV()
#-----------------------------
table <- do.call(rbind.data.frame, Map('c', post_id, post_url, post_img_url, post_likes, post_owner, post_text, post_time))
colnames(table) <- c("ID", "Post_URL", "Img_URL", "Likes", "Owner", "Text", "Date")
Date <- Sys.Date()
filename <- str_glue("table-{hashtag}-{Date}.csv")
write.csv(table,filename, fileEncoding = "UTF-8")




#May run first to set TZ
Sys.setenv(TZ="Europe/Berlin")
Sys.getenv("TZ")



# df <- read.csv("C:\\Users\\SIMIYOUNG\\Downloads\\New folder (3)\\Insta.csv")
# 
# 
# dat <- df
# dat[,sapply(dat,is.character)] <- sapply(
#   dat[,sapply(dat,is.character)],
#   iconv,"WINDOWS-1252","UTF-8")
# 
# library(utf8)
# utf8_print("\U+C81C")
# 
# wink_emoji <- "\U0001f609"
# utf8::utf8_print(wink_emoji)
# 
# 
# 
# stringi::stri_extract(text, regex = "[<U+[0-9a-z]>]")
# text
# 
# text <- df[1, 7]
# text[grepl('[\U{1F300}-\U{1F6FF}]', text)]
# 
# text[grepl('[\U{1F300}-\U{1F6FF}]', text)]
#  
# 
# 
# 
# str_detect(text, "<U+>")
# library(tidytext)
# 
# grep("[^\001-\177]", text)
# 
# utf8_print(text) %>% 
#   nchar()
# str_extract(text,"\\[[:alnum:]]{9}")
# 
# #[\u{1F600}-\u{1F6FF}]
# library(tidyverse)
# str_enc 
# 
# 
# text <- as.vector(df$Text)
# 
# Encoding(text) <- "UTF-8"
# 
# which(str_detect(xvect,"[^[:ascii:]]")==T)
# 
# 

library(tidyverse)
library(emo)

emojis <- table %>%
  mutate(emoji = emo::ji_extract_all(Text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE)


#visualize emoji
#Extracts the links to these images
# emoji_to_link <- function(x) {
#   paste0("https://emojipedia.org/emoji/",x) %>% #Enters the page on emojipedia of individual emojis
#     read_html() %>%
#     html_nodes("tr td a") %>% #Read emoji node
#     .[1] %>%
#     html_attr("href") %>%
#     paste0("https://emojipedia.org/", .) %>%
#     read_html() %>%
#     html_node('div[class="vendor-image"] img') %>%
#     html_attr("src")
# }
# 
# 
# link_to_img <- function(x, size = 25) {  #Create link for the images and the geom_rich text converts to images 
#   paste0("<img src='", x, "' width='", size, "'/>") 
# }
# 
# #Apply the above defined functions
# top_happy <- emojis %>%
#   slice(1:10) %>%
#   mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(1))),
#          label = link_to_img(url))
# 
# 
# #Plot emojis
# top_happy %>% 
#   ggplot(., aes(emoji, n, label = label)) +
#   geom_col()+
#   geom_richtext(aes(y = n), fill = NA, label.color = NA, # remove background and outline
#                 label.padding = grid::unit(rep(0, 4), "pt") # remove padding
#   ) +
#   theme_minimal()
# 
# 

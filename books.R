#################################### BEGIN #####################################
setwd("C:/Users/wu yang hong/Desktop")
book_list = read.csv("C:/Users/wu yang hong/Desktop/bookurl.csv")
# library loaded #
function_load = function() {
# library(get)
  library(httr)
  library(rvest)
  library(stringr)
  library(xml2)
# library(data.table)
  library(RCurl)
  library(tidyverse)
# library(parallel)
  library(plyr)
  library(progress)
}
# 執行以上程式 #
function_load()


# 書單 - 前置設定 #
dt = data.frame()
dt2 = data.frame()
recom_list = data.frame()
book_list_temp = data.frame()
i = 1
total_book = nrow(book_list)
pb <- tkProgressBar(title = "ProgressBar",label = "",
                    min = 0,max = total_book,initial = 0,width = 300)
setTkProgressBar(pb,i,label=paste0(round(i/total_book*100,0),"% done"))


# 同時爬下多個產品 #
function_load = function() {
  for (i in book_list[i,1]:book_list[total_book,1]){
    if(book_list[i,1]) {book_url = book_list[i,2]}


# Request: GET - 請求書本面 
  doc_book <- GET(as.character(book_url)) %>% content(encoding = "utf8")


# extract data wanted

# 選定之產品資訊 

# 書本名稱、介紹
  book_name = doc_book %>%
    html_nodes('div.grid_10 > div.mod') %>%
    html_text()
  book_name = book_name[1]
  book_name = strsplit(book_name, '\n')
  book_name = book_name[[1]][2]
  book_name = strsplit(book_name, '：')
  book_intro = book_name[[1]][2]
  book_name = book_name[[1]][1]


# 作者名稱
  book_author = doc_book %>%
    html_nodes('div.type02_p003') %>%
    html_text()
  book_author = strsplit(book_author, '\n')
  book_author = book_author[[1]][11]
  book_author = strsplit(book_author, ' ')
  book_author = book_author[[1]][17]


# 出版日期
  pub_date = doc_book %>%
    html_nodes('div.type02_p003') %>%
    html_text()
  pub_date = strsplit(pub_date, '\n')
  pub_date = pub_date[[1]][13]
  pub_date = strsplit(pub_date, '：')
  pub_date = pub_date[[1]][2]
  pub_date = strsplit(pub_date, '語言')
  pub_date = pub_date[[1]][1]


# 定價
  book_price = doc_book %>%
    html_nodes('div.prod_cont_a') %>%
    html_text()
  book_price = strsplit(book_price, '\n')
  book_price = book_price[[1]][2]
  book_price = strsplit(book_price, '：')
  book_price = book_price[[1]][2]
  book_price = parse_number(book_price)


# 書本分類
  book_type = doc_book %>%
     html_nodes('ul.container_24 li a span') %>%
    html_text()
  book_type = paste(book_type[4],book_type[5],sep = "、")


# 推薦書籍
  recom = doc_book %>%
     html_nodes('div.type02_bd-a h4 a') %>%
     html_attr("href")
  recom = substr(recom,start=1,stop=43)
  recom = data.frame(book_url=recom)

# 合併推薦書籍
  recom_list = rbind(recom_list,recom)
  recom_list = unique(recom_list)
  recom_num = nrow(recom_list)

  book_list_temp = recom_list

# 合併書本
  exdataframe = data.frame(book_name=book_name,book_type=book_type,
                           book_author=book_author,pub_date=pub_date,
                           book_price=book_price,book_intro=book_intro)

  dt = rbind(dt,exdataframe)

  
# 進度表
  for(i in i) {
    setTkProgressBar(pb,i,label=paste(round(i/total_book*100,0),"% done"))
    Sys.sleep(0.1+runif(1,0,0.5))
  }
  }
  close(pb)
}
# 執行以上程式 #
function_load()


# 推薦書單 - 前置設定 #
o = 1
number = c(1:nrow(book_list_temp))
book_list_temp = cbind(number,book_list_temp)
total_book = nrow(book_list_temp)
pb <- tkProgressBar(title = "ProgressBar",label = "",
                    min = 0,max = total_book,initial = 0,width = 300)


# 同時爬下多個產品 #
function_load() {
  for (o in book_list_temp[o,1]:book_list_temp[recom_num,1]) {
    if (book_list_temp[o,1]) {book_url = book_list_temp[o,2]}


#Request: GET - 請求書本面
  doc_book <- GET(as.character(book_url)) %>% content(encoding = "utf8") 
  


# extract data wanted

# 選定之產品資訊

# 書本名稱、介紹
  book_name = doc_book %>%
    html_nodes('div.grid_10 > div.mod') %>%
    html_text()
  book_name = book_name[1] 
  book_name = strsplit(book_name, '\n')
  book_name = book_name[[1]][2]
  book_name = strsplit(book_name, '：')
  book_intro = book_name[[1]][2]
  book_name = book_name[[1]][1]


# 作者名稱
  book_author = doc_book %>%
    html_nodes('div.type02_p003') %>%
    html_text()
  book_author = strsplit(book_author, '\n')
  book_author = book_author[[1]][11]
  book_author = strsplit(book_author, ' ')
  book_author = book_author[[1]][17]


# 出版日期
  pub_date = doc_book %>%
    html_nodes('div.type02_p003') %>%
    html_text()
  pub_date = strsplit(pub_date, '\n')
  pub_date = pub_date[[1]][13]
  pub_date = strsplit(pub_date, '：')
  pub_date = pub_date[[1]][2]
  pub_date = strsplit(pub_date, '語言')
  pub_date = pub_date[[1]][1]


# 定價
  book_price = doc_book %>%
    html_nodes('div.prod_cont_a') %>%
    html_text()
  book_price = strsplit(book_price, '\n')
  book_price = book_price[[1]][2]
  book_price = strsplit(book_price, '：')
  book_price = book_price[[1]][2]
  book_price = parse_number(book_price)


# 書本分類
  book_type = doc_book %>%
    html_nodes('ul.container_24 li a span') %>%
    html_text()
  book_type = paste(book_type[4],book_type[5],sep = "、")

 
# 合併書本
  exdataframe = data.frame(book_name=book_name,book_type=book_type,
                           book_author=book_author,pub_date=pub_date,
                           book_price=book_price,book_intro=book_intro) 
    
  dt2 = rbind(dt2,exdataframe)

    
# 進度表
  for(o in o) {
    setTkProgressBar(pb,o,label=paste0(round(o/total_book*100,0),"% done"))
    Sys.sleep(0.1+runif(1,0,0.5))
  }
  }
  close(pb)
}
# 執行以上程式 #
function_load()
##################################### END #####################################


#http://www.shxyj.org/
###############################################################################
#这个不要运行,特别慢， 
#从67行开始运行，搞了parallel运算

library(rvest)
library(magrittr)
library(stringr)
url <- 'http://www.shxyj.org/Magazine/show/?id=29311'
web <- read_html(url)

title <- web %>% html_nodes("div.C_right div") %>% html_text()
title <- title[1]
neirong <- web %>% html_nodes("div.C_right tr") %>% html_text()
clearn_data <- function(x) {
  x <- strsplit(x, '\r\n')
  x <- str_remove(x[[1]][2],  "                ")
  return(x)
}
zaiyao <- clearn_data(neirong[2])

authors <- clearn_data(neirong[5])

qikan1 <- clearn_data(neirong[9])

qikan2 <- clearn_data(neirong[11])
qikan <- paste0(qikan1, qikan2, collapse = '.')

test <- data.frame(title = title,
           authors = authors,
           issue = qikan,
           abstract = zaiyao)

id <- c(1:30053)
url_head <- 'http://www.shxyj.org/Magazine/show/?id='
resut <- data.frame()
for (id_temp in id) {
  cat(id_temp, '\n')
  url_temp <- paste0(url_head, id_temp)
  web <- read_html(url_temp)
  
  title <- web %>% html_nodes("div.C_right div") %>% html_text()
  title <- title[1]
  
  if (title != "") {
    neirong <- web %>% html_nodes("div.C_right tr") %>% html_text()
    zaiyao <- clearn_data(neirong[2])
    
    authors <- clearn_data(neirong[5])
    
    qikan1 <- clearn_data(neirong[9])
    
    qikan2 <- clearn_data(neirong[11])
    qikan <- paste0(qikan1, qikan2, collapse = '.')
    
    result_temp <- data.frame(title = title,
                              authors = authors,
                              issue = qikan,
                              abstract = zaiyao)
    resut <- rbind(resut, result_temp)
  }
  
}




###############################################################################
#parallel
library(foreach)
library(doParallel)
library(parallel)

url_head <- 'http://www.shxyj.org/Magazine/show/?id='
p_down <- function(id) {
  library(rvest)
  library(magrittr)
  library(stringr)
  
  
  
  
  clearn_data <- function(x) {
    x <- strsplit(x, '\r\n')
    x <- str_remove(x[[1]][2],  "                ")
    return(x)
  }
  
  
  cat(id, '\n')
  url_temp <- paste0(url_head, id)
  web <- read_html(url_temp)
  
  title <- web %>% html_nodes("div.C_right div") %>% html_text()
  title <- title[1]
  
  if (title != "") {
    neirong <- web %>% html_nodes("div.C_right tr") %>% html_text()
    zaiyao <- clearn_data(neirong[2])
    
    authors <- clearn_data(neirong[5])
    
    qikan1 <- clearn_data(neirong[9])
    
    qikan2 <- clearn_data(neirong[11])
    qikan <- paste0(qikan1, qikan2, collapse = '.')
    
    result_temp <- data.frame(title = title,
                              authors = authors,
                              issue = qikan,
                              abstract = zaiyao)
    return(result_temp)
  }
  
}


#前10000
cl <- makeCluster(detectCores())
registerDoParallel(cl)#并行计算
result1 <- foreach(id=c(1:10000),.combine='rbind') %dopar% p_down(id)

stopCluster(cl)#结束并行

#10001- 20000
cl <- makeCluster(detectCores())
registerDoParallel(cl)#并行计算
result2 <- foreach(id=c(10001:20000),.combine='rbind') %dopar% p_down(id)

stopCluster(cl)#结束并行
#20001-30053
cl <- makeCluster(detectCores())
registerDoParallel(cl)#并行计算
result3 <- foreach(id=c(20001:30053),.combine='rbind') %dopar% p_down(id)

stopCluster(cl)#结束并行
library(beepr)
beep(9)

papers_socrs <- rbind(result1, result2, result3)
write.csv(papers_socrs, file = "papers_socrs.csv", row.names = FALSE)

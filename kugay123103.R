library(rvest)
library(magrittr)
library(stringr)

url <- 'http://journal.psych.ac.cn/xlxb/CN/article/showOldVolumn.do'

web <- read_html(url)

neirong <- web %>% 
  html_nodes("a[class=J_WenZhang]") %>% 
  html_attrs()

link <- unlist(lapply(neirong, FUN = function(x){x['href']}))
names(link) <- NULL
link


link <- str_replace(link, pattern = '..', 'http://journal.psych.ac.cn/xlxb/CN')


################################################################################

#url <- 'http://journal.psych.ac.cn/xlxb/CN/volumn/volumn_221.shtml'


paper_link <- c()
for(temp_link in link) {
  cat(temp_link, '\n')
  web <- read_html(temp_link)
  neirong <- web %>% 
    html_nodes("a[class=txt_biaoti]") %>% html_attrs()
  temp_link <- unlist(lapply(neirong, FUN = function(x){x['href']}))
  names(temp_link) <- NULL
  paper_link <- c(paper_link, temp_link)
}

################################################################################
#download paper

#parallel



library(foreach)
library(doParallel)
library(parallel)

p_download <- function(temp_id) {
  
  library(rvest)
  library(magrittr)
  library(stringr)
  
  #Sys.sleep(abs(rnorm(n = 1, mean = 2, sd = 3)))
  library(beepr)
  #beep(1)
  cat(paper_link[temp_id], '--------',temp_id, '\n')
  
  
  
  url <- paper_link[temp_id]
  web <- read_html(url) 
  
  zaiyao <- web %>% html_node("span[class=J_zhaiyao]") %>% html_text()
  zuozhe <- web %>% html_node("td[class=J_author_cn]") %>% html_text()
  biaoti <- web %>% html_node("span[class=J_biaoti]") %>% html_text()
  issue <- web %>% html_nodes('a[class=txt_zhaiyao]') %>% html_text()
  page <- web %>% html_node('span[class=txt_zhaiyao]') %>% html_text()
  issue <- paste(issue[2], issue[3], issue[4], page)
  
  temp_result <- data.frame(title = biaoti,
                              authors = zuozhe,
                              issue = issue,
                              abstract = zaiyao)
  
  #result <- rbind(result, temp_result)
}

#1
cl <- makeCluster(detectCores())
registerDoParallel(cl)#并行计算
result1 <- foreach(id=c(1:500),.combine='rbind') %dopar% p_download(id)

stopCluster(cl)#结束并行

#cl <- makeCluster(detectCores())
#registerDoParallel(cl)#并行计算
#result2 <- foreach(id=c(501:1000),.combine='rbind') %dopar% p_download(id)
#stopCluster(cl)#结束并行
#url <- 'http://journal.psych.ac.cn/xlxb/CN/No.12, 1507-1518'
result2 <- data.frame()
for (temp_id in c(1219:2333)) {  #754\793\808\1000\1207\1218错误
  cat(paper_link[temp_id], '--------',temp_id, '\n')
  temp_result <- tryCatch({
    url <- paper_link[temp_id]
    web <- read_html(url) 
    trouble <<- url
    zaiyao <- web %>% html_node("span[class=J_zhaiyao]") %>% html_text()
    zuozhe <- web %>% html_node("td[class=J_author_cn]") %>% html_text()
    biaoti <- web %>% html_node("span[class=J_biaoti]") %>% html_text()
    issue <- web %>% html_nodes('a[class=txt_zhaiyao]') %>% html_text()
    page <- web %>% html_node('span[class=txt_zhaiyao]') %>% html_text()
    issue <- paste(issue[2], issue[3], issue[4], page)
    
    temp_result <- data.frame(title = biaoti,
                              authors = zuozhe,
                              issue = issue,
                              abstract = zaiyao)
    temp_result
  }, finally = {temp_result = NULL})
  result2 <- rbind(result2, temp_result)
}

result <- rbind(result1, result2)

write.csv(result, file = 'papers_xb.csv', row.names = FALSE)
















library(rvest)
library(magrittr)
library(stringr)
library(purrr)

#url <- 'http://journal.psych.ac.cn/xlxb/CN/article/showOldVolumn.do'
url <- 'http://journal.psych.ac.cn/xlkxjz/CN/article/showOldVolumn.do'

web <- read_html(url)

neirong <- web %>% 
  html_nodes("a[class=J_WenZhang]") %>% 
  html_attrs()

link <- unlist(lapply(neirong, FUN = function(x){x['href']}))
names(link) <- NULL
link


link <- str_replace(link, pattern = '..', 'http://journal.psych.ac.cn/xlkxjz/CN')


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


paper_link <- str_replace(paper_link, ' ', '')   #路径里面不能有空格
paper_link <- unique(paper_link) #去重

################################################################################
#download paper

# 
# result <- data.frame()
# for (temp_id in c(398:2041)) {  
#     cat(paper_link[temp_id], '--------',temp_id, '\n')
#     url <- paper_link[temp_id]
#     web <- safely(read_html, NULL)(url)$result
#     if (!is.null(web)) {
#       zaiyao <- web %>% html_node("span[class=J_zhaiyao]") %>% html_text()
#       zuozhe <- web %>% html_node("td[class=J_author_cn]") %>% html_text()
#       biaoti <- web %>% html_node("span[class=J_biaoti]") %>% html_text()
#       issue <- web %>% html_nodes('a[class=txt_zhaiyao]') %>% html_text()
#       page <- web %>% html_node('span[class=txt_zhaiyao]') %>% html_text()
#       issue <- paste(issue[2], issue[3], issue[4], page)
#       
#       temp_result <- data.frame(title = biaoti,
#                                 authors = zuozhe,
#                                 issue = issue,
#                                 abstract = zaiyao)
#     } else {
#       temp_result <- NULL
#     }
#     
# 
#   result <- rbind(result, temp_result)
# }





################################################################################
#parallel
library(foreach)
library(doParallel)
library(parallel)

p_download <- function(temp_id) {
  
  library(rvest)
  library(magrittr)
  library(stringr)
  
  
  library(purrr)
  
  cat(paper_link[temp_id], '--------',temp_id, '\n')
  
  
  
  url <- paper_link[temp_id]
  web <- safely(read_html, NULL)(url)$result 
  if (!is.null(web)) {
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
  } else {
    temp_result <- NULL
  }
  return(temp_result)
  
  #result <- rbind(result, temp_result)
}

#1
cl <- makeCluster(detectCores())
registerDoParallel(cl)#并行计算
result1 <- foreach(id=c(1:2041), .combine='rbind') %dopar% p_download(id)

stopCluster(cl)

#result <- rbind(result, result1)



write.csv(result1, file = 'papers_jz.csv', row.names = FALSE)








#http://www.society.shu.edu.cn
##################################################################################
#提取文章链接

library(rvest)
library(magrittr)
library(stringr)

nian <- c(2005:2019)
juan <- c(25: (2019-2005+25))
base_url <- 'http://www.society.shu.edu.cn/CN/article/showVolumnArticle.do?nian='

all_link <- c()


for (i in seq_along(juan)) {
  cat(i, '----', i/(2019-2005), '\n')
  url <- paste0(base_url, as.character(nian[i]), '&juan=', as.character(juan[i]))
  web <- read_html(url)
  neirong <- web %>% html_nodes('div[class=noselectrow]') %>% 
    html_nodes('a[class=txt_biaoti]') %>% html_attrs()
  
  link_temp <- unlist(lapply(neirong, FUN = function(x){x['href']}))
  names(link_temp) <- NULL
  all_link <- c(all_link, link_temp)
}


#################################################################################
all_link






library(rvest)
library(magrittr)
library(stringr)

temp_link <- 'http://www.society.shu.edu.cn/CN/Y2019/V39/I6/121'

result <- data.frame()
for (temp_link in all_link) {
  cat(temp_link, '\n')
  web <- read_html(temp_link)
  neirong <- web %>% html_nodes("div.primary-border") %>% html_text()
  
  if (length(neirong) != 0) {
    neirong <- neirong[1]
    neirong <- str_split(neirong, '\t')[[1]][16]
    
    neirong <- str_split(neirong, " ")[[1]]
    
    qikan_test <- c("社会杂志,", "运筹学学报,")[which(c(("社会杂志," %in% neirong),("运筹学学报," %in% neirong)))]
    
    if (length(qikan_test) != 0){
      fin_location <- which(neirong == qikan_test)
      
      title <- neirong[fin_location - 1]
      zuozhe <- paste0(neirong[1:(fin_location-2)], collapse = "")
      
      qikan <- paste0(neirong[fin_location: length(neirong)], collapse = "")
      
      zaiyao <- web %>% html_nodes("div[class='panel-body line-height text-justify']") %>% 
        html_node('p') %>% html_text()
      zaiyao <- str_replace(zaiyao[1], "摘要： ", '')
      
      temp_res <- data.frame(title = title,
                             authors = zuozhe,
                             issue = qikan,
                             abstract = zaiyao)
    }
  }
  result <- rbind(result, temp_res)
}


write.csv(result, "papers_soc.csv", row.names = FALSE)

  
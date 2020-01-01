#第三题
library(tidyverse)
papers_xb <- read.csv("papers_xb.csv")
papers_xb <- as_tibble(papers_xb)
papers_xb$issue <- as.character(papers_xb$issue)
#test <- papers_xb$issue[2000]

library(stringr)
year <- str_match(papers_xb$issue, '[0-9]{4}')

issue <- str_match(papers_xb$issue, '(?<=\\().+?(?=\\))')

all_num <- str_match_all(papers_xb$issue, '[0-9]{1,4}')
vol <- unlist(lapply(all_num, FUN = function(x){x[2]}))
first_page <- unlist(lapply(all_num, FUN = function(x){x[4]}))
last_page <- unlist(lapply(all_num, FUN = function(x){x[5]}))

papers_xb['year'] <- as.numeric(year)
papers_xb['vol'] <- as.numeric(vol)
papers_xb['no'] <- as.numeric(issue)
papers_xb['start_page'] = as.numeric(first_page)
papers_xb['end_page'] = as.numeric(last_page)
papers_xb['pages'] = (as.numeric(last_page) - as.numeric(first_page) + 1)

papers_xb_arranged <- arrange(papers_xb, year, vol, no)

write.csv(papers_xb_arranged, 'papers_xb_arranged.csv', row.names = FALSE)
####################################################################################

library(tidyverse)
papers_soc <- read.csv("papers_soc.csv")
papers_soc <- as_tibble(papers_soc)
papers_soc$issue <- as.character(papers_soc$issue)
#test <- papers_xb$issue[2000]

library(stringr)
year <- str_match(papers_soc$issue, '[0-9]{4}')

issue <- str_match(papers_soc$issue, '(?<=\\().+?(?=\\))')

all_num <- str_match_all(papers_soc$issue, '[0-9]{1,4}')
vol <- unlist(lapply(all_num, FUN = function(x){x[2]}))
first_page <- unlist(lapply(all_num, FUN = function(x){x[4]}))
last_page <- unlist(lapply(all_num, FUN = function(x){x[5]}))

papers_soc['year'] <- as.numeric(year)
papers_soc['vol'] <- as.numeric(vol)
papers_soc['no'] <- as.numeric(issue)
papers_soc['start_page'] = as.numeric(first_page)
papers_soc['end_page'] = as.numeric(last_page)
papers_soc['pages'] = (as.numeric(last_page) - as.numeric(first_page) + 1)

papers_soc_arranged <- arrange(papers_soc, year, vol, no)

write.csv(papers_soc_arranged, 'papers_soc_arranged.csv', row.names = FALSE)



####################################################################################

library(tidyverse)
papers_socrs <- read.csv("papers_socrs.csv")
papers_socrs <- as_tibble(papers_socrs)
papers_socrs$issue <- as.character(papers_socrs$issue)
#test <- papers_xb$issue[2000]

library(stringr)
year <- str_match(papers_socrs$issue, '[0-9]{4}')

#issue <- str_match(papers_socrs$issue, '(?<=\\().+?(?=\\))')

all_num <- str_match_all(papers_socrs$issue, '[0-9]{1,4}')
no <- unlist(lapply(all_num, FUN = function(x){x[2]}))
first_page <- unlist(lapply(all_num, FUN = function(x){x[3]}))
last_page <- unlist(lapply(all_num, FUN = function(x){x[4]}))

papers_socrs['year'] <- as.numeric(year)
#papers_socrs['vol'] <- as.numeric(vol)
papers_socrs['no'] <- as.numeric(no)
papers_socrs['start_page'] = as.numeric(first_page)
papers_socrs['end_page'] = as.numeric(last_page)
papers_socrs['pages'] = (as.numeric(last_page) - as.numeric(first_page) + 1)

papers_socrs_arranged <- arrange(papers_socrs, year, no)

papers_socrs_arranged$pages <- ifelse(papers_socrs_arranged$end_page ==0,
                                      0,
                                      papers_socrs_arranged$pages)
write.csv(papers_socrs_arranged, 'papers_socrs_arranged.csv', row.names = FALSE)

####################################################################################

library(tidyverse)
papers_jz <- read.csv("papers_jz.csv")
papers_jz <- as_tibble(papers_jz)
papers_jz$issue <- as.character(papers_jz$issue)
#test <- papers_xb$issue[2000]

library(stringr)
year <- str_match(papers_jz$issue, '[0-9]{4}')

issue <- str_match(papers_jz$issue, '(?<=\\().+?(?=\\))')

all_num <- str_match_all(papers_jz$issue, '[0-9]{1,4}')
vol <- unlist(lapply(all_num, FUN = function(x){x[2]}))
first_page <- unlist(lapply(all_num, FUN = function(x){x[4]}))
last_page <- unlist(lapply(all_num, FUN = function(x){x[5]}))

papers_jz['year'] <- as.numeric(year)
papers_jz['vol'] <- as.numeric(vol)
papers_jz['no'] <- as.numeric(issue)
papers_jz['start_page'] = as.numeric(first_page)
papers_jz['end_page'] = as.numeric(last_page)
papers_jz['pages'] = (as.numeric(last_page) - as.numeric(first_page) + 1)

papers_jz_arranged <- arrange(papers_jz, year, vol, no)

write.csv(papers_jz_arranged, 'papers_jz_arranged.csv', row.names = FALSE)





#################################################################################
#
#统计
#
################################################################################

################################################################################
#1
papers_xb_arranged <- read.csv('papers_xb_arranged.csv', header = TRUE,
                               stringsAsFactors = FALSE)

test <- papers_xb_arranged$authors[100:102]

library(stringr)
papers_xb_arranged['num_authors'] <- unlist(lapply(str_split(papers_xb_arranged$authors, ';'), 
                                                   FUN = function(x){length(x)}))
papers_xb_arranged['length_title'] <- str_length(papers_xb_arranged$title)


library(tidyverse)

stat_data_xb <- papers_xb_arranged %>%
  group_by(year) %>%
  summarise(ave_authors = mean(num_authors),
            ave_titles = mean(length_title))


#2
papers_jz_arranged <- read.csv('papers_jz_arranged.csv', header = TRUE,
                               stringsAsFactors = FALSE)


library(stringr)
papers_jz_arranged['num_authors'] <- unlist(lapply(str_split(papers_jz_arranged$authors, ';'), 
                                                 FUN = function(x){length(x)}))
papers_jz_arranged['length_title'] <- str_length(papers_jz_arranged$title)


library(tidyverse)

stat_data_jz <- papers_jz_arranged %>%
  group_by(year) %>%
  summarise(ave_authors = mean(num_authors),
            ave_titles = mean(length_title))


#3
papers_soc_arranged <- read.csv('papers_soc_arranged.csv', header = TRUE,
                               stringsAsFactors = FALSE)


library(stringr)
papers_soc_arranged['num_authors'] <- unlist(lapply(str_split(papers_soc_arranged$authors, ';'), 
                                                   FUN = function(x){length(x)}))
papers_soc_arranged['length_title'] <- str_length(papers_soc_arranged$title)


library(tidyverse)

stat_data_soc <- papers_soc_arranged %>%
  group_by(year) %>%
  summarise(ave_authors = mean(num_authors),
            ave_titles = mean(length_title))




#3
papers_socrs_arranged <- read.csv('papers_socrs_arranged.csv', header = TRUE,
                                stringsAsFactors = FALSE)


library(stringr)
papers_socrs_arranged['num_authors'] <- unlist(lapply(str_split(papers_socrs_arranged$author, ','), 
                                                    FUN = function(x){length(x)}))
papers_socrs_arranged['length_title'] <- str_length(papers_socrs_arranged$title)


library(tidyverse)

stat_data_socrs <- papers_socrs_arranged %>%
  group_by(year) %>%
  summarise(ave_authors = mean(num_authors),
            ave_titles = mean(length_title))



#################################################################################
#合并数据

stat_data_jz['qikan'] <- rep('rep', time = nrow(stat_data_jz))
stat_data_soc['qikan'] <- rep('soc', time = nrow(stat_data_soc))
stat_data_socrs['qikan'] <- rep('socrs', time = nrow(stat_data_socrs))
stat_data_xb['qikan'] <- rep('xb', time = nrow(stat_data_xb))

all_data <- rbind(stat_data_jz, stat_data_soc, stat_data_socrs, stat_data_xb)
all_data

library(ggplot2)

p1 <- ggplot(data = all_data, aes(x = year, y = ave_authors, color = qikan)) + 
  geom_point() + geom_line() + ggtitle("ave_num_author with year") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks = c(min(all_data$year, na.rm = TRUE):max(all_data$year, na.rm = TRUE)),
                     labels = c(min(all_data$year, na.rm = TRUE):max(all_data$year, na.rm = TRUE)))
p1
ggsave(filename = 'author_with_year.pdf', plot = p1, width = 8, height = 4)

p2 <- ggplot(data = all_data, aes(x = year, y = ave_titles, color = qikan)) + 
  geom_point() + geom_line() + ggtitle("ave_num_title with year") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks = c(min(all_data$year, na.rm = TRUE):max(all_data$year, na.rm = TRUE)),
                     labels = c(min(all_data$year, na.rm = TRUE):max(all_data$year, na.rm = TRUE)))
p2
ggsave(filename = 'title_with_year.pdf', plot = p2, width = 8, height = 4)



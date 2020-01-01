library(readxl)
data <- read_xls('PD201802.xls')
names(data) <- c('id', 'emotions')

library(stringr)

fenshu <- str_match_all(data$emotions, '[0-9]{1,2}')

clearn_row <- unlist(lapply(str_match_all(data$emotions, '[0-9]{1,2}'), FUN = function(x){length(x) == 3}))
clearn_fenshu <- fenshu[clearn_row]
names_data <- data$emotions[clearn_row]
clearn_id <- data$id[clearn_row]


name_str <- function(x) {
  result <- unlist(str_match_all(x, '[\u4e00-\u9fa5]'))
  if(length(result) == 6) {
    return(c(paste(result[1], result[2], sep = ''), 
             paste(result[3], result[4], sep = ''),
             paste(result[5], result[6], sep = '')))
    }
}



names_data <- unlist(lapply(names_data, FUN = name_str))
fenshu_data <- as.numeric(unlist(clearn_fenshu))

clearn_df <- data.frame(id = rep(clearn_id, each = 3), #宽数据
                        emotions = names_data,
                        score = fenshu_data)
library(tidyverse)
library(reshape2)

object_data <- dcast(clearn_df, id ~ emotions, value.var = 'score')

tidy_id <- data$id[!clearn_row]

tidy_data <- matrix(data = NA, nrow = length(tidy_id), ncol = ncol(object_data))
colnames(tidy_data) <- colnames(object_data)
tidy_data[,1] <- tidy_id

final_data <- rbind(object_data, as.data.frame(tidy_data))
final_data$id <- as.numeric(final_data$id)
final_data <- arrange(final_data, id)

write.csv(final_data, file = "question1.csv", row.names = FALSE)


####################################################################
#freq of emotions

freq_of_emotions <- data.frame(table(names_data)) %>%
  arrange(Freq)


library(lubridate)
library(readr)
library(stringi)
library(stringr)
library(splitstackshape)
library(tidyr)
library(writexl)
library(dplyr)
library(data.table)
list_of_files <- list.files(path = "c:/Users/Alex/D/phd/R/test", 
                            recursive = TRUE,
                            pattern = "\\.txt$",
                            full.names = TRUE)
DT <- sapply(list_of_files, read_file)

removeStopwordComasSpaces <- function(file_){
  gsub('<|>|\\(|\\)|меньше|меньшн|больше|более|менее|iultra|\\(|ручная|\\)|
       \\(|автоматическая|\\)', '',
       gsub("[[:space:]]", "", 
            stringi::stri_trans_tolower(file_)))
}
dateCleanFormat <- function(file_){
  dd <- gsub('(0?[1-9]|[12]\\d|30|31)[.](0?[1-9]|1[0-2])[.](\\d{4})', 
             '\\1-\\2-\\3', file_)
  gsub('(0?[1-9]|[12]\\d|30|31)[.](0?[1-9]|1[0-2])[.](\\d{2})', 
       '\\1-\\2-20\\3', dd)
}
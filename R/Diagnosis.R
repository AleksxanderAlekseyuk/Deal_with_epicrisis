library(lubridate)
library(readr)
library(stringi)
library(stringr)
library(splitstackshape)
library(tidyr)
library(writexl)
library(dplyr)
list_of_files <- list.files(path = "c:/Users/Alex/D/phd/R/test", 
                            recursive = TRUE,
                            pattern = "\\.txt$",
                            full.names = TRUE)
DT <- sapply(list_of_files, read_file)

stringi::stri_trans_totitle("hi here i want to see")
removeStopwordComasSpaces <- function(file_){
  gsub('<|>|\\(|\\)|меньше|меньшн|больше|более|менее|iultra|\\(|ручная|\\)|
       \\(|автоматическая|\\)', '',
       gsub("[[:space:]]", "", 
            stringi::stri_trans_tolower(stri_replace_all_regex(file_, ',', '.'))))
}

file_1 <- removeStopwordComasSpaces(DT[1])
file_ <- stringr::str_extract(file_1, 
      "(?<=диагноззаключительный:|диагноззаключительныйклинический:|
      диагноз:|диагнозпривыписке:).*?(?=состояниеижалобы|результаты|жалобы|
      консультации|консилиумы|врач)" )
stringr::str_extract(file_1, 
                     "(?<=диагноззаключительный).*?(?=состояниеижалобы)")
str_extract(file_, 
            "(?<=дн)\\d{1}")

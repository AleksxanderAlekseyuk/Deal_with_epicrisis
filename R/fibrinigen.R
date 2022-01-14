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

removeStopwordComasSpaces <- function(file_){
  gsub('<|>|меньше|меньшн|больше|более|менее|iultra|\\(|ручная|\\)|
       \\(|автоматическая|\\)', '',
       gsub("[[:space:]]", "", 
            stringi::stri_trans_tolower(stri_replace_all_regex(file_, ',', '.'))))
}
dateCleanFormat <- function(file_){
  dd <- gsub('(0?[1-9]|[12]\\d|30|31)[.](0?[1-9]|1[0-2])[.](\\d{4})', 
             '\\1-\\2-\\3', file_)
  gsub('(0?[1-9]|[12]\\d|30|31)[.](0?[1-9]|1[0-2])[.](\\d{2})', 
       '\\1-\\2-20\\3', dd)
}

DT <- removeStopwordComasSpaces(DT)
DT <- dateCleanFormat(DT)
getFibrinogen <- function(
  data
) {

  file_ <- gsub('(\\d{2}-\\d{2}-\\d{4})(гемостазиограмма)', 
                '\\2\\1', data)
  
  
  
  dates <- str_extract_all(file_, 
                           '(?<=гемостазиограмма)(\\d{2}-\\d{2}-\\d{4}|)')
  
  id <- rep(str_extract(file_, "\\d+"), each = length(unlist(dates)))
  
  birthday <- rep(str_extract(file_, "\\d{2}-\\d{2}-\\d{4}"), 
                  each = length(unlist(dates)))
  
  file_1 = gsub('(гемостазиограмма)(\\d{2}-\\d{2}-\\d{4})', '\\2\\1', file_)
  
  file_ <- stringr::str_extract_all(file_1, 
                                    "(?<=гемостазиограмма.).*?(?=\\d{2}-\\d{2}-\\d{4})" )
  
  values <- sapply(file_, function(
    coagulogramma
  ) {
    value <- stringr::str_extract(coagulogramma,
                                  "(?<=фибриноген)\\d{1,3}.\\d{1,2}")
    
  }
  )
  
  return(data.frame(
    dates = unlist(dates),
    birthday = unlist(birthday),
    patientIds = id,
    values = unlist(values)
  ))
}


out <- purrr::map_dfr(
  DT, getFibrinogen
)

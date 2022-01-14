library(lubridate)
library(readr)
library(stringi)
library(stringr)
library(splitstackshape)
library(tidyr)
library(writexl)
library(dplyr)
list_of_files <- list.files(path = "c:/Users/Alex/D/phd/R/script", 
                            recursive = TRUE,
                            pattern = "\\.txt$",
                            full.names = TRUE)
DT <- sapply(list_of_files, read_file)

removeStopwordComasSpaces <- function(file_){
  gsub('<|>|\\(|\\)|меньше|меньшн|больше|более|менее|iultra|\\(|ручная|\\)|
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
getBloodTestValues <- function(
  data
) {
  data <- gsub("([.]\\d{2}[:]\\d{2};)", "", data)
  
  file_ <- gsub('(\\d{2}-\\d{2}-\\d{4})(общийанализкрови)', 
                '\\2\\1', data)
  
  
  
  dates <- str_extract_all(file_, 
                           '(?<=общийанализкрови)(\\d{2}-\\d{2}-\\d{4}|)')
  
  dates <- unlist(dates)
  
  dates <- dates[which(dates != "")]
  
  id <- rep(str_extract(file_, "\\d+"), each = length(unlist(dates)))
  
  birthday <- rep(str_extract(file_, "\\d{2}-\\d{2}-\\d{4}"), 
                  each = length(unlist(dates)))
  
  file_1 = gsub('(общийанализкрови)(\\d{2}-\\d{2}-\\d{4})', '\\2\\1', file_)
  
  file_ <- stringr::str_extract_all(file_1, 
                                    "(?<=\\d{2}-\\d{2}-\\d{4}общийанализкрови).*?(?=\\d{2}-\\d{2}-\\d{4}|суммаклеток)" )
  
  values <- lapply(file_, function(
    bloodTest
  ) {
    valueLeucocytes <- stringr::str_extract(bloodTest,
                                  "(?<=лейкоциты)\\d{1,3}[.]\\d{1}")
    
    valueLymphocytesPercentage <- stringr::str_extract(bloodTest,
                                            "(?<=лимфоциты)\\d{1,2}")
    
    valueBandPercentage <- stringr::str_extract(bloodTest,
                                                       "(?<=палочкоядерные)\\d{1,2}")
    
    valueSegmentedPercentage  <- stringr::str_extract(bloodTest,
                                          "(?<=сегментоядерные)\\d{1,2}")
    
    valueMonocytesPercentage  <- stringr::str_extract(bloodTest,
                                                      "(?<=моноциты)\\d{1,2}")
    
    valuePlatlets <- stringr::str_extract(bloodTest,
                                          "(?<=тромбоциты)\\d{1,4}")
    
    valueHemoglobin <- stringr::str_extract(bloodTest,
                                          "(?<=гемоглобин)\\d{1,4}")
    
    valueESR <- stringr::str_extract(bloodTest,
                                            "(?<=соэ)\\d{1,4}")
    return(
      list(valueLeucocytes,
           valueLymphocytesPercentage,
           valueBandPercentage,
           valueSegmentedPercentage,
           valueMonocytesPercentage,
           valuePlatlets,
           valueHemoglobin,
           valueESR)
    )

  }
  )
  
  return(data.frame(
    dates = unlist(dates),
    birthday = unlist(birthday),
    patientIds = id,
    valueLeucocytes = unlist(values[[1]][1]),
    valueLymphocytesPercentage = unlist(values[[1]][2]),
    valueBandPercentage =  unlist(values[[1]][3]),
    valueSegmentedPercentage =  unlist(values[[1]][4]),
    valueMonocytesPercentage =  unlist(values[[1]][5]),
    valuePlatlets =  unlist(values[[1]][6]),
    valueHemoglobin =  unlist(values[[1]][7]),
    valueESR =  unlist(values[[1]][8])
  ))
}


out <- purrr::map_dfr(
  DT[500:1000], getBloodTestValues
)


out <- sapply(
  DT, getBloodTestValues
)

out <- data.table::rbindlist(out, fill = T)

data <- DT[38]
data <- gsub(pattern = "([.]\\d{2}[:]\\d{2}[;])", "",data)
file_ <- gsub('(\\d{2}-\\d{2}-\\d{4})(общийанализкрови)', 
              '\\2\\1', data)



dates <- str_extract_all(file_, 
                         '(?<=общийанализкрови)(\\d{2}-\\d{2}-\\d{4}|)')
dates <- str_extract_all(file_, 
                         '(?<=общийанализкрови)(\\d{2}-\\d{2}-\\d{4}|)')

dates <- unlist(dates)

dates <- dates[which(dates != "")]
id <- rep(str_extract(file_, "\\d+"), each = length(unlist(dates)))

birthday <- rep(str_extract(file_, "\\d{2}-\\d{2}-\\d{4}"), 
                each = length(unlist(dates)))

file_1 = gsub('(общийанализкрови)(\\d{2}-\\d{2}-\\d{4})', '\\2\\1', file_)

file_ <- stringr::str_extract_all(file_1, 
                                  "(?<=\\d{2}-\\d{2}-\\d{4}общийанализкрови).*?(?=\\d{2}-\\d{2}-\\d{4})" )
file_ <- stringr::str_extract_all(file_1, 
                                  "(?<=\\d{2}-\\d{2}-\\d{4}общийанализкрови).*?(?=\\d{2}-\\d{2}-\\d{4}|суммаклеток)" )
# (?<=\\d{2}-\\d{2}-\\d{4}общийанализкрови).*?(?=\\d{2}-\\d{2}-\\d{4}|суммаклеток)
ddd <- unlist(dates)
ddd[which(ddd == "")] <- "00-00-0000"

dates <- ddd[which(ddd == "")] <- "00-00-0000"
t <- "11-01-2021.12:28;"
gsub("([.]\\d{2}[:]\\d{2}[;])", "", t)

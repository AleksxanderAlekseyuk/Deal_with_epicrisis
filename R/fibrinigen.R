library(lubridate)
library(readr)
library(stringi)
library(stringr)
library(splitstackshape)
library(tidyr)
library(writexl)
library(dplyr)
list_of_files <- list.files(path = "c:/Users/Alex/D/phd/R/script", 
                            recursive = F,
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
  # ddd <- unlist(dates)
  # 
  # dates <- ddd[which(ddd != "")]
  
  id <- rep(str_extract(file_, "\\d+"), each = length(unlist(dates)))
  
  birthday <- rep(str_extract(file_, "\\d{2}-\\d{2}-\\d{4}"), 
                  each = length(unlist(dates)))
  
  file_1 = gsub('(гемостазиограмма)(\\d{2}-\\d{2}-\\d{4})', '\\2\\1', file_)
  
  file_ <- unlist(stringr::str_extract_all(file_1, 
                                           "(?<=гемостазиограмма.).*?(?=\\d{2}-\\d{2}-\\d{4}|гемостазиограмма)" )) 
  
  ff <- file_[which(!str_detect(file_, "гемостазиограмма"))]
  
  values <- lapply(ff, function(
    coagulogramma
  ) {
    value <- stringr::str_extract(coagulogramma,
                                  "(?<=фибриноген)\\d{1,3}.\\d{1,2}")
    
  }
  )
  if(!is.na(file_1)) {
  return(data.frame(
    dates = unlist(dates),
    birthday = unlist(birthday),
    patientIds = id,
    values = unlist(values)
  ))
  } else {
    NULL
  }
}  

# for(
#   file in DT
# ) {
#   print(DT)
#   test <- getFibrinogen(file)
#   
# }


out <- purrr::map_dfr(
  DT, getFibrinogen
)
out <- sapply(
  DT, getFibrinogen
)

out <- data.table::rbindlist(out, fill = T)

getFibrinogen(DT[16])

file_ <- gsub('(\\d{2}-\\d{2}-\\d{4})(гемостазиограмма)',
              '\\2\\1', DT[15])
tt <- str_extract(file_,
                'гемостазиограмма')

dates <- str_extract_all(file_,
                         '(?<=гемостазиограмма)(\\d{2}-\\d{2}-\\d{4}|)')
ddd <- unlist(dates)

dates <- ddd[which(ddd != "")]

id <- rep(str_extract(file_, "\\d+"), each = length(unlist(dates)))

birthday <- rep(str_extract(file_, "\\d{2}-\\d{2}-\\d{4}"),
                each = length(unlist(dates)))

file_1 = gsub('(гемостазиограмма)(\\d{2}-\\d{2}-\\d{4})', '\\2\\1', file_)

file_ <- unlist(stringr::str_extract_all(file_1,
                                  "(?<=гемостазиограмма.).*?(?=\\d{2}-\\d{2}-\\d{4}|гемостазиограмма)" ))

ff <- file_[which(str_detect(file_, "фибр"))]

file_ <- ifelse(str_detect(file_, "фибр"),file_, NULL )
  filter(
    str_detect(string = ., "фибр")
  )


removeStopwordComasSpaces(DT[451])
gsub("гемостазиограмма00.00.00", "", "гемостазиограмма00.00.00:гемостазиограмма00.00.00:гемостазиограмма00.00.00")

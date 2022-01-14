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
getBiochemicalBloodTestValues <- function(
  data
) {
  file_ <- gsub('(\\d{2}-\\d{2}-\\d{4})(биохимическоеисследованиекрови)', 
                '\\2\\1', data)
  
  
  
  dates <- str_extract_all(file_, 
                           '(?<=биохимическоеисследованиекрови)(\\d{2}-\\d{2}-\\d{4}|)')
  
  id <- rep(str_extract(file_, "\\d+"), each = length(unlist(dates)))
  
  birthday <- rep(str_extract(file_, "\\d{2}-\\d{2}-\\d{4}"), 
                  each = length(unlist(dates)))
  
  file_1 = gsub('(биохимическоеисследованиекрови)(\\d{2}-\\d{2}-\\d{4})', '\\2\\1', file_)
  
  file_ <- stringr::str_extract_all(file_1, 
                                    "(?<=биохимическоеисследованиекрови).*?(?=\\d{2}-\\d{2}-\\d{4})" )
  
  values <- lapply(file_, function(
    bloodTest
  ) {
    valueCreatinine <- stringr::str_extract(bloodTest,
                                            "(?<=креатинин)\\d{1,4}")
    
    valueAST <- stringr::str_extract(bloodTest,
                                    "(?<=аспартатаминотрансфераза)\\d{1,4}")
    
    valueCRP <- stringr::str_extract(bloodTest,
                                                "(?<=c-реактивныйбелок)\\d{1,4}")
    
    valueLDG  <- stringr::str_extract(bloodTest,
                                                      "(?<=лактатдегидрогеназа)\\d{1,4}")
    
    valuePotassium  <- stringr::str_extract(bloodTest,
                                                      "(?<=калий)\\d{1,2}")
    
    valueALT <- stringr::str_extract(bloodTest,
                                          "(?<=аланинаминотрансфераза)\\d{1,4}")
    
    valueAlbumin<- stringr::str_extract(bloodTest,
                                            "(?<=альбумины)\\d{1,4}[.]\\d{1,2}")
    
    valueBilirubin <- stringr::str_extract(bloodTest,
                                     "(?<=билирубинобщий)\\d{1,4}[.]\\d{1}")
    
    valueGlucose <- stringr::str_extract(bloodTest,
                                           "(?<=глюкозакрови)\\d{1,2}[.]\\d{1}")
    return(
      list(
        valueCreatinine,
        valueAST,
        valueCRP,
        valueLDG,
        valuePotassium,
        valueALT,
        valueAlbumin,
        valueBilirubin,
        valueGlucose
      )
    )
    
  }
  )
  
  return(data.frame(
    dates = unlist(dates),
    birthday = unlist(birthday),
    patientIds = id,
    valueCreatinine = unlist(values[[1]][1]),
    valueAST = unlist(values[[1]][2]),
    valueCRP =  unlist(values[[1]][3]),
    valueLDG =  unlist(values[[1]][4]),
    valuePotassium =  unlist(values[[1]][5]),
    valueALT =  unlist(values[[1]][6]),
    valueAlbumin =  unlist(values[[1]][7]),
    valueBilirubin =  unlist(values[[1]][8]),
    valueGlucose = unlist(values[[1]][9])
  ))
}


out <- purrr::map_dfr(
  DT, getBiochemicalBloodTestValues
)

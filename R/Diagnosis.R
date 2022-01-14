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
dateCleanFormat <- function(file_){
  dd <- gsub('(0?[1-9]|[12]\\d|30|31)[.](0?[1-9]|1[0-2])[.](\\d{4})', 
             '\\1-\\2-\\3', file_)
  gsub('(0?[1-9]|[12]\\d|30|31)[.](0?[1-9]|1[0-2])[.](\\d{2})', 
       '\\1-\\2-20\\3', dd)
}

file_1 <- removeStopwordComasSpaces(DT) %>% 
  dateCleanFormat()

file_ <- stringr::str_extract(file_1, 
      "(?<=диагноззаключительный|диагноззаключительныйклинический:|
      диагноз:|диагнозпривыписке:|заключительныйдиагноз|
      ).*?(?=состояниеижалобы|результаты|жалобы|
      консультации|консилиумы|врач|гепатоз)" )


diagnosisData <- function(data) {

  
  id <- str_extract(data, "\\d+")
  
  birthday <- str_extract(data, "\\d{2}-\\d{2}-\\d{4}")
  
  
  file_ <- stringr::str_extract(data, 
                                "(?<=диагноззаключительный|диагноззаключительныйклинический:|
      диагноз:|диагнозпривыписке:|заключительныйдиагноз|
      ).*?(?=состояниеижалобы|результаты|жалобы|
      консультации|консилиумы|врач|гепатоз)" )
  
  t <- lapply(
    file_, function(f) {
    rf <- ifelse(str_detect(f, "дн(?=1|2|3|ш|i)"), 1, 0)
    obesity <- ifelse(str_detect(f, "ожирение"), 1, 0)
    hypertension <- ifelse(str_detect(f, "артериальнаягипертензия|аг"), 1, 0)
    severePneumonia <- ifelse(str_detect(f, "^[e]тяж.лая|^[e]тяж.лоетечение"), 1, 0)
    COPD <- ifelse(str_detect(f, "хобл|хроническаяобструктивная"), 1, 0)
    diabetesMellitus <- ifelse(str_detect(f, "сахарный|(сд)(?:.+)(тип)|(сд)(?=тип)"), 1, 0)
    IHD <- ifelse(str_detect(f, "ибс|(ишемическая)(?=болезнь)(?=сердца)"), 1, 0)
    bronchialAsthma <- ifelse(str_detect(f, "(бронхиальная)|(ба)(?=контролируем..)|(ба)(?:.+)(контролируем..)"), 1, 0)
    cancer <- ifelse(str_detect(f, "рак|заболевание|cr|c-r|сr|злокачественн|меланома"), 1, 0)
    sepsis <- ifelse(str_detect(f, "сепсис|бактериемия|эндокардит"), 1, 0)
    gastritis <- ifelse(str_detect(f, "гастрит"), 1, 0)
    gastricUlcer <- ifelse(str_detect(f, "язва(?=желудка|12)"), 1, 0)
    myocarditis <- ifelse(str_detect(f, "миокардит|повреждениемиокарда"), 1, 0)
    stroke <- ifelse(str_detect(f, "онмк|инфарктголовного"), 1, 0)
    strokeHistory <- ifelse(str_detect(f, "(последствия)(?:.+)(онмк|инфаркт.головного)"), 1, 0)
    DEP <- ifelse(str_detect(f, "дэп|дисциркуляторнаяэнцефалопатия"), 1, 0)
    aterosclerosis <- ifelse(str_detect(f, "облитерирующийатеросклероз"), 1, 0)
    pseudomembranousColitis <- ifelse(str_detect(f, "псевдомембранозный"), 1, 0)

      return(
        list(
          rf,
          obesity,
          hypertension,
          severePneumonia,
          COPD,
          diabetesMellitus ,
          IHD ,
          bronchialAsthma,
          cancer,
          sepsis,
          gastritis, 
          gastricUlcer,
          myocarditis, 
          stroke, 
          strokeHistory, 
          DEP, 
          aterosclerosis, 
          pseudomembranousColitis
        )
      )
    })
  
  
  return(
    data.frame(
      patientId = unlist(id),
      birthday = unlist(birthday),
      rf = unlist(t[[1]]),
      obesity = unlist(t[[2]]),
      hypertension = unlist(t[[3]]),
      severePneumonia = unlist(t[[4]]),
      COPD = unlist(t[[5]]),
      diabetesMellitus = unlist(t[[6]]) ,
      IHD = unlist(t[[7]]) ,
      bronchialAsthma = unlist(t[[8]]),
      cancer = unlist(t[[9]]),
      sepsis = unlist(t[[10]]),
      gastritis = unlist(t[[11]]), 
      gastricUlcer = unlist(t[[12]]),
      myocarditis = unlist(t[[13]]), 
      stroke = unlist(t[[14]]), 
      strokeHistory = unlist(t[[15]]), 
      DEP = unlist(t[[16]]), 
      aterosclerosis = unlist(t[[17]]), 
      pseudomembranousColitis = unlist(t[[18]])
      
    )
  )
}       
v <- diagnosisData(file_1)

tt <- data.table::rbindlist(t)
str_detect('доброкачественнаяопухоль', "доброкачественнаяопухоль|рак|заболевание|cr|c-r|сr|злокачественн|меланома|(опухоль)(?:.+)(доброкачественная)")
str_detect('баконтролируемая', "(астма)(?:.+)(бронхиальная)|(ба)(?:.+)(.онтролируемая..)")

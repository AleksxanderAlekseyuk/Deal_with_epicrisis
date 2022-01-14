for(val in 1:length(unlist(file_)) {
  value <- stringr::str_extract(file_,
                                "(?<=фибриноген)\\d{1,3}.\\d{1,2}")
  if(!is.na(value)) {
    values <- append(values, value)
  } else {
    values <- append(values, "missValue")
  }
  
  file_ <- str_replace(file_, "фибриноген", "")
  
}

return(data.frame(
  dates = unlist(dates),
  patientIds = id,
  values = values
))

}
for(l in 1:length(unlist(DT))){
  print(l)
  e <- getFibrinogen(DT[l])
}
getFibrinogen(DT[3])





out <- purrr::map_dfr(
  DT, getFibrinogen
)


file_ <- gsub('(\\d{2}-\\d{2}-\\d{4})(гемостазиограмма)', '\\2\\1', file_)
#ставим дату после гемостазиограммы, чтобы проще было искать по одному шаблону.
dates <- unlist(stri_extract_all(file_, regex='(?<=гемостазиограмма)(\\d{2}-\\d{2}-\\d{4}|)' ))
id <- str_extract(file_, "\\d+")
id <- rep(id, each=length(dates))
#здесь наоборот ставим дату ПЕРЕД гемост-ой, 
#чтобы она нам не мешала. тк вариант типа "гемостазиограмма27-09-2021:ачтв.." не даст результата.


lst <- stringr::str_extract_all(file_1,
                                "(?<=фибриноген)\\d{1,3}.\\d{1,2}")


dd <- data.table::data.table(
  dates,
  id,
  lst
)


stringr::str_extract( 'dacebqeawb', "(a).*(b)" )
library(qdapRegex)
rm_between("dacebqeawb", 'a', 'b', extract=TRUE)[[1]]
dates <- str_extract_all(string, 
                         '(?=гемостазиограмма)(\\d{2}-\\d{2}-\\d{4}|)')
rm_between(string, 'гемостазиограмма', '23-10-2021', extract=TRUE)

string <- "21-10-2021гемостазиограмма LA122-10-2021гемостазиограмма LA223-10-2021гемостазиограмма         LA323-10-2021"
d <- gsub('(\\d{2}-\\d{2}-\\d{4})(гемостазиограмма)', 
          '\\2\\1', string)
dates <- str_extract_all(d, 
                         '(?<=гемостазиограмма)(\\d{2}-\\d{2}-\\d{4}|)')
z <- rm_between(string, 'гемостазиограмма', "22-10-2021", extract=TRUE)
for(date in unlist(dates)) {
  print(date)
  t <- rm_between(string, 'гемостазиограмма', date, extract=TRUE)
  print(t)
}

sub("(гемостазиограмма).*(?=\\d{2}-\\d{2}-\\d{4})", "\\2", x = string)
str_extract_all(string, "(?<=гемостазиограмма.).*?(?=\\d{2}-\\d{2}-\\d{4})")
string <- "21-10-2021гемостазиограмма 123 12-10-2021гемостазиограмма 456 23-10-2021 гемостазиограмма 789 23-10-2021"
l1 <- list(1)
l2 <- list(2)
l3 <- rbind(l1, l2)
l3 <- rbind(l1, l2)

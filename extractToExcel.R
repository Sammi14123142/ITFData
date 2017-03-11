install.packages("docxtractr")
install.packages("dplyr")
install.packages("stringr")
install.packages("magrittr")
install.packages("xlsx")
library(xlsx)
library(magrittr)
library(stringr)
library(docxtractr)
library(dplyr)

files <- list.files(path = ".", pattern = "*.docx")
df <- data.frame()

cleaning <- function(filename) {
  docx <- read_docx(filename)
  all <- docx_extract_all(docx)
  
  datalist <- list()
  for (table in c(4:14)) {
    temp <- as.data.frame(all[[table]][2])
    datalist[[table]] <- temp
  }
  
  comb <- do.call(rbind, datalist)
  
  oneDecimal <- "\\d+\\.\\d{1}"
  twoDecimal <- "\\d+\\.\\d{2}"
  
  lab <- comb[16, ] %>%
    str_extract_all(oneDecimal) %>%
    unlist()
  
  specLab <- comb[17,] %>% 
    str_extract_all(twoDecimal) %>% 
    unlist()
  
  rawThickness <- comb[24,]
  thickness <- comb[24, ] %>%
    str_extract_all(oneDecimal) %>%
    unlist() %>%
    as.numeric() %>%
    mean() %>% 
    round(digit=2)
  
  cleaned <- c(comb[c(1:4, 7, 9:12, 14:15), ], 
               lab, 
               comb[18:21, ],
               thickness,
               specLab,
               comb[25, ]
  )
  return(cleaned)
}


cleaned1 <- cleaning(files[1])
cleaned2 <- cleaning(files[2])
cleaned3 <- cleaning(files[3])
cleaned4 <- cleaning(files[4])
cleaned5 <- cleaning(files[5])
cleaned6 <- cleaning(files[6])
cleaned7 <- cleaning(files[7])

files[7]
df <- cbind(cleaned2, cleaned1)


df
warnings()
class(cleaned2)
do.call(rbind, cleaned1, cleaned2)
what <- rbind(cleaned1, cleaned2)

vec <- list()
for (i in c(1:4, 6)) {
  vec[[i]] <- cleaning(files[i])
}

final <- do.call(rbind, vec)
write.xlsx(final, "clinicalTrial.xlsx")

files
for (i in range(1:7)) {
  cleaning(files[i])
}
## code to prepare `accident 2013` dataset goes here

accident_2013  <- read.csv("C:/Users/juan_/Downloads/data/data/accident_2013.csv")[1:10,1:3]
usethis::use_data(accident_2013, overwrite = TRUE, internal = TRUE)


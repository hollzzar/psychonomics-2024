setwd("/Users/hollyzaharchuk/Mirror/dissertation/15_psychonomics/")
path <- "file://localhost/Users/hollyzaharchuk/Mirror/dissertation/15_psychonomics/"

file <- paste0(path, "Psychonomics_2024.html")
rmarkdown::render("Psychonomics_2024.Rmd")

pagedown::chrome_print(file)


library(XLConnect)
library(readxl)
path <- "~/Desktop/GrowthRate/dataPlateReader/28062017 OD600.xlsx"

data <- readxl::read_excel(path, skip = 42, n_max = 8, range = "A:M", col_names = FALSE)


range = anchored("A23", dim = c(9, 2)), col_names = FALSE

date <- readxl::read_excel(path, range = "E40", col_names ="date")

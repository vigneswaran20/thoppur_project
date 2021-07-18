library(highcharter)
library(htmlwidgets)
library(dplyr)


set.seed(111)

t1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C") , size = 100, replace=TRUE)
t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
t3 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)


'----------------------------------------'



names(d) <- c('Hospital', 'Gender', 'Outcome')
head(d)
str(d)
d <- data.frame(cbind(t1,t2,t3))
d

hchart(data_to_sankey(d), "sankey", name = "Hospital and Gender based Outcomes")
dataForSankey <- d%>%dplyr::select(Hospital, Outcome)
hchart(data_to_sankey(dataForSankey), "sankey", name = "Hospital based Outcomes")
'------------------------------------------------------'
d
data_dong <- readxl::read_xlsx("E:\\vehicles_cooccurence_temp.xlsx")
data_dong_sankey <- data_dong%>%dplyr::select(left,right,Year)

data_dong_sankey
hchart(data_to_sankey(data_dong_sankey), "sankey", name = "Accident Clashes")
View(data_dong_sankey)

library(tidyverse)
library(readxl)
library(lubridate)
library(RSocrata)

salem_trak

View(salem_trak)
'--------------------------------------------------------------------------------------------------------------------------------'
##Ignore the missing data
thoppur_acc <- salem_acc_report %>%
  filter(  !is.na(NEW_DATE)) 

'------------------------------------------------------------------------------------------------------------------------------'



''How have the number of accidents changed over time? (in months)''
'-----------------------------------------------------------------------------------------------------------------------------------------'
salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "month")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) %>%
  count(NEW_DATE, CASES)  %>%
  filter(
    NEW_DATE != last(NEW_DATE),
    NEW_DATE != first(NEW_DATE)
  )  %>%
  ggplot(aes(NEW_DATE, n, color = CASES)) +
  geom_line(size = 1.5, alpha = 0.7) +
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = NULL, y = "Number of traffic accidents per week",
    color = "CASES"
  ) + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))

'------------------------------------------------------------'

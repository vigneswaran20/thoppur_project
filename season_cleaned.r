salem_tra <- x_cleaned_

variable.names(salem_tra)

view(salem_tra)

salem_s <- salem_tra %>%
  mutate(Date = floor_date(Date, unit = "month")) %>% count(Date, Head)



salem_s

kpop<-salem_s %>% 
  group_by(month = month(Date, label = TRUE))  %>% count(month, Head) %>% mutate(pct = prop.table(n))


kpop

'Accident with respect to months'
###----------------------------------------------------------------------------------------------------------------------------------------------


salem_s <- salem_trak %>%
  mutate(NEW_DATE = floor_date(NEW_DATE, unit = "week")) %>%
  mutate(CASES = fct_collapse(CASES, Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals"),
                              
                              Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act","Medium injury`"))) 





kpop<-salem_s %>% 
  group_by(month = month(NEW_DATE, label = TRUE))  %>% count(month, CASES) %>% mutate(pct = prop.table(n))
 

View(kpop)

library(ggplot2)
library(viridis)
library(hrbrthemes)

ggplot(kpop, aes(fill=CASES, y=n, x=month)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Accident with respect to month") +
  labs(
    x = NULL, y = "Number of traffic accidents per month",
    color = "CASES"
  ) +
  theme_ipsum() +
  xlab("") + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))
  
  
  
  
  
  
  
  ###------------------------------------------------------------------------------------------------------------
  
  'Accident rate with respect to the season'
  
  


salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES,  Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%
  
  count(NEW_DATE, CASES)  %>%
  filter(CASES == "Fatal") %>%
  group_by(CASES) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  kable(
    col.names = c("Day", "Cases", "Number of people", "percentage"),
    align = "llrr"
  )

'-----------------------------------------------------------------------'


'How does the cases rate change through the week?'


salem_trak %>%
  mutate(NEW_DATE = wday(NEW_DATE, label = TRUE)) %>%
  mutate(CASES = fct_collapse(CASES, Injury = c("Grivious Injury","Minor injury", "Minor Injury", "No injury", "Non Injury" ,"Grivious injury", "Grivious injury.","Grivious injury @ Fatal",
                                         "Grivious injury","Low injury","Low injury and sec 185 MV Act",
                                         "Medium injury","Medium injury @ Low injury","Medium injury and 185 MVI Act",
                                         "Medium injury`"),  Fatal = c("Fatal","Fatal and accidental fire","Fatal.","Fatals")
                              
  )) %>%
  count(NEW_DATE, CASES) %>%
  group_by(CASES) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(percent, NEW_DATE, fill = CASES)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "% of Cases", y = NULL, fill = "Cases") + ggtitle("Case Rate") +
  
  scale_fill_brewer(palette="Paired") +
 
 theme_minimal() + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 18, family = "Open Sans"))

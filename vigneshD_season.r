'Accidents with respect to Season'

library(zoo)
yq <- as.yearqtr(as.yearmon(kpop$NEW_DATE, "%m/%d/%Y") + 1/12)

yq
kpop$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))

kpop_n<-kpop %>% 
  group_by(month = month(NEW_DATE, label = TRUE))  %>% count(Season, CASES) %>% mutate(pct = prop.table(n))

kpop_n



ggplot(kpop_n, aes(fill=CASES, y=n, x=Season)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Accident with respect to season") +
  labs(
    x = NULL, y = "Number of traffic accidents per Season",
    color = "CASES"
  ) +
  theme_ipsum() +
  xlab("") + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))







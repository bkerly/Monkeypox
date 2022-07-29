library(tidyverse)
library(lubridate)

cases_by_day <- read_csv("U.S. Monkeypox Case Trends Reported to CDC.csv") %>%
  mutate(`Epi-date` = mdy(`Epi-date`)) %>%
  mutate(days = (`Epi-date` - min(`Epi-date`)) %>%
  as.numeric())%>%
  filter(Cases != 0)

model <- lm(log(Cases) ~ days, data = cases_by_day)

lm_coef <- coef(model)

#What will the number of cases be on day 365
cases_365 <- exp(lm_coef[1])*exp(lm_coef[2]*365)

paste(
  glue::glue(
    "
    In just one year, we'll have {cases_365} cases per day! That's {round(cases_365/7000000000,2)} cases for every person on earth!
    "
  )
)

days_func <- function(days) {exp(lm_coef[1])*exp(lm_coef[2]*days )}

ggplot(data = cases_by_day)+
  geom_point(aes(x=days,y=Cases)) + 
  stat_function(fun = days_func,
                color = "blue")+
  xlim(0,90)+
  xlab("Days since First Case")+
  ylab("Cases Reported Per Day")+
  labs(title = "Monkeypox Cases Reported by Day",
       subtitle = "Blue Line Indicates Exponential Curve Fit")


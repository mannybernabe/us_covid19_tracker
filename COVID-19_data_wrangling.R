# Covid-19 Data Clean up and Analysis

library(tidyverse)
library(lubridate)

# Get the data from JHU CSSE Github account

raw_covid19_tbl <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


covid19_tbl<- raw_covid19_tbl %>% 
  mutate(`Province/State` = ifelse(is.na(`Province/State`), `Country/Region`, `Province/State`)) %>% 
  gather(names(covid19)[5:ncol(covid19)], key = "date_time", value = "total_cases") %>% 
  filter(complete.cases(.)) %>% 
  mutate(date_time = mdy(date_time, tz = "America/Chicago")) %>% 
  group_by(`Province/State`) %>% 
  mutate(new_cases = total_cases - lag(total_cases, default = first(total_cases), order_by = date_time)) 


covid19_country_tbl<- covid19_tbl %>% 
  group_by(`Country/Region`, date_time) %>% 
  summarise(total_country_cases = sum(total_cases)) %>% 
  mutate(new_cases = total_country_cases - lag(total_country_cases, 
                                               default = first(total_country_cases), 
                                               order_by = date_time)) %>% #find delta between days
  filter(total_country_cases>100) %>% 
  group_by(`Country/Region`) %>% 
  mutate(days_outbreak = row_number())#create index of days since outbreak of case > 100




#generate stats
country_stats_tbl<-covid19_country_tbl %>% 
  group_by(days_outbreak) %>% 
  summarise(min=min(total_country_cases),
            q25=quantile(total_country_cases ,.25),
            median=median(total_country_cases ),
            q75=quantile(total_country_cases ,.75),
            max=max(total_country_cases))



covid19_country_tbl<-left_join(covid19_country_tbl, country_stats_tbl,by="days_outbreak")



covid19_us_tbl <- covid19_country_tbl %>% 
  filter(`Country/Region`=="US")


write.csv(covid19_us_tbl,'COVID-19_US_data.csv')





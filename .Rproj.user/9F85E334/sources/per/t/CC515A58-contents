library(RDoubleClick)
library(stringr)
library(lubridate)
library(dplyr)
library(readr)
library(readxl)
library(openxlsx)

adwords<- read_excel("~/channel_deep_dives/stp/Search Data_STP_Weekly_work.xlsx", sheet = "AdWords") %>% 
  mutate(Week = week(Day))

bing <- read_excel("~/channel_deep_dives/stp/Search Data_STP_Weekly_work.xlsx", sheet = "Bing") %>% 
  mutate(Week = week(Day))

list <- read_excel("~/channel_deep_dives/stp/Search Data_STP_Weekly_work.xlsx", sheet = "all")


adwords_week <- adwords %>% 
  group_by(Week,Campaign) %>% 
  summarise(Impressions = sum(Impressions),
            Clicks = sum(Clicks),
            Cost = sum(Cost))

bing_week <- bing %>% 
  group_by(Week,Campaign) %>% 
  summarise(Impressions = sum(`Impr.`),
            Clicks = sum(Clicks),
            Cost = sum(Spend))



all_bind <- rbind(adwords_week,bing_week)

all_bind_join <- left_join(all_bind, list,by = "Campaign") 

all_bind_join <- all_bind_join %>% 
  group_by(Week,Tactic) %>% 
  summarise(Impressions = sum(Impressions),
            Clicks = sum(Clicks),
            Cost = sum(Cost),
            CPC = Cost/Clicks)
# turn to week to start date and end date of that week
all_bind_join <- all_bind_join %>% 
  mutate(start_date = as.Date(paste(0, Week, 2017, sep = "-"), format = "%w-%W-%Y"),
         end_date = as.Date(paste(6, Week, 2017, sep = "-"), format = "%w-%W-%Y"))


write.xlsx(all_bind_join, file = "all_bind.xlsx", row.names = FALSE)


local_search <- adwords %>% 
  filter(Account == "Sierra Trading Post Local") %>% 
  group_by(Week, Account) %>% 
  summarise(Impressions = sum(Impressions),
            Clicks = sum(Clicks),
            Cost = sum(Cost),
            CPC = Cost/Clicks)


write.xlsx(local_search, file = "local_search.xlsx", row.names = FALSE)

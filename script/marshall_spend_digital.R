library(RDoubleClick)
library(stringr)
library(lubridate)
library(dplyr)
library(readr)
library(readxl)
library(openxlsx)

client.id <- "420112744051-v16uovgvr58clj96cujohtfe1vernjrn.apps.googleusercontent.com"
client.secret <- "J843SJWGUQtHmiT5fMXs2yGq"

DCAuth(client.id,client.secret)

load("DCtoken.Rda")

dv_marshalls <- read_csv("data_for_marshall/digital/dv_marshalls.csv") %>% 
  mutate(Date = mdy(Date))

prisma_marshalls <-read_excel("data_for_marshall/digital/prisma_marshalls.xlsx") %>% 
  mutate(p_code = substr(Name,1, 6),
         `Flight start date` = ymd(`Flight start date`),
         `Flight end date` = ymd(`Flight end date`),
         flight_week_diff = abs(week(`Flight end date`) - week(`Flight start date`)))

prisma_marshalls$oct_cost_by_week <- "NA"

for(i in c(1:nrow(prisma_marshalls))){
  if(!is.na(prisma_marshalls$`2017-10 - Planned Cost`[i])){
    prisma_marshalls$oct_cost_by_week[i] <- prisma_marshalls$`2017-10 - Planned Cost`[i]/4
  } else {
    prisma_marshalls$oct_cost_by_week[i] <- prisma_marshalls$oct_cost_by_week[i - 1]
  }
}

prisma_marshalls <- prisma_marshalls %>% mutate(oct_cost_by_week = as.numeric(oct_cost_by_week))


## Marshals data
### download DCM report
dcm_marshal_reports <-  files.get(fileId=635641975,reportId = 108701505)

dcm_marshal_reports_cleaned <- dcm_marshal_reports %>% 
  mutate(p_code = substr(Placement,1, 6),
         Week = week(Date)) %>% 
  rename("DCM_clicks" = Clicks,
         "DCM_impressions" = Impressions) %>% 
  group_by(p_code, Week) %>% 
  summarise(DCM_clicks = sum(DCM_clicks),
            DCM_impressions= sum(DCM_impressions)) %>% 
  select(Week,p_code,DCM_clicks,DCM_impressions)

### double verify
dv_marshalls_cleaned <- dv_marshalls %>% 
  mutate(p_code = substr(`Placement Name`,1, 6),
         Week = week(Date)) %>% 
  select(`Campaign Name`,`Media Property`,`Placement Name`,Date,Week,p_code,`GroupM Billable Impressions`) %>% 
  group_by(p_code, Week) %>% 
  summarise(DV_impressions = sum(`GroupM Billable Impressions`))

### combine dv and dcm
marshalls_dcm_dc <- full_join(dcm_marshal_reports_cleaned,dv_marshalls_cleaned, by = c("p_code","Week"))

### prisma
prisma_marshalls_cleaned<-prisma_marshalls %>% 
  select(p_code,Supplier, `Cost method` ,Rate, `Total Planned Cost`,oct_cost_by_week, flight_week_diff)


marshal_dig_spend <- data.frame()

for(i in unique(marshalls_dcm_dc$Week)){
  marshall_dcm_dc_loop <- marshalls_dcm_dc %>% 
    filter(Week == i) %>% 
    select(-Week)
  
  data <- left_join(prisma_marshalls_cleaned, marshall_dcm_dc_loop, by = "p_code")%>% 
    mutate(DCM_clicks = ifelse(is.na(DCM_clicks),0,DCM_clicks),
           DCM_impressions= ifelse(is.na(DCM_impressions),0,DCM_impressions),
           DV_impressions= ifelse(is.na(DV_impressions),0,DV_impressions))%>% 
    mutate(Spend = case_when(`Cost method` == "CPMV" ~(DV_impressions/1000)*Rate,
                             `Cost method` == "CPA" ~ oct_cost_by_week,
                             `Cost method` == "Flat" & DV_impressions >0 & flight_week_diff != 0 ~ `Total Planned Cost`/flight_week_diff,
                             `Cost method` == "Flat" & DV_impressions >0 & flight_week_diff == 0 ~ `Total Planned Cost`,
                             `Cost method` == "Flat" & DV_impressions <= 0 ~ 0,
                             `Cost method` == "Free" ~ 0))
  data_spend <- data %>% 
    group_by(Supplier) %>% 
    summarise(Spend = sum(Spend)) 
  
  data_spend$Week <- i
  
  data_spend %>% 
    select(Week,Supplier,Spend)
  
  marshal_dig_spend <- rbind(marshal_dig_spend,data_spend)
  
  marshall_dcm_dc_loop <- NULL
  
  data <- NULL
  
  data_spend <- NULL
  
}

# turn to week to start date and end date of that week
marshal_dig_spend <- marshal_dig_spend %>% 
  mutate(start_date = as.Date(paste(0, Week, 2017, sep = "-"), format = "%w-%W-%Y"),
         end_date = as.Date(paste(6, Week, 2017, sep = "-"), format = "%w-%W-%Y"))


write.csv(marshal_dig_spend, file = "DIG/Marshall_DIG_Spend_2.csv", row.names = FALSE)




write.xlsx(marshal_dig_spend, file = "DIG/Marshall_DIG_Spend_2.xlsx", row.names = FALSE)














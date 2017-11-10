library(RDoubleClick)
library(stringr)
library(lubridate)
library(dplyr)
library(readr)
library(readxl)
library(openxlsx)

client.id <- "420112744051-v16uovgvr58clj96cujohtfe1vernjrn.apps.googleusercontent.com"
client.secret <- "J843SJWGUQtHmiT5fMXs2yGq"

DCAuth(client.id,client.secret,runwebserver = FALSE)


dv_homegoods <- read_csv("data_for_hg/dv_homegoods.csv") %>% 
  mutate(Date = mdy(Date))

prisma_homegood_1 <-read_excel("data_for_hg/prisma_homegood.xlsx", sheet = "q4") %>% 
  mutate(p_code = substr(Name,1, 6),
         `Flight start date` = ymd(`Flight start date`),
         `Flight end date` = ymd(`Flight end date`),
         flight_week_diff = abs(week(`Flight end date`) - week(`Flight start date`)))

prisma_homegood_2 <-read_excel("data_for_hg/prisma_homegood.xlsx", sheet = "2h") %>% 
  mutate(p_code = substr(Name,1, 6),
         `Flight start date` = ymd(`Flight start date`),
         `Flight end date` = ymd(`Flight end date`),
         flight_week_diff = abs(week(`Flight end date`) - week(`Flight start date`)))

## combine
prisma_homegood <- rbind(prisma_homegood_2, prisma_homegood_1)


## Marshals data
### download DCM report
dcm_homegood_reports <-  files.get(fileId=635138357,reportId = 108705867)

dcm_homegood_reports_cleaned <- dcm_homegood_reports %>% 
  mutate(p_code = substr(Placement,1, 6),
         Week = week(Date)) %>% 
  rename("DCM_clicks" = Clicks,
         "DCM_impressions" = Impressions) %>% 
  group_by(p_code, Week) %>% 
  summarise(DCM_clicks = sum(DCM_clicks),
            DCM_impressions= sum(DCM_impressions)) %>% 
  select(Week,p_code,DCM_clicks,DCM_impressions)

### double verify
dv_homegood_cleaned <- dv_homegoods %>% 
  mutate(p_code = substr(`Placement Name`,1, 6),
         Week = week(Date)) %>% 
  select(`Campaign Name`,`Media Property`,`Placement Name`,Date,Week,p_code,`GroupM Billable Impressions`) %>% 
  group_by(p_code, Week) %>% 
  summarise(DV_impressions = sum(`GroupM Billable Impressions`))

### combine dv and dcm
homegood_dcm_dc <- full_join(dcm_homegood_reports_cleaned,dv_homegood_cleaned, by = c("p_code","Week"))

### prisma
prisma_homegood_cleaned<-prisma_homegood %>% 
  select(p_code,Supplier, `Cost method` ,Rate, `Total Planned Cost`,
         # oct_cost_by_week, 
         flight_week_diff)

homegood_dig_spend <- data.frame()

for(i in unique(homegood_dcm_dc$Week)){
  homegood_dcm_dc_loop <- homegood_dcm_dc %>% 
    filter(Week == i) %>% 
    select(-Week)
  
  data <- left_join(prisma_homegood_cleaned, homegood_dcm_dc_loop, by = "p_code")%>% 
    mutate(DCM_clicks = ifelse(is.na(DCM_clicks),0,DCM_clicks),
           DCM_impressions= ifelse(is.na(DCM_impressions),0,DCM_impressions),
           DV_impressions= ifelse(is.na(DV_impressions),0,DV_impressions))%>% 
    mutate(Spend = case_when(`Cost method` == "CPMV" ~(DV_impressions/1000)*Rate,
                             #`Cost method` == "CPA" ~ oct_cost_by_week,
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
  
  homegood_dig_spend <- rbind(homegood_dig_spend,data_spend)
  
  homegood_dcm_dc_loop <- NULL
  
  data <- NULL
  
  data_spend <- NULL
  
}


write.csv(homegood_dig_spend, file = "DIG/homegood_DIG_Spend.csv", row.names = FALSE)

write.xlsx(homegood_dig_spend, file = "DIG/homegood_DIG_Spend.xlsx", row.names = FALSE)

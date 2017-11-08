library(RDoubleClick)
library(stringr)
library(lubridate)
library(dplyr)
library(readr)

client.id <- "420112744051-v16uovgvr58clj96cujohtfe1vernjrn.apps.googleusercontent.com"
client.secret <- "J843SJWGUQtHmiT5fMXs2yGq"

DCAuth(client.id,client.secret)

dv_marshalls <- read_csv("dv_marshalls.csv") %>% 
  mutate(Date = mdy(Date))

dv_homegoods <- read_csv("dv_homegoods.csv") %>% 
  mutate(Date = mdy(Date))

prisma_marshalls <-read_excel("prisma_marshalls.xlsx") %>% 
  mutate(p_code = substr(Name,1, 6),
         `Flight start date` = ymd(`Flight start date`),
         `Flight end date` = ymd(`Flight end date`),
         oct_costs_by_week = `2017-10 - Planned Cost`/4,
         sep_costs_by_week = `2017-09 - Planned Cost`/4)

## Marshals data
### download DCM report
dcm_marshal_reports <-  files.get(fileId=635063455,reportId = 108701505)

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
  select(p_code,Supplier, `Cost method` ,Rate, `Total Planned Cost`,oct_costs_by_week,sep_costs_by_week)


#marshalls_combined_data <- full_join(prisma_marshalls_cleaned, marshalls_dcm_dc, by = "p_code")

marshalls_combined_data <- left_join(prisma_marshalls_cleaned, marshalls_dcm_dc, by = "p_code")
s


marshalls_combined_data <- marshalls_combined_data %>% 
  mutate(DCM_clicks = ifelse(is.na(DCM_clicks),0,DCM_clicks),
         DCM_impressions= ifelse(is.na(DCM_impressions),0,DCM_impressions),
         DV_impressions= ifelse(is.na(DV_impressions),0,DV_impressions))

marshalls_dcm_dc_week_36 <- marshalls_dcm_dc %>% 
  filter(Week == 36) %>% 
  select(-Week)

marshalls_combined_data_week36 <- left_join(prisma_marshalls_cleaned, marshalls_dcm_dc_week_36, by = "p_code")%>% 
  mutate(DCM_clicks = ifelse(is.na(DCM_clicks),0,DCM_clicks),
         DCM_impressions= ifelse(is.na(DCM_impressions),0,DCM_impressions),
         DV_impressions= ifelse(is.na(DV_impressions),0,DV_impressions)) 


marshalls_combined_data_week36 <- marshalls_combined_data_week36 %>% 
  mutate(Spend = case_when(`Cost method` == "CPMV" ~(DV_impressions/1000)*Rate,
                           `Cost method` == "CPA" ~ `Total Planned Cost`,
                           `Cost method` == "Flat" ~ `Total Planned Cost`,
                           `Cost method` == "Free" ~ `Total Planned Cost`)) %>% 
  group_by(Supplier) %>% 
  summarise(Spend = sum(Spend))



for( i in mars)
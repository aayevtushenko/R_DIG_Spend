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


dv_stp <- read_csv("data_for_stp/dv_stp.csv") %>% 
  mutate(Date = mdy(Date))

prisma_stp<-read_excel("data_for_stp/prisma_stp.xlsx") %>% 
  mutate(p_code = substr(Name,1, 6),
         `Flight start date` = ymd(`Flight start date`),
         `Flight end date` = ymd(`Flight end date`),
         flight_week_diff = abs(week(`Flight end date`) - week(`Flight start date`)))


## Marshals data
### download DCM report
dcm_stp_reports <-  files.get(fileId=635562382,reportId = 108834654)

dcm_stp_reports_cleaned <- dcm_stp_reports %>% 
  mutate(p_code = substr(Placement,1, 6),
         Week = week(Date)) %>% 
  rename("DCM_clicks" = Clicks,
         "DCM_impressions" = Impressions,
         "DCM_video_completions" = Video.Completions) %>% 
  group_by(p_code, Week) %>% 
  summarise(DCM_clicks = sum(DCM_clicks),
            DCM_impressions= sum(DCM_impressions),
            DCM_video_completions = sum(DCM_video_completions)) %>% 
  select(Week,p_code,DCM_clicks,DCM_impressions,DCM_video_completions)

### double verify
dv_stp_cleaned <- dv_stp %>% 
  mutate(p_code = substr(`Placement Name`,1, 6),
         Week = week(Date)) %>% 
  select(`Campaign Name`,`Media Property`,`Placement Name`,Date,Week,p_code,`GroupM Billable Impressions`) %>% 
  group_by(p_code, Week) %>% 
  summarise(DV_impressions = sum(`GroupM Billable Impressions`))

### combine dv and dcm
stp_dcm_dc <- full_join(dcm_stp_reports_cleaned,dv_stp_cleaned, by = c("p_code","Week"))

### prisma
prisma_stp_cleaned<-prisma_stp %>% 
  select(p_code,Supplier, `Cost method` ,Rate, `Total Planned Cost`,
         # oct_cost_by_week, 
         flight_week_diff)

stp_dig_spend <- data.frame()

for(i in unique(stp_dcm_dc$Week)){
  stp_dcm_dc_loop <- stp_dcm_dc %>% 
    filter(Week == i) %>% 
    select(-Week)
  
  data <- left_join(prisma_stp_cleaned, stp_dcm_dc_loop, by = "p_code")%>% 
    mutate(DCM_clicks = ifelse(is.na(DCM_clicks),0,DCM_clicks),
           DCM_impressions= ifelse(is.na(DCM_impressions),0,DCM_impressions),
           DV_impressions= ifelse(is.na(DV_impressions),0,DV_impressions))%>% 
    mutate(Spend = case_when(`Cost method` == "CPMV" ~(DV_impressions/1000)*Rate,
                             `Cost method` == "Flat" & DV_impressions >0 & flight_week_diff != 0 ~ `Total Planned Cost`/flight_week_diff,
                             `Cost method` == "Flat" & DV_impressions >0 & flight_week_diff == 0 ~ `Total Planned Cost`,
                             `Cost method` == "Flat" & DV_impressions <= 0 ~ 0,
                             `Cost method` == "Free" ~ 0,
                             `Cost method` == "CPCV" ~ DCM_video_completions*Rate,
                             `Cost method` == "CPM"~ (DCM_impressions/1000)*Rate))
  data_spend <- data %>% 
    group_by(Supplier) %>% 
    summarise(Spend = sum(Spend)) 
  
  data_spend$Week <- i
  
  data_spend %>% 
    select(Week,Supplier,Spend)
  
  stp_dig_spend <- rbind(stp_dig_spend,data_spend)
  
  stp_dcm_dc_loop <- NULL
  
  data <- NULL
  
  data_spend <- NULL
  
}

# turn to week to start date and end date of that week
stp_dig_spend <- stp_dig_spend %>% 
  mutate(start_date = as.Date(paste(0, Week, 2017, sep = "-"), format = "%w-%W-%Y"),
         end_date = as.Date(paste(6, Week, 2017, sep = "-"), format = "%w-%W-%Y"))


write.xlsx(stp_dig_spend, file = "DIG/stp_DIG_Spend.xlsx", row.names = FALSE)

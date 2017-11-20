# import files from dv, dcm and prisma and join them by using the pcodes. Then calculate the cost based on the cost method in prisma.
# the scripts allow calculating single or multiple weekly spend
# raw data are put in folder starts with "data_for"
# output excel files are put into "DIG" folder

library(RDoubleClick) # package for dcm
library(stringr) # for string mainipulation
library(lubridate) # for date manipulation
library(dplyr) # for data cleaning
library(readr) # for reading files
library(readxl) # for reading xlsx files
library(openxlsx) # for writing excel files

# DCN API: get access by using client.id and client.secret
client.id <- "963284085963-6tehp3a30ovmjuk3v4bohg2t16no79pj.apps.googleusercontent.com"
client.secret <- "hwatT9yHYdTZBnoGgNCVBS2J"

DCAuth(client.id,client.secret,runwebserver = FALSE)

# import double verify 
dv_homegoods <- read_csv("data_for_hg/dv_homegoods.csv") %>% 
  mutate(Date = mdy(Date))

# import prisma files
prisma_homegood_1 <-read_excel("data_for_hg/prisma_homegood.xlsx", sheet = "q4") %>% 
  mutate(p_code = substr(Name,1, 6), # get the pcode
         `Flight start date` = ymd(`Flight start date`),
         `Flight end date` = ymd(`Flight end date`),
         flight_week_diff = abs(week(`Flight end date`) - week(`Flight start date`)))

prisma_homegood_2 <-read_excel("data_for_hg/prisma_homegood.xlsx", sheet = "2h") %>% 
  mutate(p_code = substr(Name,1, 6), # get the pcode
         `Flight start date` = ymd(`Flight start date`),
         `Flight end date` = ymd(`Flight end date`),
         flight_week_diff = abs(week(`Flight end date`) - week(`Flight start date`)))

## combine
prisma_homegood <- rbind(prisma_homegood_2, prisma_homegood_1)


## download DCM report, reports needs to be built on DCM site. ONly problems for this R package
dcm_homegood_reports <-  files.get(fileId=635138357,reportId = 108705867)

dcm_homegood_reports_cleaned <- dcm_homegood_reports %>% 
  mutate(p_code = substr(Placement,1, 6), # get the pcodes
         Week = week(Date)) %>% 
  rename("DCM_clicks" = Clicks,
         "DCM_impressions" = Impressions) %>%  # rename columns
  group_by(p_code, Week) %>%   # sum based on p_code and week (sumifs in excel)
  summarise(DCM_clicks = sum(DCM_clicks),
            DCM_impressions= sum(DCM_impressions)) %>% 
  select(Week,p_code,DCM_clicks,DCM_impressions) # keep relevant columns

### double verify
dv_homegood_cleaned <- dv_homegoods %>% 
  mutate(p_code = substr(`Placement Name`,1, 6), # get pcode
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

# loops to go combine homegood_dcm_dc and prisma_cleaned data based on the weeks in the former dataset. Only keep relevant pcodes' data that are 
#  present in prisma_cleaned data. Then calculate based on "cost method" column 

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

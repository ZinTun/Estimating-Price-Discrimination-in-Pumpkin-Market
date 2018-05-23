library(readr)
data <- read_csv("C:/Users/SONG/Desktop/Data All - Cleaned.csv")

data_all <- data[,c("City_Name","Package","Variety","Origin","Repack","Trans_Distance_(kM)", "Item_Size","packagetype","High_Price","Unit_Price","Net_Unit_Price","is_selected","Trans_Cost","Unified_Package_Size","Per_Capita_Income","Year","Week")]

data_all$`City Name`  <- as.factor(data_all$City_Name)
data_all$Variety <- as.factor(data_all$Variety)
data_all$Package <- as.factor(data_all$Package)
data_all$Origin <- as.factor(data_all$Origin)
data_all$`Item Size` <- as.factor(data_all$Item_Size)
data_all$Repack <- as.factor(data_all$Repack)

library(dplyr)

bnb_data<- data_all %>% filter(is_selected ==1)


bnb_data$`City Name`=relevel(bnb_data$`City Name`,ref="ST. LOUIS")
bnb_data$`Item Size`=relevel(bnb_data$`Item Size`,ref="sml")


########################################################################
###########Question 1: 2nd & 3rd degree discrimination##################
########################################################################

#Baseline Model

lm_baseline = lm(formula = Net_Unit_Price ~ `City Name`+Unified_Package_Size +Year+Week +Variety*`Item Size`*Origin ,data=bnb_data)
summary(lm_baseline)






########################################################################
###########Question 2: Cause for Price discrimination##################
########################################################################

# replace city with per capita income

bnb_data$Per_Capita_Income=bnb_data$Per_Capita_Income/10000

lm_income = lm(formula = Net_Unit_Price ~ Per_Capita_Income+ Unified_Package_Size +Year+Week +Variety*`Item Size`*Origin ,data=bnb_data)
summary(lm_income)





########################################################################
############################  Robustness Test ##########################
########################################################################


#robustness test 1: Test assumption on Unified Package Size
#1) 36 inch bin
bnb_data$`City Name`=relevel(bnb_data$`City Name`,ref="ST. LOUIS")
bnb_data_36inchbin<- bnb_data %>% filter(Package =="36 inch bins") 

lm_net_hp_36inchbin = lm(formula = High_Price ~ `City Name`+`Trans_Distance_(kM)` +Year+Week +Variety*`Item Size`*Origin ,data=bnb_data_36inchbin)
summary(lm_net_hp_36inchbin)

#2) 24 inch bin
bnb_data$`City Name`=relevel(bnb_data$`City Name`,ref="ST. LOUIS")
bnb_data_24inchbin<- bnb_data %>% filter(Package =="24 inch bins") 

lm_net_hp_24inchbin = lm(formula = High_Price ~ `City Name`+`Trans_Distance_(kM)` +Year+Week +Variety*`Item Size`*Origin ,data=bnb_data_24inchbin)
summary(lm_net_hp_24inchbin)



#robustness test 2 : Test assumption on transportation cost

bnb_data$`City Name`=relevel(bnb_data$`City Name`,ref="SAN FRANCISCO")

lm_distance = lm(formula = Unit_Price ~ `City Name`+ Unified_Package_Size  + `Trans_Distance_(kM)`+Year+Week +Variety*`Item Size`*Origin ,data=bnb_data)
summary(lm_distance)










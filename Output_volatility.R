## By Dr. Farah Mugrabi
#Instructions: Follow @ to select options

## Load Libraries
library(dplyr)
library(lubridate)
library(DisaggregateTS)
library(zoo)
library(csodata)
library(ggplot2)
library(ecb)
library(openxlsx)
library(stringr)
library(forecast)

#Paths and folders
rm(list = ls())
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
getwd()

dir.create(file.path(path, "B_Results", "Plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "B_Results", "Tables"), recursive = TRUE, showWarnings = FALSE)

#Irish GNI
#GNI----------------
#Yearly GNI
GNI<- cso_get_data('NA001', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
  filter(Item=='10. Modified gross national income at current market prices')%>%
  filter(Statistic== 'Modified Gross National Income at Current Market Prices') %>% 
  dplyr::select(Year,value) %>% 
  rename(GNI=value) %>% 
  mutate(Year= as.Date(paste0(Year, '-10-01', "%Y"))) #raw data in EU MILLION

#Quarterly GNI PRE 1998-01-01
#Source: Central Bank of Ireland - Macro Financial Division (MFD) - gdp_Haver.xlsx - Sheet: GNIstar_adjustment - Column D: gnistar
GNI_pre_1998Q1<- readxl::read_xlsx(paste0(path,"/A.Data/", "Data_pre.xlsx"),sheet='Quarterly', range = "A1:E10000")  %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  mutate(GNI_pre_1998Q1 = GNI_pre_1998Q1*1000) %>% 
  dplyr::select(Date,GNI_pre_1998Q1) %>% 
  filter(Date<="1997-10-01")

#Quarterly Modified Total Domestic Demand
MDD<-cso_get_data('NAQ05', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>%  
  filter(Sector=='Modified Total Domestic Demand')%>%
  filter(Statistic== '\tModified Total Domestic Demand and Components of Modified Gross Domestic Fixed Capital Formation at Current Market Prices') %>% 
  dplyr::select(Year,value) %>% 
  rename(MDD=value)%>% 
  mutate( lag0=lag(MDD,0),
          lag1=lag(MDD,1),
          lag2=lag(MDD,2),
          lag3=lag(MDD,3)) %>% 
  na.omit() %>% 
  mutate(MDD_4qra=lag0+lag1+lag2+lag3) %>% 
  dplyr::select(Year,MDD,MDD_4qra) 

#GNI quarterly linear interpolation
data<- merge.data.frame(MDD,GNI, all.x = T, by= "Year") %>% 
  mutate(ratio=GNI/MDD_4qra) %>% 
  mutate(quarter=ifelse(month(Year)==1, 1,ifelse(month(Year)==4,2,ifelse(month(Year)==7,3,4)))) %>% 
  mutate(ratio_q1= lag(ratio,0)) %>%
  mutate(ratio_q2= lag(ratio,1)) %>%
  mutate(ratio_q3= lag(ratio,2)) %>%
  mutate(ratio_q4= lag(ratio,3)) %>%
  mutate(ratio1= coalesce(ratio_q1, ratio_q2, ratio_q3, ratio_q4)) %>% 
  dplyr::select(!c(ratio_q1, ratio_q2, ratio_q3, ratio_q4)) %>% 
  mutate(ratio1=ifelse(is.na(ratio1),last(ratio, na_rm = T), ratio1)) %>% 
  mutate(GNIq=ifelse(!is.na(GNI), GNI, ratio1*MDD_4qra)) %>% #GNI quarterly linear interpolation
  dplyr::select(!quarter) %>% 
  rename(Date=Year)

#Unemployment
unemployment<-  cso_get_data("MUM01", pivot_format = "tall", use_factors = FALSE, use_dates = TRUE) %>% 
  filter(Statistic=="Seasonally Adjusted Monthly Unemployment Rate", Age.Group == "15 - 74 years",Sex == "Both sexes") %>% 
  mutate(Date = as.Date(as.yearqtr(Month, format = "%Y-%m-%d"))) %>% 
  dplyr::select(Date,value) %>% group_by(Date) %>% summarise(value = mean(value)) %>% 
  rename(unemployment=value)
data<- merge.data.frame(data, unemployment, by = 'Date')

#Chow-Li interpolation
GNI_cl<-disaggregate(
  Y=as.matrix(na.omit(data[,'GNI'])),
  X =as.matrix(data[,c("MDD", 'unemployment')]),
  aggMat = "sum",
  aggRatio = 4,
  method = "Chow-Lin",
  Denton = "additive-first-diff")

data$GNI_cl<- as.vector(GNI_cl$y_Est)
data<- data %>%  mutate( lag0=lag(GNI_cl,0),
                         lag1=lag(GNI_cl,1),
                         lag2=lag(GNI_cl,2),
                         lag3=lag(GNI_cl,3)) %>%
  mutate(GNI_cl=lag0+lag1+lag2+lag3) 
data$GNI_cl<- rowSums(data[c("lag0", "lag1", "lag2", "lag3")], na.rm = T)
data<- data %>% dplyr::select(!c(lag0, lag1, lag2, lag3))
data[1,'GNI_cl']<- data[1,'GNIq']
data[2,'GNI_cl']<- data[2,'GNIq']
data[3,'GNI_cl']<- data[3,'GNIq']

#Merge GNI pre 1998 and post estimations
data<- bind_rows(data,GNI_pre_1998Q1) %>% arrange(Date) %>% 
  mutate(GNI_cl=ifelse(!is.na(GNI_cl),GNI_cl,GNI_pre_1998Q1),
         GNIq=ifelse(!is.na(GNIq),GNIq,GNI_pre_1998Q1)) %>% 
  dplyr::select(!GNI_pre_1998Q1)

#CPI Ireland all items--------------
CPI_all<- cso_get_data('CPM01', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
  filter(Statistic=='Consumer Price Index (Base Dec 2016=100)')%>%
  filter(Commodity.Group== 'All items') %>% 
  rename(Date=Month) %>% 
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Date,value) %>% 
  rename(CPI_all=value) %>% 
  as.data.frame()

CPI_all_q<- CPI_all %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Date =as.Date(as.yearqtr(Date))) %>% 
  group_by(Date) %>%
  mutate(CPI_all_q=mean(CPI_all, na.rm = T)) %>% #in the original file the take the value of the last month of the quarter, here we take the average across the quarter 
  ungroup() %>% 
  dplyr::select(Date, CPI_all_q) %>% 
  distinct()%>% 
  as.data.frame() %>% 
  mutate(CPI= CPI_all_q/last(CPI_all_q))# rebased to 100 LAST quarter, so variables are expressed in contemporaneous values

data<-merge.data.frame(data, CPI_all_q, by='Date')

#Compute Q-O-Q change
data<- data %>% 
  mutate(GNI_cl_qoq=(GNI_cl/lag(GNI_cl)-1)) %>% 
  mutate(GNIq_qoq=(GNIq/lag(GNIq)-1)) %>% 
  mutate(GNI_lcl= log10(GNI_cl/1000)) %>% 
  mutate(GNI_lq= log10(GNIq/1000)) %>% 
  mutate(GNI_rlcl= log10((GNI_cl/CPI)/1000)) %>% 
  mutate(GNI_rlq= log10((GNIq/CPI)/1000)) %>% 
  mutate(GNI_rcl= (GNI_cl*CPI)) %>% 
  mutate(GNI_rq= (GNIq*CPI))

#Plots
GNI_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_cl/1000, color='Chow-Li'), size=1.5)+
  geom_line(aes(y=GNIq/1000, color='Linear'), size=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI (Euro billion)")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

GNI_r_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_rlcl, color='Chow-Li'), size=1.5)+
  geom_line(aes(y=GNI_rlq, color='Linear'), size=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI (real Euro billion)")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

GNI_yoy_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_cl_qoq, color='Chow-Li'), size=1.5)+
  geom_line(aes(y=GNIq_qoq, color='Linear'), size=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI q-o-q growth (%)")+
  guides(color = guide_legend(title = "Interpolation"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

chowlinvslinear<- (sd(data$GNIq_qoq,na.rm = T)-sd(data$GNI_cl_qoq,na.rm = T))*100 #dispersion of linear interpolation is x percentage points above the one obtained with Chow-Li

ggsave(paste0(path,"/B_Results/Plots/GNI.pdf"), GNI_plot, height = 20, width = 25)
ggsave(paste0(path,"/B_Results/Plots/GNI_yoy.pdf"), GNI_yoy_plot, height = 20, width = 25)
ggsave(paste0(path,"/B_Results/Plots/GNI_r.pdf"), GNI_r_plot, height = 20, width = 25)

#EU countries
data_eu<- 

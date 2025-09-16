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
library(writexl)
library(countrycode)
library(ggrepel)

#Paths and folders
rm(list = ls())
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
getwd()

dir.create(file.path(path, "B.Results", "Plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "B.Results", "Tables"), recursive = TRUE, showWarnings = FALSE)
source('ECB_get_data.R')
ecb_api = "https://data-api.ecb.europa.eu/service/data" 

#Irish GNI----------------------------------------------
#GNI----------------
#Yearly GNI
GNI<- csodata::cso_get_data('NA001', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
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

#Quarterly Modified Total Domestic Demand
MDD_f<-cso_get_data('NAQ05', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>%  
  filter(Sector=='Modified Final Domestic Demand')%>%
  filter(Statistic== 'Modified Total Domestic Demand and Components of Modified Gross Domestic Fixed Capital Formation at Current Market Prices (Seasonally Adjusted)') %>%
  dplyr::select(Year,value) %>% 
  rename(MDD_f=value) %>%  
  na.omit() %>% 
  dplyr::select(Year,MDD_f) 

MDD<- merge.data.frame(MDD, MDD_f, by="Year")

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
data_gni<- data %>% 
  mutate(GNI_cl_qoq=(GNI_cl/lag(GNI_cl)-1)) %>% 
  mutate(GNIq_qoq=(GNIq/lag(GNIq)-1)) %>% 
  mutate(GNI_lcl= log10(GNI_cl/1000)) %>% 
  mutate(GNI_lq= log10(GNIq/1000)) %>% 
  mutate(GNI_rlcl= log10((GNI_cl/CPI)/1000)) %>% 
  mutate(GNI_rlq= log10((GNIq/CPI)/1000)) %>% 
  mutate(GNI_rcl= (GNI_cl*CPI)) %>% 
  mutate(GNI_rq= (GNIq*CPI))

#Plots
GNI_plot<- ggplot(data=data_gni, aes(x=Date))+
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

GNI_r_plot<- ggplot(data=data_gni, aes(x=Date))+
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

GNI_yoy_plot<- ggplot(data=data_gni, aes(x=Date))+
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

chowlinvslinear<- (sd(data_gni$GNIq_qoq,na.rm = T)-sd(data_gni$GNI_cl_qoq,na.rm = T))*100 #dispersion of linear interpolation is x percentage points above the one obtained with Chow-Li

ggsave(paste0(path,"/B.Results/Plots/GNI.pdf"), GNI_plot, height = 20, width = 25)
ggsave(paste0(path,"/B.Results/Plots/GNI_yoy.pdf"), GNI_yoy_plot, height = 20, width = 25)
ggsave(paste0(path,"/B.Results/Plots/GNI_r.pdf"), GNI_r_plot, height = 20, width = 25)


#GNI --------------------------------
# data_gni<-read.csv(paste0(path,"/A.Data/", "data_full.csv"))[,c('Date', 'GNI_rcl')]

#EU countries--------------------------------------------------------------------
EA_countries <- c("AT", "BE", "CY", "DE", "EE", "ES", "FI", "FR", "GR", "IE", "IT", "LU", "LT", "LV", "MT", "NL", "PT", "SI", "SK")
countrycode(EA_countries, origin = "iso2c", destination = 'country.name')
countries <- c(EA_countries, "I8", "U2") #@Select countries 
countries <- c(EA_countries, "I8","U2") #@Select countries 

## GDP (level)
gdp_keys <- readxl::read_xlsx(paste0(path,"/A.data/Series_Keys.xlsx"), sheet = "GDP") %>% 
  filter(Country %in% countries)

gdp<- list()  
for(i in 1:length(gdp_keys$Key)){
  gdp[[i]] = get_data(ecb_api, gdp_keys$Key[i])}

gdp<- bind_rows(gdp) %>% 
  dplyr::select(ref_area, obstime, obsvalue) %>% 
  rename(ISO2 = ref_area, Date = obstime, GDP = obsvalue) %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) 

## Inflation (CPI - Percentage change)
cpi_keys <- readxl::read_xlsx(paste0(path,"/A.data/Series_Keys.xlsx"), sheet = "CPI") %>% 
  filter(Country %in% countries)

cpi<- list()  
for(i in 1:length(cpi_keys$Key)){
  cpi[[i]] = get_data(ecb_api, cpi_keys$Key[i])}

cpi <- cpi %>% 
  bind_rows() %>% 
  dplyr::select(ref_area, obstime, obsvalue) %>% 
  rename(ISO2 = ref_area, Date = obstime, CPI = obsvalue) %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-%m")))%>% 
  group_by(ISO2, Date) %>% 
  summarise(CPI = mean(CPI, na.rm = TRUE), .groups = "drop") %>% 
  arrange(ISO2, Date)

data <- gdp %>%
  inner_join(cpi, by = c("ISO2", "Date")) %>% 
  mutate(GDP_r=GDP / (1 + CPI/100)) %>% 
  arrange(ISO2, Date)

#Investment
inv_keys <- readxl::read_xlsx(paste0(path,"/A.data/Series_Keys.xlsx"), sheet = "Investment") %>% 
  filter(ISO2 %in% countries)

inv<- list()  
for(i in 1:length(inv_keys$key)){
  inv[[i]] = get_data(ecb_api, inv_keys$key[i])}

inv <- inv %>% 
  bind_rows() %>% 
  dplyr::select(ref_area, obstime, obsvalue) %>% 
  rename(ISO2 = ref_area, Date = obstime, inv = obsvalue) %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  group_by(ISO2, Date) %>% 
  summarise(INV = mean(inv, na.rm = TRUE), .groups = "drop") %>% 
  arrange(ISO2, Date)

inv <- inv %>%
  inner_join(cpi, by = c("ISO2", "Date")) %>% 
  mutate(INV_r=INV / (1 + CPI/100)) %>% 
  arrange(ISO2, Date) %>% 
  dplyr::select(-CPI)

#Demand
dem_keys <- readxl::read_xlsx(paste0(path,"/A.data/Series_Keys.xlsx"), sheet = "Demand") %>% 
  filter(ISO2 %in% countries)
dem_keys$key[dem_keys$key == "MNA.Q.Y.U2.W0.S1.S1.D.P3T5._Z._Z._Z.EUR.V.N"] <- "MNA.Q.Y.I8.W0.S1.S1.D.P3T5._Z._Z._Z.EUR.V.N"
dem_keys$ISO2[dem_keys$ISO2 == "U2"] <- "I8"

dem<- list()  
for(i in 1:length(dem_keys$key)){
  dem[[i]] = get_data(ecb_api, dem_keys$key[i])}

dem <- dem %>% 
  bind_rows() %>% 
  dplyr::select(ref_area, obstime, obsvalue) %>% 
  rename(ISO2 = ref_area, Date = obstime, dem = obsvalue) %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  group_by(ISO2, Date) %>% 
  summarise(DEM = mean(dem, na.rm = TRUE), .groups = "drop") %>% 
  arrange(ISO2, Date)

dem <- dem %>%
  inner_join(cpi, by = c("ISO2", "Date")) %>% 
  mutate(DEM_r=DEM / (1 + CPI/100)) %>% 
  arrange(ISO2, Date) %>% 
  dplyr::select(-CPI)

data <- data %>%
  left_join(inv, by = c("ISO2", "Date")) %>%
  left_join(dem, by = c("ISO2", "Date"))

#GNI for IE
data_gni <- data_gni %>%
  mutate(Date = as.Date(Date))

ie_gni_rows <- data_gni %>%
  transmute(
    ISO2   = "IE_GNI",
    Date,
    GDP    = NA_real_,
    GDP  = GNI_cl,
    DEM  = MDD_f)%>%
  mutate(Date = as.Date(Date))

ie_cpi <- cpi %>%
  filter(ISO2 == "IE") %>%
  group_by(Date) %>%
  summarise(CPI = first(CPI), .groups = "drop")%>%
  mutate(Date = as.Date(Date))

ie_gni_rows <- ie_gni_rows %>%
  left_join(ie_cpi, by = "Date") %>% 
  mutate(GDP_r=GDP / (1 + CPI/100)) %>% 
  mutate(DEM_r=DEM / (1 + CPI/100)) %>% 
  mutate(Date = as.Date(Date))

data <- data %>%
  bind_rows(ie_gni_rows) %>%
  arrange(ISO2, Date)

#Output volatility------------------------------------------------
#Defined as the 8 quarters rolling standard deviation of the real GDP growth
year_set<-1
changeset<- 1

outputvol_data <- data %>%
  group_by(ISO2) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(gdp_growth = 100 * (GDP_r / lag(GDP_r,year_set) - 1)) %>%          
  mutate(output_mean = rollapply(data = gdp_growth, widt = 4*year_set, FUN = mean,align = "right", fill = NA,na.rm = TRUE)) %>%
  mutate(output_sd = rollapply(data = gdp_growth, width = 4*year_set, FUN = sd,align = "right", fill = NA,na.rm = TRUE)) %>%
  mutate(ouput_z_score = (GDP_r - output_mean) / output_sd) %>%
  ungroup()

outputvol_data<-  outputvol_data %>%
  group_by(ISO2) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(INV_growth = 100 * (INV_r / lag(INV_r,year_set) - 1)) %>%    
  mutate(DEM_growth = 100 * (DEM_r / lag(DEM_r,year_set) - 1)) %>% 
  # mutate(INV_sd = rollapply(data = INV_growth, width = 4*year_set, FUN = sd,align = "right", fill = NA,na.rm = TRUE)) %>%
  # mutate(DEM_sd = rollapply(data = DEM_growth, width = 4*year_set, FUN = sd,align = "right", fill = NA,na.rm = TRUE)) %>%
  mutate(INV_mean = rollapply(data = INV_growth, width = 4*year_set, FUN = mean,align = "right", fill = NA,na.rm = TRUE)) %>%
  mutate(DEM_mean = rollapply(data = DEM_growth, width = 4*year_set, FUN = mean,align = "right", fill = NA,na.rm = TRUE)) %>%
  mutate(INV_sd = rollapply(data = INV_growth, width = 4*year_set, FUN = sd,align = "right", fill = NA,na.rm = TRUE)) %>%
  mutate(DEM_sd = rollapply(data = INV_growth, width = 4*year_set, FUN = sd,align = "right", fill = NA,na.rm = TRUE)) %>%
  ungroup()

ref_eu <- outputvol_data %>%
  filter(ISO2 == "I8") %>%
  select(Date, output_vol_EU = output_sd, INV_vol_EU = INV_sd, DEM_vol_EU = DEM_sd)

ref_eu <- outputvol_data %>%
  group_by(Date) %>%                                
  summarise(                                       
    output_vol_EU = mean(output_sd, na.rm = TRUE),
    INV_vol_EU    = mean(INV_sd,     na.rm = TRUE),
    DEM_vol_EU    = mean(DEM_sd,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Date)     

outputvol_data <- outputvol_data %>%
  left_join(ref_eu, by = "Date") %>%
  mutate(output_vol_rel = if_else(!is.na(output_vol_EU) & output_vol_EU != 0,output_sd / output_vol_EU, NA_real_)) %>%
  mutate(INV_vol_rel = if_else(!is.na(INV_vol_EU) & INV_vol_EU != 0,INV_sd / INV_vol_EU, NA_real_)) %>%
  mutate(DEM_vol_rel = if_else(!is.na(DEM_vol_EU) & DEM_vol_EU != 0,DEM_sd / DEM_vol_EU, NA_real_)) %>%
  arrange(ISO2, Date)

#Plot----------
cbi_palette = c("#0B5471", "#7C477E", "#0083A0", "#5EC5C2", "#D2E288", "#007DC5", "#D12E7C", "#F57D20", "#FCAF17", "#DFCA94", "#000000", "#7e878e")
iso_levels <- sort(unique(outputvol_data$ISO2))
pal_all <- setNames(rep(cbi_palette, length.out = length(iso_levels)), iso_levels)
outputvol_data <- outputvol_data %>%
  mutate(ISO2 = factor(ISO2, levels = iso_levels))

plotdata<- outputvol_data %>% 
  dplyr::filter(Date >'1990-01-01') 

min_d <- floor_date(min(plotdata$Date, na.rm = TRUE), unit = "quarter")
max_d <- ceiling_date(max(plotdata$Date, na.rm = TRUE), unit = "quarter")
breaks_q <- label_quarter <- function(x) paste0(year(x), " Q", quarter(x))
label_quarter <- function(x) paste0(year(x), " Q", quarter(x))

plot_outputvol<- plotdata %>% 
  dplyr::filter(ISO2 %in% c('IE','IE_GNI', 'U2','DE')) %>% 
  ggplot(., aes(x = Date, y = output_sd, color = ISO2, group = ISO2)) +
  geom_line(na.rm = TRUE, linewidth = 1.5) +
  labs(
    title = "Output volatility (rolling 8 quarters standard deviation)",
    x = "Date", y = "(%)", color = "Country") +
  scale_x_date( date_breaks = "2 years", labels  = label_quarter, limits = c(min_d, max_d),
    expand = expansion(mult = c(0.01, 0.03)))+
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = pal_all) +
  scale_linewidth_manual(values = c("IE_GNI" = 5), guide = "none")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50),
        axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(paste0(path,"/B.Results/Plots/plot_ouputvol.pdf"), plot_outputvol, height = 20, width = 25)
ggsave(filename = file.path(path, "B.Results/Plots/plot_outputvol.png"),plot= plot_outputvol,height   = 20, width    = 25, dpi      = 300)

#Plot relative GNI---
iso_subset <- c('I8', "IE","IE_GNI",'ES','FR','DE',"GR","BE")
lty_all <- setNames(rep("solid", length(iso_subset)), iso_subset)
lty_all["IE_GNI"] <- "dashed"   # o "dotted", "longdash", etc.
lab_all <- setNames(iso_subset, iso_subset)
lab_all["I8"] <- "Euro area (19 countries)"

plot_diff<- plotdata %>% 
  dplyr::filter(ISO2 %in% iso_subset) %>% 
  ggplot(., aes(x = Date, y = output_vol_rel, color = ISO2, linetype = ISO2, group = ISO2)) +
  geom_line(na.rm = TRUE, linewidth = 1.5) +
  labs(
    title = "Output volatility Relative to Euro Area",
    x = "Date",y = "(%)",color = "Country") +
  scale_x_date( date_breaks = "2 years", labels  = label_quarter, limits = c(min_d, max_d),
                expand = expansion(mult = c(0.01, 0.03)))+
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = pal_all, labels = lab_all) +
  scale_linetype_manual(values = lty_all, guide = "none") +  
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50),
        axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(paste0(path,"/B.Results/Plots/plot_relative_ouputvol.pdf"), plot_diff, height = 20, width = 25)
ggsave(filename = file.path(path, "B.Results/Plots/plot_relative_outputvol.png"),plot     = plot_diff,height   = 20, width    = 25, dpi      = 300)

#Plot relative Demand---
iso_subset <- c('I8', "IE",'ES','FR','DE',"GR","CY")
lty_all <- setNames(rep("solid", length(iso_subset)), iso_subset)
lty_all["IE"] <- "dashed"   # o "dotted", "longdash", etc.
lab_all <- setNames(iso_subset, iso_subset)
lab_all["I8"] <- "Euro area (19 countries)"

plot_diff_dem<- plotdata %>% 
  dplyr::filter(ISO2 %in% iso_subset) %>% 
  ggplot(., aes(x = Date, y = DEM_vol_rel, color = ISO2, linetype = ISO2, group = ISO2)) +
  geom_line(na.rm = TRUE, linewidth = 1.5) +
  labs(
    title = "Demand volatility Relative to Euro Area",
    x = "Date",y = "(%)",color = "Country") +
  scale_x_date( date_breaks = "2 years", labels  = label_quarter, limits = c(min_d, max_d),
                expand = expansion(mult = c(0.01, 0.03)))+
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = pal_all, labels = lab_all) +
  scale_linetype_manual(values = lty_all, guide = "none") +  
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50),
        axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(paste0(path,"/B.Results/Plots/plot_relative_demvol.pdf"), plot_diff_dem, height = 20, width = 25)
ggsave(filename = file.path(path, "B.Results/Plots/plot_relative_demvol.png"),plot= plot_diff_dem,height   = 20, width    = 25, dpi      = 300)

#Scarlett plots---------------------------------------
# plotdata<- plotdata %>%  mutate(ISO2= ifelse(ISO2=="I8", 'EU Average (I8)',ISO2 ))
plotdata <- plotdata %>%
  mutate(ISO2 = as.character(ISO2),
         ISO2 = ifelse(ISO2 == "I8", "EU (I8)", ISO2),
         ISO2 = factor(ISO2))

#Scarlett relative Demand---

scatter_plot_data<- plotdata %>% 
  mutate(DEM_growth_yoy=DEM_r/lag(DEM_r,4)-1) %>% 
  filter(Date>="1999-1-1") %>% 
  filter(!between(Date, as.Date("2020-01-01"), as.Date("2021-01-01")))%>%
  filter(!ISO2 %in% c("IE_GNI", "U2")) %>%  
  group_by(ISO2) %>%                                
  summarise(                                       
    DEM_sd = sd(DEM_growth_yoy, na.rm = TRUE),
    DEM_growth  = mean(DEM_growth_yoy,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ISO2) 

p_scatter_dem <- ggplot(scatter_plot_data, aes(x = DEM_growth, y = DEM_sd)) +
  geom_point(aes(color = ISO2), size = 15, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.5, color = "grey30") +
  geom_text_repel(aes(label = ISO2, color = ISO2),
                  size = 20, max.overlaps = Inf, box.padding = 0.4, point.padding = 0.2, show.legend = FALSE) +
  scale_color_manual(values = rep(cbi_palette, length.out = dplyr::n_distinct(scatter_plot_data$ISO2))) +
  labs(
    title = "Demand volatility vs. demand growth",
    x = "Demand growth (YoY, %)",
    y = "Demand volatility (country / EU)",
    color = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
p_scatter_dem
ggsave(paste0(path,"/B.Results/Plots/demand_full.pdf"), p_scatter_dem, height = 20, width = 25)
ggsave(filename = file.path(path, "B.Results/Plots/demand_full.png"),plot= p_scatter_dem,height   = 20, width    = 25, dpi      = 300)

#Pre GFC
scatter_plot_data<- plotdata %>% 
  mutate(DEM_growth_yoy=DEM_r/lag(DEM_r,4)-1) %>% 
  filter(Date>="1999-1-1") %>% 
  filter(Date<="2007-1-1") %>% 
  filter(!ISO2 %in% c("IE_GNI", "U2")) %>%  
  group_by(ISO2) %>%                                
  summarise(                                       
    DEM_sd = sd(DEM_growth_yoy, na.rm = TRUE),
    DEM_growth  = mean(DEM_growth_yoy,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ISO2) 

p_scatter_dem <- ggplot(scatter_plot_data, aes(x = DEM_growth, y = DEM_sd)) +
  geom_point(aes(color = ISO2), size = 15, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.5, color = "grey30") +
  geom_text_repel(aes(label = ISO2, color = ISO2),
                  size = 20, max.overlaps = Inf, box.padding = 0.4, point.padding = 0.2, show.legend = FALSE) +
  scale_color_manual(values = rep(cbi_palette, length.out = dplyr::n_distinct(scatter_plot_data$ISO2))) +
  labs(
    title = "Demand volatility vs. demand growth (Pre-GFC)",
    x = "Demand growth (YoY, %)",
    y = "Demand volatility (country / EU)",
    color = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
p_scatter_dem
ggsave(paste0(path,"/B.Results/Plots/demand_pre_GFC.pdf"), p_scatter_dem, height = 20, width = 25)
ggsave(filename = file.path(path, "B.Results/Plots/demand_pre_GFC.png"),plot= p_scatter_dem,height   = 20, width    = 25, dpi      = 300)

#Post GFC
scatter_plot_data<- plotdata %>% 
  mutate(DEM_growth_yoy=DEM_r/lag(DEM_r,4)-1) %>% 
  filter(Date>="2010-1-1") %>% 
  filter(!between(Date, as.Date("2020-01-01"), as.Date("2021-01-01")))%>%
  filter(!ISO2 %in% c("IE_GNI", "U2")) %>%  
  group_by(ISO2) %>%                                
  summarise(                                       
    DEM_sd = sd(DEM_growth_yoy, na.rm = TRUE),
    DEM_growth  = mean(DEM_growth_yoy,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ISO2) 
p_scatter_dem <- ggplot(scatter_plot_data, aes(x = DEM_growth, y = DEM_sd)) +
  geom_point(aes(color = ISO2), size = 15, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.5, color = "grey30") +
  geom_text_repel(aes(label = ISO2, color = ISO2),
                  size = 20, max.overlaps = Inf, box.padding = 0.4, point.padding = 0.2, show.legend = FALSE) +
  scale_color_manual(values = rep(cbi_palette, length.out = dplyr::n_distinct(scatter_plot_data$ISO2))) +
  labs(
    title = "Demand volatility vs. demand growth (Post-GFC)",
    x = "Demand growth (YoY, %)",
    y = "Demand volatility (country / EU)",
    color = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
p_scatter_dem
ggsave(paste0(path,"/B.Results/Plots/demand_post_GFC.pdf"), p_scatter_dem, height = 20, width = 25)
ggsave(filename = file.path(path, "B.Results/Plots/demand_post_GFC.png"),plot= p_scatter_dem,height   = 20, width    = 25, dpi      = 300)

#Scarlett relative Output---
scatter_plot_data<- plotdata %>% 
  mutate(Output_growth_yoy=GDP_r/lag(GDP_r,4)-1) %>% 
  filter(Date>="1999-1-1") %>%
  filter(!between(Date, as.Date("2020-01-01"), as.Date("2021-01-01")))%>%
  filter(!ISO2 %in% c("U2")) %>%  
  group_by(ISO2) %>%                                
  summarise(                                       
    output_sd = mean(output_sd, na.rm = TRUE),
    Output_growth_yoy  = mean(Output_growth_yoy,na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ISO2) 

p_scatter_GDP <- ggplot(scatter_plot_data, aes(x = Output_growth_yoy, y = output_sd)) +
  geom_point(aes(color = ISO2), size = 15, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.5, color = "grey30") +
  geom_text_repel(aes(label = ISO2, color = ISO2),
                  size = 20, max.overlaps = Inf, box.padding = 0.4, point.padding = 0.2, show.legend = FALSE) +
  scale_color_manual(values = rep(cbi_palette, length.out = dplyr::n_distinct(scatter_plot_data$ISO2))) +
  labs(
    title = "Output volatility vs. Output growth",
    x = "Output growth (YoY, %)",
    y = "Output volatility (country / EU)",
    color = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

p_scatter_GDP

ggsave(paste0(path,"/B.Results/Plots/gdp_scatter.pdf"), p_scatter_GDP, height = 20, width = 25)
ggsave(filename = file.path(path, "B.Results/Plots/gdp_scatter.png"),plot= p_scatter_GDP,height   = 20, width    = 25, dpi      = 300)

#Scarlett relative Investment---
scatter_plot_data<- plotdata %>% 
  mutate(INV_growth_yoy=INV_r/lag(INV_r,4)-1) %>% 
  filter(Date>="2000-1-1") %>% 
  filter(!between(Date, as.Date("2020-01-01"), as.Date("2021-01-01")))%>%
  filter(!ISO2 %in% c("U2")) %>%  
  group_by(ISO2) %>%                                
  summarise(                                       
    INV_sd = mean(output_sd, na.rm = TRUE),
    INV_growth_yoy  = mean(INV_growth_yoy,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ISO2) 

p_scatter_inv <- ggplot(scatter_plot_data, aes(x = INV_growth_yoy, y = INV_sd)) +
  geom_point(aes(color = ISO2), size = 15, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.5, color = "grey30") +
  geom_text_repel(aes(label = ISO2, color = ISO2),
                  size = 20, max.overlaps = Inf, box.padding = 0.4, point.padding = 0.2, show.legend = FALSE) +
  scale_color_manual(values = rep(cbi_palette, length.out = dplyr::n_distinct(scatter_plot_data$ISO2))) +
  labs(
    title = "Investment volatility vs. Investment growth",
    x = "Investment growth (YoY, %)",
    y = "Investment volatility (country / EU)",
    color = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position = "none",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

p_scatter_inv

ggsave(paste0(path,"/B.Results/Plots/inv_scatter.pdf"), p_scatter_inv, height = 20, width = 25)
ggsave(filename = file.path(path, "B.Results/Plots/inv_scatter.png"),plot= p_scatter_inv,height   = 20, width    = 25, dpi      = 300)



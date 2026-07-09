
########## Loading Packages #############################

library(tidyverse)
library(kableExtra)
library(here)
library(knitr)
library(ggthemes)
library(lfe)
library(gt)
library(did)
library(xaringan)
library(miceadds)
library(sandwich)
library(lmtest)
library(patchwork)
library(bacondecomp)
library(multcomp)
library(fastDummies)
library(magrittr)
library(MCPanel)
library(gganimate)
library(gifski)
library(zoo)
library(remotes)
library(data.table)

#### Importing the main data ########
# main_df <- read.csv("Data/Banned_and_Narrow_Controls_08_13.csv",row.names = 1)
main_df <- fread("Data/Banned_and_Broad_Controls_08_13.csv",drop  = 1)
main_df$Date <- as.yearmon(main_df$Date,format = "%B %Y")
main_df$Ban_Date <- as.yearmon(main_df$Ban_Date,format = "%B %Y")

########### Table: 1 - Dependent Variable: Log of Average Price Regressions at the Molecule Level ###################

### Log_Average Process  Data ###
df = main_df %>% filter(ASU_mg != 0) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
     group_by(Sub.Group,Date,Month) %>% 
    summarise(Average_Price = mean(Price_per_mg,na.rm = T),Average_PTR = mean(PTR_per_mg,na.rm = T),Post = mean(Post),
              Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)), Variety = n_distinct(SKU)) %>%
    mutate(Log_Price = log(Average_Price + 1),
           Interaction = Post*Treatment,Log_Avg_PTR = log(Average_PTR + 1),
           Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))


## Double Difference: Log Average Price 
model_1 <-  felm(Log_Price ~ Interaction   | Sub.Group + Date |0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")
summary(model_1)

## Double Difference: Log Average Price + Treatment Monthly Trends
model_2 <-  felm(Log_Price ~ Interaction  + Treatment:Month_Contin | Sub.Group + Date |0 |Sub.Group, 
                            data = df ,cmethod = "reghdfe")

summary(model_2)
## Double Difference:   Log Quantity  ##  
model_3 <- felm(Log_Sales ~  Interaction| Sub.Group + Date | (Log_Price ~ Variety) |Sub.Group, 
                data = df , cmethod = "reghdfe")
summary(model_3)
summary(model_3$stage1)
## Double Difference:   Log Quantity + Treatment Monthly Trends ##  
model_4 <- (felm(Log_Sales ~ Interaction  + Treatment:Month_Contin | Sub.Group + Date| (Log_Price ~ Variety)  |Sub.Group, 
                   data = df , cmethod = "reghdfe"))

summary(model_4)
summary(model_4$stage1)

intercept_4 <- getfe(model_4,ef="zm2") ## Intercept = -0.346


## Saving Regressions ##
# save(model_1,model_2,model_3,model_4,file = "Average_Price_Reg.RData")

############ Table 2: Dependent Variable: Price Volatility Regressions at the Molecule Level #######

### Price Volatility Data : At the Molecule Month Level; Price Volatility = diff(Log(Average_Price_molecule_month)) ###

df = main_df %>% filter(ASU_mg != 0) %>% group_by(Sub.Group,Date)  %>%
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment)) %>% 
  mutate(Log_Price = log(Average_Price + 1),Price_Volatility = append(diff(Log_Price),NA,after = 0),Interaction = Post*Treatment,
         Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0)) %>% 
  separate(col="Date",into = c("Month","Year"),remove = FALSE)


## Price Volatility: diff(Log Average Price) 
model_5 <-  felm(Price_Volatility ~ Interaction   | Sub.Group + Date |0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")
summary(model_5)


## Double Difference: diff(Log Average Price) + Treatment Monthly Trends
model_6 <-  felm(Price_Volatility ~ Interaction  + Treatment:Month_Contin | Sub.Group + Date |0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")

summary(model_6)

### Price Dispersion Data : At the Molecule Month Level  ### Difference b/w range of Process for a particular molecule in a particular month


df = main_df %>% filter(ASU_mg != 0) %>% group_by(Sub.Group,Date) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Max_Price = max(Price_per_mg,na.rm = T), Min_Price = min(Price_per_mg),Post = mean(Post),Treatment = mean(Treatment),
            Price_per_mg_10 = quantile(Price_per_mg,probs = c(0.1),type = 1), 
            Price_per_mg_25 = quantile(Price_per_mg,probs = c(0.25),type = 1),
            Price_per_mg_75 = quantile(Price_per_mg,probs = c(0.75),type = 1),
            Price_per_mg_90 = quantile(Price_per_mg,probs = c(0.9),type = 1)) %>% 
  mutate(Price_Dispersion = (Max_Price - Min_Price),
         Price_Dispersion_19 = (Price_per_mg_90 - Price_per_mg_10),
         Price_Dispersion_27 = (Price_per_mg_75 - Price_per_mg_25),
         Interaction = Post*Treatment,Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0)) %>%
  separate(col="Date",into = c("Month","Year"),remove = FALSE)



## Price Dispersion

model_7 <-  felm(Price_Dispersion ~ Interaction   | Sub.Group + Date|0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")
summary(model_7)


## Price Dispersion + Monthly Time Trend
model_8 <-  felm(Price_Dispersion ~ Interaction  + Treatment:Month_Contin | Sub.Group + Date |0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")

summary(model_8)



## Price Dispersion - 10-90

model_9 <-  felm(Price_Dispersion_19 ~ Interaction   | Sub.Group + Date|0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")
summary(model_9)


## Price Dispersion + Monthly Time Trend - 10-90
model_10 <-  felm(Price_Dispersion_19 ~ Interaction  + Treatment:Month_Contin | Sub.Group + Date |0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")

summary(model_10)


## Price Dispersion -25-75

model_11 <-  felm(Price_Dispersion_27 ~ Interaction   | Sub.Group + Date|0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")
summary(model_11)


## Price Dispersion + Monthly Time Trend - 25-75
model_12 <-  felm(Price_Dispersion_27 ~ Interaction  + Treatment:Month_Contin | Sub.Group + Date |0 |Sub.Group, 
                 data = df ,cmethod = "reghdfe")

summary(model_12)

### Printing through LATEX Table ###############################

## Table (a): Log Average Price and Log Quantity at the molecule level
stargazer::stargazer(model_1,model_2,model_3,model_4,
                     title = "Effect of Drug Bans on Log Average Price (Broad set of Controls)",column.labels = c("Panel A: log(Average Price+1)","Panel B: log quantity"),
                     column.separate = c(2,2),table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","Ever Ban*Monthly Trend"),font.size = "footnotesize",
                     add.lines = list(c("First Stage F"," "," ","1250","1246"),c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses",
                             "Log Price is instrumented by Variety"))

##  Table (b): Price Volatility and Price Dispersion
stargazer::stargazer(model_5,model_6,model_7,model_8,
                     title = "Effect of Drug Bans on Price Volatility and Price Dispersion (Broad Controls)",column.labels = c("Panel A: Price Volatility","Panel B: Price Dispersion"),
                     column.separate = c(2,2),table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","Ever Ban*Monthly Trend"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))

##  Table (c): Price Dispersion at 25-75, and 10-90 percentile
stargazer::stargazer(model_9,model_10,model_11,model_12,
                     title = "Effect of Drug Bans on diff quartiles of Price Dispersion (Broad Controls)",column.labels = c("Panel A: Price Dispersion 10-90 quartile","Panel B: Price Dispersion 25-75 percentile"),
                     column.separate = c(2,2),table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","Ever Ban*Monthly Trend"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))

## Combining Table(a) and Table(b) using sideways ####
stargazer::stargazer(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,
                     title = "Effect of Drug Bans on Log q and different measures of Price volatility at the molecule level ",
                     column.labels = c("Panel A: log(Average Price+1)","Panel B: log quantity","Panel C: Price Volatility","Panel D: Price Dispersion"),
                     column.separate = c(2,2,2,2), flip = T,
                     covariate.labels = c("Domestic","Treatment*Post Ban","Ever Ban*Monthly Trend","Treatment*Post Ban*Domestic"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE?","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt")

## Regressions with number of Firms and HHI at the Molecule Time Level  ########

B = main_df %>% filter(ASU_mg != 0) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
  group_by(Sub.Group,Date,Month,Company) %>% 
  summarise(Total = sum(ASU_mg,na.rm = T),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  group_by(Sub.Group,Date,Month)  %>%
  summarise(HHI = sum( (Total/sum(Total))^2), N_Firms = n_distinct(Company),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  mutate(Interaction = Post*Treatment,Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))


#### HHI Model without Controlling for Monthly Trend in case of Treated Molecules

model_1 <-  felm(HHI ~ Interaction   | Sub.Group + Date |0 |Sub.Group, 
                 data = B ,cmethod = "reghdfe")
summary(model_1)


#### HHI Model After Controlling for Monthly Trend in case of Treated Molecules

model_2 <- felm(HHI ~ Interaction + Treatment:Month_Contin | Sub.Group + Date |0 |Sub.Group, 
                data = B ,cmethod = "reghdfe" )

summary(model_2)


#### Number of Firms without Controlling for Monthly Trend in case of Treated Molecules


model_3 <- felm(N_Firms  ~ Interaction   | Sub.Group + Date |0 |Sub.Group, 
                data = B ,cmethod = "reghdfe")
summary(model_3)

#### Number of Firms After Controlling for Monthly Trend in case of Treated Molecules

model_4 <- felm(N_Firms ~ Interaction + Treatment:Month_Contin | Sub.Group + Date |0 |Sub.Group, 
                data = B ,cmethod = "reghdfe" )

summary(model_4)


stargazer::stargazer(model_1,model_2,model_3,model_4,
                     title = "Effect of Drug Bans on No. of Firms and Sales Concentration (Broad set of Controls)",column.labels = c("Panel A: HHI","Panel B: Number of Firms"),
                     column.separate = c(2,2),table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","Ever Ban*Monthly Trend"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))



########################################################################

### Repeating the same analysis at the firm molecule time level ############

### PTR_per_mg = (PTR/MRP)*Price_per_mg : This variable will be used for instrumenting Price 

df = main_df %>% filter(ASU_mg != 0) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
  group_by(Company,Sub.Group,Date)  %>%
  summarise(Average_Price = mean(Price_per_mg,na.rm = T), Average_PTR = mean(PTR_per_mg,na.rm = T) ,Post = mean(Post),Treatment = mean(Treatment),Domestic = mean(Domestic),Log_Sales = log(sum(ASU_mg,na.rm = T)),
            Max_Price = max(Price_per_mg,na.rm = T), Min_Price = min(Price_per_mg),Variety = n_distinct(SKU),
            Price_per_mg_10 = quantile(Price_per_mg,probs = c(0.1),type = 1), 
            Price_per_mg_25 = quantile(Price_per_mg,probs = c(0.25),type = 1),
            Price_per_mg_75 = quantile(Price_per_mg,probs = c(0.75),type = 1),
            Price_per_mg_90 = quantile(Price_per_mg,probs = c(0.9),type = 1)) %>% 
  mutate(Log_Price = log(Average_Price + 1), Log_Avg_PTR = log(Average_PTR + 1) ,Price_Volatility = append(diff(Log_Price),NA,after = 0),Interaction = Post*Treatment,
         Price_Dispersion = Max_Price - Min_Price,
         Price_Dispersion = (Max_Price - Min_Price),
         Price_Dispersion_19 = (Price_per_mg_90 - Price_per_mg_10),
         Price_Dispersion_27 = (Price_per_mg_75 - Price_per_mg_25),
         Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0)) %>% 
  separate(col="Date",into = c("Month","Year"),remove = FALSE) %>% mutate(MNC = ifelse(Domestic==1,0,1))


### Model:1- Log_Average_Price ##################

model_1 <-  felm(Log_Price ~ Interaction  |  Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                   Sub.Group:Company, 
                 data = df ,cmethod = "reghdfe")
summary(model_1)

### Triple Diff

model_1_1 <- felm(Log_Price ~   Interaction +  MNC:Interaction |  # Normal Covariates
                    Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)| # FE
                    0 | # Instrumental Variables
                    Sub.Group:Company,   # Clustered S.E.
                  data = df ,cmethod = "reghdfe")

summary(model_1_1)

### Model:2- Log Firm Molecule Sales

model_2 <-  felm(Log_Sales ~ Interaction  |  # Normal Covariates
                   Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)| # FE
                  (Log_Price ~ Variety) | # Instrumental Variables
                  Sub.Group:Company,   # Clustered S.E.
                data = df ,cmethod = "reghdfe")

summary(model_2)
summary(model_2$stage1)


### Triple Diff

model_2_1 <- felm(Log_Sales ~  Interaction + MNC:Interaction |  # Normal Covariates
                    Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)| # FE
                    (Log_Price ~ Variety) | # Instrumental Variables
                    Sub.Group:Company,   # Clustered S.E.
                  data = df ,cmethod = "reghdfe")

summary(model_2_1)
summary(model_2_1$stage1)

### Model:3 - Price Volatility

model_3 <-  felm(Price_Volatility ~ Interaction |  Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                   Sub.Group:Company, 
                 data = df ,cmethod = "reghdfe")
summary(model_3)

model_3_1 <- felm(Price_Volatility ~ Interaction + Interaction:MNC |  Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                    Sub.Group:Company, 
                  data = df ,cmethod = "reghdfe")
summary(model_3_1)


### Model_4: Price Dispersion

model_4 <-  felm(Price_Dispersion ~ Interaction | Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                   Sub.Group:Company, 
                 data = df ,cmethod = "reghdfe")
summary(model_4)


model_4_1 <-  felm(Price_Dispersion ~ Interaction + Interaction:MNC | Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                   Sub.Group:Company, 
                 data = df ,cmethod = "reghdfe")
summary(model_4_1)

### Model_5 : Price Dispersion 25-75 percentile

model_5 <- felm(Price_Dispersion_27 ~ Interaction | Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                  Sub.Group:Company, 
                data = df ,cmethod = "reghdfe")
summary(model_5)

model_5_1 <-  felm(Price_Dispersion_27 ~ Interaction + Interaction:MNC | Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                     Sub.Group:Company, 
                   data = df ,cmethod = "reghdfe")
summary(model_5_1)

### Model_6 : Proce Dispersion 10-90 percentile

model_6 <- felm(Price_Dispersion_19 ~ Interaction | Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                  Sub.Group:Company, 
                data = df ,cmethod = "reghdfe")
summary(model_6)

model_6_1 <-  felm(Price_Dispersion_19 ~ Interaction + Interaction:MNC | Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month)|0|
                     Sub.Group:Company, 
                   data = df ,cmethod = "reghdfe")
summary(model_6_1)


### Printing through LATEX Table ###############################

## Table (a): Log Average Price and Log Quantity at the Company level
stargazer::stargazer(model_1,model_1_1,model_2,model_2_1,
                     title = "Effect of Drug Bans on Log Average Price and Log Quantity(For broader set of controls)",column.labels = c("Panel A: log(Average Price+1)","Panel B: log quantity"),
                     column.separate = c(2,2),table.placement = "H", 
                     covariate.labels = c("Treatment*Post","Treatment*Post*MNC","Log Price(per mg)"),font.size = "footnotesize",
                     add.lines = list(c("First Stage F"," "," ","3660","3660"),c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y"),c("Molecule*Calendar Month","Y","Y","Y","Y"),
                                      c("Molecule*Firm FE","Y","Y","Y","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses",
                             "Log Price is instrumented by Variety"))

## Table (b): Price Volatility  and Price Dispersion at the Company level
stargazer::stargazer(model_3,model_3_1,model_4,model_4_1,
                     title = "Effect of Drug Bans on Price Volatility and Price Dispersion(For broad set of controls)",column.labels = c("Panel A: Price Volatility","Panel B: Price Dispersion"),
                     column.separate = c(2,2),table.placement = "H", 
                     covariate.labels = c("Treatment*Post","Treatment*Post*MNC"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y"),c("Molecule*Calendar Month","Y","Y","Y","Y"),
                                      c("Molecule*Firm FE","Y","Y","Y","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))

## Table (c): Price Dispersion at the Company level : 75-25 and 90-10
stargazer::stargazer(model_5,model_5_1,model_6,model_6_1,
                     title = "Effect of Drug Bans on  Price Dispersion(For broad set of controls)",column.labels = c("Panel A: Price Volatility(25-75)","Panel B: Price Dispersion(10-90)"),
                     column.separate = c(2,2),table.placement = "H", 
                     covariate.labels = c("Treatment*Post","Treatment*Post*MNC"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y"),c("Molecule*Calendar Month","Y","Y","Y","Y"),
                                      c("Molecule*Firm FE","Y","Y","Y","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))


#'''''''''''''''''''''''''''''''''''''''''''''''
#' Regressions at the State-Molecule-Time-Level
#'
#''''''''''''''''''''''''''''''''''''''''''''''

## 1> Dependent Variable: Sales Data

df = main_df %>% filter(ASU_mg != 0) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
  group_by(Sub.Group,State,Date) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Average_PTR = mean(PTR_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)),
            Total = sum(ASU_mg,na.rm = T),Variety = n_distinct(SKU)) %>%
  mutate(Log_Price = log(Average_Price + 1),
         Interaction = Post*Treatment,Log_Avg_PTR = log(Average_PTR + 1),
         Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))

HHI =  main_df %>% filter(ASU_mg != 0) %>%  
  group_by(Sub.Group,State,Date,Company) %>% 
  summarise(num = sum(ASU_mg,na.rm = T)) %>%
  group_by(State,Sub.Group,Date) %>%
  summarise(dem = sum(num),HHI = sum((num/dem)^2)) %>%
  dplyr::select(-c(dem))


df <- df %>% left_join(.,HHI)

### First: Log_Sales Estimation

model_1 <-  felm(Log_Sales ~ Interaction + HHI  |  # Normal Covariates
                   Treatment:as.factor(Date) + State + Sub.Group + Date | # FE
                   (Log_Price ~ Variety) | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_1)


## Second: Log_Sales Estimation with Interaction of Molecule*Geography FE's

model_2 <-  felm(Log_Sales ~ Interaction + HHI  |  # Normal Covariates
                   Treatment:as.factor(Date) + (State) + (Sub.Group) + (Date) + as.factor(Sub.Group):as.factor(State) | # FE
                   (Log_Price ~ Variety) | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_2)

## Third: Log_Sales Estimation with Interaction of Geography*Time FE's

model_3 <-  felm(Log_Sales ~ Interaction + HHI  |  # Normal Covariates
                   Treatment:as.factor(Date) + State + Sub.Group + Date + as.factor(State):as.factor(Date) | # FE
                   (Log_Price ~ Variety) | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_2$stage1)



stargazer::stargazer(model_1,model_2,model_3,
                     title = "Effect of Drug Bans on Log Sales at the Molecule Geography Time Level",
                     table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","HHI","Log_Price"),font.size = "footnotesize",
                     add.lines = list(c("First Stage","16880.4","2130","3004"),c("Treatment*Time","Y","Y","Y"),c("Time FE","Y","Y","Y"),
                                          c("Molecule FE","Y","Y","Y"),c("Geography Dummy","Y","Y","Y"),
                                         c("Molecule*Geography","N","Y","N"),c("Geography*Time","N","N","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses")


## 2> Dependent Variable: Variety of Molecules

df = main_df %>% filter(ASU_mg != 0) %>%  
  group_by(Sub.Group,State,Date,Month) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)),
            Total = sum(ASU_mg,na.rm = T),N_Firms = n_distinct(Company)) %>%
  mutate(Log_Price = log(Average_Price + 1),
         Interaction = Post*Treatment,
         Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))

HHI =  main_df %>% filter(ASU_mg != 0) %>%  
  group_by(Sub.Group,State,Date,Company) %>% 
  summarise(num = sum(ASU_mg,na.rm = T),Variety_C = n_distinct(SKU)) %>%
  group_by(State,Sub.Group,Date) %>%
  summarise(dem = sum(num),HHI = sum((num/dem)^2),Variety = sum(Variety_C)) %>%
  dplyr::select(-c(dem))


df <- df %>% left_join(.,HHI)


### First: Variety Estimation

library(speedglm)


model_4 <- glm(data = df,Variety ~ Interaction + HHI + Treatment:as.factor(Date) + as.factor(State) + 
                          as.factor(Sub.Group) + as.factor(Date),
                          family = poisson(link = "log"))

# sqrt(diag(vcovHC(model, type="HC2"))) # To Create Robust S.E. without clustering
# model <- coeftest(model_1, vcov. = vcovCL(model_1, cluster = as.factor(df$id), type = "HC0")) # with clustering
# model_4_1 <- coeftest(model_4)
summary(model_4)
SE_robust <- sqrt(diag(vcovCL(model_4,cluster = as.factor(df$Sub.Group):as.factor(df$State),  type="HC0"))) # in a vector form

# Merging Robust s.E. in the original model
# model_5 <- summary(model_4)
# model_5$coefficients[,2] <- SE_robust
# model_5
# # Saving the Model Output
# load("Variety_Reg_1.RData")

## Second: Variety Estimation with Interaction of Molecule*Geography FE's

# Not able to run
model_5 <-  speedglm(data = df,Variety ~ Interaction + HHI + Treatment:as.factor(Date) + as.factor(State) + 
                  as.factor(Sub.Group) + as.factor(Date) + as.factor(Sub.Group):as.factor(State),
                family = "poisson"(link="log"))

# SE_robust_2 <- sqrt(diag(vcovCL(model_5,cluster = as.factor(df$Sub.Group):as.factor(df$State),  type="HC0"))) # in a vector form

# Merging Robust s.E. in the original model
# model_2_1 <- summary(model_2)
# model_2_1$coefficients[,2] <- SE_robust_2
# model_2_1


# save(model_2,file = "Variety_Reg_2.RData")

## Third: Variety Estimation with Interaction of Geography*Time FE's

# Not able to run
model_6 <-  glm(data = df,Variety ~ Interaction + HHI + Treatment:as.factor(Date) + as.factor(State) + 
                  as.factor(Sub.Group) + as.factor(Date) + as.factor(Date):as.factor(State),
                family = "poisson"(link = "log"))

# SE_robust_3 <- sqrt(diag(vcovCL(model_6,cluster = as.factor(df$Sub.Group):as.factor(df$State),  type="HC0"))) # in a vector form

SE_robust_3 <- as.data.frame(SE_robust)


## Printing in a standard LATEX Table

stargazer::stargazer(model_1_1, keep = c("Interaction","HHI"),summary = F)


class(model_1)
class(model_1_1)

stargazer::stargazer(model_1,model_2,model_3, model_4,
                     title = "Effect of Drug Bans on Log Sales and Variety at the Molecule Geography Time Level (Broad set of controls)",column.labels = c("Panel A: Log Sales","Panel B: Variety"),
                     column.separate = c(3,1),table.placement = "H", keep = c(1:3),
                     covariate.labels = c("Treatment*Post Ban","HHI","Log Price"),font.size = "footnotesize",
                     add.lines = list(c("First Stage","9335","2844","1090",""),c("Treatment*Time","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y","Y"),c("Geography Dummy","Y","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N","N"),c("Geography*Time","N","N","Y","N")),
                     header=FALSE,no.space = TRUE,
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses",
                     column.sep.width = "1pt")

model_4_1 <- coeftest(model_4)

### Price Regressions at the State-Molecule-Time Level ####

df = main_df %>% filter(ASU_mg != 0) %>%  
  group_by(Sub.Group,State,Date) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)),
            Total = sum(ASU_mg,na.rm = T),Max_Price = max(Price_per_mg,na.rm = T), Min_Price = min(Price_per_mg),Variety = n_distinct(SKU),
            Price_per_mg_10 = quantile(Price_per_mg,probs = c(0.1),type = 1), 
            Price_per_mg_25 = quantile(Price_per_mg,probs = c(0.25),type = 1),
            Price_per_mg_75 = quantile(Price_per_mg,probs = c(0.75),type = 1),
            Price_per_mg_90 = quantile(Price_per_mg,probs = c(0.9),type = 1)) %>%
  mutate(Log_Price = log(Average_Price + 1),Price_Volatility = append(diff(Log_Price),NA,after = 0),
         Price_Dispersion = (Max_Price - Min_Price),
         Price_Dispersion_19 = (Price_per_mg_90 - Price_per_mg_10),
         Price_Dispersion_27 = (Price_per_mg_75 - Price_per_mg_25),
         Interaction = Post*Treatment,
         Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))

## Log Average Price -1

model_1 <-  felm(Log_Price ~ Interaction  |  
                   Treatment:as.factor(Date) + State + Sub.Group + Date | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_1)

## Second: Log Average Price - 2

model_2 <-  felm(Log_Price ~ Interaction   |  # Normal Covariates
                   Treatment:as.factor(Date) + (State) + (Sub.Group) + (Date) + as.factor(Sub.Group):as.factor(State) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_2)

## Third: Log Average Price -3

model_3 <-  felm(Log_Price ~ Interaction  |  # Normal Covariates
                   Treatment:as.factor(Date) + State + Sub.Group + Date + as.factor(State):as.factor(Date) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_3)

## Price Dispersion -1

model_4 <-  felm(Price_Dispersion ~ Interaction  |  
                   Treatment:as.factor(Date) + State + Sub.Group + Date | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_4)

## Second: Price Volatility - 2

model_5 <-  felm(Price_Dispersion ~ Interaction   |  # Normal Covariates
                   Treatment:as.factor(Date) + (State) + (Sub.Group) + (Date) + as.factor(Sub.Group):as.factor(State) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_5)

## Third: Price Volatility -3

model_6 <-  felm(Price_Dispersion ~ Interaction  |  # Normal Covariates
                   Treatment:as.factor(Date) + State + Sub.Group + Date + as.factor(State):as.factor(Date) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_6)



### Printing the LATEX Table ####

stargazer::stargazer(model_1,model_2,model_3, model_4,model_5,model_6,
                     title = "Effect of Drug Bans on Log Prices and Price Dispersion at the Molecule Geography Time Level",column.labels = c("Panel A: Log Average Price","Panel B: Price Dispersion"),
                     column.separate = c(3,3),table.placement = "H", 
                     covariate.labels = c("Treatment*Post Ban"),font.size = "footnotesize",
                     add.lines = list(c("Treatment*Time","Y","Y","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y","Y","Y","Y"),c("Geography Dummy","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N","N","Y","N"),c("Geography*Time","N","N","Y","N","N","Y")),
                     header=FALSE,no.space = TRUE,
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses",
                     column.sep.width = "-7pt")



## Price Dispersion 10-90 -1

model_1 <-  felm(Price_Dispersion_19 ~ Interaction  |  
                   Treatment:as.factor(Date) + State + Sub.Group + Date | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_1)

## Second: Price Dispersion 10-90 -2

model_2 <-  felm(Price_Dispersion_19 ~ Interaction   |  # Normal Covariates
                   Treatment:as.factor(Date) + (State) + (Sub.Group) + (Date) + as.factor(Sub.Group):as.factor(State) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_2)

## Third: Price Dispersion 10-90 -3

model_3 <-  felm(Price_Dispersion_19 ~ Interaction  |  # Normal Covariates
                   Treatment:as.factor(Date) + State + Sub.Group + Date + as.factor(State):as.factor(Date) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_3)

## First: Price Dispersion 25-75-1

model_4 <-  felm(Price_Dispersion_27 ~ Interaction  |  
                   Treatment:as.factor(Date) + State + Sub.Group + Date | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_4)

## Second: Price Dispersion 25-75 -1

model_5 <-  felm(Price_Dispersion_27 ~ Interaction   |  # Normal Covariates
                   Treatment:as.factor(Date) + (State) + (Sub.Group) + (Date) + as.factor(Sub.Group):as.factor(State) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_5)

## Third: Price Dispersion 25-75 -1

model_6 <-  felm(Price_Dispersion_27 ~ Interaction  |  # Normal Covariates
                   Treatment:as.factor(Date) + State + Sub.Group + Date + as.factor(State):as.factor(Date) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_6)

### Printing the LATEX Table ####

stargazer::stargazer(model_1,model_2,model_3, model_4,model_5,model_6,
                     title = "Effect of Drug Bans on Price Dispersion (different quantiles) at the Molecule Geography Time Level",column.labels = c("Panel A: Price Dispersion (10-90)","Panel B: Price Dispersion (25-75)"),
                     column.separate = c(3,3),table.placement = "H", 
                     covariate.labels = c("Treatment*Post Ban"),font.size = "footnotesize",
                     add.lines = list(c("Treatment*Time","Y","Y","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y","Y","Y","Y"),c("Geography Dummy","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N","N","Y","N"),c("Geography*Time","N","N","Y","N","N","Y")),
                     header=FALSE,no.space = TRUE,
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses",
                     column.sep.width = "-15pt")


#### HHI and Number of Firms at the State-molecule time level ###


df = main_df %>% filter(ASU_mg != 0) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
  group_by(Sub.Group,State,Date,Company) %>% 
  summarise(Total = sum(ASU_mg,na.rm = T),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  group_by(Sub.Group,State,Date)  %>%
  summarise(HHI = sum( (Total/sum(Total))^2), N_Firms = n_distinct(Company),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  mutate(Interaction = Post*Treatment,Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))


## First: HHI -1 

model_1 <-  felm(HHI ~ Interaction  |  
                   Treatment:as.factor(Date) + State + Sub.Group + Date | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_1)

## Second: HHI-2

model_2 <-  felm(HHI ~ Interaction   |  # Normal Covariates
                   Treatment:as.factor(Date) + (State) + (Sub.Group) + (Date) + as.factor(Sub.Group):as.factor(State) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_2)

## Third: HHI-3

model_3 <-  felm(HHI~ Interaction  |  # Normal Covariates
                   Treatment:as.factor(Date) + State + Sub.Group + Date + as.factor(State):as.factor(Date) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_3)

## First: N_Firms- 1

model_4 <-  felm(N_Firms ~ Interaction  |  
                   Treatment:as.factor(Date) + State + Sub.Group + Date | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_4)

## Second: N_Firms - 2

model_5 <-  felm(N_Firms ~ Interaction   |  # Normal Covariates
                   Treatment:as.factor(Date) + (State) + (Sub.Group) + (Date) + as.factor(Sub.Group):as.factor(State) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_5)

## Third: N_Firms -3

model_6 <-  felm(N_Firms ~ Interaction  |  # Normal Covariates
                   Treatment:as.factor(Date) + State + Sub.Group + Date + as.factor(State):as.factor(Date) | # FE
                   0 | # Instrumental Variables
                   Sub.Group:State,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_6)


### Printing the LATEX Table ####

stargazer::stargazer(model_1,model_2,model_3, model_4,model_5,model_6,
                     title = "Effect of Drug Bans on HHI and No. of Firms at the Molecule Geography Time Level",column.labels = c("Panel A: HHI","Panel B: No. of Firms"),
                     column.separate = c(3,3),table.placement = "H", 
                     covariate.labels = c("Treatment*Post Ban"),font.size = "footnotesize",
                     add.lines = list(c("Treatment*Time","Y","Y","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y","Y","Y","Y"),c("Geography Dummy","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N","N","Y","N"),c("Geography*Time","N","N","Y","N","N","Y")),
                     header=FALSE,no.space = TRUE,
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses",
                     column.sep.width = "-15pt")


############################ End of Do File ##########################################################################################################


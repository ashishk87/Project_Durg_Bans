
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


#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#--------------------------- Merging all three long format data files ----------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#


India_09_10 <- fread("Data/Data/2016_Banned_Drugs/Ban_Control_Molecule_09_10_long.csv ")
India_11_14 <- fread("Data/Data/2016_Banned_Drugs/Ban_Control_Molecule_11_14_long.csv ")
India_15_20 <- fread("Data/Data/2016_Banned_Drugs/Ban_Control_Molecule_15_20_long.csv ")


#--- In India_09_10 dataset, there are some missing variables in comparison to India_11_14, and India_15_20:
#--- 1> State- Region level info is present in the variable OLD STATE.             (NA variable created in 09_10)
#--- 2> Plain-Combination info is inferred from Plain-Combination split variable. (Variable created in 09_10)

#--- Variable (3) amnd Variable (4) are absent in both 09_10 and 11_14 df but present in 15_20 df.
#--- 3> NLEM 15-20  (Removed from 15_20)
#--- 4> MotherBrand (Removed from 15_20)


#V** NOT REQUIRED ANYMORE***

# 1> India_09_10 cleaned and processed : 

# India_09_10 <- India_09_10 %>% mutate(STATE = NA, "PLAIN/COMBINATION" = ifelse(`PLAIN/COMBINATION SPLIT`=="PLAIN","PLAIN","COMBINATION"))
# India_09_10$`BRAND LAUNCH DATE` <-  as.yearmon(India_09_10$`BRAND LAUNCH DATE`,format = "%b-%y")
# India_09_10$`SUBGROUP LAUNCH DATE` <- as.yearmon(India_09_10$`SUBGROUP LAUNCH DATE`,format = "%b-%y")
# India_09_10$`SKU LAUNCH DATE` <- as.yearmon(India_09_10$`SKU LAUNCH DATE`,format = "%b-%y")
# India_09_10 <- India_09_10 %>% rename(`ACUTE/CHRONIC` = ACUTECHRONIC,`SUPER GROUP` = SUPERGROUP,`DRUG TYPE` = DRUGTYPE,`DRUG CATEGORY` = DRUGCATEGORY,DATE = variable)
# India_09_10$DATE <- as.yearmon(India_09_10$DATE,format = "%b-%y")


# 2> India_11_14 cleaned and processed
India_11_14$`BRAND LAUNCH DATE` <-  as.yearmon(India_11_14$`BRAND LAUNCH DATE`,format = "%b-%y")
India_11_14$`SUBGROUP LAUNCH DATE` <- as.yearmon(India_11_14$`SUBGROUP LAUNCH DATE`,format = "%b-%y")
India_11_14$`SKU LAUNCH DATE` <- as.yearmon(India_11_14$`SKU LAUNCH DATE`,format = "%b-%y")
India_11_14 <- India_11_14 %>%  rename(DATE = variable)
India_11_14$DATE <- as.yearmon(India_11_14$DATE,format = "%b-%y")


#3> India_15_20 cleaned and processed 
India_15_20 <- India_15_20 %>% dplyr::select(-c(MOTHERBRAND,`NLEM 15`))
India_15_20$`BRAND LAUNCH DATE` <-  as.yearmon(India_15_20$`BRAND LAUNCH DATE`,format = "%b-%y")
India_15_20$`SUBGROUP LAUNCH DATE` <- as.yearmon(India_15_20$`SUBGROUP LAUNCH DATE`,format = "%b-%y")
India_15_20$`SKU LAUNCH DATE` <- as.yearmon(India_15_20$`SKU LAUNCH DATE`,format = "%b-%y")
India_15_20 <- India_15_20 %>%  rename(DATE = variable)
India_15_20$DATE <- as.yearmon(India_15_20$DATE,format = "%b-%y")


#4> Rowbinding all the files
India_09_20 <- rbind(India_11_14,India_15_20)

India_09_20$Ban_Date <- as.yearmon("March 2016", format = "%B %Y")


#5> Removing the datapoiints corresponding to All India Observations Only
India_09_20 <- India_09_20[`OLD STATE`!="ALL INDIA ONLY"]


#6> Creating relevant variables

India_09_20 <- India_09_20 %>% mutate(Post = ifelse(DATE> Ban_Date, 1,0),ASU_mg = `SALES UNIT`*MG*Strips,Revenue = `SALES UNIT`*MRP,
                                      Price_per_mg = ifelse(ASU_mg==0,NA,Revenue/ASU_mg),MNC = ifelse(`INDIAN/MNC`=="INDIAN",0,1))

#-------------------------------------------------------------------------------------------------------------------#
#--------------------- Regression Analysis Starts ----------------------------------------------------------------- #
#------------------------------------------------------------------------------------------------------------------ #


########### Table: 1 - Dependent Variable: Log of Average Price Regressions at the Molecule Level ###################

### Log_Average Process  Data ###

df = India_09_20 %>% 
     group_by(`SG CODE`,DATE) %>% 
     summarise(Post = mean(Post),
            Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)+1), Log_Revenue = log(sum(Revenue,na.rm = T)+1) ,Variety = n_distinct(SKU)) %>%
  mutate(Interaction = Post*Treatment,
         Month_Contin = round((DATE - as.yearmon("January 2011",format = "%B %Y"))/0.083 + 1,digits = 0))

df_HHI = India_09_20 %>% 
         group_by(`SG CODE`,DATE,COMPANY) %>% 
         summarise(Total = (sum(ASU_mg,na.rm = T)), N_Firms = n_distinct(COMPANY)) %>%
         group_by(`SG CODE`,DATE)  %>%
         summarise(HHI = ifelse(sum(Total)==0,1/N_Firms,sum((Total/sum(Total))^2)))


df <- df %>% left_join(.,df_HHI)

## Double Difference: Log Average Price :  Not Needed

# model_1 <-  felm(Log_Price ~ Interaction   | `SG CODE` + DATE |0 |`SG CODE`, 
#                  data = df ,cmethod = "reghdfe")
# summary(model_1)

## Double Difference: Log Average Price + Treatment Monthly Trends

# model_2 <-  felm(Log_Price ~ Interaction  + Treatment:Month_Contin | `SG CODE` + DATE |0 |`SG CODE`, 
#                  data = df ,cmethod = "reghdfe")
# 
# summary(model_2)

## Double Difference:   Log Quantity  ##  

model_3 <-   felm(Log_Sales ~  Interaction + HHI | `SG CODE` + DATE | 0 |`SG CODE`, 
                                                    data = df , cmethod = "reghdfe")
summary(model_3)



## Double Difference:   Log Quantity + Treatment Monthly Trends ##  
model_4 <- felm(Log_Sales ~ Interaction + HHI + Treatment:Month_Contin | `SG CODE` + DATE | 0 |`SG CODE`, 
                 data = df , cmethod = "reghdfe")
summary(model_4)

## Double Difference: Log Revenue ##
df <- data.table(df)
model_5 <-  felm(Log_Revenue ~  Interaction + HHI | `SG CODE` + DATE | 0 |`SG CODE`, 
                data = df , cmethod = "reghdfe")
summary(model_5)


## Double Difference:   Log Revenue + Treatment Monthly Trends ##  
model_6 <- felm(Log_Revenue ~ Interaction  + HHI  + Treatment:Month_Contin  | `SG CODE` + DATE | 0 |`SG CODE`, 
                data = df, cmethod = "reghdfe")
summary(model_6)


### Printing  LATEX Table ###############################

## Table (a): Log Average Price and Log Quantity at the molecule level
stargazer::stargazer(model_3,model_4,model_5,model_6,
                     title = "Effect of Drug Bans on Log Sales, Log Revenue",column.labels = c("Panel A: log quantity","Panel B: log Revenue"),
                     column.separate = c(2,2),table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","HHI","Ever Ban*Monthly Trend"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))


#-------- ACUTE : Log_Sales & Log_Revenue Regressions -----------------------------------#

df = India_09_20 %>% filter(`ACUTE/CHRONIC` !="ACUTE") %>% 
  group_by(`SG CODE`,DATE) %>% 
  summarise(Post = mean(Post),
            Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)+1), Log_Revenue = log(sum(Revenue,na.rm = T)+1) ,Variety = n_distinct(SKU)) %>%
  mutate(Interaction = Post*Treatment,
         Month_Contin = round((DATE - as.yearmon("January 2011",format = "%B %Y"))/0.083 + 1,digits = 0))

df_HHI = India_09_20 %>% filter(`ACUTE/CHRONIC`!="ACUTE") %>% 
  group_by(`SG CODE`,DATE,COMPANY) %>% 
  summarise(Total = (sum(ASU_mg,na.rm = T)), N_Firms = n_distinct(COMPANY)) %>%
  group_by(`SG CODE`,DATE)  %>%
  summarise(HHI = ifelse(sum(Total)==0,1/N_Firms,sum((Total/sum(Total))^2)))

df <- df %>% left_join(.,df_HHI)

## Double Difference:   Log Quantity  ##  

model_3 <-   felm(Log_Sales ~  Interaction + HHI | `SG CODE` + DATE | 0 |`SG CODE`, 
                  data = df , cmethod = "reghdfe")
summary(model_3)



## Double Difference:   Log Quantity + Treatment Monthly Trends ##  
model_4 <- felm(Log_Sales ~ Interaction  +HHI  + Treatment:Month_Contin  | `SG CODE` + DATE | 0 |`SG CODE`, 
                data = df , cmethod = "reghdfe")
summary(model_4)

## Double Difference: Log Revenue ##
df <- data.table(df)
model_5 <-  felm(Log_Revenue ~  Interaction + HHI | `SG CODE` + DATE | 0 |`SG CODE`, 
                 data = df , cmethod = "reghdfe")
summary(model_5)


## Double Difference:   Log Revenue + Treatment Monthly Trends ##  
model_6 <- felm(Log_Revenue ~ Interaction  + HHI  + Treatment:Month_Contin  | `SG CODE` + DATE | 0 |`SG CODE`, 
                data = df, cmethod = "reghdfe")
summary(model_6)


### Printing  LATEX Table ###############################

## Table (a): Log Average Price and Log Quantity at the molecule level
stargazer::stargazer(model_3,model_4,model_5,model_6,
                     title = "Effect of Drug Bans on (logs)  Sales, Revenue-Chronic Sample",column.labels = c("Panel A: log quantity","Panel B: log Revenue"),
                     column.separate = c(2,2),table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","HHI","Ever Ban*Monthly Trend"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))



############ Table 2: Dependent Variable: Price Volatility Regressions at the Molecule Level #######

# Note: No need to focus on the Price Volatility or Price Dispersion data

### Price Volatility Data : At the Molecule Month Level; Price Volatility = diff(Log(Average_Price_molecule_month)) ###

df = India_09_20 %>% filter(ASU_mg != 0) %>% group_by(`SG CODE`,DATE)  %>%
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment)) %>% 
  mutate(Log_Price = log(Average_Price + 1),Price_Volatility = append(diff(Log_Price),NA,after = 0),Interaction = Post*Treatment,
         Month_Contin = round((DATE - as.yearmon("January 2011",format = "%B %Y"))/0.083 + 1,digits = 0))


## Price Volatility: diff(Log Average Price) 
model_5 <-  felm(Price_Volatility ~ Interaction   | `SG CODE` + DATE |0 |`SG CODE`, 
                 data = df ,cmethod = "reghdfe")
summary(model_5)


## Double Difference: diff(Log Average Price) + Treatment Monthly Trends
model_6 <-  felm(Price_Volatility ~ Interaction  + Treatment:Month_Contin | `SG CODE` + DATE |0 |`SG CODE`, 
                 data = df ,cmethod = "reghdfe")

summary(model_6)



### Price Dispersion Data : At the Molecule Month Level  ### Difference b/w range of Process for a particular molecule in a particular month

df = India_09_20 %>% filter(ASU_mg != 0) %>% group_by(`SG CODE` ,DATE ) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Max_Price = max(Price_per_mg,na.rm = T), Min_Price = min(Price_per_mg),Post = mean(Post),Treatment = mean(Treatment),
            Price_per_mg_10 = quantile(Price_per_mg,probs = c(0.1),type = 1), 
            Price_per_mg_25 = quantile(Price_per_mg,probs = c(0.25),type = 1),
            Price_per_mg_75 = quantile(Price_per_mg,probs = c(0.75),type = 1),
            Price_per_mg_90 = quantile(Price_per_mg,probs = c(0.9),type = 1)) %>% 
  mutate(Price_Dispersion = (Max_Price - Min_Price),
         Price_Dispersion_19 = (Price_per_mg_90 - Price_per_mg_10),
         Price_Dispersion_27 = (Price_per_mg_75 - Price_per_mg_25),
         Interaction = Post*Treatment,Month_Contin = round((DATE - as.yearmon("January 2011",format = "%B %Y"))/0.083 + 1,digits = 0)) 


## Price Dispersion

model_7 <-  felm(Price_Dispersion ~ Interaction   | `SG CODE` + DATE|0 |`SG CODE`, 
                 data = df ,cmethod = "reghdfe")
summary(model_7)


## Price Dispersion + Monthly Time Trend
model_8 <-  felm(Price_Dispersion ~ Interaction  + Treatment:Month_Contin | `SG CODE` + DATE |0 |`SG CODE`, 
                 data = df ,cmethod = "reghdfe")

summary(model_8)



## Price Dispersion - 10-90

model_9 <-  felm(Price_Dispersion_19 ~ Interaction   | `SG CODE` + DATE |0 |`SG CODE`, 
                 data = df ,cmethod = "reghdfe")
summary(model_9)


## Price Dispersion + Monthly Time Trend - 10-90
model_10 <-  felm(Price_Dispersion_19 ~ Interaction  + Treatment:Month_Contin | `SG CODE` + DATE | 0 |`SG CODE`, 
                  data = df ,cmethod = "reghdfe")

summary(model_10)


## Price Dispersion -25-75

model_11 <-  felm(Price_Dispersion_27 ~ Interaction   | `SG CODE` + DATE |0 |`SG CODE`, 
                  data = df ,cmethod = "reghdfe")
summary(model_11)


## Price Dispersion + Monthly Time Trend - 25-75
model_12 <-  felm(Price_Dispersion_27 ~ Interaction  + Treatment:Month_Contin | `SG CODE` + DATE |0 |`SG CODE`, 
                  data = df ,cmethod = "reghdfe")

summary(model_12)


##  Table (b): Price Volatility and Price Dispersion
stargazer::stargazer(model_5,model_6,model_7,model_8,
                     title = "Effect of Drug Bans on Price Volatility and Price Dispersion",column.labels = c("Panel A: Price Volatility","Panel B: Price Dispersion"),
                     column.separate = c(2,2),table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","Ever Ban*Monthly Trend"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))

##  Table (c): Price Dispersion at 25-75, and 10-90 percentile
stargazer::stargazer(model_9,model_10,model_11,model_12,
                     title = "Effect of Drug Bans on diff quartiles of Price Dispersion",column.labels = c("Panel A: Price Dispersion 10-90 quartile","Panel B: Price Dispersion 25-75 percentile"),
                     column.separate = c(2,2),table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","Ever Ban*Monthly Trend"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))



#-----------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------- Table (3): HHI, N_Firms, Variety Regressions ---------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------------#

df =  India_09_20 %>%  filter(ASU_mg != 0) %>%
      group_by(`SG CODE`,DATE) %>% 
      summarise(Post = mean(Post),Treatment = mean(Treatment),Variety = n_distinct(SKU)) %>%
      mutate(Interaction = Post*Treatment,
         Month_Contin = round((DATE - as.yearmon("January 2011",format = "%B %Y"))/0.083 + 1,digits = 0))




df_HHI = India_09_20 %>%  filter(ASU_mg != 0) %>%
        group_by(`SG CODE`,DATE,COMPANY) %>% 
        summarise(Total = (sum(ASU_mg  ,na.rm = T)+1),Post=  mean(Post),Treatment = mean(Treatment)) %>%
        group_by(`SG CODE`,DATE)  %>%
        summarise(HHI = sum( (Total/sum(Total))^2), N_Firms = n_distinct(COMPANY),Post=  mean(Post),Treatment = mean(Treatment)) %>%
        mutate(Interaction = Post*Treatment,Month_Contin = round((DATE - as.yearmon("January 2011",format = "%B %Y"))/0.083 + 1,digits = 0))


df <- df %>% left_join(.,df_HHI)


#### HHI Model without Controlling for Monthly Trend in case of Treated Molecules

model_1 <-  felm(HHI ~ Interaction   | `SG CODE` + DATE |0 |`SG CODE`, 
                 data = df ,cmethod = "reghdfe")
summary(model_1)


#### HHI Model After Controlling for Monthly Trend in case of Treated Molecules

model_2 <- felm(HHI ~ Interaction + Treatment:Month_Contin |`SG CODE` + DATE |0 |`SG CODE`, 
                data = df ,cmethod = "reghdfe" )

summary(model_2)


#### Number of Firms without Controlling for Monthly Trend in case of Treated Molecules


model_3 <- felm(N_Firms  ~ Interaction   |`SG CODE` + DATE |0 |`SG CODE`, 
                data = df ,cmethod = "reghdfe")
summary(model_3)

#### Number of Firms After Controlling for Monthly Trend in case of Treated Molecules

model_4 <- felm(N_Firms ~ Interaction + Treatment:Month_Contin | `SG CODE` + DATE |0 |`SG CODE`, 
                data = df ,cmethod = "reghdfe" )

summary(model_4)

#--- Variety of SKU's without Controlling for Monthly Trend in case of Treated Molecules



model_5 <- glm(data = df,Variety ~ Interaction + HHI + as.factor(DATE) + as.factor(`SG CODE`),
               family = poisson(link = "log"))
# summary(model_5)
SE_robust <- sqrt(diag(vcovCL(model_5,cluster = as.factor(df$`SG CODE`),  type="HC0")))

print(SE_robust[1:5])

#--- Variety of SKU's without Controlling for Monthly Trend in case of Treated Molecules

model_6 <- glm(data = df,Variety ~ Interaction + + Treatment:Month_Contin + HHI + as.factor(DATE) + as.factor(`SG CODE`),
               family = poisson(link = "log"))
# summary(model_6)
SE_robust_1 <- sqrt(diag(vcovCL(model_6,cluster = as.factor(df$`SG CODE`),  type="HC0")))
print(SE_robust_1[1013])


#--- Printing LATEX Output -------------------------------------#
stargazer::stargazer(model_1,model_2,model_3,model_4,model_5,model_6,
                     title = "Effect of Drug Bans on No. of Firms and Sales Concentration (Broad set of Controls)",column.labels = c("Panel A: HHI","Panel B: Number of Firms","Panel C: Variety"),
                     column.separate = c(2,2,2),table.placement = "H", keep = c(1:4),
                     covariate.labels = c("Treatment*Post Ban","Ever Ban*Monthly Trend","HHI"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "-20pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))



#--- Printing LATEX Output -------------------------------------# Without Variety
stargazer::stargazer(model_1,model_2,model_3,model_4,
                     title = "Effect of Drug Bans on No. of Firms and Sales Concentration ",column.labels = c("Panel A: HHI","Panel B: Number of Firms"),
                     column.separate = c(2,2),table.placement = "H", keep = c(1:4),
                     covariate.labels = c("Treatment*Post Ban","Ever Ban*Monthly Trend","HHI"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "-20pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))
#------------------------------- Molecule-Time level analysis ends ---------------------------------------------------------#


#'''''''''''''''''''''''''''''''''''''''''''''''
#' Regressions at the Firm-Molecule-Time-Level
#'
#''''''''''''''''''''''''''''''''''''''''''''''



df = India_09_20 %>%  
    group_by(`SG CODE`,DATE,COMPANY)  %>%
    summarise(Average_Price = mean(Price_per_mg,na.rm = T), Post = mean(Post),Treatment = mean(Treatment),Domestic = mean(Domestic),Log_Sales = log(sum(ASU_mg,na.rm = T)),Log_Revenue = log(sum(Revenue,na.rm = T)),
            Max_Price = max(Price_per_mg,na.rm = T), Min_Price = min(Price_per_mg),Variety = n_distinct(SKU),
            Price_per_mg_10 = quantile(Price_per_mg,probs = c(0.1),type = 1), 
            Price_per_mg_25 = quantile(Price_per_mg,probs = c(0.25),type = 1),
            Price_per_mg_75 = quantile(Price_per_mg,probs = c(0.75),type = 1),
            Price_per_mg_90 = quantile(Price_per_mg,probs = c(0.9),type = 1)) %>% 
    mutate(Log_Price = log(Average_Price + 1), Price_Volatility = append(diff(Log_Price),NA,after = 0),Interaction = Post*Treatment,
         Price_Dispersion = Max_Price - Min_Price,
         Price_Dispersion = (Max_Price - Min_Price),
         Price_Dispersion_19 = (Price_per_mg_90 - Price_per_mg_10),
         Price_Dispersion_27 = (Price_per_mg_75 - Price_per_mg_25),
         Month_Contin = round((DATE - as.yearmon("April 2009",format = "%B %Y"))/0.083 + 1,digits = 0))

df$molecule_comp <- (as.numeric(as.factor(df$`SG CODE`):as.factor(df$COMPANY)))


## Including Zero Sales

df = India_09_20 %>% 
  group_by(`SG CODE`,DATE,COMPANY)  %>%
  summarise(Post = mean(Post),Treatment = mean(Treatment),MNC = mean(MNC),Log_Sales = log(sum(ASU_mg,na.rm = T)+1),Log_Revenue = log(sum(Revenue,na.rm = T)+1)) %>% 
  mutate(Interaction = Post*Treatment,Month_Contin = round((DATE - as.yearmon("January 2011",format = "%B %Y"))/0.083 + 1,digits = 0))

df$molecule_comp <- (as.numeric(as.factor(df$`SG CODE`):as.factor(df$COMPANY)))


### Model:1- Log_Average_Price ##################

model_1 <-  felm(Log_Price ~ Interaction  |  COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                   molecule_comp,data = df ,cmethod = "reghdfe")

summary(model_1)

### Triple Diff ###

model_1_1 <- felm(Log_Price ~   Interaction +  Domestic:Interaction |  # Normal Covariates
                    COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin| # FE
                    0 | # Instrumental Variables
                    `SG CODE`:COMPANY,   # Clustered S.E.
                  data = df ,cmethod = "reghdfe")

summary(model_1_1)

### Model:2- Log Firm Molecule Sales

model_4 <-  felm(Log_Sales ~ Interaction  |  # Normal Covariates
                   COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin| # FE
                   0 | # Instrumental Variables
                   molecule_comp,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_4)


### Triple Diff

model_4_1 <- felm(Log_Sales ~  Interaction + MNC:Interaction |  # Normal Covariates
                    COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin| # FE
                    0 | # Instrumental Variables
                    molecule_comp,   # Clustered S.E.
                  data = df ,cmethod = "reghdfe")

summary(model_4_1)

### Model:3 - Price Volatility

model_3 <-  felm(Price_Volatility ~ Interaction |  COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                   `SG CODE`:COMPANY, 
                 data = df ,cmethod = "reghdfe")
summary(model_3)

model_3_1 <- felm(Price_Volatility ~ Interaction + Interaction:Domestic |  COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                    `SG CODE`:COMPANY, 
                  data = df ,cmethod = "reghdfe")
summary(model_3_1)


### Model_4: Price Dispersion

model_4 <-  felm(Price_Dispersion ~ Interaction | COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                   `SG CODE`:COMPANY, 
                 data = df ,cmethod = "reghdfe")
summary(model_4)


model_4_1 <-  felm(Price_Dispersion ~ Interaction + Interaction:Domestic | COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                     `SG CODE`:COMPANY, 
                   data = df ,cmethod = "reghdfe")
summary(model_4_1)

### Model_5 : Price Dispersion 25-75 percentile

model_5 <- felm(Price_Dispersion_27 ~ Interaction | COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                  `SG CODE`:COMPANY, 
                data = df ,cmethod = "reghdfe")
summary(model_5)

model_5_1 <-  felm(Price_Dispersion_27 ~ Interaction + Interaction:Domestic | COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                     `SG CODE`:COMPANY, 
                   data = df ,cmethod = "reghdfe")
summary(model_5_1)

### Model_6 : Proce Dispersion 10-90 percentile

model_6 <- felm(Price_Dispersion_19 ~ Interaction | COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                  `SG CODE`:COMPANY, 
                data = df ,cmethod = "reghdfe")
summary(model_6)

model_6_1 <-  felm(Price_Dispersion_19 ~ Interaction + Interaction:Domestic | COMPANY + `SG CODE` + DATE + as.factor(`SG CODE`):as.factor(COMPANY )+ as.factor(`SG CODE`):Month_Contin|0|
                     `SG CODE`:COMPANY, 
                   data = df ,cmethod = "reghdfe")
summary(model_6_1)


### Printing through LATEX Table ###############################

## Table (a): Log Average Price and Log Quantity at the Company level
stargazer::stargazer(model_1,model_1_1,model_2,model_2_1,
                     title = "Effect of Drug Bans on Log Average Price and Log Quantity",column.labels = c("Panel A: log(Average Price+1)","Panel B: log quantity"),
                     column.separate = c(2,2),table.placement = "H", 
                     covariate.labels = c("Treatment*Post","Treatment*Post*MNC","Log Price(per mg)"),font.size = "footnotesize",
                     c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y"),c("Molecule*Calendar Month","Y","Y","Y","Y"),c("Molecule*Firm FE","Y","Y","Y","Y"),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))

## Table (b): Price Volatility  and Price Dispersion at the Company level
stargazer::stargazer(model_3,model_3_1,model_4,model_4_1,
                     title = "Effect of Drug Bans on Price Volatility and Price Dispersion",column.labels = c("Panel A: Price Volatility","Panel B: Price Dispersion"),
                     column.separate = c(2,2),table.placement = "H", 
                     covariate.labels = c("Treatment*Post","Treatment*Post*MNC"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y"),c("Molecule*Calendar Month","Y","Y","Y","Y"),
                                      c("Molecule*Firm FE","Y","Y","Y","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))

## Table (c): Price Dispersion at the Company level : 75-25 and 90-10
stargazer::stargazer(model_5,model_5_1,model_6,model_6_1,
                     title = "Effect of Drug Bans on  Price Dispersion",column.labels = c("Panel A: Price Volatility(25-75)","Panel B: Price Dispersion(10-90)"),
                     column.separate = c(2,2),table.placement = "H", 
                     covariate.labels = c("Treatment*Post","Treatment*Post*MNC"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y"),c("Molecule*Calendar Month","Y","Y","Y","Y"),
                                      c("Molecule*Firm FE","Y","Y","Y","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))

## Table (d): Log Sales Regressions: Full Sample,Acute Sample, Chronic Sample
stargazer::stargazer(model_2,model_2_1,model_3,model_3_1,model_4, model_4_1,
                     title = "Effect of Drug Bans on  Log Sales",column.labels = c("Panel A: Full Sample","Panel B: Acute Sample","Panel C: Chronic Sample"),
                     column.separate = c(2,2,2),table.placement = "H", 
                     covariate.labels = c("Treatment*Post","Treatment*Post*MNC"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y","Y","Y"),c("Molecule*Calendar Month","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule*Firm FE","Y","Y","Y","Y","Y","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))



#'''''''''''''''''''''''''''''''''''''''''''''''
#' Regressions at the State-Molecule-Time-Level
#'
#''''''''''''''''''''''''''''''''''''''''''''''
## 1> Dependent Variable: Sales Data

df = India_09_20 %>% filter(`ACUTE/CHRONIC`!="ACUTE") %>%
    group_by(`SG CODE`,STATE ,DATE) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)+1),Log_Revenue = log(sum(Revenue,na.rm = T)+1),Variety = n_distinct(SKU)) %>%
  mutate(Log_Price = log(Average_Price + 1),
         Interaction = Post*Treatment,
         Month_Contin = round((DATE - as.yearmon("January 2011",format = "%B %Y"))/0.083 + 1,digits = 0))

HHI =  India_09_20  %>% filter(`ACUTE/CHRONIC`!="ACUTE") %>%
  group_by(`SG CODE`,STATE ,DATE,COMPANY) %>% 
  summarise(num = sum(ASU_mg,na.rm = T),N_Firms = n_distinct(COMPANY)) %>%
  group_by(`SG CODE`,STATE ,DATE) %>%
  summarise(dem = sum(num),HHI =ifelse(dem==0,1/N_Firms,sum((num/dem)^2))) %>%
  dplyr::select(-c(dem))


df <- df %>% left_join(.,HHI)

df$molecule_geo <- (as.numeric(as.factor(df$`SG CODE`):as.factor(df$STATE)))


# df = India_09_20 %>% 
#   group_by(`SG CODE`,STATE,DATE) %>% 
#   summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),
#             Treatment = mean(Treatment),Sales = (sum(ASU_mg,na.rm = T)), Revenue = (sum(Revenue,na.rm = T)) ,Variety = n_distinct(SKU)) %>%
#   mutate(Log_Sales = log(Sales + 1),
#          Log_Revenue = log(Revenue),
#          Log_Price = log(Average_Price + 1),
#          Interaction = Post*Treatment,
#          Month_Contin = round((DATE - as.yearmon("April 2009",format = "%B %Y"))/0.083 + 1,digits = 0))
# 
# df_HHI = India_09_20 %>% filter(ASU_mg != 0) %>% 
#   group_by(`SG CODE`,STATE,DATE,COMPANY) %>% 
#   summarise(Total = sum(ASU_mg,na.rm = T)) %>%
#   group_by(`SG CODE`,STATE,DATE)  %>%
#   summarise(HHI = sum( (Total/sum(Total))^2) )
# 
# df <- df %>% left_join(.,df_HHI)


### First: Log_Sales Estimation

model_1 <-  felm(Log_Sales ~ Interaction + HHI + Treatment:Month_Contin |  # Normal Covariates
                   (STATE) + (`SG CODE`)  + DATE| 0 | molecule_geo,
                 data = df ,cmethod = "reghdfe",exactDOF = T)

summary(model_1)


## Second: Log_Sales Estimation with Interaction of Molecule*Geography FE's

model_2 <-  felm(Log_Sales ~ Interaction + HHI + Treatment:Month_Contin   |  # Normal Covariates
                    (STATE) + (`SG CODE`)  + as.factor(molecule_geo) + DATE | 0|molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe",exactDOF = T)

summary(model_2)




## Third: Log_Sales Estimation with Interaction of Geography*Time FE's

model_3 <-  felm(Log_Sales ~ Interaction + HHI + Treatment:Month_Contin   |  # Normal Covariates
                   STATE + `SG CODE` + as.factor(STATE):as.factor(DATE) + DATE | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_3)



# stargazer::stargazer(model_1,model_2,model_3,
#                      title = "Effect of Drug Bans on Log Sales at the Molecule Geography Time Level",
#                      table.placement = "H",
#                      covariate.labels = c("Treatment*Post Ban","HHI","Log_Price"),font.size = "footnotesize",
#                      add.lines = list(c("First Stage","16880.4","2130","3004"),c("Treatment*Time","Y","Y","Y"),c("Time FE","Y","Y","Y"),
#                                       c("Molecule FE","Y","Y","Y"),c("Geography Dummy","Y","Y","Y"),
#                                       c("Molecule*Geography","N","Y","N"),c("Geography*Time","N","N","Y")),
#                      header=FALSE,no.space = TRUE, column.sep.width = "1pt",
#                      notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses")



### 1_1 : Log_Revenue Estimation

model_1_1 <-  felm(Log_Revenue ~ Interaction + HHI + Treatment:Month_Contin |  # Normal Covariates
                     (STATE) + (`SG CODE`)  + DATE| 0 | molecule_geo,
                   data = df ,cmethod = "reghdfe",exactDOF = T)

summary(model_1_1)


## Second: Log_Sales Estimation with Interaction of Molecule*Geography FE's

model_2_1 <-  felm(Log_Revenue ~ Interaction + HHI + Treatment:Month_Contin   |  # Normal Covariates
                     (STATE) + (`SG CODE`)  + as.factor(molecule_geo) + DATE | 0|molecule_geo,   # Clustered S.E.
                   data = df ,cmethod = "reghdfe",exactDOF = T)
summary(model_2_1)

## Third: Log_Sales Estimation with Interaction of Geography*Time FE's

model_3_1 <-  felm(Log_Revenue ~ Interaction + HHI + Treatment:Month_Contin   |  # Normal Covariates
                     STATE + `SG CODE` + as.factor(STATE):as.factor(DATE) + DATE | # FE
                     0 | # Instrumental Variables
                     molecule_geo,   # Clustered S.E.
                   data = df ,cmethod = "reghdfe")

summary(model_3_1)



stargazer::stargazer(model_1,model_2,model_3,model_1_1,model_2_1,model_3_1,
                     title = "Effect of Drug Bans on (Log) Sales & Revenue at the Molecule Geography Time Level",
                     table.placement = "H",column.labels = c("Panel A: log(Sales+1)","Panel B: log(Revenue+1"),
                     column.separate = c(3,3), keep = c("Interaction","HHI"),
                     covariate.labels = c("Treatment*Post Ban","HHI"),font.size = "footnotesize",
                     add.lines = list(c("Treatment*Time","Y","Y","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y","Y","Y","Y"),c("Geography Dummy","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N","N","Y","N"),c("Geography*Time","N","N","Y","N","N","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "-25pt",
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses")


## 2> Dependent Variable: Variety of Molecules

df = India_09_20 %>% filter(ASU_mg != 0) %>%  
  group_by(`SG CODE`,STATE,DATE) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)),
            Total = sum(ASU_mg,na.rm = T),N_Firms = n_distinct(COMPANY)) %>%
  mutate(Log_Price = log(Average_Price + 1),
         Interaction = Post*Treatment,
         Month_Contin = round((DATE - as.yearmon("April 2009",format = "%B %Y"))/0.083 + 1,digits = 0))

HHI =  India_09_20 %>% filter(ASU_mg != 0) %>%  
  group_by(`SG CODE`,STATE,DATE,COMPANY) %>% 
  summarise(num = sum(ASU_mg,na.rm = T),Variety_C = n_distinct(SKU)) %>%
  group_by(STATE,`SG CODE`,DATE) %>%
  summarise(dem = sum(num),HHI = sum((num/dem)^2),Variety = sum(Variety_C)) %>%
  dplyr::select(-c(dem))


df <- df %>% left_join(.,HHI)
df$molecule_geo <- (as.numeric(as.factor(df$`SG CODE`):as.factor(df$STATE)))

### First: Variety Estimation

library(speedglm)


model_4 <- glm(data = df,Variety ~ Interaction + HHI + Treatment*as.factor(DATE) + as.factor(STATE) + as.factor(`SG CODE`),family = poisson(link = "log"))


# sqrt(diag(vcovHC(model, type="HC2"))) # To Create Robust S.E. without clustering
# model <- coeftest(model_1, vcov. = vcovCL(model_1, cluster = as.factor(df$id), type = "HC0")) # with clustering
# model_4_1 <- coeftest(model_4)
# summary(model_4)
SE_robust <- sqrt(diag(vcovCL(model_4,cluster = as.factor(df$`SG CODE`):as.factor(df$STATE),  type="HC0"))) # in a vector form

# Merging Robust s.E. in the original model
# model_5 <- summary(model_4)
# model_5$coefficients[,2] <- SE_robust
# model_5
# # Saving the Model Output
# load("Variety_Reg_1.RData")

## Second: Variety Estimation with Interaction of Molecule*Geography FE's

# Not able to run
model_5 <-  speedglm(data = df,Variety ~ Interaction + HHI + Treatment*as.factor(DATE) + as.factor(STATE) + 
                       as.factor(`SG CODE`) + as.factor(`SG CODE`):as.factor(STATE),
                     family = "poisson"(link="log"))

SE_robust_1 <- sqrt(diag(vcovCL(model_5,cluster = as.factor(df$`SG CODE`):as.factor(df$STATE),  type="HC0"))) # in a vector form

# Merging Robust s.E. in the original model
# model_2_1 <- summary(model_2)
# model_2_1$coefficients[,2] <- SE_robust_2
# model_2_1


# save(model_2,file = "Variety_Reg_2.RData")

## Third: Variety Estimation with Interaction of Geography*Time FE's

# Not able to run
model_6 <-  speedglm(data = df,Variety ~ Interaction + HHI + Treatment*as.factor(DATE) + as.factor(STATE) + 
                  as.factor(`SG CODE`) +  as.factor(DATE):as.factor(STATE),
                family = "poisson"(link = "log"))

SE_robust_2 <- sqrt(diag(vcovCL(model_6,cluster = as.factor(df$`SG CODE`):as.factor(df$STATE),  type="HC0"))) # in a vector form

# SE_robust_3 <- as.data.frame(SE_robust)


# ------- Results printed till here.----------

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

### Price Regressions at the STATE-Molecule-Time Level ####

df = India_09_20 %>% filter(ASU_mg != 0) %>%  
  group_by(`SG CODE`,STATE,DATE) %>% 
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
         Month_Contin = round((DATE - as.yearmon("April 2009",format = "%B %Y"))/0.083 + 1,digits = 0))

df$molecule_geo <- (as.numeric(as.factor(df$`SG CODE`):as.factor(df$STATE)))


## Log Average Price -1

model_1 <-  felm(Log_Price ~ Interaction + Treatment*as.factor(DATE)  |  
                    STATE + `SG CODE`  | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_1)

## Second: Log Average Price - 2

model_2 <-  felm(Log_Price ~ Interaction + Treatment*as.factor(DATE)    |  # Normal Covariates
                   (STATE) + (`SG CODE`) +  as.factor(`SG CODE`):as.factor(STATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_2)

## Third: Log Average Price -3

model_3 <-  felm(Log_Price ~ Interaction + Treatment*as.factor(DATE)  |  # Normal Covariates
                   STATE + `SG CODE` + as.factor(STATE):as.factor(DATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_3)

## Price Dispersion -1

model_4 <-  felm(Price_Dispersion ~ Interaction + Treatment*as.factor(DATE) |  
                    STATE + `SG CODE`  | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_4)

## Second: Price Volatility - 2

model_5 <-  felm(Price_Dispersion ~ Interaction + Treatment*as.factor(DATE) |  # Normal Covariates
                    (STATE) + (`SG CODE`) +  as.factor(`SG CODE`):as.factor(STATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_5)

## Third: Price Volatility -3

model_6 <-  felm(Price_Dispersion ~ Interaction + Treatment*as.factor(DATE) |  # Normal Covariates
                    STATE + `SG CODE` +  as.factor(STATE):as.factor(DATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_6)



### Printing the LATEX Table ####

stargazer::stargazer(model_1,model_2,model_3, model_4,model_5,model_6,
                     title = "Effect of Drug Bans on Log Prices and Price Dispersion at the Molecule Geography Time Level",column.labels = c("Panel A: Log Average Price","Panel B: Price Dispersion"),
                     column.separate = c(3,3),table.placement = "H", keep = c(1),
                     covariate.labels = c("Treatment*Post Ban"),font.size = "footnotesize",
                     add.lines = list(c("Treatment*Time","Y","Y","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y","Y","Y","Y"),c("Geography Dummy","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N","N","Y","N"),c("Geography*Time","N","N","Y","N","N","Y")),
                     header=FALSE,no.space = TRUE,
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses",
                     column.sep.width = "-7pt")



## Price Dispersion 10-90 -1

model_1 <-  felm(Price_Dispersion_19 ~ Interaction + Treatment*as.factor(DATE)   |  
                    STATE + `SG CODE`  | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_1)

## Second: Price Dispersion 10-90 -2

model_2 <-  felm(Price_Dispersion_19 ~ Interaction + Treatment*as.factor(DATE)  |  # Normal Covariates
                    (STATE) + (`SG CODE`) +  as.factor(`SG CODE`):as.factor(STATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_2)

## Third: Price Dispersion 10-90 -3

model_3 <-  felm(Price_Dispersion_19 ~ Interaction + Treatment*as.factor(DATE)   |  # Normal Covariates
                    STATE + `SG CODE` +  as.factor(STATE):as.factor(DATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_3)

## First: Price Dispersion 25-75-1

model_4 <-  felm(Price_Dispersion_27 ~ Interaction  + Treatment*as.factor(DATE) |  
                   STATE + `SG CODE`  | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_4)

## Second: Price Dispersion 25-75 -1

model_5 <-  felm(Price_Dispersion_27 ~ Interaction  + Treatment*as.factor(DATE)  |  # Normal Covariates
                    (STATE) + (`SG CODE`) + as.factor(`SG CODE`):as.factor(STATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_5)

## Third: Price Dispersion 25-75 -1

model_6 <-  felm(Price_Dispersion_27 ~ Interaction + Treatment*as.factor(DATE)  |  # Normal Covariates
                   STATE + `SG CODE` +  as.factor(STATE):as.factor(DATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_6)

### Printing the LATEX Table ####

stargazer::stargazer(model_1,model_2,model_3, model_4,model_5,model_6,
                     title = "Effect of Drug Bans on Price Dispersion (different quantiles) at the Molecule Geography Time Level",column.labels = c("Panel A: Price Dispersion (10-90)","Panel B: Price Dispersion (25-75)"),
                     column.separate = c(3,3),table.placement = "H", keep = c(1),
                     covariate.labels = c("Treatment*Post Ban"),font.size = "footnotesize",
                     add.lines = list(c("Treatment*Time","Y","Y","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y","Y","Y","Y"),c("Geography Dummy","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N","N","Y","N"),c("Geography*Time","N","N","Y","N","N","Y")),
                     header=FALSE,no.space = TRUE,
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses",
                     column.sep.width = "-15pt")


#### HHI and Number of Firms at the STATE-molecule time level ###


df = India_09_20 %>% filter(ASU_mg != 0) %>%  
  group_by(`SG CODE`,STATE,DATE,COMPANY) %>% 
  summarise(Total = sum(ASU_mg,na.rm = T),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  group_by(`SG CODE`,STATE,DATE)  %>%
  summarise(HHI = sum( (Total/sum(Total))^2), N_Firms = n_distinct(COMPANY),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  mutate(Interaction = Post*Treatment,Month_Contin = round((DATE - as.yearmon("April 2009",format = "%B %Y"))/0.083 + 1,digits = 0))

df$molecule_geo <- (as.numeric(as.factor(df$`SG CODE`):as.factor(♥df$STATE)))

## First: HHI -1 

model_1 <-  felm(HHI ~ Interaction + Treatment*as.factor(DATE)  |  
                    STATE + `SG CODE`  | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_1)

## Second: HHI-2

model_2 <-  felm(HHI ~ Interaction + Treatment*as.factor(DATE)   |  # Normal Covariates
                      (STATE) + (`SG CODE`)  + as.factor(`SG CODE`):as.factor(STATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_2)

## Third: HHI-3

model_3 <-  felm(HHI~ Interaction + Treatment*as.factor(DATE) |  # Normal Covariates
                   STATE + `SG CODE` + as.factor(STATE):as.factor(DATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_3)

## First: N_Firms- 1

model_4 <-  felm(N_Firms ~ Interaction + Treatment*as.factor(DATE) |  
                    STATE + `SG CODE` | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")
summary(model_4)

## Second: N_Firms - 2

model_5 <-  felm(N_Firms ~ Interaction + Treatment*as.factor(DATE)  |  # Normal Covariates
                   (STATE) + (`SG CODE`) +  as.factor(`SG CODE`):as.factor(STATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_5)

## Third: N_Firms -3

model_6 <-  felm(N_Firms ~ Interaction + Treatment*as.factor(DATE) |  # Normal Covariates
                    STATE + `SG CODE` +  as.factor(STATE):as.factor(DATE) | # FE
                   0 | # Instrumental Variables
                   molecule_geo,   # Clustered S.E.
                 data = df ,cmethod = "reghdfe")

summary(model_6)


### Printing the LATEX Table ####

stargazer::stargazer(model_1,model_2,model_3, model_4,model_5,model_6,
                     title = "Effect of Drug Bans on HHI and No. of Firms at the Molecule Geography Time Level",column.labels = c("Panel A: HHI","Panel B: No. of Firms"),
                     column.separate = c(3,3),table.placement = "H", keep = c(1),
                     covariate.labels = c("Treatment*Post Ban"),font.size = "footnotesize",
                     add.lines = list(c("Treatment*Time","Y","Y","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y","Y","Y","Y"),c("Geography Dummy","Y","Y","Y","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N","N","Y","N"),c("Geography*Time","N","N","Y","N","N","Y")),
                     header=FALSE,no.space = TRUE,
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses",
                     column.sep.width = "-15pt")


############################ End of Do File ##########################################################################################################





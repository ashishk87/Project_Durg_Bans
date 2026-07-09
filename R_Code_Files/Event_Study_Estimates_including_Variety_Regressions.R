
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
library(miceadds)
library(sandwich)
library(lmtest)

#### If data not already loaded, load the data ##########
main_df <- fread("Data/Banned_and_Broad_Controls_08_13.csv",drop  = 1)
main_df$Date <- as.yearmon(main_df$Date,format = "%B %Y")
main_df$Ban_Date <- as.yearmon(main_df$Ban_Date,format = "%B %Y")

###  Sample Code for  Event Study Design Plot ##########

#'''''''''''''''''''''''''''''''''''''# 
 # At the Molecule Time Level
#'''''''''''''''''''''''''''''''''''''#

# Creating data for the purpose of Event Study Design
df = main_df %>% filter(ASU_mg != 0) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
  group_by(Sub.Group,Date,Month) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Average_PTR = mean(PTR_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)),
            Max_Price = max(Price_per_mg,na.rm = T), Min_Price = min(Price_per_mg),Variety = n_distinct(SKU)) %>%
  mutate(Log_Price = log(Average_Price + 1),
         Price_Dispersion =(Max_Price - Min_Price),
         Interaction = Post*Treatment,Log_Avg_PTR = log(Average_PTR + 1),
         Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 ,digits = 0)) 


# Data for Price Volatility 
df = main_df %>% filter(ASU_mg != 0) %>% group_by(Sub.Group,Date)  %>%
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment)) %>% 
  mutate(Log_Price = log(Average_Price + 1),Price_Volatility = append(diff(Log_Price),NA,after = 0),Interaction = Post*Treatment,
         Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0)) %>% 
  separate(col="Date",into = c("Month","Year"),remove = FALSE)


# Data for Price Dispersion
df = main_df %>% filter(ASU_mg != 0) %>% group_by(Sub.Group,Date) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Max_Price = max(Price_per_mg,na.rm = T), Min_Price = min(Price_per_mg),Post = mean(Post),
            Treatment = mean(Treatment),
            Price_per_mg_10 = quantile(Price_per_mg,probs = c(0.1),type = 1), 
            Price_per_mg_25 = quantile(Price_per_mg,probs = c(0.25),type = 1),
            Price_per_mg_75 = quantile(Price_per_mg,probs = c(0.75),type = 1),
            Price_per_mg_90 = quantile(Price_per_mg,probs = c(0.9),type = 1)) %>% 
  mutate(Price_Dispersion =(Max_Price - Min_Price),
         Price_Dispersion = Max_Price - Min_Price,
         Price_Dispersion = (Max_Price - Min_Price),
         Price_Dispersion_19 = (Price_per_mg_90 - Price_per_mg_10),
         Price_Dispersion_27 = (Price_per_mg_75 - Price_per_mg_25),
         Interaction = Post*Treatment,Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0)) %>%
  separate(col="Date",into = c("Month","Year"),remove = FALSE)



# Ban_Date df
Ban_Date <- main_df %>% group_by(Sub.Group) %>%
            mutate(Ban_M = ifelse(Treatment==1,1,0),A = Ban_Date) %>%
            filter(Ban_M == 1) %>% 
            dplyr::select(Sub.Group,A) %>% 
            rename(Ban_Date = A) %>%
            distinct()

# Merging Ban df with Original df
df <- df %>% left_join(., Ban_Date)

# Creating a lead-lag indicator for the datasets
df_E <- df %>%
  # variable with relative date
  mutate(rel_m_year = round((Date - Ban_Date)/0.083 ,digits = 0)) %>%
  # make dummies
  dummy_cols(select_columns = "rel_m_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_m_year_"),~replace_na(.,0)))
  

# Sorting by months
months <- sort(unique(df_E$rel_m_year))
months <- months[which(months != min(months) & months != -1 )]

# Make formula
covs <- c(paste0("`", "rel_m_year_", months, "`"))

## For Log_Price : Note- Exclude Treatment:Month Contin Interaction Term
form <- as.formula(paste0("Log_Price ~", paste0(covs, collapse = " + "),
                          "| Sub.Group + Date | 0 | Sub.Group"))

## For Log_Quantity:
form <- as.formula(paste0("Log_Sales~",paste0(covs,collapse = "+"),
                          "|Sub.Group + Date|(Log_Price ~ Variety)|Sub.Group"))


## For Price_Volatility : Note- Exclude Treatment:Month Contin Interaction Term
form <- as.formula(paste0("Price_Volatility ~", paste0(covs, collapse = " + "),
                          "| Sub.Group + Date | 0 | Sub.Group"))


## For Price Dispersion : Note- Exclude Treatment:Month Contin Interaction Term
form <- as.formula(paste0("Price_Dispersion_27 ~", paste0(covs, collapse = " + "),
                          "| Sub.Group + Date | 0 | Sub.Group"))



# estimate the model and plot
# estimate the model
broom::tidy(felm(form, data = df_E, exactDOF = TRUE, cmethod = "reghdfe"),
            conf.int = TRUE, se = "cluster") %>%
  ## Only keep the estimates of years
  mutate(Interaction = ifelse(term != "`Log_Price(fit)`" & term != "Treatment:Month_Contin",1,0)) %>%
  filter(Interaction==1) %>%
  # add in the relative time variable
  mutate(t = months) %>% 
  filter(t %>% between(-24, 24)) %>% 
  dplyr::select(t, estimate, conf.low, conf.high) %>% 
  # make two different periods for the connection
  mutate(group = case_when(
    t < -1 ~ 1,
    t > -1 ~ 2,
    TRUE ~ NA_real_
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate, group = group)) + 
  geom_point(fill = "white", shape = 21) + geom_line(color = "darkblue",size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "red") + 
  ggtitle(paste0("Price Dispersion (25-75) E.S. Estimates")) + 
  labs(y = "Change in \n Dispersion", x = "Months Relative to Ban") + 
  scale_x_continuous(breaks = seq(-24, 24, by = 2)) + 
  #scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5)) + 
  theme_bw()



###  Sample Code for  Event Study Design Plot ##########

#'''''''''''''''''''''''''''''''''''''# 
# At the Firm Molecule Time Level
#'''''''''''''''''''''''''''''''''''''#

# Creating data for the purpose of Event Study Design

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



# Merging Ban df with Original df
Ban_Date$Ban_Date <- as.yearmon(Ban_Date$Ban_Date,format = "%B %Y")
df <- df %>% left_join(., Ban_Date)

# Creating a lead-lag indicator for the datasets
df_E <- df %>%
  # variable with relative date
  mutate(rel_m_year = round((Date - Ban_Date)/0.083 ,digits = 0)) %>%
  # make dummies
  dummy_cols(select_columns = "rel_m_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_m_year_"),~replace_na(.,0)))

# Sorting by months
months <- sort(unique(df_E$rel_m_year))
months <- months[which(months != min(months) & months != -1 )]

# Make formula
covs_1 <- c(paste0("`", "rel_m_year_", months, "`",":MNC")) # For Triple Interaction Term
covs <-  c(paste0("`", "rel_m_year_", months, "`")) # For Double Interaction Term


## Log Average Price both for Interaction and MNC Term
form <- as.formula(paste0("Log_Price ~", paste0(covs, collapse = " + "), "+" ,paste0(covs_1,collapse = " + "), 
                          "| Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month) | 0 | as.factor(Sub.Group):as.factor(Company)"))


## Log Sales  both for interaction and MNC Term
form <- as.formula(paste0("Log_Sales ~", paste0(covs, collapse = " + "), "+" ,paste0(covs_1,collapse = " + "), 
                          "| Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month) | (Log_Price ~ Log_Avg_PTR) |as.factor(Sub.Group):as.factor(Company)"))


## Price Volatility both for interaction and MNC Term
form <- as.formula(paste0("Price_Volatility ~", paste0(covs, collapse = " + "), "+" ,paste0(covs_1,collapse = " + "), 
                          "| Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month) |0 | as.factor(Sub.Group):as.factor(Company)"))

## Price Dispersion both for interaction and MNC Term
form <- as.formula(paste0("Price_Dispersion ~", paste0(covs, collapse = " + "), "+" ,paste0(covs_1,collapse = " + "), 
                          "| Company + Sub.Group + Date + as.factor(Sub.Group):as.factor(Company )+ as.factor(Sub.Group):as.factor(Month) |0 | as.factor(Sub.Group):as.factor(Company)"))




# estimate the model and plot
# estimate the model
broom::tidy(felm(form, data = df_E, exactDOF = TRUE, cmethod = "reghdfe"),
            conf.int = TRUE, se = "cluster") %>%
  mutate(NW = ifelse(term != "`Log_Price(fit)`",1,0 )) %>%
  filter(NW==1) %>%
  ## Only keep the estimates of years
  mutate(Interaction = ifelse(grepl("MNC",term),"MNC*Interaction","Interaction")) %>%
  # filter(Interaction==1) %>%
  # Group_by MNC and Domestic Firms Status
  group_by(Interaction) %>%
  # add in the relative time variable
  mutate(t = months) %>% 
  filter(t %>% between(-24, 24)) %>% 
  dplyr::select(t, estimate, conf.low, conf.high) %>% 
  # make two different periods for the connection
  mutate(group = case_when(
    t < -1 ~ 1,
    t > -1 ~ 2,
    TRUE ~ NA_real_
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate, group = group)) + 
  geom_point(fill = "white", shape = 21) + geom_line(color = "darkblue",size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "red") + 
  ggtitle(paste0("Price Dispersion Event Study Estimates for \n Double and Triple Interaction Terms")) + 
  labs(y = "Percentage \n Dispersion", x = "Months Relative to Ban") + 
  scale_x_continuous(breaks = seq(-24, 24, by = 5)) + 
  #scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~Interaction,scales =  "free") + 
  theme_bw()



###  Sample Code for  Event Study Design Plot ##########

#'''''''''''''''''''''''''''''''''''''# 
# At the  Molecule Time Level for HHI and Number of Firms
#'''''''''''''''''''''''''''''''''''''#

# Creating data for the purpose of Event Study Design
df = main_df %>% filter(ASU_mg != 0) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
  group_by(Sub.Group,Date,Month,Company) %>% 
  summarise(Total = sum(ASU_mg,na.rm = T),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  group_by(Sub.Group,Date,Month)  %>%
  summarise(HHI = sum( (Total/sum(Total))^2), N_Firms = n_distinct(Company),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  mutate(Interaction = Post*Treatment,Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))


# Merging Ban df with Original df
Ban_Date$Ban_Date <- as.yearmon(Ban_Date$Ban_Date,format = "%B %Y")
df <- df %>% left_join(., Ban_Date)


# Creating a lead-lag indicator for the datasets
df_E <- df %>%
  # variable with relative date
  mutate(rel_m_year = round((Date - Ban_Date)/0.083 ,digits = 0)) %>%
  # make dummies
  dummy_cols(select_columns = "rel_m_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_m_year_"),~replace_na(.,0)))


# Sorting by months
months <- sort(unique(df_E$rel_m_year))
months <- months[which(months != min(months) & months != -1 )]

# Make formula
covs <- c(paste0("`", "rel_m_year_", months, "`"))


## For HHI 
form <- as.formula(paste0("HHI ~", paste0(covs, collapse = " + "),
                          "| Sub.Group + Date | 0 | Sub.Group"))


## For Number of Firms
form <- as.formula(paste0("N_Firms ~", paste0(covs, collapse = " + "),
                          "| Sub.Group + Date | 0 | Sub.Group"))


# estimate the model and plot
# estimate the model
broom::tidy(felm(form, data = df_E, exactDOF = TRUE, cmethod = "reghdfe"),
            conf.int = TRUE, se = "cluster") %>%
  ## Only keep the estimates of years
  # mutate(Interaction = ifelse(term != "`Log_Price(fit)`" & term != "Treatment:Month_Contin",1,0)) %>%
  # filter(Interaction==1) %>%
  # add in the relative time variable
  mutate(t = months) %>% 
  filter(t %>% between(-24, 24)) %>% 
  dplyr::select(t, estimate, conf.low, conf.high) %>% 
  # make two different periods for the connection
  mutate(group = case_when(
    t < -1 ~ 1,
    t > -1 ~ 2,
    TRUE ~ NA_real_
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate, group = group)) + 
  geom_point(fill = "white", shape = 21) + geom_line(color = "darkblue",size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "red") + 
  ggtitle(paste0("No. of Firms Event Study Estimates")) + 
  labs(y = "Change in No. of \n Firms ", x = "Months Relative to Ban") + 
  scale_x_continuous(breaks = seq(-24, 24, by = 2)) + 
  #scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5)) + 
  theme_bw()


###  Sample Code for  Event Study Design Plot ##########

#'''''''''''''''''''''''''''''''''''''# 
# At the State Molecule Time Level for HHI and Number of Firms
#'''''''''''''''''''''''''''''''''''''#

# Creating data for the purpose of Event Study Design



## 1> Dependent Variable: Sales Data
df = main_df %>% dplyr::filter(ASU_mg != 0 ) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
  group_by(Sub.Group,State,Date,Month) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Average_PTR = mean(PTR_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)),
            Total = sum(ASU_mg,na.rm = T),Variety = n_distinct(SKU)) %>%
  mutate(Log_Price = log(Average_Price + 1),
         Interaction = Post*Treatment,Log_Avg_PTR = log(Average_PTR + 1),
         Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))

HHI =  main_df %>% dplyr::filter(ASU_mg != 0) %>%  
  group_by(Sub.Group,State,Date,Company) %>% 
  summarise(num = sum(ASU_mg,na.rm = T)) %>%
  group_by(State,Sub.Group,Date) %>%
  summarise(dem = sum(num),HHI = sum((num/dem)^2)) %>%
  dplyr::select(-c(dem))

df <- df %>% left_join(.,HHI)


# Merging Ban df with Original df
df <- df %>% left_join(., Ban_Date)

# df$Ban_Date <- as.yearmon(df$Ban_Date,format = "%B %Y")


# Creating a lead-lag indicator for the datasets
df_E <- df %>%
  # variable with relative date
  mutate(rel_m_year = round((Date - Ban_Date)/0.083 ,digits = 0)) %>%
  # make dummies
  dummy_cols(select_columns = "rel_m_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_m_year_"),~replace_na(.,0)))


# Sorting by months
months <- sort(unique(df_E$rel_m_year))
months <- months[which(months != min(months) & months != -1 )]

# Make formula
covs <- c(paste0("`", "rel_m_year_", months, "`"))


## For HHI 
form <- as.formula(paste0("Log_Sales ~", paste0(covs, collapse = " + "),"+ HHI",
                          "| Treatment:as.factor(Date) + State + Sub.Group + Date | (Log_Price ~ Variety) | Sub.Group:State"))




# estimate the model and plot
# estimate the model
broom::tidy(felm(form, data = df_E, exactDOF = TRUE, cmethod = "reghdfe"),
            conf.int = TRUE, se = "cluster") %>%
  ## Only keep the estimates of years
    mutate(Interaction = ifelse(term != "`Log_Price(fit)`" & term != "HHI",1,0)) %>%
  filter(Interaction==1) %>%
  # add in the relative time variable
  mutate(t = months) %>% 
  filter(t %>% between(-24, 24)) %>% 
  dplyr::select(t, estimate, conf.low, conf.high) %>% 
  # make two different periods for the connection
  mutate(group = case_when(
    t < -1 ~ 1,
    t > -1 ~ 2,
    TRUE ~ NA_real_
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate, group = group)) + 
  geom_point(fill = "white", shape = 21) + geom_line(color = "darkblue",size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "red") + 
  ggtitle(paste0("Log_Sales Event Study Estimates ")) + 
  labs(y = "Percent  \n Change ", x = "Months Relative to Ban") + 
  scale_x_continuous(breaks = seq(-24, 24, by = 2)) + 
  #scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5)) + 
  theme_bw()


## 2> Dependent Variable: Variety of Molecules

df = main_df %>% filter(ASU_mg != 0) %>%  
  group_by(Sub.Group,State,Date,Month) %>% 
  summarise(Average_Price = mean(Price_per_mg,na.rm = T),Post = mean(Post),Treatment = mean(Treatment),Log_Sales = log(sum(ASU_mg,na.rm = T)),
            Total = sum(ASU_mg,na.rm = T)) %>%
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

# Merging Ban df with Original df
df <- df %>% left_join(., Ban_Date)




# Creating a lead-lag indicator for the datasets
df_E <- df %>%
  # variable with relative date
  mutate(rel_m_year = round((Date - Ban_Date)/0.083 ,digits = 0)) %>%
  # make dummies
  dummy_cols(select_columns = "rel_m_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_m_year_"),~replace_na(.,0)))


# Sorting by months
months <- sort(unique(df_E$rel_m_year))
months <- months[which(months != min(months) & months != -1 )]

# Make formula
covs <- c(paste0("`", "rel_m_year_", months, "`"))


## For VARIETY
form <- as.formula(paste0("Variety ~", paste0(covs, collapse = " + "),"+ HHI + Treatment:as.factor(Date) + as.factor(State) + 
                  as.factor(Sub.Group) + as.factor(Date)"))



# estimate the model and plot
# estimate the model
model_4 <-  glm(data = df_E,formula = form ,
                family = poisson(link = "log"))


## Creating df of coefficients
A <- broom::tidy(model_4,conf.int = FALSE)

A_1 <- A[c(2:89),] # Only retaining the relevant variables

## Creating df of clustered se seperately
df_E$id <- as.factor(df_E$Sub.Group):as.factor(df_E$State)
B <- broom::tidy(sqrt(diag(vcovCL(model_4,cluster = as.factor(df_E$id),  type="HC0"))))
B_1 <- B[c(2:89),] # Only retaining the relevant variables

## Merging the clustered s.e. with original df
A_1$CSE <- B_1$x


A_1 %>%
  # Making columns for C.I.
  mutate(conf.low = estimate - 1.96*(CSE), conf.high = estimate + 1.96*(CSE)) %>%
  # add in the relative time variable
  mutate(t = months) %>% 
  filter(t %>% between(-24,24)) %>% 
  dplyr::select(t, estimate, conf.low, conf.high) %>% 
  # make two different periods for the connection
  mutate(group = case_when(
    t < -1 ~ 1,
    t > -1 ~ 2,
    TRUE ~ NA_real_
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate, group = group)) + 
  geom_point(fill = "white", shape = 21) + geom_line(color = "darkblue",size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "red") + 
  ggtitle(paste0("Variety Event Study Estimates ")) + 
  labs(y = "Change in  \n Log Count  ", x = "Months Relative to Ban") + 
  scale_x_continuous(breaks = seq(-24, 24, by = 2)) + 
  #scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5)) + 
  theme_bw()




#### 3> Log Price ############

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




# Merging Ban df with Original df
df <- df %>% left_join(., Ban_Date)

# df$Ban_Date <- as.yearmon(df$Ban_Date,format = "%B %Y")


# Creating a lead-lag indicator for the datasets
df_E <- df %>%
  # variable with relative date
  mutate(rel_m_year = round((Date - Ban_Date)/0.083 ,digits = 0)) %>%
  # make dummies
  dummy_cols(select_columns = "rel_m_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_m_year_"),~replace_na(.,0)))


# Sorting by months
months <- sort(unique(df_E$rel_m_year))
months <- months[which(months != min(months) & months != -1 )]

# Make formula
covs <- c(paste0("`", "rel_m_year_", months, "`"))


## For HHI 
form <- as.formula(paste0("Price_Dispersion_19 ~", paste0(covs, collapse = " + "),
                          "| Treatment:as.factor(Date) + State + Sub.Group + Date | 0 | as.factor(Sub.Group):as.factor(State)"))



# estimate the model and plot
# estimate the model
broom::tidy(felm(form, data = df_E, exactDOF = TRUE, cmethod = "reghdfe"),
            conf.int = TRUE, se = "cluster") %>%
  ## Only keep the estimates of years
  # mutate(Interaction = ifelse(term != "`Log_Price(fit)`" & term != "HHI",1,0)) %>%
  # filter(Interaction==1) %>%
  # add in the relative time variable
  mutate(t = months) %>% 
  filter(t %>% between(-24, 24)) %>% 
  dplyr::select(t, estimate, conf.low, conf.high) %>% 
  # make two different periods for the connection
  mutate(group = case_when(
    t < -1 ~ 1,
    t > -1 ~ 2,
    TRUE ~ NA_real_
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate, group = group)) + 
  geom_point(fill = "white", shape = 21) + geom_line(color = "darkblue",size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "red") + 
  ggtitle(paste0("Price_Dispersion (10-90) percentile Event Study Estimates ")) + 
  labs(y = "Change in \n Dispersion ", x = "Months Relative to Ban") + 
  scale_x_continuous(breaks = seq(-24, 24, by = 2)) + 
  #scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5)) + 
  theme_bw()


### 3> HHI and No. of Firms


df = main_df %>% filter(ASU_mg != 0) %>%  mutate(PTR_per_mg = (PTR/MRP)* Price_per_mg ) %>%
  group_by(Sub.Group,State,Date,Company) %>% 
  summarise(Total = sum(ASU_mg,na.rm = T),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  group_by(Sub.Group,State,Date)  %>%
  summarise(HHI = sum( (Total/sum(Total))^2), N_Firms = n_distinct(Company),Post=  mean(Post),Treatment = mean(Treatment)) %>%
  mutate(Interaction = Post*Treatment,Month_Contin = round((Date - as.yearmon("April 2007",format = "%B %Y"))/0.083 + 1,digits = 0))


# Merging Ban df with Original df
df <- df %>% left_join(., Ban_Date)

# df$Ban_Date <- as.yearmon(df$Ban_Date,format = "%B %Y")


# Creating a lead-lag indicator for the datasets
df_E <- df %>%
  # variable with relative date
  mutate(rel_m_year = round((Date - Ban_Date)/0.083 ,digits = 0)) %>%
  # make dummies
  dummy_cols(select_columns = "rel_m_year", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>%
  mutate(across(starts_with("rel_m_year_"),~replace_na(.,0)))


# Sorting by months
months <- sort(unique(df_E$rel_m_year))
months <- months[which(months != min(months) & months != -1 )]

# Make formula
covs <- c(paste0("`", "rel_m_year_", months, "`"))


## For HHI 
form <- as.formula(paste0("N_Firms ~", paste0(covs, collapse = " + "),
                          "| Treatment:as.factor(Date) + State + Sub.Group + Date | 0 | Sub.Group:State"))



# estimate the model and plot
# estimate the model
broom::tidy(felm(form, data = df_E, exactDOF = TRUE, cmethod = "reghdfe"),
            conf.int = TRUE, se = "cluster") %>%
  ## Only keep the estimates of years
  mutate(Interaction = ifelse(term != "`Log_Price(fit)`" & term != "HHI",1,0)) %>%
  filter(Interaction==1) %>%
  # add in the relative time variable
  mutate(t = months) %>% 
  filter(t %>% between(-24, 24)) %>% 
  dplyr::select(t, estimate, conf.low, conf.high) %>% 
  # make two different periods for the connection
  mutate(group = case_when(
    t < -1 ~ 1,
    t > -1 ~ 2,
    TRUE ~ NA_real_
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate, group = group)) + 
  geom_point(fill = "white", shape = 21) + geom_line(color = "darkblue",size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = -1,  linetype = "longdash", color = "red") + 
  ggtitle(paste0("No. of Firms Event Study Estimates ")) + 
  labs(y = "Change", x = "Months Relative to Ban") + 
  scale_x_continuous(breaks = seq(-24, 24, by = 2)) + 
  #scale_y_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5)) + 
  theme_bw()







•########### End of Results ###########################


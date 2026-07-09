############  Merging the CELECOXIB dataset  ##############################

#----------Importing Libraries-----------------#

lapply(c("XLConnect","GGally","tidyverse","visNetwork","magrittr","DiagrammeR","data.table",
         "plotly","intergraph","networkD3","optrees","disparityfilter","network","Matrix","igraph"
         ,"CINNA","ggplot2","poweRlaw","devtools","ForceAtlas2","remotes","netdiffuseR","lfe","stargazer","foreign","MASS","lubridate","ggthemes","readtext","zoo"), require,character.only = TRUE)
library(fs)

`%notin%` <- Negate(`%in%`)
#---- Setting WD--------------------------------#

# setwd("F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma")

#---- Importing the Data------------------------#

files <- dir_ls(path = "F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Data_Banned_Drugs/", glob = "*txt") # To see how many files are in the folder

#----- Writing a general Function to Make a Final Dataset----------#

data_maker <- function(data){
  
  df <- data.table(read.delim(data,skip = 2))
  
  df <- df[,-c("Month")]
  
  
  df <- df[df$Metrics %in% c("Bonus Qty","Actual Sale Units","PTR","MRP")]
  
  
  df <- df %>% gather( key = "Months", value = "n", -c(1:8),convert = T) %>%
    spread(key = Metrics, value = n,convert = T )
  
  dflist <- c("Actual Sale Units","Bonus Qty","MRP","PTR")
  df[,dflist] = apply(df[,dflist],2,function(x) as.numeric(gsub(",","",x)))
  
  #--- Seperating the Month Year Information----------#
  df  <- df  %>% separate(col="Months",into = c("Month","Year"))%>%
    unite(col = "Date", Month,Year, sep = "-")
  
  #--- Creating a Date Class Column--------------------#
  df$Date <- as.yearmon(df$Date,format = "%b-%y")
  
  #--- Again Seperating the Month Year Info------------#
  df <- df  %>% separate(col="Date",into = c("Month","Year"), remove = F)
  
  df
  
}

## Applying Function ##

final_df <- map_dfr(files,data_maker)
lapply(final_df, class)
final_df <- data.table(final_df)

## Converting the Information Into Mg #################



SKU_Info <- final_df %>% distinct(SKU)

SKU_Info$SKU <- as.character(SKU_Info$SKU)

SKU_Info <- data.frame(SKU_Info)

SKU_A <- SKU_Info %>% separate(col= "SKU",into=c("1","2","3","4","5","6","7","8","9","10","11"),remove=F)

SKU_3 <- SKU_A %>% filter(is.na(`4`) & is.na(`5`) & is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_4 <- SKU_A %>% filter( !is.na(`4`) & is.na(`5`) & is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_5 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_6 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_7 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_8 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & is.na(`9`))
SKU_9 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & !is.na(`9`) &is.na(`10`) & is.na(`11`))
SKU_10 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & !is.na(`9`) & !is.na(`10`) & is.na(`11`) )
SKU_11 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & !is.na(`9`)& !is.na(`10`) & !is.na(`11`))



##### SKU_3 Processed and Merged #######
SKU_3 <- SKU_3 %>% mutate(mg = c(460,700,600,600,525,25,120,NA,NA), Strips = `3`,Non_Oral = ifelse(`2` %in% c("TABLET","CAPSULE"),0,1)) 
SKU_3 <- SKU_3[-c(8:9),-c(2:12)]  ## Concatenate this part


##### SKU_4 Processed and Merged ########

SKU_4_1 <- SKU_4 %>% filter(`4`=="ML") %>% mutate(mg = as.numeric(`3`)*1000,Strips = 1)
SKU_4_1 <- SKU_4_1[,-c(2:12)] ## Do not Concatenate this part coz MG info to convert from ML is not given

SKU_4_2 <- SKU_4 %>% filter(`4`!= "ML" & `4`=="GM") %>% mutate(mg = as.numeric(`3`)*1000,Strips = 1)
SKU_4_2 <- SKU_4_2[,-c(2:12)] ## Do not Concatenate this part coz MG info to convert from GM is not given

SKU_4_3 <- SKU_4 %>% filter(`4`!= "ML" & `4`!="GM" & `3`=="TABLET") %>% mutate(mg = c(460,400,400,NA,580,400,502,500,NA),Strips=`4`,Non_Oral = ifelse(`3` %in% c("TABLET","CAPSULE"),0,1))
SKU_4_3 <- SKU_4_3[-c(4,9),-c(2:12)] ## Concatenate this part

SKU_4_4 <- SKU_4 %>% filter(`4`!= "ML" & `4`!="GM" & `3`!="TABLET") %>% mutate(mg = c(NA,400,NA,10),Strips=c(1,15,NA,10),Non_Oral = ifelse(`3` %in% c("TABLET","CAPSULE"),0,1))
SKU_4_4 <- SKU_4_4[-c(1,3),-c(2:12)] ## Concatenate this part


###### SKU_5 Processed and Merged ########

SKU_5_1 <- SKU_5 %>% filter(`4`=="TABLET") %>% mutate(mg = ifelse(`2`=="GUJARAT",4,ifelse(`2`=="N",500,`2`)),Strips=`5`,Non_Oral = 0)
SKU_5_1 <- SKU_5_1[,-c(2:12)] ## Concatenate this part

SKU_5_2 <- SKU_5 %>% filter(`4`!="TABLET" & `5` %in% c("GM")) %>% mutate(mg = as.numeric(`4`)*1000,Strips=1)
SKU_5_2 <- SKU_5_2[,-c(2:12)] ## Do not Concatenate this part coz MG info to convert from GM is not given

SKU_5_3 <- SKU_5 %>% filter(`4`!="TABLET" & `5` %notin% c("ML","GM") & `3`=="MG") %>% mutate(mg =`2`,Strips= 1,Non_Oral = ifelse(`4` %in% c("TABLET","CAPSULE"),0,1))
SKU_5_3 <- SKU_5_3[,-c(2:12)] ## Concatenate this part


###### SKU_6 Processed and Merged #########


SKU_6_1 <- SKU_6 %>% mutate(`13` = as.numeric(`2`)) %>% filter(`5`=="TABLET") %>% mutate(mg = ifelse(SKU == "MOTEN 2.5 MG TABLET 10",2.5,ifelse(`3`==50000,20,ifelse(`2`== 1,1.5,ifelse(`2`==12,12.5,ifelse(is.na(`13`), `3`,as.numeric(`2`) + as.numeric(`3`) ))))
),Strips=`6`,Non_Oral = 0)
SKU_6_1 <- SKU_6_1[,-c(2:13)] ## Concatenate this part


SKU_6_2 <- SKU_6 %>% filter(`5` != "TABLET" & `3`=="MG" ) %>% mutate(mg = ifelse(`4`=="TABLET",as.numeric(`2`)*as.numeric(`6`),as.numeric(`2`)*as.numeric(`5`)),Strips=1,Non_Oral = ifelse(`4` %in% c("TABLET","CAPSULE"),0,1))
SKU_6_2 <- SKU_6_2[,-c(2:12)] ## Concatenate this part


SKU_6_3 <- SKU_6 %>% filter(`5` != "TABLET" & `3`!="MG" & (`4`=="GM"| `4`=="MG" | `6`=="GM" )) %>% mutate(mg = c(30,1.5,1500,1000,750,5000,5,10,15), Strips=c(10,1,1,1,1,1,10,10,10),Non_Oral = ifelse(`5` %in% c("TABLET","CAPSULE"),0,1))
SKU_6_3 <- SKU_6_3[,-c(2:12)] ## Concatenate this part


###### SKU_7 Processed and Merged #########

SKU_7_1 <- SKU_7 %>% mutate(`13` = as.numeric(`2`),`14` = as.numeric(`3`)) %>%  filter(`6`=="TABLET")   %>% mutate(mg = case_when( `2`== 1~ 1.5 + as.numeric(`4`), `2`==12~12.5 + as.numeric(`4`),is.na(`13`) & !is.na(`14`) ~ as.numeric(`3`) + as.numeric(`4`), !is.na(`13`) & !is.na(`14`) ~ as.numeric(`2`)+ as.numeric(`3`) + as.numeric(`4`),is.na(`14`)~ as.numeric(`4`)), Strips=`7`,Non_Oral = 0)
SKU_7_1 <- SKU_7_1[,-c(2:14)] ## Concatenate this part

SKU_7_2 <- SKU_7 %>% mutate(`13` = as.numeric(`2`)) %>% filter(`6`!="TABLET" &(`4` %in% c("MG","GM") | `5` %in% c("MG","GM") | `7` %in% c("MG","GM"))) %>% mutate(mg = case_when(`7`=="GM"~as.numeric(`6`)*1000,`5`=="GM"~1000,`4`=="MG"& `5`=="TABLET"~as.numeric(`3`)*as.numeric(`7`),`2`==12~12.5*as.numeric(`6`),`4`=="MG" & `5`!="TABLET" & is.na(`13`)~as.numeric(`3`)*as.numeric(`6`),`4`=="MG"& `5`!="TABLET" & !is.na(`13`)~(as.numeric(`2`)+as.numeric(`3`))*as.numeric(`6`) ),Strips=1,
                                                                                                                                                                  Non_Oral = ifelse(`5`  %in% c("TABLET","CAPSULE") | `6` %in% c("TABLET","CAPSULE"), 0,1))
SKU_7_2[c(1,24),c(14)] <- c(2375,25) 
SKU_7_2 <- SKU_7_2[,-c(2:13)] ## Concatenate this part

###### SKU_8 Processed and Merged #########

SKU_8_1 <- SKU_8 %>% filter(`8`!="ML" & `7`=="TABLET") %>% mutate(mg = case_when(`3` %in% c(2,5)~as.numeric(`3`)+as.numeric(`4`)+as.numeric(`5`), `3` %in% c("PHARMA","FORTE","KIT")~as.numeric(`4`)+as.numeric(`5`), `3` %in% c("JB")~ as.numeric(`5`),`2` %in% c("GARY","CEBRAN")~ as.numeric(`4`) + as.numeric(`5`) ,`3`== 1~as.numeric(`5`) + 1.5),Strips=`8`,Non_Oral = 0)
SKU_8_1 <- SKU_8_1[,-c(2:12)] ## Concatenate this part

SKU_8_2 <- SKU_8 %>% filter(`8`!="ML" & `7`!="TABLET") %>% mutate(mg=c(NA,1000,250,22,22,12,22),Strips=c(1,1,1,10,10,10,10),Non_Oral = ifelse(`6` %in% c("TABLET","CAPSULE"),0,1))
SKU_8_2 <- SKU_8_2[-c(1),-c(2:12)] ## Concatenate this part


####### SKU_9 Processed and Merged ###########

SKU_9 <-  SKU_9 %>% filter(`5` %in% c("MG","GM") |`6` %in% c("MG","GM") |`7` %in% c("MG","GM") ) %>% filter(`1` != "OCUPOL") %>% mutate(mg = c(16.3,16.3,3750,12.5),Strips=c(1,1,1,10),Non_Oral = c(0,0,0,1))
SKU_9 <- SKU_9[,-c(2:12)] ## Concatenate this part

####### SKU_10 Processed and Merged ###########

SKU_10 <- SKU_10[-c(2),] %>% mutate(mg=c(3361),Strips=1)
SKU_10 <- SKU_10[,-c(2:12)] ## Do not Concatenate this part as Mg info is absent

####### SKU_11 Processed and Merged ###########

SKU_11 ## Do not Concatenate this Part


##### Final_SKU Created #########

SKU_Final <- rbind(SKU_3,SKU_4_3,SKU_4_4,SKU_5_1,
                   SKU_5_3,SKU_6_1,SKU_6_2,SKU_6_3,SKU_7_1,SKU_7_2,SKU_8_1,SKU_8_2,SKU_9)
SKU_Final$mg <- as.numeric(SKU_Final$mg)
SKU_Final$Strips <- as.numeric(SKU_Final$Strips)

lapply(SKU_Final, class)

#### Adding Letrozole Molecule's SKU Info ###################

SKU_Info <- final_df %>% distinct(SKU) ### Updated SKU Information with Letrozole

SKU_Letrozole <-SKU_Info %>% left_join(.,SKU_Final) %>% filter(is.na(mg))

SKU_Letrozole <- data.table(SKU_Letrozole)

`%notlike%`<- negate(`%like%`)
SKU_Letrozole_2 <- SKU_Letrozole[SKU_Letrozole$SKU %notlike% "MG", ]
SKU_Letrozole_1 <- SKU_Letrozole_1[,-c(2,3,4)]
SKU_Letrozole_1 <-  SKU_Letrozole_1 %>% mutate(mg = c(NA,NA,400,200,2.5,5,2.5,2.5,2.5,2.5,2.5,2.5,
                                                        5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,
                                                        2.5,5,2.5,2.5,2.5,2.5,2.5,2.5,5,2.5,2.5,2.5,
                                                        2.5,2.5,2.5,5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,
                                                        2.5,2.5,2.5,2.5,5,2.5,2.5,200,250,250,130,130),
                                               Strips = c(NA,NA,50,50,5,5,10,5,5,5,5,10,5,10,5,10,5,10,
                                                          5,10,5,10,10,10,5,5,5,5,10,15,5,5,5,10,10,5,10,
                                                          5,5,5,5,5,5,5,10,10,5,5, 5,5,5,10,10,10,5,30,30,60,60,60),
                                               Non_Oral = c(NA,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                            0,0,0,0,0,0,0,1,1,1,1,1)) %>% filter(!is.na(mg))
                                               
## Binding with the SKU_Final ##

SKU_Final <- rbind(SKU_Final,SKU_Letrozole_1)


### Merging the Updated SKU's data with the final_df

## Creating a List of Molecules that faced ban during 2007-13 period
Molecules_07_13 <- c("CISAPRIDE","SIMETHICONE + CISAPRIDE","SIBUTRAMINE","GATIFLOXACIN","AMBROXOL + GATIFLOXACIN",
                     "GATIFLOXACIN + ORNIDAZOLE","GATIFLOXACIN + METRONIDAZOLE","ROSIGLITAZONE","ROSIGLITAZONE + METFORMIN",
                     "ROSIGLITAZONE + GLIMEPIRIDE","GLICLAZIDE + METFORMIN + ROSIGLITAZONE","GLIBENCLAMIDE + METFORMIN + ROSIGLITAZONE",
                     "GLIMEPIRIDE + METFORMIN + ROSIGLITAZONE","TEGASEROD","LETROZOLE")

## df of Molecules tha were banned between the period 2007-13
Experiment_df <- final_df %>% left_join(.,SKU_Final) %>% 
                 filter(Sub.Group %in% Molecules_07_13)



### Converting ASU's to mg ##########

Experiment_df <- Experiment_df %>% mutate(ASU_mg = `Actual Sale Units`*mg*Strips)

### Revenue and Price per mg ########

Experiment_df <- Experiment_df %>% mutate(Revenue = `Actual Sale Units`*MRP)

Experiment_df <- Experiment_df %>% mutate(Price_per_mg = Revenue/ASU_mg)


### Adding Company's Domicile Status

Companies <- final_df %>%  filter(Sub.Group %in% Molecules_07_13) %>% distinct(Company)

Companies$Type <- ifelse(Companies$Company %in% Company_Info_Control$Company,Company_Info_Control$Company_Type,NA)

Companies$Type <- ifelse(is.na(Companies$Type),"Domestic",Companies$Type)

### Merging the Company's Info

Experiment_df <- Experiment_df %>% left_join(.,Companies)


### Creating Banned Date Variable  ###

Cisapride = c("CISAPRIDE","SIMETHICONE + CISAPRIDE")
Cisa_ban = as.yearmon("Feb 2011" ,format = "%B %Y")

GATIFLOXACIN = c("GATIFLOXACIN","AMBROXOL + GATIFLOXACIN","GATIFLOXACIN + METRONIDAZOLE",
                 "GATIFLOXACIN + ORNIDAZOLE")
Gatiflox_ban = as.yearmon("March 2011" ,format = "%B %Y")

ROSIGLITAZONE = c("ROSIGLITAZONE","GLIBENCLAMIDE + METFORMIN + ROSIGLITAZONE","GLICLAZIDE + METFORMIN + ROSIGLITAZONE",
                  "GLIMEPIRIDE + METFORMIN + ROSIGLITAZONE","ROSIGLITAZONE + GLIMEPIRIDE","ROSIGLITAZONE + METFORMIN")
Rosiglit_ban = as.yearmon("Nov 2010" ,format = "%B %Y")

Sibutramine_ban = as.yearmon("Feb 2011" ,format = "%B %Y")

Tega_ban =   as.yearmon("Mar 2011" ,format = "%B %Y")

Letrozole_Ban = as.yearmon("Oct 2011", format = "%B %Y")

Experiment_df <- Experiment_df %>% mutate(Ban_Date = case_when(Sub.Group %in% Cisapride ~ Cisa_ban, Sub.Group %in% GATIFLOXACIN ~ Gatiflox_ban,
                                                       Sub.Group %in% ROSIGLITAZONE ~ Rosiglit_ban, Sub.Group == "SIBUTRAMINE"~Sibutramine_ban,
                                                       Sub.Group == "TEGASEROD"~Tega_ban,Sub.Group == "LETROZOLE"~Letrozole_Ban))

Experiment_df$Ban_Date <- as.yearmon(Experiment_df$Ban_Date ,format = "%B %Y")


#### Now Creating Relevant Dummies #############3

## Treatment, Post and Domestic Dummies ##

Experiment_df <- Experiment_df %>% mutate(Treatment = 1, Post = ifelse(Date > Ban_Date,1,0),
                                          Domestic = ifelse(Type == "Domestic",1,0),Treatment_Molecule = NA) %>% 
                                   rename(Company_Type = Type) 

## Removing the observations corresponding to All India 
Experiment_df <- Experiment_df[!(Experiment_df$State=="ALL INDIA ONLY")]


### Banned_Drugs_1: Does not contain molecules that were banned for human use between 2008 and 2013
### Banned_Drugs_2: Added those molecules
### Banned_Drugs_3 : Added all the relevant info and all banned drugs for running regressions

write.csv(Experiment_df,"Data/Banned_Drugs_3.csv") 


### Merging both Control and Treatment Data #####

Control_Molecules_07_13 <- Control_Molecules_07_13 %>% mutate(ASU_mg = Actual.Sale.Units*mg*Strips,
                                                              Revenue = Actual.Sale.Units*MRP,
                                                              Price_per_mg = Revenue/ASU_mg,
                                                              Ban_Date = NA) %>%
                                                        rename("Actual Sale Units" = Actual.Sale.Units,
                                                               "Bonus Qty"  = Bonus.Qty)
## Do this for Date and Ban Date
Control_Molecules_07_13$Ban_Date <- as.yearmon(Control_Molecules_07_13$Ban_Date ,format = "%B %Y")


                            
final_df <- rbind(Experiment_df,Control_Molecules_07_13)



### Saving the merged file

write.csv(final_df,"Data/Banned_Controls_08_13.csv") 

### End of Do-File ##########################################################





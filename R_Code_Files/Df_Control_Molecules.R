############  Merging the CELECOXIB dataset  ##############################

#----------Importing Libraries-----------------#

lapply(c("XLConnect","GGally","tidyverse","visNetwork","magrittr","DiagrammeR","data.table",
         "plotly","intergraph","networkD3","optrees","disparityfilter","network","Matrix","igraph"
         ,"CINNA","ggplot2","poweRlaw","devtools","ForceAtlas2","remotes","netdiffuseR","lfe","stargazer","foreign","MASS","lubridate","ggthemes","readtext","zoo"), require,character.only = TRUE)
library(fs)
library(stringr)




`%notin%` <- Negate(`%in%`)

#---- Importing the Data------------------------#

files <- dir_ls(path = "F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Data_Control_Drugs_07_13/Sample/", glob = "*txt") # To see how many files are in the folder

files


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

print(object.size(final_df),units="Gb")  ## Size of the Dataframe ###


## Converting the Information Into Mg #################



SKU_Info <- final_df %>% distinct(SKU)

SKU_Info$SKU <- as.character(SKU_Info$SKU)

SKU_Info <- data.frame(SKU_Info)

SKU_A <- SKU_Info %>% separate(col= "SKU",into=c("1","2","3","4","5","6","7","8","9","10","11"),remove=F)

SKU_3 <- SKU_A %>% filter(is.na(`4`) & is.na(`5`) & is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`) & is.na(`10`) & is.na(`11`))
SKU_4 <- SKU_A %>% filter( !is.na(`4`) & is.na(`5`) & is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`) & is.na(`10`) & is.na(`11`))
SKU_5 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`) & is.na(`10`) & is.na(`11`))
SKU_6 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`) & is.na(`10`) & is.na(`11`))
SKU_7 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & is.na(`8`) & is.na(`9`) & is.na(`10`) & is.na(`11`))
SKU_8 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & is.na(`9`) & is.na(`10`) & is.na(`11`))
SKU_9 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & !is.na(`9`) & is.na(`10`) & is.na(`11`))
SKU_10 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & !is.na(`9`) & !is.na(`10`)&  is.na(`11`) )
SKU_11 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & !is.na(`9`)& !is.na(`10`) & !is.na(`11`))

##### SKU_3 Processed and Merged #######

## All values were missing:coded values from internet ##
SKU_3 <- SKU_3 %>% mutate(mg = c(NA,NA,NA,10,1000,NA,500,250,2,NA,
                                 1100,35,30,30,510,510,50,50,50,NA,
                                 NA,100,100,510,30,5,505,580,580,516,
                                 2,NA,NA,200,NA,NA,500,5,10,250,
                                 500,NA,1250,1250,501,NA,705,5,NA,500,
                                 1000,NA,700,700,NA,700,NA,400,NA,200,
                                 52.75,57.5,57.5,57.5,2.75,2.75,2.75,NA,20,NA,
                                 NA,NA,NA,NA,350,NA,NA,NA,20,NA,
                                 NA,200,NA,NA,500), Strips = `3`)
SKU_3 <- data.table(SKU_3)
SKU_3[c(8,9,30),c(14)] <- c(10,10,15)
SKU_3 <- SKU_3[!is.na(SKU_3$mg)]
SKU_3$Non_Oral <-  ifelse( SKU_3$`2` %notin% c("CAPSULE","TABLET"),1,0)
SKU_3 <- SKU_3[,-c(2:12)]  ## Concatenate this part


##### SKU_4 Processed and Merged ########


### SKU_4_1 : mg info was missing; coded values from internet.
SKU_4_1 <- SKU_4 %>% filter(`4`!="ML"& `3` %in% c("CAPSULE","TABLET")) %>% mutate(mg = c(NA,NA,NA,NA,NA,700,1000,1100,1100,1100,
                                                                                         10,550,35,50,70,30,30,NA,30,30,
                                                                                         30,30,30,30,50,70,70,70,70,70,
                                                                                         50,30,50,70,10,NA,50,50,50,50,
                                                                                         NA,NA,50,30,50,50,30,NA,160,150,
                                                                                         160,150,60,200,NA,250,NA,254,NA,150,
                                                                                         2,130,35,710,335,510,600,505,505,580,
                                                                                         580,15,60,580,580,516,501,501,501,501,
                                                                                         505,505,150,10,500,750,115,100,95,5,
                                                                                         1000,1000,NA,1000,100,1000,400,700,NA,NA,
                                                                                        NA,NA,800,NA,NA,700,NA,NA,500,NA,
                                                                                        15,20,200,NA,NA,NA,NA,NA,NA,500,225),Strips = `4`)

SKU_4_1 <- data.table(SKU_4_1)
SKU_4_1 <- SKU_4_1[!is.na(SKU_4_1$mg)]
SKU_4_1$Non_Oral <-  ifelse( SKU_4_1$`3` %notin% c("CAPSULE","TABLET"),1,0)
SKU_4_1 <- SKU_4_1[,-c(2:12)] ## Concatenate this part

### SKU_4_2 : Grams converted to mg's

SKU_4_2 <- SKU_4 %>% filter(`4`!="ML"& `3` %notin% c("CAPSULE","TABLET") & `4`=="GM") %>% mutate(mg = as.numeric(`3`)*1000,Strips = 1,Non_Oral=1)
SKU_4_2 <- SKU_4_2[,-c(2:12)] ## Concatenate this part

### SKU_4_3 : Grams converted to mg's

SKU_4_3 <- SKU_4 %>% filter(`4`!="ML"& `3` %notin% c("CAPSULE","TABLET") & `4`!="GM") %>%
           mutate(mg = case_when(`3`=="MG"~ as.numeric(`2`), `3`=="GM"~as.numeric(`2`)*1000),Strips=1,
                  Non_Oral = ifelse(`2` %in% c("TABLET") | `4` %in% c("TABLET"),0,1))

SKU_4_3[c(11),c(13,14)] <- c(c(510,10)) 
SKU_4_3 <- data.table(SKU_4_3)
SKU_4_3 <- SKU_4_3[!is.na(SKU_4_3$mg)]
SKU_4_3 <- SKU_4_3[,-c(2:12)] ## Concatenate this part 

##### SKU_5 Processed and Merged ########

## SKU_5_1 : Column 4 contains "TABLET" or "CAPSULE" String

SKU_5_1 <- SKU_5 %>% filter(`4` %in% c("TABLET","CAPSULE")) %>% mutate(mg = `2`,Strips = `5`,Non_Oral = 0)
SKU_5_1 <- SKU_5_1[,-c(2:12)] # Concatenate this part

## SKU_5_2 :Non-Oral in mg units

SKU_5_2 <- SKU_5 %>% filter(`4` %notin% c("TABLET","CAPSULE") & `3`=="MG") %>% mutate(mg = `2`,Strips = `5`,
                                                                                      Non_Oral = ifelse(`4` %in% c("OINTMENT","INJECTION","OPTICOPS","INFUSION"),1,0))

SKU_5_2[c(52),c(13,14)] <- c(c(250,1))
SKU_5_2 <- SKU_5_2[,-c(2:12)] # Concatenate this part

## SKU_5_3 : Non-Oral in grams

SKU_5_3 <- SKU_5 %>% filter(`4` %notin% c("TABLET","CAPSULE") & `3`!="MG" & `5`=="GM") %>% mutate(mg = as.numeric(`4`)*1000,Strips=1,Non_Oral=1)
SKU_5_3[c(106),c(13)] <- c(1200)
SKU_5_3 <- SKU_5_3[,-c(2:12)] # Concatenate this part


## SKU_5_4 : Mixed Cases 

SKU_5_4 <- SKU_5 %>% filter(`4` %notin% c("TABLET","CAPSULE") & `3`!="MG" & `5`%notin% c("GM","ML")) %>%
           mutate(mg =    c(NA,500,1000,1000,NA,3,50,20,500,100,190,30,1000,NA),
                  Strips = c(NA,1,1,1,NA,10,1,1,1,1,10,15,1,NA), 
                  Non_Oral = c(NA,1,1,1,1,0,0,0,0,0,0,0,1,1))
SKU_5_4 <- data.table(SKU_5_4)
SKU_5_4 <- SKU_5_4[!is.na(SKU_5_4$mg)]
SKU_5_4 <- SKU_5_4[,-c(2:12)] # Concatenate this part

##### SKU_6 Processed and Merged ########

## SKU_6_1 : Only Tablets & Capsules
SKU_6_1 <- SKU_6 %>% filter(`5` %in% c("TABLET","CAPSULE")) %>% mutate(Extra = as.numeric(`2`)) %>% 
                     mutate(mg = case_when( !is.na(Extra) & Extra == 0 ~ as.numeric(`3`)/10, !is.na(Extra) & Extra !=0~ as.numeric(`2`) + as.numeric(`3`),
                                            is.na(Extra)~ as.numeric(`3`)),Strips=`6`,Non_Oral = 0)

SKU_6_1 <- SKU_6_1[,-c(2:13)] # Concatenate this part

## SKU_6_2 : Only Tablets & Capsules
SKU_6_2 <- SKU_6 %>% filter(`5` %notin% c("TABLET","CAPSULE") & `4` %in% c("TABLET","CAPSULE")) %>%
  mutate(mg = `2`,Strips=`6`,Non_Oral = 0)

SKU_6_2 <- SKU_6_2[,-c(2:12)] # Concatenate this part


## SKU_6_3 : Non_Orals with mg info
SKU_6_3 <- SKU_6 %>% filter(`5` %notin% c("TABLET","CAPSULE") & `4` %notin% c("TABLET","CAPSULE") & `3`=="MG") %>%
  mutate(mg = `2`,Strips=`5`,Non_Oral = 1)

SKU_6_3 <- SKU_6_3[,-c(2:12)] # Concatenate this part


## SKU_6_4 : Mixed Cases 
SKU_6_4 <- SKU_6 %>% filter(`5` %notin% c("TABLET","CAPSULE") & `4` %notin% c("TABLET","CAPSULE") & `3`!="MG") %>%
  mutate(mg = case_when( `4`=="GM" & `2`==1 ~ 1500, `4`=="GM" & `2`!=1 ~ as.numeric(`3`)*1000,`4`=="MG" ~as.numeric(`3`),
                         `6`=="GM" ~as.numeric(`5`)*1000,`6`=="GM" &`4`=="3"~3500, `6`=="TABLET"~1100),
         Strips = case_when(`4`=="GM" & `2`==1 ~1,`4`=="GM" & `2`!=1~1,`4`=="MG"~as.numeric(`6`),
                            `6`=="GM" ~1, `6`=="GM" &`4`==3~1, `6`=="TABLET"~10),
         Non_Oral = ifelse(`6`=="TABLET",0,1))

SKU_6_4 <- data.table(SKU_6_4)
SKU_6_4 <- SKU_6_4[!is.na(SKU_6_4$mg)]
SKU_6_4 <- SKU_6_4[,-c(2:12)] # Concatenate this part

##### SKU_7 Processed and Merged ########

## SKU_7_1 : Only Tablets & Capsules
SKU_7_1 <- SKU_7 %>% filter(`6` %in% c("TABLET","CAPSULE")) %>% mutate("D" = as.numeric(`2`),"E" = as.numeric(`3`)) %>%
          mutate(mg = case_when(!is.na(D) & !is.na(E) &  D ==0 ~ 0.5 +  as.numeric(`4`) ,
                                !is.na(D) & !is.na(E) & D ==2 & `3` == 5 ~ 2.5  + as.numeric(`4`),
                                !is.na(D) & !is.na(E) &  D ==7 & `4` == 5 ~ 7.5 + as.numeric(`2`),
                                !is.na(D) & !is.na(E) &  E ==23  ~ 60 + 23.75,
                                !is.na(D) & !is.na(E)~ as.numeric(`2`) + as.numeric(`3`) + as.numeric(`4`),
                                SKU=="LEVOBA 500 MG 500 MG TABLET 10"~1000,
                                is.na(D) & !is.na(E) & `3` == 0 ~  as.numeric(`4`)/10,
                                is.na(D) & !is.na(E) & `3` == 2 & `4` == 5 ~ 2.5,
                                is.na(D) & !is.na(E) ~ as.numeric(`3`) + as.numeric(`4`),
                                (is.na(D)& is.na(E) | !is.na(D)& is.na(E)) ~ as.numeric(`4`)),
                                Strips = `7`, Non_Oral = 0)

SKU_7_1 <- SKU_7_1[,-c(2:14)] ## Concatenate this part

## SKU_7_2 : Tablets and Injections
SKU_7_2 <- SKU_7 %>% filter(`6` %notin% c("TABLET","CAPSULE") & `4`=="MG") %>% mutate("D" = as.numeric(`2`),"E"=as.numeric(`6`),"E1" = as.numeric(`7`)) %>%
          mutate(mg = case_when(!is.na(D)  &  D ==0 ~ as.numeric(`3`)/10,
                                !is.na(D) &  D ==2 & `3`==5 ~ 2.5,
                                !is.na(D) ~ D + as.numeric(`3`),
                                is.na(D) ~ as.numeric(`3`)),
                 Strips = ifelse(!is.na(E),E,E1),
                 Non_Oral = ifelse(`5` %in% c("TABLET","CAPSULE"),0,1))

SKU_7_2 <- SKU_7_2[,-c(2:15)] ## Concatente this part


## ## SKU_7_3 : Mix Cases
SKU_7_3 <- SKU_7 %>% filter(`6` %notin% c("TABLET","CAPSULE") & `4`!="MG" & `3`=="MG") %>%
                     mutate(mg = `2`,
                            Strips = ifelse(`7` %in% c("GM","ML"),`6`,as.numeric(`5`)*as.numeric(`7`)),
                            Non_Oral = ifelse(`4` %in% c("TABLET","CAPSULE"),0,1))
SKU_7_3 <- SKU_7_3[,-c(2:12)]  ## Concatenate this part


### SKU_7_4 : Mix Case
SKU_7_4 <- SKU_7 %>% filter(`6` %notin% c("TABLET","CAPSULE") & `4`!="MG" & `3`!="MG" & `5`=="MG") %>%
                     mutate(mg = c(30,516,517),Strips = c(3,10,10),Non_Oral = c(0,0,0))

SKU_7_4 <- SKU_7_4[,-c(2:12)]  ## Concatenate this part

### SKU_7_5 : Non-Oral info in ml
SKU_7_5 <- SKU_7 %>% filter(`6` %notin% c("TABLET","CAPSULE") & `4`!="MG" & `3`!="MG" & `5`!="MG")

# Do not concatenate this part.

##### SKU_8 Processed and Merged ########

## SKU_8_1 : TABLETS AND CAPSULES
SKU_8_1 <- SKU_8 %>% filter(`6` %in% c("TABLET","CAPSULE")) %>% mutate("D" = as.numeric(`2`),"E"=as.numeric(`3`),"E1" = as.numeric(`4`)) %>%
                     mutate(mg = case_when(!is.na(D) & !is.na(E) & !is.na(E1) ~ D+E+E1,
                                           is.na(D) & !is.na(E) & !is.na(E1) & E == 0 ~ E1/10,
                                           is.na(D) & !is.na(E) & !is.na(E1) ~ E + E1,
                                           is.na(D) & is.na(E) & !is.na(E1) ~ 1000),
                            Strips = ifelse(`7`==10,`7`,`8`),
                            Non_Oral = 0)
SKU_8_1 <- SKU_8_1[,-c(2:15)]  ## Concatenate this part

## SKU_8_2 : Injections and Non-Orals w/info in mg's
SKU_8_2 <- SKU_8 %>% filter(`6` %notin% c("TABLET","CAPSULE") & `5`=="MG") %>% mutate("D" = as.numeric(`2`),"E"=as.numeric(`3`),"E1" = as.numeric(`4`)) %>%
  mutate(mg = case_when(!is.na(D) & !is.na(E) & !is.na(E1) & D == 0 & E == 64 ~ E/100 + E1,
                        !is.na(D) & !is.na(E) & !is.na(E1) & D == 0 ~ E/10 + E1,
                        !is.na(D) & !is.na(E) & !is.na(E1) & D == 1 ~ 126.25,
                        !is.na(D) & !is.na(E) & !is.na(E1) ~ D+E+E1,
                        E == 0 ~ 0.4,
                        is.na(D) & !is.na(E) & !is.na(E1) ~ E + E1,
                        is.na(D) & is.na(E) & !is.na(E1) ~E1),
         Strips = `7`,
         Non_Oral = 1)
SKU_8_2 <- SKU_8_2[,-c(2:15)]  ## Concatenate this part


## SKU_8_3 : TABLETS AND CAPSULES
SKU_8_3 <- SKU_8 %>% filter(`6` %notin% c("TABLET","CAPSULE") & `5`!="MG" & `6` == "MG") %>%  mutate("D" = as.numeric(`2`),"D1"=as.numeric(`3`),"E" = as.numeric(`4`),"E1" = as.numeric(`5`)) %>%
   mutate(mg = case_when(!is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & D == 0  ~ D1/100 + E +  E1,
                         !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & D == 2 & D1 == 5  ~ 2.5 +0.025,
                         !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & E == 2 & E1 == 5  ~ 2.5 +D  + D1,
                         !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & E == 7 & E1 == 5  ~ 7.5 +D  + D1,
                         !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1)  ~ D + D1 + E +  E1,
                         !is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1) ~ E + E1,
                         is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & D1 == 0~ E/10 + E1,
                         is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & E == 0~ E1/10 + D1,
                         is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & E == 7~ E + E1/10 + D1,
                         is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & D1 %in% c(2,7) & E == 5~ D1 + E/10 + E1,
                         is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1)  & E == 62~ D1 + 62.5,
                         is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) ~ D1 + E + E1,
                         is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1) & E == 0 ~ E1/10,
                         is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1) & E == 7 ~ 7 + E1/10,
                         is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1)  ~ E + E1,
                         is.na(D) & is.na(D1) & is.na(E) & !is.na(E1)  ~ E1),
          Strips = `8`,
          Non_Oral = 0)

SKU_8_3 <- SKU_8_3[,-c(2:16)] ## Concatenate this part

## SKU_8_4 : Mix Cases

SKU_8_4 <- SKU_8 %>% filter(`6` %notin% c("TABLET","CAPSULE") & `5`!="MG" & `6` != "MG" & `4`=="MG") %>%
                    mutate(mg = c(500,0.3,0.2,200,0.9,0.9,1,5,0.5,0.3),
                           Strips = c(30,100,100,60,5,5,5,5,5,5),
                           Non_Oral = ifelse(`5`=="TABLET",0,1))  

SKU_8_4 <- SKU_8_4[,-c(2:12)]   ### Concatenate this part                   
  
## SKU_8_5 : Mix Cases

SKU_8_5 <- SKU_8 %>% filter(`6` %notin% c("TABLET","CAPSULE") & `5`!="MG" & `6` != "MG" & `4`!="MG")
  
### Do not Concatenate this part

##### SKU_9 Processed and Merged ########

## SKU_9_1 : Tablets and Capsules 
SKU_9_1 <- SKU_9 %>% filter(`7` %in% c("TABLET","CAPSULE")) %>% mutate("D" = as.numeric(`2`),"D1"=as.numeric(`3`),"E" = as.numeric(`4`),"E1" = as.numeric(`5`)) %>%
  mutate(mg = case_when(!is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & D == 0  ~ D1/100 + E +  E1,
                        !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & E == 7 & E1 == 5  ~ 7.5 +D  + D1,
                        !is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1) ~ E + E1,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & D1 == 0~ E/10 + E1,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & E == 0~ E1/10 + D1,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & E == 7~ E + E1/10 + D1,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & D1 %in% c(2,7) & E == 5~ D1 + E/10 + E1,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1)  & E == 37~ D1 + 37.5,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) ~ D1 + E + E1,
                        is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1)  ~ E + E1,
                        is.na(D) & is.na(D1) & is.na(E) & !is.na(E1)  ~ E1),
        Strips = ifelse(`9` %notin% c("S","X"),`9`,ifelse(`9`=="X",as.numeric(`8`)*as.numeric(`10`),`8`)),
        Non_Oral = 0)

SKU_9_1 <- SKU_9_1[,-c(2:16)] ## Concatenate this part



## SKU_9_2 : One Tablet
SKU_9_2 <- SKU_9 %>% filter(`7` %notin% c("TABLET","CAPSULE") & `4`=="MG") %>%
                     mutate(mg = 1000,Strips=30 ,Non_Oral = 0)
SKU_9_2 <- SKU_9_2[,-c(2:12)] ## Concatenate this part

## SKU_9_3: Mix cases 
SKU_9_3 <- SKU_9 %>% filter(`7` %notin% c("TABLET","CAPSULE") & `5`=="MG") %>%
                     mutate(mg = case_when(`1`=="OCUPOL"~ as.numeric(`3`) + as.numeric(`4`) + 0.67,
                                           `3` !="LILLY" ~as.numeric(`3`) + as.numeric(`4`),
                                            `3`=="LILLY"~ 60),
                            Strips = ifelse(`8`=="X",as.numeric(`7`)*as.numeric(`9`),`8`),
                            Non_Oral = ifelse(`6` %in% c("TABLET","CAPSULE"),0,1))

SKU_9_3 <- SKU_9_3[,-c(2:12)] ## Concatenate this part

## SKU_9_4 : Mix Cases
SKU_9_4 <- SKU_9 %>% filter(`7` %notin% c("TABLET","CAPSULE") & `6`=="MG") %>%
                     mutate(mg = ifelse(`2`==0,0.322,ifelse(`3` %in% c(400,900),
                                                            as.numeric(`4`) + as.numeric(`5`),ifelse(`4`=="BRAWN",50,as.numeric(`4`) + as.numeric(`5`)))),
                            Strips = `8`,
                            Non_Oral = ifelse(`7` %notin% c("TABLET","CAPSULE"),1,0))

SKU_9_4 <- SKU_9_4[,-c(2:12)] ## Concatenate this part


## SKU_9_5 : Mixed Cases 

SKU_9_5 <- SKU_9 %>% filter(`7` %notin% c("TABLET","CAPSULE") & `7`=="MG") %>% mutate("D" = as.numeric(`3`),"D1"=as.numeric(`4`),"E" = as.numeric(`5`),"E1" = as.numeric(`6`)) %>%
  mutate(mg = case_when(!is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & D == 5 & D1 == 0~52.75, #Special Case
                        !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & D == 0  ~ D1/100 + E +  E1,
                        !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & (E %in% c(0,2,7) & E1 < 10) ~ E1/10 + E +D  + D1,
                        !is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1) ~ E + E1,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) & (D1 %in% c(0,2,7) & E < 10 )~ D1 + E/10 + E1,
                        is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & (E %in% c(0,2,7)& E1 < 10) ~ E1/10 + E  + D1,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) ~ D1 + E + E1,
                        is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1) & E %in% c(0,2,7) ~ E + E1/10,
                        is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1)   ~ E + E1,
                        is.na(D) & is.na(D1) & is.na(E) & !is.na(E1)  ~ E1),
         Strips = `9`,
         Non_Oral = ifelse(`7` %notin% c("TABLET","CAPSULE"),1,0))

SKU_9_5 <- SKU_9_5[,-c(2:16)] ### Concatenate this part


## SKU_9_6 : MG Info not given, units info given in percentage format
SKU_9_6 <- SKU_9 %>% filter(`7` %notin% c("TABLET","CAPSULE") & (`4`!="MG" & `5` != "MG" & `6`!="MG" & `7` !="MG"))
# Do not concatenate this part


##### SKU_10 Processed and Merged ########

## SKU_10_1 : MG Info in 6th Column

SKU_10_1 <- SKU_10 %>% filter(`6`=="MG") %>%
                       mutate(mg = c(160,200,200,700),Strips = c(100,1000,30,100),Non_Oral=c(0,0,1,0))
SKU_10_1 <- SKU_10_1[,-c(2:12)] #3 Concatenate this part


## SKU_10_2 : MG Info in 7th column

SKU_10_2 <- SKU_10 %>% filter(`7`=="MG") %>% mutate("D" = as.numeric(`3`),"D1"=as.numeric(`4`),"E" = as.numeric(`5`),"E1" = as.numeric(`6`)) %>%
  mutate(mg = case_when(!is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) &( E %in% c(0,2,7) & E1 < 10) & D %in% c(0) ~ E1/10 + E +D  + D1/10,
                        !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & D == 0  ~ D1/100 + E +  E1,
                        !is.na(D) & !is.na(D1) &!is.na(E) & !is.na(E1) & E %in% c(2,7) & E1==5  ~ E1/10 + E +D  + D1,
                        !is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) ~ D + D1 + E + E1,
                        is.na(D) & !is.na(D1) & !is.na(E) & !is.na(E1) ~ D1 + E + E1,
                        is.na(D) & is.na(D1) & !is.na(E) & !is.na(E1)   ~ E + E1),
         Strips = ifelse(`9` %in% c(5,100),`9`,`10`),
         Non_Oral = ifelse(`7` %notin% c("TABLET","CAPSULE"),1,0))

SKU_10_2 <- SKU_10_2[,-c(2:16)] # Concatenate this part

## SKU_10_3 : MG Info in 8th column

SKU_10_3 <- SKU_10 %>% filter(`8`=="MG") %>%
                       mutate(mg = c(50,509.5,1008.5,1008.5,1008.5,1008.5,516,517,1008.5,1009.5),
                              Strips = `10`,
                              Non_Oral = 0)
SKU_10_3 <- SKU_10_3[,-c(2:12)] # Concatenate this part

## SKU_10_4 : Injections and Suryps with quantity info in Percentahes rather than MG

SKU_10_4 <- SKU_10 %>% filter(`8` != "MG" & `6` != "MG" & `7` != "MG" )
## DO not Concatenate this part


##### SKU_11 Processed and Merged ########

SKU_11_1 <- SKU_11 %>% mutate(mg = c(NA,NA,260,508.5,509.5,1008.5,1009.5,508.5,509.5,508.5,509.5,508.5,509.5,508.5,NA,NA,NA,NA,501.3,502.3,501.2,502.2),
                             Strips = ifelse(`10` %notin% c("SR","TABLET","ER"),`10`,`11`),
                             Non_Oral = ifelse( `9` %in% c("TABLET","CAPSULE") | `10` %in% c("TABLET","CAPSULE"),0,1))

SKU_11_1 <- data.table(SKU_11_1)
SKU_11_1 <- SKU_11_1[!is.na(SKU_11_1$mg)]
SKU_11_1 <- SKU_11_1[,-c(2:12)]  ### Concatenate this part 


### Final SKU List with Mg, Strips and Non_Oral Dummy

SKU_Final <- rbind(SKU_3,SKU_4_1,SKU_4_2,SKU_4_3,SKU_5_1,SKU_5_2,SKU_5_3,SKU_5_4,SKU_6_1,SKU_6_2,
                   SKU_6_3,SKU_6_4,SKU_7_1,SKU_7_2,SKU_7_3,SKU_7_4,SKU_8_1,SKU_8_2,SKU_8_3,SKU_8_4,
                   SKU_9_1,SKU_9_2,SKU_9_3,SKU_9_4,SKU_9_5,SKU_10_1,SKU_10_2,SKU_10_3,SKU_11_1)

SKU_Final <- data.table(SKU_Final)

## MG info neither in data nor on internet, so dropping these variables
Drop_List = c("CORAL F (MANKIND) TABLET 10","CHLORAMPHENICOL (WIN-MEDICARE) CAPSULE 100")
SKU_Final <- SKU_Final[!(SKU_Final$SKU %in% Drop_List)]


## MG info borrowed from internet 
List = c("NORFLOX TZ LB (NEW) TABLET 10","PENTOZA D FORTE TABLET 10","PROZOL D (INGA) TABLET 10","GLYCOMIN (EXOTIC LAB) TABLET 10",
         "P FLOX OZ TABLET 10","REKCIN KID 3 % TABLET 10","ZORYL M FORTE TABLET 10","NORLOX M KID TABLET 10","VOZUCA M ACTIV TABLET 60",
         "DIAPRIDE M3 FORTE TABLET 10","DIAPRIDE M4 FORTE TABLET 10","ZORYL MP-2 TABLET 15","NAUSIFAR MPS (WALTER) TABLET 10","SPROT N NEW TABLET 10",
         "JONFLOX O (JOHNS) TABLET 10","DOMITAB P (INTEL) TABLET 10","ZEROFAT R (MANKIND) TABLET 10","TERRAMYCIN S F CAPSULE 8","NORZEE T Z TABLET 10",
         "NOVAFLOX TZ (WYETH) TABLET 10","NORFLOX TZ NEW TABLET 6","PAZO D (ALGEN HEALTHCARE) TABLET 10","PAZO DSR (ALGEN HEALTHCARE) CAPSULE 10",
         "PYLOREX DSR (BSP PHARMACEUTICALS) CAPSULE 10","LIVOMAX (ENZO BIOPHARMAL) TABLET 10","LIVOQUIN (EXOTIC LAB) TABLET 10","SPARNOLE (EXOTIC LAB) TABLET 10")

Value = c(1000,30,20,5,200,333,1001,100,500.2,1003,1004,517,130,500,700,335,20,500,1000,500,1000,50,70,50,500,250,100)

for(i in (1:length(List))){
SKU_Final$mg[SKU_Final$SKU == List[i]] = Value[i]
}

SKU_Final$mg <- as.numeric(SKU_Final$mg)

## Adding Strips Info for some of the Molecules

List = c("ORNOF OZ TABLET","MAKIN 250 MG INJECTION 2ML","GLICLAN 80 MG TABLET 10 S","HYPHORAL 200 MG TABLET 10 S","MOXIFLOX 400 MG TABLET 5 TAB",
         "RECLIMET MR 30/500 MG TABLET MR")
Value = c(1,1,10,10,5,1)

for(i in (1:length(List))){
  SKU_Final$Strips[SKU_Final$SKU == List[i]] = Value[i]
}

SKU_Final$Strips <- as.numeric(SKU_Final$Strips)

lapply(SKU_Final, class)

# Note: Original SKU have 10,058 items but after cleaning we are left with only 8822 items;88% of the items ###

### SKU Data Ready to merge with the main control dataset ##

### Merging the Updated SKU's data with the final_df

Experiment_df <- merge(final_df,SKU_Final,all.x = T,all.y = T,by="SKU")


##########################################################

# :::                                  ::: #
#     Now Adding MNC Info of Companies
# :::                                 :::#

Company_Info <- Experiment_df %>% distinct(Company)

Company_Info$Type <- ifelse(Company_Info$Company %in% Companies$Company,Companies$Company_Type,NA)

Matched_Companies <- merge(Company_Info,Companies,all.x = T,by="Company")

Unmatched_Companies <- Matched_Companies[is.na(Matched_Companies$Company_Type)]


Company_Status <- c("D","D","D","D","D","D","D","D","D","D","D","D","D","D","M","D","D","D","D","D","M","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","M","D","M","D","D","D","D","D","D","D","D","M","D","D",
                    "D","D","D","D","D","D","D","D","D","D","D","M","D","M","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D",
                    "D","D","D","D","D","D","D","M","D","D","D","D","D","D","D","D","D","D","M","D","D","D","D","D","D","M","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D",
                    "D","M","M","D","D","D","D","D","D","D","D","D","D","D","D","M","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D",
                    "D","D","D","D","D","D","D","D","D","D","D","D","D","M","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","M","D","M","D","D","D","D","M",
                    "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","M","D",
                    "D","D","D","D","D","D","D","D","D","D","D","D","M","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","M","D","D","D",
                    "D","D","D","D","D","D","D","D","D","D","D","D","M","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D",
                    "D","D","D","D")
Unmatched_Companies$Type <- Company_Status
Companies_New <- Unmatched_Companies %>% mutate(Company_Type = case_when(Type == "D"~"Domestic", Type=="M"~"MNC"))
Matched_Companies <- Matched_Companies[!is.na(Matched_Companies$Type)]
Final_Company <- rbind(Matched_Companies,Companies_New)
Final_Company <- Final_Company[,-c(2)]

### Company's Domicile Status Collated ########

### Merging Company_Info with the original data #########

## Creating Domestic Dummy Variable
Experiment_df <- Experiment_df %>% left_join(.,Company_Info_Control) %>% 
                  mutate(Domestic = ifelse(Company_Type == "Domestic",1,0))
                 

## Creating Various Dummy Variables 

# 1> Treatment and Post Dummy = 0; since this is a control dataset

Experiment_df <- Experiment_df %>% mutate(Treatment = 0,Post = 0)

# 2> Comparison Group: i.e. Matching the molecule to the banned molecule for which it is acting as control

## This function takes a particular Treatment Molecule and returns all the names 
# of the control molecules corresponding to it in the directory as specified by the path

molecule_category <- function(path){
  data = data.frame(list.files(path))
  stopwords <- c(".txt")
  word <-  word(path, -2, sep = '/')
  df_word <- gsub(paste0(stopwords,collapse = "|"),"",data[,c(1)])
  return(df_word)
}

## This function prepares a dataframe by collating names of all control molecules
# for all treatment groups using "molecule_category :function"

control_mol_assign <- function(path){
  word <- word(path,-2,sep = '/')
  data <- as.tibble(1) %>%  rowwise() %>% mutate(Treatment_Molecule = word,
                               Control_Molecules = list(molecule_category(path))) 
  return(data) # result is a df where one column documents the name of treatment group and 
              # second column stores the name of all control molecules corresponding to it in the form of the list
}

## Creating a path vector: specifying where the control molecules for each treated molecule is stored

Path <- c(c("F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Data_Control_Drugs_07_13/LETROZOLE/  ",
            "F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Data_Control_Drugs_07_13/CISAPRIDE/  ",
            "F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Data_Control_Drugs_07_13/GATIFLOXACIN/  ",
            "F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Data_Control_Drugs_07_13/ROSIGLITAZONE/  ",
            "F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Data_Control_Drugs_07_13/SIBUTRAMINE/  ",
            "F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Data_Control_Drugs_07_13/TEGASEROD/  "))

## Finally Creating the required dataset using the above function

## In the wide format
Treat_Control_df <- map_dfr(Path,control_mol_assign)

## In the long format
Treat_Control_df <- Treat_Control_df %>% unnest_longer(Control_Molecules) %>%
                    rename(Sub.Group = Control_Molecules) 
Treat_Control_df <- Treat_Control_df[,-c(1)]

## Merging with the main data
Experiment_df <- Experiment_df %>% left_join(., Treat_Control_df)

## Removing the observations corresponding to All India 
Experiment_df <- Experiment_df[!(Experiment_df$State=="ALL INDIA ONLY")]

## Saving the CSV File
write.csv(Experiment_df,"Control_Molecules_07_13.csv")



## Concatinating the Letrizole Left-out Controls Molecules Info ##


### First adding the SKU Info ####

SKU_Info <- final_df %>% distinct(SKU)

SKU_Info$SKU <- as.character(SKU_Info$SKU)

SKU_Info <- data.frame(SKU_Info)

SKU_Info$mg <- c(1,1,1,1,1,1,1,1,1,1,
                 1,1,1,1,1,1,1,1,1,1,
                 1,50,150,50,50,50,50,50,50,50,
                 50,50,50,50,50,50,50,250,250,250,250)

SKU_Info$Strips <- c(10,5,14,14,28,10,10,10,10,10,
                     10,10,10,10,10,5,10,10,14,10,
                     10,10,14,7,10,10,10,30,10,10,
                     10,10,30,10,10,10,10,10,10,30,10)

final_df <- merge(final_df,SKU_Info,all.x = T,all.y = T,by="SKU")

### Now Adding the MNC Info ####

Company_Info <- final_df %>% distinct(Company)

## For this step import the Company database from control Molecules
Company_Info$Type <- ifelse(Company_Info$Company %in% Company_Info_Control$Company,Company_Info_Control$Company_Type,NA)
Company_Info$Type[Company_Info$Company=="UNIMARK REMEDIES LTD"] <- "Domestic"
colnames(Company_Info) <- c("Company","Company_Type")

final_df <- final_df %>% left_join(.,Company_Info) %>% 
  mutate(Domestic = ifelse(Company_Type == "Domestic",1,0))


### Now adding the other relevant variables ####

# 1> Treatment and Post Dummy = 0; since this is a control dataset
# 2> Variable Treatement Molecule
# 3> Non_Oral & Ban_Date

final_df <- final_df %>% mutate(Treatment = 0,Post = 0,
                                Treatment_Molecule = "LETROZOLE",
                                Ban_Date = NA,Non_Oral = 1,
                                ASU_mg = `Actual Sale Units`*mg*Strips,
                                Revenue = `Actual Sale Units`*MRP,
                                Price_per_mg = Revenue/ASU_mg)

### Merging with the Banned_Control_08_13 data (i.e. Banned Drugs + Control Data)


final_df <- final_df %>% rename("Actual.Sale.Units" = `Actual Sale Units`,
                                "Bonus.Qty"  = `Bonus Qty`)

final_df <- final_df[!(final_df$State=="ALL INDIA ONLY")]

final_df <- read.csv("Data/Letrizole_Control.csv",row.names = 1)

final_df$Date <- as.yearmon(final_df$Date,format = "%B %Y")


final_df <- final_df %>% rename("Actual Sale Units" = Actual.Sale.Units,
                                "Bonus Qty"  = Bonus.Qty)

Banned_Controls_08_13 <- fread("C:/Users/Ashish/Desktop/Project_Banned_Drugs/Project_Banned_Drugs/Data/Banned_Controls_08_13.csv",drop=1)
Banned_Controls_08_13$Date <- as.yearmon(Banned_Controls_08_13$Date,format = "%B %Y")
Banned_Controls_08_13 <- rbind(Banned_Controls_08_13,final_df)
Banned_Controls_08_13 <- Banned_Controls_08_13 %>% filter(Sub.Group != "TEGASEROD")

  
  ### Saving the merged file

# Dataset with banned drugs and broader set of controls.
# In this file Tegaserod- A banned molecule has been left-out becoz we don't have any corresponding controls for it.

# fwrite(Banned_Controls_08_13, "Data/Banned_and_Broad_Controls_08_13.csv")
write.csv(Banned_Controls_08_13,"Data/Banned_and_Broad_Controls_08_13.csv")

Banned_Controls_08_13 %>% filter(Treatment==1) %>% distinct(Sub.Group)



# Constructing a data set with a narrower set of controls.
# In this file following banned drugs were left out due to the unavailibility of control drugs for them:
# 1>  GATIFLOXACIN + METRONIDAZOLE
# 2>  GATIFLOXACIN
# 3>  SIBUTRAMINE


Exclude_List <- c("GATIFLOXACIN + METRONIDAZOLE","GATIFLOXACIN","SIBUTRAMINE")

Banned_Controls_08_13 <- Banned_Controls_08_13 %>% 
                         filter(Sub.Group %notin% Exclude_List)

## Now Creating a List of Narrower set of controls
# 1> This function will collate a list of all the molecules that

molecule_category <- function(path){
  data = data.frame(list.files(path))
  stopwords <- c(".txt")
  word <-  word(path, -2, sep = '/')
  df_word <- gsub(paste0(stopwords,collapse = "|"),"",data[,c(1)])
  df_word <- data.frame(df_word)
  return(df_word)
}

## Creating a path vector: specifying where the control molecules for each treated molecule is stored

Path <- c(c("Data/Narrower_Controls/Cisapride/",
            "Data/Narrower_Controls/Gatifloxacin/",
            "Data/Narrower_Controls/Letrozole/",
            "Data/Narrower_Controls/Rosiglitazone/"))


## Iteratively running the Function

Narrow_Controls  <- map_dfr(Path,molecule_category) ## contains only 41 controls

## Only retaining the narrower controls with the Treatment from our main specification

Narrow_Controls_08_13 <- Banned_Controls_08_13 %>% 
                        subset(Sub.Group %in% Narrow_Controls$df_word)


Banned_Narrow <- Banned_Controls_08_13 %>% filter(Treatment==1)


## Merging the above two df's

Banned_Narrow_Controls <- rbind(Banned_Narrow,Narrow_Controls_08_13)
# fwrite(Banned_Narrow_Controls, "Data/Banned_and_Narrow_Controls_08_13.csv", quote=FALSE, sep=",")

write.csv(Banned_Narrow_Controls,"Data/Banned_and_Narrow_Controls_08_13.csv")


#### End of Do-File ###################################






#-----------------------------------------------------------------------------------------------------------------------------#
#----------------------------------    Importing Libraries -------------------------------------------------------------------#
lapply(c("tidyverse","kableExtra","here","knitr","ggthemes","lfe","gt","did","xaringan","miceadds","sandwich","lmtest","patchwork",
         "bacondecomp","multcomp","fastDummies","magrittr","MCPanel","gganimate","gifski","zoo","remotes","data.table","fs"), require,character.only = TRUE)
library(readxl)
`%notin%` <- Negate(`%in%`)


#--------------------------- Loading the Data -------------------------------------------------------------------------------#
df <- read_excel("azithro_jan15_jun20.xlsx",sheet = "Sheet1")
df<-  antimalaria_jan15_jun20
df <- data.table(df)

df$`Month-Year` <- as.yearmon(df$`Month-Year`,format = "%b-%y")
df$DATE <- as.yearmon("March 2020", format = "%B %Y")


#-------------------------- Converting the Sales info to mg totals ----------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------#

##-------------------------- Incorporating the per mg dosage of every SKU---------------------------------------#

#---------------------------------------------------------------------------------------------------------------#

SKU <- df %>% group_by(SKU) %>%  dplyr::select(`DRUG TYPE`,`DRUG CATEGORY`,STRENGTH,PACK) 
SKU <- unique(SKU ,by = "SKU") ### Retaining only unique SKU's
SKU <- data.table(SKU)


#------------------------------------- First Category : SOLIDS-------------------------------------------#


SKU_Solids <- SKU[`DRUG CATEGORY`=="SOLIDS"]
SKU_Solids_1 <- SKU_Solids
# 1.1> Excluding all the extra symbols and names other than numbers and mg
# SKU_Solids_1 <- SKU_Solids[- grep("%|MIU|GM|MCG|MD|IU|AU|iu|W|B|ILLION", SKU_Solids$STRENGTH),] # Those strings removed containing the given patterns
# SKU_Solids_1 <- SKU_Solids[- grep("GM|ML", SKU_Solids$PACK),] # Also exclude gels and tubes(GM and ML info in Packs column). Recover afterwards
SKU_Solids_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids$STRENGTH))
SKU_Solids_1$Raw_No <- gsub("\\MG|M|S","",SKU_Solids_1$Raw_No)
SKU_Solids_1 <- SKU_Solids_1[!which(SKU_Solids_1$Raw_No == ""),] ## Removed all those SKU's for which we do not have data on the mg composition
SKU_Solids_1 <- SKU_Solids_1[!is.na(SKU_Solids_1$Raw_No),] ## Removed all those SKU's for which mg  = NA
SKU_Solids_1$MG <- unlist(apply(SKU_Solids_1[,6], 1, function(x) eval(parse(text=x)))) # Concatenate this part
SKU_Solids_1$Strips <- gsub("\\TAB|S","",SKU_Solids_1$PACK)
SKU_Solids_1$Strips <- as.numeric(as.character(SKU_Solids_1$Strips)) ## Concatenate this part


#-------- -----------------------------Cleaning for Solid SKU Finished-------------------------------------#

#------------------------------------- Second Category : Injections-------------------------------------------#


SKU_Inject <- SKU[`DRUG CATEGORY` =="INJECTABLES"]
SKU_Inject_1 <- SKU_Inject

#--1> Retaining only those which contain mg info and are not in percentages ------------
# SKU_Inject_1 <- SKU_Inject[- grep("%|MIU|IU|AU|iu|GM", SKU_Inject$STRENGTH),] ## Leaving percent here but include it, and also gram cases in strenth column
SKU_Inject_1  <- SKU_Inject_1[!is.na(SKU_Inject_1$STRENGTH),]
SKU_Inject_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Inject_1$STRENGTH))
SKU_Inject_1$Raw_No <- gsub("\\MG|GM|IT|MCG|G|Y|K|,","",SKU_Inject_1$Raw_No)
SKU_Inject_1$Raw_No <- gsub("\\ML","*1000",SKU_Inject_1$Raw_No)
SKU_Inject_1$MG <- unlist(apply(SKU_Inject_1[,6], 1, function(x) eval(parse(text=x))))
# SKU_Inject_1[grep("GM", SKU_Inject_1$STRENGTH), c("MG")] <- SKU_Inject_1[grep("GM", SKU_Inject_1$STRENGTH), c("MG")]*1000
SKU_Inject_1$Strips <- 1  
# SKU_Inject_1 [which(SKU_Inject_1 $PACK == "1 GM"), c("Strips")] <- 1000
# SKU_Inject_1 [which(SKU_Inject_1 $PACK == "1.5 GM"), c("Strips")] <- 1500      ## Concatenate this part


#-------- Cleaning for Injection SKU Finished-------------------------------------#

#------------------------------------- Third Category : Liquids -------------------------------------------#

SKU_Liquids <- SKU[`DRUG CATEGORY`=="LIQUIDS"]
SKU_Liquids_1 <- SKU_Liquids

#--1> Retaining only those which contain mg info and are not in percentages ------------
# SKU_Liquids_1 <- SKU_Liquids[- grep("%|MIU|IU|AU|iu|GM", SKU_Liquids$STRENGTH),] ## Leaving percent here but include it, and also gram cases in strenth column
SKU_Liquids_1  <- SKU_Liquids_1[!is.na(SKU_Liquids_1$STRENGTH),]
SKU_Liquids_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Liquids_1$STRENGTH))
SKU_Liquids_1$Raw_No <- gsub("MG","",SKU_Liquids_1$Raw_No)
SKU_Liquids_1$Raw_No <- gsub("%","*0.05",SKU_Liquids_1$Raw_No)
SKU_Liquids_1$MG <- unlist(apply(SKU_Liquids_1[,6], 1, function(x) eval(parse(text=x))))

SKU_Liquids_1$Strips <- 1

#------------------------------------ Liquids Part Completed ---------------------------------------------#

#------------------------------------- Third Category : Others -------------------------------------------#

SKU_Other <- SKU[`DRUG CATEGORY` %notin% c("SOLIDS","INJECTABLES","LIQUIDS")]
SKU_Other <- SKU_Other[-c(2),]
SKU_Other$Raw_No <- NA
SKU_Other$MG <- 300
SKU_Other$Strips <- 1


#-------- Combining all the SKU Information ----------------------------------#
#---- Out of 478 SKU's, we were able to retain info on 369; i.e.: 77%

SKU_F <- rbind(SKU_Solids_1,SKU_Inject_1,SKU_Other)
SKU_F <- SKU_F %>% dplyr::select(SKU,MG, Strips)



#----------- Merging with the main dataset-----------------------------#
df <- df %>% left_join(.,SKU_F)
df$ASU_mg <- df$`Sales Unit`*df$MG*df$Strips
df_copy <- df
# df <- df[!is.na(ASU_mg),]




#-------- Regions ----------------------------------------------------#

Regions <-  df %>% distinct(STATE) 

Regions <- Regions %>% mutate(Population_Density = case_when(STATE=="CHATTISGARH" ~ 189,
                                                             STATE=="MADHYA PRADESH" ~236,
                                                             STATE=="MUMBAI CITY"~19652,
                                                             STATE=="N MAHARASHTRA"~266.42,
                                                             STATE=="NORTH GUJARAT"~418.39,
                                                             STATE== "OUTER MUMBAI"~20979.74,
                                                             STATE== "S MAHARASHTRA"~308.08,
                                                             STATE=="SAURASHTRA"~1641.8,
                                                             STATE=="SOUTH GUJARAT"~668.74,
                                                             STATE=="VIDARBHA"~240,
                                                             STATE=="KERALA"~860,
                                                             STATE=="NORTH AP"~308,
                                                             STATE=="SOUTH AP"~308,
                                                             STATE=="NORTH KARNATAKA"~280,
                                                             STATE=="SOUTH KARNATAKA"~370,
                                                             STATE=="TAMIL NADU"~555,
                                                             STATE=="TELANGANA"~306,
                                                             STATE=="BIHAR"~1106,
                                                             STATE=="JHARKHAND"~414,
                                                             STATE=="KOLKATA"~24306.45,
                                                             STATE=="NORTH EAST"~173,
                                                             STATE=="ODISHA"~270,
                                                             STATE=="WEST BENGAL REST"~980.85,
                                                             STATE=="DELHI"~11320,
                                                             STATE=="HARYANA"~573,
                                                             STATE=="N RAJASTHAN"~200,
                                                             STATE=="S RAJASTHAN"~200,
                                                             STATE=="PUNJAB"~551,
                                                             STATE=="UP EAST"~829,
                                                             STATE=="UTTARAKHAND UP WEST"~189) )


Regions <- Regions %>% mutate(Metro = ifelse(STATE %in% c("MUMBAI CITY","DELHI","KOLKATA","OUTER MUMBAI"),1,0),
                              Density = ifelse(Population_Density>368,1,0))



#----- Merging with the main dataset------------------------------#
df <- df %>% left_join(., Regions)




#------- For Regressions see Malaria.R file------------------------#


df %>% filter(`DRUG CATEGORY` %in% c("INJECTABLES","LIQUIDS")) %>% group_by(`Month-Year`) %>%
        summarise(Total = sum(`Sales Value`,na.rm = T)) %>%
        ggplot(aes(x = `Month-Year`, y = Total)) +  geom_line(color = "red",size=1.1)  + scale_x_yearmon(format ="%b-%y" ) + theme_bw()


SKU_I_L <- rbind(SKU_Inject,SKU_Liquids)

write.csv(SKU_I_L,"Azithromycin_SKU_Liq_Inj.csv")




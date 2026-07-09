#----------Importing Libraries-----------------#

lapply(c("XLConnect","GGally","tidyverse","visNetwork","magrittr","DiagrammeR","data.table",
         "plotly","intergraph","networkD3","optrees","disparityfilter","network","Matrix","igraph"
         ,"CINNA","ggplot2","poweRlaw","devtools","ForceAtlas2","remotes","netdiffuseR","lfe","stargazer","foreign","MASS","lubridate","ggthemes","readtext","zoo"), require,character.only = TRUE)
library(fs)

`%notin%` <- Negate(`%in%`)


#---------Importing the Raw Data: Creating India level files for 3 different time periods -----------#

#---1>  NE + SW (Apr09-Dec10)

NE_09_10 <- fread("Data/Data/CSV_Format/Statewise SKU Basefile North+East+AI Apr-09 to Dec-10 -Entire trend.csv",skip = 2)
SW_09_10 <- fread("Data/Data/CSV_Format/Statewise SKU Basefile South+West Apr-09 to Dec-10 Entire Trend.csv",skip = 2)
India_09_10 <- rbind(NE_09_10,SW_09_10)
fwrite(India_09_10,"India_09_10.csv")


#---2> NE + SW (Jan11-Dec14)

NE_11_14 <- fread("Data/Data/CSV_Format/Statewise SKU Basefile North+East+AI Jan-11 - Dec 14 -Entire trend.csv",skip = 2)
SW_11_14 <- fread("Data/Data/CSV_Format/Statewise SKU Basefile South+West Jan-11 - Dec 14- Entire Trend.csv",skip = 2)
India_11_14 <- rbind(NE_11_14,SW_11_14)
fwrite(India_11_14,"India_11_14.csv")


#---3> NE + SW (Jan15-Jun20)

NE_15_20 <- fread("Data/Data/CSV_Format/Statewise SKU Basefile North+East+AI JAN-15 to JUN-20 Entire trend.csv",skip = 2)
SW_15_20 <- fread("Data/Data/CSV_Format/Statewise SKU Basefile South+West JAN-15 to JUN 20 Entire trend.csv",skip = 2)
India_15_20 <- rbind(NE_15_20,SW_15_20)
fwrite(India_15_20,"India_15_20.csv")


#--------------------------------------------------------------------------#

India_09_10 <- fread("Data/Data/CSV_Format/India_09_10.csv")
India_11_14 <- fread("Data/Data/CSV_Format/India_11_14.csv")
India_15_20 <- fread("Data/Data/CSV_Format/India_15_20.csv")


#---- R.W.----------------#

A <- India_09_10 %>% filter(`ITEM CODE` %in%  c(91396,65146))

## Adding missing variables in the Apr09-Dec10 data ##

A <- A %>% mutate("PLAIN/COMBINATION" = ifelse(`PLAIN/COMBINATION SPLIT`=="PLAIN","PLAIN","COMBINATION"))

A <- A %>% separate(col = SUBGROUP, into = c("left", "right"), sep = "\\|",remove = F) %>% rename("SG CODE" = "right") %>% dplyr::select(-left)

A$STATE = NA


B <- India_11_14 %>% filter(`ITEM CODE` %in%  c(91396,76807))




#--------Identifying Unique SKU'S in all the files------------------#
# 1> India_09_10 :  We have 2851 distinct molecules overall
# 2> India_11_14 :  We have 3018 distinct molecules overall
# 3> India_15_20 :  We have 3154 distinct molecules overall
# 4> India_09_20 : We have 3557 distinct molecules overall


Sub_Group_09_10 <- India_09_10 %>% distinct(SUBGROUP) %>% separate(col = SUBGROUP, into = c("left", "right"), sep = "\\|",remove = T) %>% rename("SUBGROUP" = "left","SG CODE" = "right") 



Sub_Group_09_10 <- India_09_10 %>%  separate(col = SUBGROUP, into = c("left", "right"), sep = "\\|",remove = T) %>% rename("SUBGROUP" = "left","SG CODE" = "right") 
Sub_Group_09_10_A <- Sub_Group_09_10 %>% group_by(`SG CODE`,`SUBGROUP`) %>% distinct(`SG CODE`)
Sub_Group_09_10_A$`SG CODE`<- sub(".*? ", "", Sub_Group_09_10_A$`SG CODE`)



Sub_Group_11_14 <- India_11_14 %>% group_by(`SG CODE`,`SUBGROUP`) %>% distinct(`SG CODE`) %>% separate(col = SUBGROUP, into = c("left", "right"), sep = "\\|",remove = T) %>% rename("SUBGROUP" = "left","B" = "right") %>% dplyr::select(-c(B))    
  
Sub_Group_15_20 <- India_15_20 %>% group_by(`SG CODE`,`SUBGROUP`) %>% distinct(`SG CODE`) %>% separate(col = SUBGROUP, into = c("left", "right"), sep = "\\|",remove = T) %>% rename("SUBGROUP" = "left","B" = "right") %>% dplyr::select(-c(B))    



#### Merging all the distinct SKU's ########################

A <- rbind(Sub_Group_09_10_A,Sub_Group_11_14,Sub_Group_15_20)
B <- unique(A,by = "SG CODE")

fwrite(B,"Molecules_09_20_1.csv")



#------------------ Plotting Descriptive----------------------------------------------------------------#

# 1> First Merging the data


Master_file <-  fread("Data/Data/2016_Banned_Drugs/Molecules_09_20.csv",header = T)
Master_file <- Master_file[,-c(3)]
Master_file_1  <-  fread("Data/Data/2016_Banned_Drugs/Molecules_09_20_1.csv",header = T)



df <- read.csv("Banned_Drugs_List_Identifier.csv")
df <- data.table(df)
df <- df %>% rename("SG CODE" = "SG.CODE") %>% dplyr::select(-"X") 
df$`SG CODE` <- as.character(df$`SG CODE`)
df$`SG CODE`  <- sub(".*? ", "", df$`SG CODE`)
# df <- unique(df,by = "SG CODE")

Master_file_2 <- Master_file_1 %>% left_join(., df, by = "SG CODE") %>% dplyr::select(-SUBGROUP.y)
Master_file_2 <- unique(Master_file_2,by = "SG CODE")
Master_file_2$Treatment <- as.numeric(Master_file_2$Treatment)
Master_file_2$Treatment[is.na(Master_file_2$Treatment)] <- 0

write.csv(Master_file_2,"Data/Data/2016_Banned_Drugs/Master_File_w_Treatment_status.csv")

#-------------------------- Turning to Longitudinal Dataset----------------------------------------------------#


#-----------------1> India_09_10 File cleaned and reshapes and neccessary variables generated------------------#
`%notin%` <- Negate(`%in%`)

#--- Importing the raw data files-----------------------------#
India_09_10 <- fread("Data/Data/CSV_Format/India_09_10.csv")
India_15_20 <- fread("Data/Data/CSV_Format/India_15_20.csv")

#--- Importing the Master file which contains info on Treated Drugs-------------------#
Master_File <- fread("Data/Data/2016_Banned_Drugs/Master_File_w_Treatment_status.csv")
Master_File <- Master_File[,-c(1)]


#-- Seperating the SG CODE from SUB GROUP-----#
India_09_10 <- India_09_10 %>%  separate(col = SUBGROUP, into = c("left", "right"), sep = "\\|",remove = T) %>% rename("SUBGROUP" = "left","SG CODE" = "right") 
India_09_10$`SG CODE` <- sub(".*? ", "", India_09_10$`SG CODE`)

#--- Merging the Treatment Dummy----------#
India_09_10_1 <- India_09_10 %>% left_join(.,Master_File) %>% dplyr::select(-("SUBGROUP.x")) 

## Creating a vector of Groups that got Treatment ##
A <-India_09_10_1 %>%  group_by(GROUP,Treatment) %>% summarise(N_Sub_Groups = n_distinct(SUBGROUP)) %>% filter(Treatment==1) %>% dplyr::select(-N_Sub_Groups)

## Subsetting  the Treatmet group: it includes Treatment and their corresponding controls
India_09_10_2 <- India_09_10_1 %>% filter(GROUP %in% A$GROUP)

## Sanity Check: If the No. of Treatemant Drugs are matched  after merging. 82 Treatment Frugs Matched here.
India_09_10_1 %>% group_by(SUBGROUP) %>% summarise(A = mean(Treatment)) %>% summarise(sum(A))
India_09_10_2 %>% group_by(SUBGROUP) %>% summarise(A = mean(Treatment)) %>% summarise(sum(A))

#---------------------------------------------------------------------------------------------------------------#

##-------------------------- Incorporating the per mg dosage of every SKU---------------------------------------#

#---------------------------------------------------------------------------------------------------------------#

SKU <- India_09_10_2 %>% group_by(SKU) %>%  dplyr::select(DRUGTYPE,DRUGCATEGORY,STRENGTH,PACK) 

SKU <- unique(SKU ,by = "SKU") ### Retaining only unique SKU's
SKU <- data.table(SKU)

#------------------------------------- First Category : SOLIDS-------------------------------------------#


SKU_Solids <- SKU[DRUGCATEGORY=="SOLIDS"]

# 1.1> Excluding all the extra symbols and names other than numbers and mg
SKU_Solids_1 <- SKU_Solids[- grep("%|MIU|GM|MCG|MD|IU|AU|iu|W|B", SKU_Solids$STRENGTH),] # Those strings removed containing the given patterns
SKU_Solids_1 <- SKU_Solids_1[- grep("GM|ML", SKU_Solids_1$PACK),] # Also exclude gels and tubes(GM and ML info in Packs column). Recover afterwards
SKU_Solids_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_1$STRENGTH))
SKU_Solids_1$Raw_No <- gsub("\\MG|M","",SKU_Solids_1$Raw_No)
SKU_Solids_1[c(7299),c(6)] <- c("501+6")
SKU_Solids_1 <- SKU_Solids_1[!which(SKU_Solids_1$Raw_No == ""),] ## Removed all those SKU's for which we do not have data on the mg composition
SKU_Solids_1$MG <- unlist(apply(SKU_Solids_1[,6], 1, function(x) eval(parse(text=x)))) # Concatenate this part

SKU_Solids_1$Strips <- gsub("\\TAB|S","",SKU_Solids_1$PACK)
SKU_Solids_1[which(SKU_Solids_1$Strips == ""),c("Strips")] <- 1
SKU_Solids_1[which(SKU_Solids_1$Strips == "1 X10"),c("Strips")] <- 10
SKU_Solids_1[5764,c("Strips")] <- 2250
SKU_Solids_1[21204,c("Strips")] <- 250
SKU_Solids_1$Strips <- as.numeric(as.character(SKU_Solids_1$Strips)) ## Concatenate this part



# 1.2> Retaing all the SKU's comprising of the mentioned strings 
#- Note: The SKU comprising of the following strings were drooped coz no verfied method exist to convert the units to mg doses in their case:
#--- MIU, MD, AU, iu, IU, spores(when b is present)

SKU_Solids_2 <- SKU_Solids[grep("%|GM|MCG|MD|IU|AU|iu|W|B", SKU_Solids$STRENGTH),]
SKU_Solids_2 <- SKU_Solids_2[-grep("MIU|MD|AU|IU|iu|b", SKU_Solids_2$STRENGTH),]

#- 1.2.1> Extracting only the strings comprising of % sign

SKU_Solids_2_1 <- SKU_Solids_2[grep("%", SKU_Solids_2$STRENGTH),]
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_2_1$STRENGTH))
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\%|%W/W","",SKU_Solids_2_1$Raw_No))
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\W+W|W|MG"," ",SKU_Solids_2_1$Raw_No))
SKU_Solids_2_1$MG <- unlist(apply(SKU_Solids_2_1[,6], 1, function(x) eval(parse(text=x)))) # Concatenate this part
SKU_Solids_2_1 <- SKU_Solids_2_1[!(DRUGTYPE=="TABLET"| DRUGTYPE=="TABLET SR")] ## Dropping Tablets whose mg composition is not provided
SKU_Solids_2_1$MG <- SKU_Solids_2_1$MG/100 # Converting mg to decimals
SKU_Solids_2_1$Strips <- (gsub("\\GM|MG|ML","",SKU_Solids_2_1$PACK))
SKU_Solids_2_1$Strips <- as.numeric(SKU_Solids_2_1$Strips)  ## Concatenate this part


#-- 1.3> Rows which have GM/MG in the strength column were dropped earlier, and here I process and clean that part.
#-- Those observations are dropped fpor which we do not have strength of the SKU

SKU_Solids_3 <- SKU_Solids[- grep("%|MIU|GM|MCG|MD|IU|AU|iu|W|B", SKU_Solids$STRENGTH),] # Those strings removed containing the given patterns
SKU_Solids_3 <- SKU_Solids_3[grep("GM|ML", SKU_Solids_3$PACK),]
SKU_Solids_3 <- SKU_Solids_3[!which(SKU_Solids_3$STRENGTH == ""),]
SKU_Solids_3$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_3$STRENGTH))
SKU_Solids_3$Raw_No <- gsub("\\MG|GM|ML","",SKU_Solids_3$Raw_No)
SKU_Solids_3$MG <- unlist(apply(SKU_Solids_3[,6], 1, function(x) eval(parse(text=x))))
SKU_Solids_3$Strips <- (gsub("\\GM|MG|ML","",SKU_Solids_3$PACK))
SKU_Solids_3$Strips <- as.numeric(SKU_Solids_3$Strips)
SKU_Solids_3[19,c("Strips")] <- 1        ## Concatenate this part


#-------- Cleaning for Solid SKU Finished-------------------------------------#

#------------------------------------- Second Category : Injections-------------------------------------------#


SKU_Inject <- SKU[DRUGCATEGORY =="INJECTABLES"]


#--1> Retaining only those which contain mg info and are not in percentages ------------
SKU_Inject_1 <- SKU_Inject[- grep("%|MIU|IU|AU|iu|GM", SKU_Inject$STRENGTH),] ## Leaving percent here but include it, and also gram cases in strenth column
SKU_Inject_1  <- SKU_Inject_1 [!which(SKU_Inject_1 $STRENGTH == ""),]
SKU_Inject_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Inject_1$STRENGTH))
SKU_Inject_1$Raw_No <- gsub("\\MG|GM|ML","",SKU_Inject_1$Raw_No)
SKU_Inject_1$Raw_No <- gsub("\\IT|MCG|G|Y|K| ,","",SKU_Inject_1$Raw_No)
SKU_Inject_1$MG <- unlist(apply(SKU_Inject_1[,6], 1, function(x) eval(parse(text=x))))
# SKU_Inject_1[grep("GM", SKU_Inject_1$STRENGTH), c("MG")] <- SKU_Inject_1[grep("GM", SKU_Inject_1$STRENGTH), c("MG")]*1000
SKU_Inject_1$Strips <- 1  
# SKU_Inject_1 [which(SKU_Inject_1 $PACK == "1 GM"), c("Strips")] <- 1000
# SKU_Inject_1 [which(SKU_Inject_1 $PACK == "1.5 GM"), c("Strips")] <- 1500      ## Concatenate this part


#--2> Retaining only those which contain gm info in Strength Column ------------
SKU_Inject_2 <- SKU_Inject[ grep("GM", SKU_Inject$STRENGTH),]
SKU_Inject_2$Raw_No <-  data.frame(gsub("\\GM","*1000",SKU_Inject_2$STRENGTH))
SKU_Inject_2$Raw_No_1 <-  data.frame(gsub("\\/","+",SKU_Inject_2$Raw_No))
SKU_Inject_2$Raw_No_1 <- gsub("\\MG|GM|ML","",SKU_Inject_2$Raw_No_1)
SKU_Inject_2$MG <- unlist(apply(SKU_Inject_2[,7], 1, function(x) eval(parse(text=x))))
SKU_Inject_2$Strips <- 1     
SKU_Inject_2 <- SKU_Inject_2[,-c("Raw_No_1")] ## Concatenate this part

#--3> Retaining only those which contain % info in Strength Column ------------
SKU_Inject_3 <- SKU_Inject[ grep("%", SKU_Inject$STRENGTH),]
SKU_Inject_3$Raw_No <-  data.frame(gsub("\\%|%W/V","/100",SKU_Inject_3$STRENGTH))
SKU_Inject_3$MG <- unlist(apply(SKU_Inject_3[,6], 1, function(x) eval(parse(text=x))))
SKU_Inject_3$Strips <- (gsub("\\GM|MG|ML","*1000",SKU_Inject_3$PACK))
SKU_Inject_3$Strips <- unlist(apply(SKU_Inject_3[,8], 1, function(x) eval(parse(text=x)))) ## Concatenate this part

#-------- Cleaning for Injection SKU Finished-------------------------------------#


#------------------------------------- Third Category : Others -------------------------------------------#

SKU_Other <- SKU[DRUGCATEGORY %notin% c("SOLIDS","INJECTABLES")]
SKU_Other  <- SKU_Other[- grep("MIU", SKU_Other$STRENGTH),] 
SKU_Other$Raw_No <-  data.frame(gsub("\\GM|ML","*1000",SKU_Other$STRENGTH))
SKU_Other$Raw_No <-  data.frame(gsub("\\MCG","*0.001",SKU_Other$Raw_No))
SKU_Other$Raw_No <-  data.frame(gsub("\\MG|G|%","",SKU_Other$Raw_No))
SKU_Other$Raw_No_1 <-  data.frame(gsub("\\/","+",SKU_Other$Raw_No))
SKU_Other <- SKU_Other[!which(SKU_Other$STRENGTH == ""),]
SKU_Other[c(303,235,1954,2219,4118),c("Raw_No_1")] <- c("209.11","1","1066.25","55.25","66.2") 
SKU_Other  <- SKU_Other[- grep("M", SKU_Other$Raw_No_1),] 
SKU_Other$MG <- unlist(apply(SKU_Other[,7], 1, function(x) eval(parse(text=x))))
SKU_Other[grep("%", SKU_Other$STRENGTH), c("MG")] <- SKU_Other[grep("%", SKU_Other$STRENGTH), c("MG")]/100

SKU_Other$Strips <-  data.frame(gsub("\\GM|ML|MG","",SKU_Other$PACK))
SKU_Other$Strips <- as.numeric(as.character(SKU_Other$Strips))
SKU_Other[is.na(SKU_Other$Strips),c("Strips")] <- 1  
SKU_Other <- SKU_Other[,-c("Raw_No_1")] ## Concatenate this part
#-------- Cleaning for Other SKU Finished-------------------------------------#


#-------- Combining all the SKU Information ----------------------------------#
#---- Out of 35023 SKU's, we were able to retain info on 30355; i.e.: 87%

SKU_F <- rbind(SKU_Solids_1,SKU_Solids_2_1,SKU_Solids_3,SKU_Inject_1,SKU_Inject_2,SKU_Inject_3, SKU_Other)
SKU_F <- SKU_F %>% dplyr::select(SKU,MG, Strips)


#----------- Merging with the main dataset-----------------------------#
India_09_10_2 <- India_09_10_2 %>% left_join(.,SKU_F)
India_09_10_2$`BRAND LAUNCH DATE` <-  as.yearmon(India_09_10_2$`BRAND LAUNCH DATE`,format = "%b-%y")
India_09_10_2$`SUBGROUP LAUNCH DATE` <- as.yearmon(India_09_10_2$`SUBGROUP LAUNCH DATE`,format = "%b-%y")

#---------------------------------------------------------------------------------------------------------------#
#-------------------------- Reshaping to Long Format -----------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

times <- gsub("SALES UNIT", "", grep("SALES UNIT", names(India_09_10_2), value = TRUE)) 
times <- sub(".*? ", "", times)
India_09_10_f <- reshape(India_09_10_2, direction = "long", varying = grep("SALES UNIT|SALES VALUE",names(India_09_10_2)), sep = " ",
                         v.names = c("SALES UNIT", "SALES VALUE"), timevar = "date", times = times)

fwrite(India_09_10_f,"Data/Data/2016_Banned_Drugs/Ban_Control_Molecule_09_10_long.csv")

#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#----------------  India_09_10 cleaned, processed , and transformed into long format.-------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#

#****************************************************************************************************************************************
#****************************************************************************************************************************************#

#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------- India_11_14 cleaning starts -------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#

`%notin%` <- Negate(`%in%`)
India_11_14 <- fread("Data/Data/CSV_Format/India_11_14.csv")

#--- Importing the Master file which contains info on Treated Drugs-------------------#
Master_File <- fread("Data/Data/2016_Banned_Drugs/Master_File_w_Treatment_status.csv")
Master_File <- Master_File[,-c(1)]


#-- Seperating the SG CODE from SUB GROUP-----#
India_11_14 <- India_11_14 %>%  separate(col = SUBGROUP, into = c("left", "right"), sep = "\\|",remove = T) %>% rename("SUBGROUP" = "left") %>% dplyr::select(-"right") 

#--- Merging the Treatment Dummy----------#
India_11_14_1 <- India_11_14 %>% left_join(.,Master_File) %>% dplyr::select(-("SUBGROUP.x")) 

## Creating a vector of Groups that got Treatment ##
A <-India_11_14_1 %>%  group_by(GROUP,Treatment) %>% summarise(N_Sub_Groups = n_distinct(SUBGROUP)) %>% filter(Treatment==1) %>% dplyr::select(-N_Sub_Groups)

## Subsetting  the Treatmet group: it includes Treatment and their corresponding controls
India_11_14_2 <- India_11_14_1 %>% filter(GROUP %in% A$GROUP)

## Sanity Check: If the No. of Treatemant Drugs are matched  after merging: 86 Treatment Frugs Matched here.
India_11_14_1 %>% group_by(SUBGROUP) %>% summarise(A = mean(Treatment)) %>% summarise(sum(A))
India_11_14_2 %>% group_by(SUBGROUP) %>% summarise(A = mean(Treatment)) %>% summarise(sum(A))

#---------------------------------------------------------------------------------------------------------------#

##-------------------------- Incorporating the per mg dosage of every SKU---------------------------------------#

#---------------------------------------------------------------------------------------------------------------#

SKU <- India_11_14_2 %>% group_by(SKU) %>%  dplyr::select(`DRUG TYPE`,`DRUG CATEGORY`,STRENGTH,PACK) 

SKU <- unique(SKU ,by = "SKU") ### Retaining only unique SKU's
SKU <- data.table(SKU)

#------------------------------------- First Category : SOLIDS-------------------------------------------#


SKU_Solids <- SKU[`DRUG CATEGORY`=="SOLIDS"]

# 1.1> Excluding all the extra symbols and names other than numbers and mg
SKU_Solids_1 <- SKU_Solids[- grep("%|MIU|GM|MCG|MD|IU|AU|iu|W|B|ILLION", SKU_Solids$STRENGTH),] # Those strings removed containing the given patterns
SKU_Solids_1 <- SKU_Solids_1[- grep("GM|ML", SKU_Solids_1$PACK),] # Also exclude gels and tubes(GM and ML info in Packs column). Recover afterwards
SKU_Solids_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_1$STRENGTH))
SKU_Solids_1$Raw_No <- gsub("\\MG|M|S","",SKU_Solids_1$Raw_No)
SKU_Solids_1 <- SKU_Solids_1[!which(SKU_Solids_1$Raw_No == ""),] ## Removed all those SKU's for which we do not have data on the mg composition
SKU_Solids_1[c(5602,4060),c(6)] <- c("565","507")
SKU_Solids_1$MG <- unlist(apply(SKU_Solids_1[,6], 1, function(x) eval(parse(text=x)))) # Concatenate this part

SKU_Solids_1$Strips <- gsub("\\TAB|S","",SKU_Solids_1$PACK)
SKU_Solids_1[which(SKU_Solids_1$Strips == ""),c("Strips")] <- 1
SKU_Solids_1$Strips <- as.numeric(as.character(SKU_Solids_1$Strips)) ## Concatenate this part



# 1.2> Retaing all the SKU's comprising of the mentioned strings 
#- Note: The SKU comprising of the following strings were drooped coz no verfied method exist to convert the units to mg doses in their case:
#--- MIU, MD, AU, iu, IU, spores(when b is present)

SKU_Solids_2 <- SKU_Solids[grep("%|GM|MCG|MD|IU|AU|iu|W|B", SKU_Solids$STRENGTH),]
SKU_Solids_2 <- SKU_Solids_2[-grep("MIU|MD|AU|IU|iu|b", SKU_Solids_2$STRENGTH),]

#- 1.2.1> Extracting only the strings comprising of % sign

SKU_Solids_2_1 <- SKU_Solids_2[grep("%", SKU_Solids_2$STRENGTH),]
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\%|%W/W|W/W|%W/W","",SKU_Solids_2_1$STRENGTH))
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_2_1$Raw_No))
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\%|W+W|W|MG"," ",SKU_Solids_2_1$Raw_No))
SKU_Solids_2_1$MG <- unlist(apply(SKU_Solids_2_1[,6], 1, function(x) eval(parse(text=x)))) # Concatenate this part
SKU_Solids_2_1 <- SKU_Solids_2_1[!(`DRUG TYPE`=="TABLET"| `DRUG TYPE`=="TABLET SR")] ## Dropping Tablets whose mg composition is not provided
SKU_Solids_2_1$MG <- SKU_Solids_2_1$MG/100 # Converting mg to decimals
SKU_Solids_2_1$Strips <- (gsub("\\GM|MG|ML","",SKU_Solids_2_1$PACK))
SKU_Solids_2_1$Strips <- as.numeric(SKU_Solids_2_1$Strips)  ## Concatenate this part


#-- 1.3> Rows which have GM/MG in the strength column were dropped earlier, and here I process and clean that part.
#-- Those observations are dropped fpor which we do not have strength of the SKU

SKU_Solids_3 <- SKU_Solids[- grep("%|MIU|GM|MCG|MD|IU|AU|iu|W|B", SKU_Solids$STRENGTH),] # Those strings removed containing the given patterns
SKU_Solids_3 <- SKU_Solids_3[grep("GM|ML", SKU_Solids_3$PACK),]
SKU_Solids_3 <- SKU_Solids_3[!which(SKU_Solids_3$STRENGTH == ""),]
SKU_Solids_3$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_3$STRENGTH))
SKU_Solids_3$Raw_No <- gsub("\\MG|GM|ML","",SKU_Solids_3$Raw_No)
SKU_Solids_3$MG <- unlist(apply(SKU_Solids_3[,6], 1, function(x) eval(parse(text=x))))
SKU_Solids_3$Strips <- (gsub("\\GM|MG|ML","",SKU_Solids_3$PACK))
SKU_Solids_3$Strips <- as.numeric(SKU_Solids_3$Strips)  ## Concatenate this part


#-------- Cleaning for Solid SKU Finished-------------------------------------#

#------------------------------------- Second Category : Injections-------------------------------------------#


SKU_Inject <- SKU[`DRUG CATEGORY` =="INJECTABLES"]


#--1> Retaining only those which contain mg info and are not in percentages ------------
SKU_Inject_1 <- SKU_Inject[- grep("%|MIU|IU|AU|iu|GM", SKU_Inject$STRENGTH),] ## Leaving percent here but include it, and also gram cases in strenth column
SKU_Inject_1  <- SKU_Inject_1 [!which(SKU_Inject_1 $STRENGTH == ""),]
SKU_Inject_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Inject_1$STRENGTH))
SKU_Inject_1$Raw_No <- gsub("\\MG|GM|IT|MCG|G|Y|K|,","",SKU_Inject_1$Raw_No)
SKU_Inject_1$Raw_No <- gsub("\\ML","*1000",SKU_Inject_1$Raw_No)
SKU_Inject_1$MG <- unlist(apply(SKU_Inject_1[,6], 1, function(x) eval(parse(text=x))))
# SKU_Inject_1[grep("GM", SKU_Inject_1$STRENGTH), c("MG")] <- SKU_Inject_1[grep("GM", SKU_Inject_1$STRENGTH), c("MG")]*1000
SKU_Inject_1$Strips <- 1  
# SKU_Inject_1 [which(SKU_Inject_1 $PACK == "1 GM"), c("Strips")] <- 1000
# SKU_Inject_1 [which(SKU_Inject_1 $PACK == "1.5 GM"), c("Strips")] <- 1500      ## Concatenate this part


#--2> Retaining only those which contain gm info in Strength Column ------------
SKU_Inject_2 <- SKU_Inject[ grep("GM", SKU_Inject$STRENGTH),]
SKU_Inject_2$Raw_No <-  data.frame(gsub("\\GM","*1000",SKU_Inject_2$STRENGTH))
SKU_Inject_2$Raw_No_1 <-  data.frame(gsub("\\/","+",SKU_Inject_2$Raw_No))
SKU_Inject_2$Raw_No_1 <- gsub("\\MG|GM|ML","",SKU_Inject_2$Raw_No_1)
SKU_Inject_2$MG <- unlist(apply(SKU_Inject_2[,7], 1, function(x) eval(parse(text=x))))
SKU_Inject_2$Strips <- 1     
SKU_Inject_2 <- SKU_Inject_2[,-c("Raw_No_1")] ## Concatenate this part

#--3> Retaining only those which contain % info in Strength Column ------------
SKU_Inject_3 <- SKU_Inject[ grep("%", SKU_Inject$STRENGTH),]
SKU_Inject_3$Raw_No <-  data.frame(gsub("\\%|%W/V","/100",SKU_Inject_3$STRENGTH))
SKU_Inject_3$MG <- unlist(apply(SKU_Inject_3[,6], 1, function(x) eval(parse(text=x))))
SKU_Inject_3$Strips <- (gsub("\\GM|MG|ML","*1000",SKU_Inject_3$PACK))
SKU_Inject_3$Strips <- unlist(apply(SKU_Inject_3[,8], 1, function(x) eval(parse(text=x)))) ## Concatenate this part

#-------- Cleaning for Injection SKU Finished-------------------------------------#


#------------------------------------- Third Category : Others -------------------------------------------#

SKU_Other <- SKU[`DRUG CATEGORY` %notin% c("SOLIDS","INJECTABLES")]
SKU_Other  <- SKU_Other[- grep("MIU", SKU_Other$STRENGTH),] 
SKU_Other$Raw_No <-  data.frame(gsub("\\GM|ML","*1000",SKU_Other$STRENGTH))
SKU_Other$Raw_No <-  data.frame(gsub("\\MCG","*0.001",SKU_Other$Raw_No))
SKU_Other$Raw_No <-  data.frame(gsub("\\MG|G|%|IU","",SKU_Other$Raw_No))
SKU_Other$Raw_No_1 <-  data.frame(gsub("\\/","+",SKU_Other$Raw_No))
SKU_Other <- SKU_Other[!which(SKU_Other$STRENGTH == ""),]
SKU_Other  <- SKU_Other[- grep("M", SKU_Other$Raw_No_1),] 
SKU_Other[c(1134,1412,2039,4237),c("Raw_No_1")] <- c("35","209.11","54.25","66.2") 
SKU_Other$MG <- unlist(apply(SKU_Other[,7], 1, function(x) eval(parse(text=x))))
SKU_Other[grep("%", SKU_Other$STRENGTH), c("MG")] <- SKU_Other[grep("%", SKU_Other$STRENGTH), c("MG")]/100

SKU_Other$Strips <-  data.frame(gsub("\\GM|ML|MG","",SKU_Other$PACK))
SKU_Other$Strips <- as.numeric(as.character(SKU_Other$Strips))
SKU_Other[is.na(SKU_Other$Strips),c("Strips")] <- 1  
SKU_Other <- SKU_Other[,-c("Raw_No_1")] ## Concatenate this part
#-------- Cleaning for Other SKU Finished-------------------------------------#

#-------- Combining all the SKU Information ----------------------------------#
#---- Out of 35820 SKU's, we were able to retain info on 31031; i.e.: 87%

SKU_F <- rbind(SKU_Solids_1,SKU_Solids_2_1,SKU_Solids_3,SKU_Inject_1,SKU_Inject_2,SKU_Inject_3, SKU_Other)
SKU_F <- SKU_F %>% dplyr::select(SKU,MG, Strips)


#----------- Merging with the main dataset-----------------------------#
India_11_14_2 <- India_11_14_2 %>% left_join(.,SKU_F)
India_11_14_2$`BRAND LAUNCH DATE` <-  as.yearmon(India_11_14_2$`BRAND LAUNCH DATE`,format = "%b-%y")
India_11_14_2$`SUBGROUP LAUNCH DATE` <- as.yearmon(India_11_14_2$`SUBGROUP LAUNCH DATE`,format = "%b-%y")
India_11_14_2$`SKU LAUNCH DATE` <- as.yearmon(India_11_14_2$`SKU LAUNCH DATE`,format = "%b-%y")

#---------------------------------------------------------------------------------------------------------------#
#-------------------------- Reshaping to Long Format -----------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

times <- gsub("SALES UNIT", "", grep("SALES UNIT", names(India_11_14_2), value = TRUE)) 
times <- sub(".*? ", "", times)
India_11_14_f <- reshape(India_11_14_2, direction = "long", varying = grep("SALES UNIT|SALES VALUE",names(India_11_14_2)), sep = " ",
                         v.names = c("SALES UNIT", "SALES VALUE"), timevar = "date", times = times)

fwrite(India_11_14_f,"Data/Data/2016_Banned_Drugs/Ban_Control_Molecule_11_14_long.csv")

#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#----------------  India_11_14 cleaned, processed , and transformed into long format.-------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------- India_50_20 cleaning starts -------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
`%notin%` <- Negate(`%in%`)
India_15_20 <- fread("Data/Data/CSV_Format/India_15_20.csv")

#--- Importing the Master file which contains info on Treated Drugs-------------------#
Master_File <- fread("Data/Data/2016_Banned_Drugs/Master_File_w_Treatment_status.csv")
Master_File <- Master_File[,-c(1)]


#-- Seperating the SG CODE from SUB GROUP-----#
India_15_20 <- India_15_20 %>%  separate(col = SUBGROUP, into = c("left", "right"), sep = "\\|",remove = T) %>% rename("SUBGROUP" = "left") %>% dplyr::select(-"right") 

#--- Merging the Treatment Dummy----------#
India_15_20_1 <- India_15_20 %>% left_join(.,Master_File) %>% dplyr::select(-("SUBGROUP.x")) 

## Creating a vector of Groups that got Treatment ##
A <-India_15_20_1 %>%  group_by(GROUP,Treatment) %>% summarise(N_Sub_Groups = n_distinct(SUBGROUP)) %>% filter(Treatment==1) %>% dplyr::select(-N_Sub_Groups)

## Subsetting  the Treatmet group: it includes Treatment and their corresponding controls
India_15_20_2 <- India_15_20_1 %>% filter(GROUP %in% A$GROUP)

## Sanity Check: If the No. of Treatemant Drugs are matched  after merging: 85 Treatment Drugs Matched here.
India_15_20_1 %>% group_by(SUBGROUP) %>% summarise(A = mean(Treatment)) %>% summarise(sum(A))
India_15_20_2 %>% group_by(SUBGROUP) %>% summarise(A = mean(Treatment)) %>% summarise(sum(A))

#---------------------------------------------------------------------------------------------------------------#

##-------------------------- Incorporating the per mg dosage of every SKU---------------------------------------#

#---------------------------------------------------------------------------------------------------------------#

SKU <- India_15_20_2 %>% group_by(SKU) %>%  dplyr::select(`DRUG TYPE`,`DRUG CATEGORY`,STRENGTH,PACK) 
SKU <- unique(SKU ,by = "SKU") ### Retaining only unique SKU's
SKU <- data.table(SKU)

#------------------------------------- First Category : SOLIDS-------------------------------------------#


SKU_Solids <- SKU[`DRUG CATEGORY`=="SOLIDS"]

# 1.1> Excluding all the extra symbols and names other than numbers and mg
SKU_Solids_1 <- SKU_Solids[- grep("%|MIU|GM|MCG|MD|IU|AU|iu|W|B|ILLION", SKU_Solids$STRENGTH),] # Those strings removed containing the given patterns
SKU_Solids_1 <- SKU_Solids_1[- grep("GM|ML", SKU_Solids_1$PACK),] # Also exclude gels and tubes(GM and ML info in Packs column). Recover afterwards
SKU_Solids_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_1$STRENGTH))
SKU_Solids_1$Raw_No <- gsub("\\MG|M|S","",SKU_Solids_1$Raw_No)
SKU_Solids_1 <- SKU_Solids_1[!which(SKU_Solids_1$Raw_No == ""),] ## Removed all those SKU's for which we do not have data on the mg composition
SKU_Solids_1$MG <- unlist(apply(SKU_Solids_1[,6], 1, function(x) eval(parse(text=x)))) # Concatenate this part

SKU_Solids_1$Strips <- gsub("\\TAB|S","",SKU_Solids_1$PACK)
SKU_Solids_1$Strips <- as.numeric(as.character(SKU_Solids_1$Strips)) ## Concatenate this part



# 1.2> Retaing all the SKU's comprising of the mentioned strings 
#- Note: The SKU comprising of the following strings were drooped coz no verfied method exist to convert the units to mg doses in their case:
#--- MIU, MD, AU, iu, IU, spores(when b is present)

SKU_Solids_2 <- SKU_Solids[grep("%|GM|MCG|MD|IU|AU|iu|W|B", SKU_Solids$STRENGTH),]
SKU_Solids_2 <- SKU_Solids_2[-grep("MIU|MD|AU|IU|iu|b", SKU_Solids_2$STRENGTH),]

#- 1.2.1> Extracting only the strings comprising of % sign

SKU_Solids_2_1 <- SKU_Solids_2[grep("%", SKU_Solids_2$STRENGTH),]
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\%|%W/W|W/W|%W/W","",SKU_Solids_2_1$STRENGTH))
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_2_1$Raw_No))
SKU_Solids_2_1$Raw_No <-  data.frame(gsub("\\%|W+W|W|MG"," ",SKU_Solids_2_1$Raw_No))
SKU_Solids_2_1$MG <- unlist(apply(SKU_Solids_2_1[,6], 1, function(x) eval(parse(text=x)))) # Concatenate this part
SKU_Solids_2_1 <- SKU_Solids_2_1[!(`DRUG TYPE`=="TABLET"| `DRUG TYPE`=="TABLET SR")] ## Dropping Tablets whose mg composition is not provided
SKU_Solids_2_1$MG <- SKU_Solids_2_1$MG/100 # Converting mg to decimals
SKU_Solids_2_1$Strips <- (gsub("\\GM|MG|ML","",SKU_Solids_2_1$PACK))
SKU_Solids_2_1$Strips <- as.numeric(SKU_Solids_2_1$Strips)  ## Concatenate this part


#-- 1.3> Rows which have GM/MG in the strength column were dropped earlier, and here I process and clean that part.
#-- Those observations are dropped fpor which we do not have strength of the SKU

SKU_Solids_3 <- SKU_Solids[- grep("%|MIU|GM|MCG|MD|IU|AU|iu|W|B", SKU_Solids$STRENGTH),] # Those strings removed containing the given patterns
SKU_Solids_3 <- SKU_Solids_3[grep("GM|ML", SKU_Solids_3$PACK),]
SKU_Solids_3 <- SKU_Solids_3[!which(SKU_Solids_3$STRENGTH == ""),]
SKU_Solids_3$Raw_No <-  data.frame(gsub("\\/","+",SKU_Solids_3$STRENGTH))
SKU_Solids_3$Raw_No <- gsub("\\MG|GM|ML","",SKU_Solids_3$Raw_No)
SKU_Solids_3$MG <- unlist(apply(SKU_Solids_3[,6], 1, function(x) eval(parse(text=x))))
SKU_Solids_3$Strips <- (gsub("\\GM|MG|ML","",SKU_Solids_3$PACK))
SKU_Solids_3$Strips <- as.numeric(SKU_Solids_3$Strips)  ## Concatenate this part


#-------- -----------------------------Cleaning for Solid SKU Finished-------------------------------------#

#------------------------------------- Second Category : Injections-------------------------------------------#


SKU_Inject <- SKU[`DRUG CATEGORY` =="INJECTABLES"]


#--1> Retaining only those which contain mg info and are not in percentages ------------
SKU_Inject_1 <- SKU_Inject[- grep("%|MIU|IU|AU|iu|GM", SKU_Inject$STRENGTH),] ## Leaving percent here but include it, and also gram cases in strenth column
SKU_Inject_1  <- SKU_Inject_1 [!which(SKU_Inject_1 $STRENGTH == ""),]
SKU_Inject_1$Raw_No <-  data.frame(gsub("\\/","+",SKU_Inject_1$STRENGTH))
SKU_Inject_1$Raw_No <- gsub("\\MG|GM|IT|MCG|G|Y|K|,","",SKU_Inject_1$Raw_No)
SKU_Inject_1$Raw_No <- gsub("\\ML","*1000",SKU_Inject_1$Raw_No)
SKU_Inject_1$MG <- unlist(apply(SKU_Inject_1[,6], 1, function(x) eval(parse(text=x))))
# SKU_Inject_1[grep("GM", SKU_Inject_1$STRENGTH), c("MG")] <- SKU_Inject_1[grep("GM", SKU_Inject_1$STRENGTH), c("MG")]*1000
SKU_Inject_1$Strips <- 1  
# SKU_Inject_1 [which(SKU_Inject_1 $PACK == "1 GM"), c("Strips")] <- 1000
# SKU_Inject_1 [which(SKU_Inject_1 $PACK == "1.5 GM"), c("Strips")] <- 1500      ## Concatenate this part


#--2> Retaining only those which contain gm info in Strength Column ------------
SKU_Inject_2 <- SKU_Inject[ grep("GM", SKU_Inject$STRENGTH),]
SKU_Inject_2$Raw_No <-  data.frame(gsub("\\GM","*1000",SKU_Inject_2$STRENGTH))
SKU_Inject_2$Raw_No_1 <-  data.frame(gsub("\\/","+",SKU_Inject_2$Raw_No))
SKU_Inject_2$Raw_No_1 <- gsub("\\MG|GM|ML","",SKU_Inject_2$Raw_No_1)
SKU_Inject_2$MG <- unlist(apply(SKU_Inject_2[,7], 1, function(x) eval(parse(text=x))))
SKU_Inject_2$Strips <- 1     
SKU_Inject_2 <- SKU_Inject_2[,-c("Raw_No_1")] ## Concatenate this part

#--3> Retaining only those which contain % info in Strength Column ------------
SKU_Inject_3 <- SKU_Inject[ grep("%", SKU_Inject$STRENGTH),]
SKU_Inject_3$Raw_No <-  data.frame(gsub("\\%|%W/V","/100",SKU_Inject_3$STRENGTH))
SKU_Inject_3$MG <- unlist(apply(SKU_Inject_3[,6], 1, function(x) eval(parse(text=x))))
SKU_Inject_3$Strips <- (gsub("\\GM|MG|ML","*1000",SKU_Inject_3$PACK))
SKU_Inject_3$Strips <- unlist(apply(SKU_Inject_3[,8], 1, function(x) eval(parse(text=x)))) ## Concatenate this part

#-------- Cleaning for Injection SKU Finished-------------------------------------#


#------------------------------------- Third Category : Others -------------------------------------------#

SKU_Other <- SKU[`DRUG CATEGORY` %notin% c("SOLIDS","INJECTABLES")]
SKU_Other  <- SKU_Other[- grep("MIU", SKU_Other$STRENGTH),] 
SKU_Other$Raw_No <-  data.frame(gsub("\\GM|ML","*1000",SKU_Other$STRENGTH))
SKU_Other$Raw_No <-  data.frame(gsub("\\MCG","*0.001",SKU_Other$Raw_No))
SKU_Other$Raw_No <-  data.frame(gsub("\\MG|G|%|IU","",SKU_Other$Raw_No))
SKU_Other$Raw_No_1 <-  data.frame(gsub("\\/","+",SKU_Other$Raw_No))
SKU_Other <- SKU_Other[!which(SKU_Other$STRENGTH == ""),]
SKU_Other  <- SKU_Other[- grep("M", SKU_Other$Raw_No_1),] 
SKU_Other$MG <- unlist(apply(SKU_Other[,7], 1, function(x) eval(parse(text=x))))
SKU_Other[grep("%", SKU_Other$STRENGTH), c("MG")] <- SKU_Other[grep("%", SKU_Other$STRENGTH), c("MG")]/100

SKU_Other$Strips <-  data.frame(gsub("\\GM|ML|MG","",SKU_Other$PACK))
SKU_Other$Strips <- as.numeric(as.character(SKU_Other$Strips))
SKU_Other[is.na(SKU_Other$Strips),c("Strips")] <- 1  
SKU_Other <- SKU_Other[,-c("Raw_No_1")] ## Concatenate this part
#-------- Cleaning for Other SKU Finished-------------------------------------#

#-------- Combining all the SKU Information ----------------------------------#
#---- Out of 30711 SKU's, we were able to retain info on 26736; i.e.: 87%

SKU_F <- rbind(SKU_Solids_1,SKU_Solids_2_1,SKU_Solids_3,SKU_Inject_1,SKU_Inject_2,SKU_Inject_3, SKU_Other)
SKU_F <- SKU_F %>% dplyr::select(SKU,MG, Strips)


#----------- Merging with the main dataset-----------------------------#
India_15_20_2 <- India_15_20_2 %>% left_join(.,SKU_F)
India_15_20_2$`BRAND LAUNCH DATE` <-  as.yearmon(India_15_20_2$`BRAND LAUNCH DATE`,format = "%b-%y")
India_15_20_2$`SUBGROUP LAUNCH DATE` <- as.yearmon(India_15_20_2$`SUBGROUP LAUNCH DATE`,format = "%b-%y")
India_15_20_2$`SKU LAUNCH DATE` <- as.yearmon(India_15_20_2$`SKU LAUNCH DATE`,format = "%b-%y")

#---------------------------------------------------------------------------------------------------------------#
#-------------------------- Reshaping to Long Format -----------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

times <- gsub("SALES UNIT", "", grep("SALES UNIT", names(India_15_20_2), value = TRUE)) 
times <- sub(".*? ", "", times)
India_15_20_f <- reshape(India_15_20_2, direction = "long", varying = grep("SALES UNIT|SALES VALUE",names(India_15_20_2)), sep = " ",
                         v.names = c("SALES UNIT", "SALES VALUE"), timevar = "date", times = times)

fwrite(India_15_20_f,"Data/Data/2016_Banned_Drugs/Ban_Control_Molecule_15_20_long.csv")

#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#----------------  India_11_14 cleaned, processed , and transformed into long format.-------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#

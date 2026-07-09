
### Created a control Datset corresponding to Rosiglitazone & Sibutramine from PIOGLITAZONE & ORLISTAT resp.

#### Control Dataset ###############

files <- dir_ls(path = "F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Practise_Data/", glob = "*txt") # To see how many files are in the folder

Control_data <-  map_dfr(files,data_maker)
lapply(Control_data, class)
Control_data <- data.table(Control_data)

SKU_Info <- Control_data%>% distinct(SKU)

SKU_Info$SKU <- as.character(SKU_Info$SKU)

SKU_Info <- data.frame(SKU_Info)

SKU_A <- SKU_Info %>% separate(col= "SKU",into=c("1","2","3","4","5","6","7","8","9"),remove=F)

SKU_4 <- SKU_A %>% filter( !is.na(`4`) & is.na(`5`) & is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_5 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_6 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_7 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & is.na(`8`) & is.na(`9`))
SKU_8 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & is.na(`9`))
SKU_9 <- SKU_A %>% filter( !is.na(`4`) & !is.na(`5`) & !is.na(`6`) & !is.na(`7`) & !is.na(`8`) & !is.na(`9`))


### SKU_4 : Processed and Merged #####
SKU_4 <- SKU_4 %>% mutate(mg = c(120,30), Strips = c(10,10)) 
SKU_4 <- SKU_4[,-c(2:10)]

### SKU_5 : Processed and Merged ####
SKU_5 <- SKU_5 %>% mutate(mg =`2`, Strips = `5`)
SKU_5 <- SKU_5[,-c(2:10)]

### SKU_6 : Processed and Merged ####
SKU_6 <- SKU_6 %>% mutate(mg = ifelse(`2`==7,7.5,`3`), Strips = `6`)
SKU_6 <- SKU_6[,-c(2:10)]

### SKU_7 : Processed and Merged ####
SKU_7 <- SKU_7 %>% mutate(mg = c(15,30,15,30,15,30,515,15),Strips=c(10,10,25,25,10,10,10,120))
SKU_7 <- SKU_7[,-c(2:10)]


### SKU_8: Processed and Merged ####
#Empty

### SKU_9: Processed and Merged ###
SKU_9 <- SKU_9 %>% mutate(mg = c(0.5),Strips = c(4))
SKU_9 <- SKU_9[,-c(2:10)]
### Combining the Data ####

SKU_Final <- rbind(SKU_4,SKU_5,SKU_6,SKU_7,SKU_9)
SKU_Final$mg <- as.numeric(SKU_Final$mg)
SKU_Final$Strips <- as.numeric(SKU_Final$Strips)

Control_df <- merge(Control_data,SKU_Final,all.x = T,all.y = T,by="SKU")


# A <- Experiment_df %>% filter(!is.na(`Actual Sale Units`))

## Dropping Those Obseravtions which do not have data for mg (20 Percent Loss of Data)
Control_df <- Control_df %>% filter(!is.na(mg))


### Converting ASU's to mg ##########

Control_df <- Control_df %>% mutate(ASU_mg = `Actual Sale Units`*mg*Strips)

### Revenue and Proce per mg ########

Control_df <- Control_df %>% mutate(Revenue = `Actual Sale Units`*MRP)

Control_df <- Control_df %>% mutate(Price_per_mg = Revenue/ASU_mg)

Control_df <- data.frame(Control_df)


### Created a control Datset corresponding to Rosiglitazone & Sibutramine from PIOGLITAZONE & ORLISTAT resp.

write.csv(Control_df,"Data/Control_df_Rosig_Sibutram.csv") 

### End of Do-File ##########################################################


#-----------------------------------------------------------------------------------------------------------------------------#
#----------------------------------    Importing Libraries -------------------------------------------------------------------#
lapply(c("XLConnect","GGally","tidyverse","visNetwork","magrittr","DiagrammeR","data.table",
         "plotly","intergraph","networkD3","optrees","disparityfilter","network","Matrix","igraph"
         ,"CINNA","ggplot2","poweRlaw","devtools","ForceAtlas2","remotes","netdiffuseR","lfe","stargazer","foreign","MASS","lubridate","ggthemes","readtext","zoo"), require,character.only = TRUE)
library(fs)

`%notin%` <- Negate(`%in%`)


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


# 1> India_09_10 cleaned and processed
India_09_10 <- India_09_10 %>% mutate(STATE = NA, "PLAIN/COMBINATION" = ifelse(`PLAIN/COMBINATION SPLIT`=="PLAIN","PLAIN","COMBINATION"))
India_09_10$`BRAND LAUNCH DATE` <-  as.yearmon(India_09_10$`BRAND LAUNCH DATE`,format = "%b-%y")
India_09_10$`SUBGROUP LAUNCH DATE` <- as.yearmon(India_09_10$`SUBGROUP LAUNCH DATE`,format = "%b-%y")
India_09_10$`SKU LAUNCH DATE` <- as.yearmon(India_09_10$`SKU LAUNCH DATE`,format = "%b-%y")
India_09_10 <- India_09_10 %>% rename(`ACUTE/CHRONIC` = ACUTECHRONIC,`SUPER GROUP` = SUPERGROUP,`DRUG TYPE` = DRUGTYPE,`DRUG CATEGORY` = DRUGCATEGORY,DATE = variable)
India_09_10$DATE <- as.yearmon(India_09_10$DATE,format = "%b-%y")


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
India_09_20 <- rbind(India_09_10,India_11_14,India_15_20)

India_09_20$Ban_Date <- as.yearmon("March 2016", format = "%B %Y")
#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------  Descriptive Plots --------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#

# Creating Relevant Variables

# 1> ASU_mg, Revenue, Price_per_mg

India_09_20 <- India_09_20[`OLD STATE`!="ALL INDIA ONLY"]
# India_09_20$SUBGROUP  <- gsub("\\ + ","+",India_09_20$SUBGROUP)

India_09_20  <- India_09_20 %>% mutate(ASU_mg = `SALES UNIT`*MG*Strips,Revenue = `SALES UNIT`*MRP,
                                       Price_per_mg = ifelse(ASU_mg==0,0,Revenue/ASU_mg))



#-------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------- National Averages --------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#


#---- Credit: Pallavi's Idea--------------------#


#--- 1> ASU_mg --------------------------------------------------------------------------------------#

A <- India_09_20 %>% filter(Treatment==1) %>% group_by(GROUP,`SG CODE`,DATE,Treatment,`INDIAN/MNC`)  %>% summarise(Total = sum(ASU_mg,na.rm=T))


B <- India_09_20 %>% filter(Treatment==0) %>%  group_by(GROUP,`SG CODE`,DATE,`INDIAN/MNC`) %>% summarise(A = sum(ASU_mg,na.rm=T)) %>% group_by(GROUP,DATE,`INDIAN/MNC`)  %>% summarise(mean = mean(A))

# max_A <- max(A$Total)
# max_B <- max(B$mean)
# prop <- 1
# B$mean <- B$mean/prop

C <- India_09_20 %>% group_by(GROUP,`SG CODE`,Treatment,DATE,`INDIAN/MNC`) %>% filter(Treatment==1) %>% summarise(A = n_distinct(`SG CODE`)) %>%
     dplyr::select(-c(A,Treatment)) %>% mutate(Treatment=0)  %>% left_join(.,B) %>% rename("Total" = "mean")

D <- rbind(A,C)

E =  India_09_20 %>% group_by(`SG CODE`) %>% filter(Treatment==1) %>% distinct(SUBGROUP)
E = E[!duplicated(E$`SG CODE`),]
E$ID <- c(1:94)

D <- D %>% left_join(., E)

D$Description = case_when(D$Treatment==1 & D$`INDIAN/MNC`=="INDIAN"~"Treatment-Indian", D$Treatment==1 & D$`INDIAN/MNC` =="MNC"~"Treatment-MNC",
                          D$Treatment==0 & D$`INDIAN/MNC`=="INDIAN"~"Control-Indian",  D$Treatment==0 & D$`INDIAN/MNC`=="MNC"~"Control-MNC")



D <- data.table(D)

# mycolors <- c("Treatment"="blue", "Control"="red")
# 
# ggplot(D[ID>0 & ID<3], aes(x=DATE, y=Total, group=Description, color=Description)) +
#   geom_line(size=0.7) +
#   geom_point(size=0.7) +
#   scale_y_continuous(name="Treatment", sec.axis = sec_axis(trans = ~ .*(prop), name="Control")) +
#   scale_color_manual(name="Description", values = mycolors) +
#   facet_wrap(~ SUBGROUP,scales =  "free") +
#   labs(title = paste("Evolution of total sales (in mg) and Revenue for molecules in company"))+
#   theme(
#     axis.title.y = element_text(color = mycolors["Treatment"]),
#     axis.text.y = element_text(color = mycolors["Treatment"]),
#     axis.title.y.right = element_text(color = mycolors["Control"]),
#     axis.text.y.right = element_text(color = mycolors["Control"]),
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   scale_x_yearmon(format ="%b %y" )


for (i in c(0,16,32,48,64,80)) {

  path <- paste('EDS/EDS#1/National_Averages/ASU_mg/',i,".jpeg")
  jpeg(file = path,width = 16, height = 16, units = "in", res = 700, pointsize = 1/700)
  
p <- ggplot(data = D[ID>i & ID< i+16+1], aes(DATE, Total)) +
    geom_line(aes(color=Description),size=1.1) +
    labs(title = "Evolution of Aggregate Sale Units(in mg) for Banned and the Control Molecules",
         subtitle = "Note:Control time series is the mean of the total sales of all the molecules belonging to the control group",
         y = " ", x = "")  +
    geom_vline(mapping = aes(xintercept = as.yearmon("March 2016", format = "%B %Y")),linetype = 4,size = 1,colour = "black") + 
    facet_wrap(~ SUBGROUP,scales =  "free") + theme_economist_white(base_size = 6) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_colour_manual(values = c("Treatment-Indian" = "royalblue4", "Control-Indian" = "cyan3","Treatment-MNC" = "indianred4","Control-MNC" = "lightpink3")) + 
    scale_x_yearmon(format ="%b-%y" )


print(p)
dev.off()


}







#--- 2> Price_per_mg --------------------------------------------------------------------------------------#

A <- India_09_20 %>% filter(Treatment==1) %>% group_by(GROUP,`SG CODE`,DATE,Treatment,`INDIAN/MNC`)  %>% summarise(Total = mean(Price_per_mg,na.rm=T))


# B <- India_09_20 %>% filter(Treatment==0) %>% group_by(GROUP,DATE,`INDIAN/MNC`)  %>% summarise(mean = mean(Price_per_mg,na.rm=T))
B <- India_09_20 %>% filter(Treatment==0) %>% group_by(GROUP,DATE,`SG CODE`,`INDIAN/MNC`)  %>% summarise(Total = mean(Price_per_mg,na.rm=T)) %>% group_by(GROUP,DATE, `INDIAN/MNC`) %>% summarise(mean = mean(Total,na.rm=T))


C <- India_09_20 %>% group_by(GROUP,`SG CODE`,Treatment,DATE,`INDIAN/MNC`) %>% filter(Treatment==1) %>% summarise(A = n_distinct(`SG CODE`)) %>%
  dplyr::select(-c(A,Treatment)) %>% mutate(Treatment=0)  %>% left_join(.,B) %>% rename("Total" = "mean")

D <- rbind(A,C)

E =  India_09_20 %>% group_by(`SG CODE`) %>% filter(Treatment==1) %>% distinct(SUBGROUP)
E = E[!duplicated(E$`SG CODE`),]
E$ID <- c(1:94)

D <- D %>% left_join(., E)

D$Description = case_when(D$Treatment==1 & D$`INDIAN/MNC`=="INDIAN"~"Treatment-Indian", D$Treatment==1 & D$`INDIAN/MNC` =="MNC"~"Treatment-MNC",
                          D$Treatment==0 & D$`INDIAN/MNC`=="INDIAN"~"Control-Indian",  D$Treatment==0 & D$`INDIAN/MNC`=="MNC"~"Control-MNC")



D <- data.table(D)


for (i in c(0,16,32,48,64,80)) {
  
  path <- paste('EDS/EDS#1/National_Averages/Price_per_mg/',i,".jpeg")
  jpeg(file = path,width = 16, height = 16, units = "in", res = 700, pointsize = 1/700)
  
  p <- ggplot(data = D[ID>i & ID< i+16+1], aes(DATE, Total)) +
    geom_line(aes(color=Description),size=1.1) +
    labs(title = "Evolution of Price per mg for Banned and the Control Molecules",
         y = "", x = "")  +
    geom_vline(mapping = aes(xintercept = as.yearmon("March 2016", format = "%B %Y")),linetype = 4,size = 1,colour = "black") + 
    facet_wrap(~ SUBGROUP,scales =  "free") + theme_economist_white(base_size = 6) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_colour_manual(values = c("Treatment-Indian" = "royalblue4", "Control-Indian" = "cyan3","Treatment-MNC" = "indianred4","Control-MNC" = "lightpink3")) + 
    scale_x_yearmon(format ="%b-%y" )
  print(p)
  dev.off()
  
  
}


#--- 3> Variety --------------------------------------------------------------------------------------#

A <- India_09_20 %>% filter(Treatment==1 & ASU_mg>0) %>% group_by(GROUP,`SG CODE`,DATE,Treatment,`INDIAN/MNC`)  %>% summarise(Total = n_distinct(SKU))

B <- India_09_20 %>% filter(Treatment==0 & ASU_mg>0) %>%  group_by(GROUP,`SG CODE`,DATE,`INDIAN/MNC`) %>% summarise(A = n_distinct(SKU)) %>% group_by(GROUP,DATE,`INDIAN/MNC`)  %>% summarise(mean = mean(A))


C <- India_09_20 %>% group_by(GROUP,`SG CODE`,Treatment,DATE,`INDIAN/MNC`) %>% filter(Treatment==1) %>% summarise(A = n_distinct(`SG CODE`)) %>%
  dplyr::select(-c(A,Treatment)) %>% mutate(Treatment=0)  %>% left_join(.,B) %>% rename("Total" = "mean")

D <- rbind(A,C)

E =  India_09_20 %>% group_by(`SG CODE`) %>% filter(Treatment==1) %>% distinct(SUBGROUP)
E = E[!duplicated(E$`SG CODE`),]
E$ID <- c(1:94)

D <- D %>% left_join(., E)

D$Description = case_when(D$Treatment==1 & D$`INDIAN/MNC`=="INDIAN"~"Treatment-Indian", D$Treatment==1 & D$`INDIAN/MNC` =="MNC"~"Treatment-MNC",
                          D$Treatment==0 & D$`INDIAN/MNC`=="INDIAN"~"Control-Indian",  D$Treatment==0 & D$`INDIAN/MNC`=="MNC"~"Control-MNC")

D <- data.table(D)



for (i in c(0,16,32,48,64,80)) {
  
  path <- paste('EDS/EDS#1/National_Averages/Variety/',i,".jpeg")
  jpeg(file = path,width = 16, height = 16, units = "in", res = 700, pointsize = 1/700)
  
  p <- ggplot(data = D[ID>i & ID< i+16+1], aes(DATE, Total)) +
    geom_line(aes(color=Description),size=1.1) +
    labs(title = "Evolution of Varieties for Banned and the Control Molecules",
         subtitle = "Note:Control time series is the mean of the total varieties of the molecules belonging to the control group",
         y = " ", x = "")  +
    geom_vline(mapping = aes(xintercept = as.yearmon("March 2016", format = "%B %Y")),linetype = 4,size = 1,colour = "black") + 
    facet_wrap(~ SUBGROUP,scales =  "free") + theme_economist_white(base_size = 6) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_colour_manual(values = c("Treatment-Indian" = "royalblue4", "Control-Indian" = "cyan3","Treatment-MNC" = "indianred4","Control-MNC" = "lightpink3")) + 
    scale_x_yearmon(format ="%b-%y" )
  print(p)
  dev.off()
  
}


#--- 4> HHI --------------------------------------------------------------------------------------#

A <- India_09_20 %>% filter(Treatment==1) %>% group_by(GROUP,`SG CODE`,DATE,Treatment,COMPANY,`INDIAN/MNC`)  %>% summarise(Total = sum(ASU_mg,na.rm=T)) %>%
     group_by(GROUP,`SG CODE`,DATE,Treatment,`INDIAN/MNC`) %>% summarise(Total = sum( (Total/sum(Total))^2))


B <- India_09_20 %>% filter(Treatment==0) %>%  group_by(GROUP,`SG CODE`,DATE,`INDIAN/MNC`,COMPANY) %>% summarise(A = sum(ASU_mg,na.rm=T)) %>% group_by(GROUP,DATE,`INDIAN/MNC`,COMPANY) %>% 
     summarise(B = mean(A)) %>%  group_by(GROUP,DATE,`INDIAN/MNC`) %>% summarise(mean = sum( (B/sum(B))^2))
 
# max_A <- max(A$Total)
# max_B <- max(B$mean)
# prop <- 1
# B$mean <- B$mean/prop

C <- India_09_20 %>% group_by(GROUP,`SG CODE`,Treatment,DATE,`INDIAN/MNC`) %>% filter(Treatment==1) %>% summarise(A = n_distinct(`SG CODE`)) %>%
  dplyr::select(-c(A,Treatment)) %>% mutate(Treatment=0)  %>% left_join(.,B) %>% rename("Total" = "mean")

D <- rbind(A,C)

E =  India_09_20 %>% group_by(`SG CODE`) %>% filter(Treatment==1) %>% distinct(SUBGROUP)
E = E[!duplicated(E$`SG CODE`),]
E$ID <- c(1:94)

D <- D %>% left_join(., E)

D$Description = case_when(D$Treatment==1 & D$`INDIAN/MNC`=="INDIAN"~"Treatment-Indian", D$Treatment==1 & D$`INDIAN/MNC` =="MNC"~"Treatment-MNC",
                          D$Treatment==0 & D$`INDIAN/MNC`=="INDIAN"~"Control-Indian",  D$Treatment==0 & D$`INDIAN/MNC`=="MNC"~"Control-MNC")



D <- data.table(D)

# mycolors <- c("Treatment"="blue", "Control"="red")
# 
# ggplot(D[ID>0 & ID<3], aes(x=DATE, y=Total, group=Description, color=Description)) +
#   geom_line(size=0.7) +
#   geom_point(size=0.7) +
#   scale_y_continuous(name="Treatment", sec.axis = sec_axis(trans = ~ .*(prop), name="Control")) +
#   scale_color_manual(name="Description", values = mycolors) +
#   facet_wrap(~ SUBGROUP,scales =  "free") +
#   labs(title = paste("Evolution of total sales (in mg) and Revenue for molecules in company"))+
#   theme(
#     axis.title.y = element_text(color = mycolors["Treatment"]),
#     axis.text.y = element_text(color = mycolors["Treatment"]),
#     axis.title.y.right = element_text(color = mycolors["Control"]),
#     axis.text.y.right = element_text(color = mycolors["Control"]),
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   scale_x_yearmon(format ="%b %y" )


for (i in c(0,16,32,48,64,80)) {
  
  path <- paste('EDS/EDS#1/National_Averages/HHI/',i,".jpeg")
  jpeg(file = path,width = 16, height = 16, units = "in", res = 700, pointsize = 1/700)
  
  p <- ggplot(data = D[ID>i & ID< i+16+1], aes(DATE, Total)) +
    geom_line(aes(color=Description),size=1.1) +
    labs(title = "HHI for Banned and the Control Molecules",
         y = " ", x = "")  +
    geom_vline(mapping = aes(xintercept = as.yearmon("March 2016", format = "%B %Y")),linetype = 4,size = 1,colour = "black") + 
    facet_wrap(~ SUBGROUP,scales =  "free") + theme_economist_white(base_size = 6) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_colour_manual(values = c("Treatment-Indian" = "royalblue4", "Control-Indian" = "cyan3","Treatment-MNC" = "indianred4","Control-MNC" = "lightpink3")) + 
    scale_x_yearmon(format ="%b-%y" )
  
  
  print(p)
  dev.off()
  
  
}




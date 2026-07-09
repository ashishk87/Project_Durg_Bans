########## Creating Plot for all the banned Drugs ############################

#----------Importing Libraries-----------------#

lapply(c("XLConnect","GGally","tidyverse","visNetwork","magrittr","DiagrammeR","data.table",
         "plotly","intergraph","networkD3","optrees","disparityfilter","network","Matrix","igraph"
         ,"CINNA","ggplot2","poweRlaw","devtools","ForceAtlas2","remotes","netdiffuseR","lfe","stargazer","foreign","MASS","lubridate","ggthemes","readtext","zoo"), require,character.only = TRUE)
library(fs)
`%notin%` <- Negate(`%in%`)

#---- Importing the Data------------------------#
df <- read.csv("Data/Banned_Drugs_2.csv", row.names=1)

final_df <- data.table(df)
final_df <- final_df[!(final_df$State=="ALL INDIA ONLY")]

lapply(final_df,class)   # Checking the classes of all the columns

## Repairing the Date Format
final_df$Date <- as.yearmon(final_df$Date,format = "%B %Y")

#---- Creating National Averages----------------#

# 1> Molecule Aggregate Sale: Monthly Trends ##############

Molecule_Agg_Sale <- final_df_1 %>% group_by(Sub.Group,Date,Company_Type) %>% summarise(Total = sum(ASU_mg,na.rm = T))

path <- 'Descriptive_Plots/EDS#4/National_Averages/Agg_ASU_Monthly.jpeg'
jpeg(file = path,width = 15, height = 10, units = "in", res = 800, pointsize = 1/800)

ggplot(data = Molecule_Agg_Sale, aes(Date, Total)) +
  geom_line(aes(color=Company_Type),size=1) +
  labs(title = "Evolution of Aggregate Sale Units(in mg) for Banned Molecules",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()


# 2> Molecule Aggregate Revenue: Monthly Trends ##############

Molecule_Agg_Revenue <- final_df_1 %>% group_by(Sub.Group,Date,Company_Type) %>% summarise(Total = sum(Revenue,na.rm = T))

path <- 'Descriptive_Plots/EDS#4/National_Averages/Agg_Revenue_Monthly.jpeg'
jpeg(file = path,width = 15, height = 10, units = "in", res = 800, pointsize = 1/800)

ggplot(data = Molecule_Agg_Revenue, aes(Date, Total)) +
  geom_line(aes(color=Company_Type),size=1) +
  labs(title = "Evolution of Revenue for Banned Molecules",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()

# 3> Molecule Aggregate Price: Monthly Trends ##############

Molecule_Agg_Price <- final_df_1 %>% group_by(Sub.Group,Date,Company_Type) %>% summarise(Total = mean(Price_per_mg,na.rm=T))

path <- 'Descriptive_Plots/EDS#4/National_Averages/Agg_Price_Monthly.jpeg'
jpeg(file = path,width = 15, height = 10, units = "in", res = 800, pointsize = 1/800)

ggplot(data = Molecule_Agg_Price, aes(Date, Total)) +
  geom_line(aes(color=Company_Type),size=1) +
  labs(title = "Evolution of Price(per mg) for Banned Molecules",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()


# 4> Molecule Aggregate Price: Monthly Trends ##############

Molecule_Agg_Price <- final_df_1  %>% group_by(Sub.Group,Date,Company_Type) %>%
                      filter(ASU_mg>0) %>% summarise(SKU = n_distinct(SKU))
path <- 'Descriptive_Plots/EDS#4/National_Averages/Agg_SKU_Monthly.jpeg'
jpeg(file = path,width = 15, height = 10, units = "in", res = 800, pointsize = 1/800)

ggplot(data = Molecule_Agg_Price, aes(Date, SKU)) +
  geom_line(aes(color=Company_Type),size=1) +
  labs(title = "Variety of drugs in Banned Molecules",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()

# 5> Molecule-wise HHI Index #######################33

Molecule_HH_Index <- final_df_1 %>% group_by(Sub.Group,Brand,Date,Company_Type) %>% summarise(Total = sum(ASU_mg,na.rm = T)) %>% 
                     group_by(Sub.Group,Date,Company_Type) %>% summarise(HHI = sum((Total/sum(Total,na.rm = T)*100)^2)) 


path <- 'Descriptive_Plots/EDS#4/National_Averages/HHI_Moecules.jpeg'
jpeg(file = path,width = 15, height = 10, units = "in", res = 800, pointsize = 1/800)

ggplot(data = Molecule_HH_Index, aes(Date, HHI)) +
  geom_line(aes(color=Company_Type),size=1) +
  labs(title = "Evolution of HHI for Banned Molecules",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()

# 5> Now only Plotting those Molecules for which faced ban during the period of 2008-13 #

List = c("CISAPRIDE","SIMETHICONE + CISAPRIDE","SIBUTRAMINE","GATIFLOXACIN","AMBROXOL + GATIFLOXACIN",
         "GATIFLOXACIN + ORNIDAZOLE","GATIFLOXACIN + METRONIDAZOLE","ROSIGLITAZONE","ROSIGLITAZONE + METFORMIN",
         "ROSIGLITAZONE + GLIMEPIRIDE","GLICLAZIDE + METFORMIN + ROSIGLITAZONE","GLIBENCLAMIDE + METFORMIN + ROSIGLITAZONE",
         "GLIMEPIRIDE + METFORMIN + ROSIGLITAZONE","TEGASEROD")

ASU_08_13 <- final_df_1 %>% filter(Sub.Group %in% List ) %>%
  group_by(Sub.Group,Date,Company_Type) %>% summarise(Total = sum(ASU_mg,na.rm = T), Price = mean(Price_per_mg,na.rm=T))

## Gatifloxacin : March 2011
## Cesapride : Feb 2011
## Rosiglitazone : November 2010
## Sibutramine : Feb 2011
## Tegaserod :  March 2011

### Now Adding the Ban Data to the Dataset  ##########
ASU_08_13 %>% distinct(Sub.Group)

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

ASU_08_13 <- ASU_08_13 %>% mutate(Ban_Date = case_when(Sub.Group %in% Cisapride ~ Cisa_ban, Sub.Group %in% GATIFLOXACIN ~ Gatiflox_ban,
                                                       Sub.Group %in% ROSIGLITAZONE ~ Rosiglit_ban, Sub.Group == "SIBUTRAMINE"~Sibutramine_ban,
                                                       Sub.Group == "TEGASEROD"~Tega_ban))


ASU_08_13$Ban_Date<- as.yearmon(ASU_08_13$Ban_Date,format = "%B %Y")


#### Sales Info ##############

path <- 'Descriptive_Plots/EDS#4/National_Averages/Drugs_08_13/ASU.jpeg'
jpeg(file = path,width = 12, height = 8, units = "in", res = 1200, pointsize = 1/1200)

ggplot(data = ASU_08_13, aes(Date, Total)) +
  geom_line(aes(color=Company_Type),size=1) +
  geom_vline(mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "red") + 
  geom_text(aes(x = Ban_Date,label = 'BAN',y = 500000), angle = 0,size=3) + 
  labs(title = "Evolution of Aggregate Sale Units(in mg) for Banned Molecules",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()                      

##### Price Info ##############33

path <- 'Descriptive_Plots/EDS#4/National_Averages/Drugs_08_13/Price.jpeg'
jpeg(file = path,width = 12, height = 8, units = "in", res = 1200, pointsize = 1/1200)

ggplot(data = ASU_08_13, aes(Date, Price)) +
  geom_line(aes(color=Company_Type),size=1) +
  geom_vline(mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "red") + 
  labs(title = "Evolution of Price (per mg) for Banned Molecules",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()    


#### SKU_Plot ################

SKU  <-  final_df_1 %>% filter(Sub.Group %in% List ) %>%
  group_by(Sub.Group,Date,Company_Type) %>% filter(ASU_mg>0) %>% summarise(SKU = n_distinct(SKU))

SKU <- SKU %>% mutate(Ban_Date = case_when(Sub.Group %in% Cisapride ~ Cisa_ban, Sub.Group %in% GATIFLOXACIN ~ Gatiflox_ban,
                                           Sub.Group %in% ROSIGLITAZONE ~ Rosiglit_ban, Sub.Group == "SIBUTRAMINE"~Sibutramine_ban,
                                           Sub.Group == "TEGASEROD"~Tega_ban))
SKU$Ban_Date <- as.yearmon(SKU$Ban_Date)

path <- 'Descriptive_Plots/EDS#4/National_Averages/Drugs_08_13/SKU.jpeg'
jpeg(file = path,width = 12, height = 8, units = "in", res = 1200, pointsize = 1/1200)

ggplot(data = SKU, aes(Date, SKU)) +
  geom_line(aes(color=Company_Type),size=1) +
  geom_vline(mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "red") + 
  labs(title = "Evolution of Distinct SKU's for Banned Molecules",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()    

#### HHI Index Plot ##################

HHI_Index <- final_df_1 %>% filter(Sub.Group %in% List) %>% group_by(Sub.Group,Brand,Date,Company_Type) %>% summarise(Total = sum(ASU_mg,na.rm = T)) %>%
            group_by(Sub.Group,Date,Company_Type) %>% summarise(HHI = sum((Total/sum(Total,na.rm = T)*100)^2)) 


HHI_Index <- HHI_Index %>% mutate(Ban_Date = case_when(Sub.Group %in% Cisapride ~ Cisa_ban, Sub.Group %in% GATIFLOXACIN ~ Gatiflox_ban,
                                                       Sub.Group %in% ROSIGLITAZONE ~ Rosiglit_ban, Sub.Group == "SIBUTRAMINE"~Sibutramine_ban,
                                                       Sub.Group == "TEGASEROD"~Tega_ban))
HHI_Index$Ban_Date <- as.yearmon(HHI_Index$Ban_Date)

path <- 'Descriptive_Plots/EDS#4/National_Averages/Drugs_08_13/HHI_Index.jpeg'
jpeg(file = path,width = 12,height = 8,units = "in",res = 1200,pointsize = 1/1200)

ggplot(data = HHI_Index, aes(Date, HHI)) +
  geom_line(aes(color=Company_Type),size=1) +
  geom_vline(mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "red") + 
  labs(title = "HHI for Banned Molecules before and after the ban",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()


##### Plotting Info on Sibutramine and Orlistat

#---- Importing the Control Data------------------------#
Control_df <- read.csv("Data/Control_df_Rosig_Sibutram.csv", row.names=1)

Control_df <- data.table(Control_df)
Control_df <- Control_df[!(Control_df$State =="ALL INDIA ONLY")]

lapply(Control_df,class)   # Checking the classes of all the columns

## Repairing the Date Format
Control_df$Date <- as.yearmon(Control_df$Date,format = "%B %Y")

## Seperate Datasets Ready ##
Sibutramine <- final_df %>% filter(Sub.Group=="ROSIGLITAZONE")
Orlistat <- Control_df %>% filter(Sub.Group=="PIOGLITAZONE")

# 1> ASU_MG and Price Comparison

ASU_SIbutramine <- Sibutramine %>% group_by(Sub.Group,Date) %>% summarise(Total = sum(ASU_mg,na.rm = T), Price = mean(Price_per_mg,na.rm=T))
ASU_Orlistat <- Orlistat %>% group_by(Sub.Group,Date) %>% summarise(Total = sum(ASU_mg,na.rm = T), Price = mean(Price_per_mg,na.rm=T))

Control_1 <- rbind(ASU_Orlistat,ASU_SIbutramine)
Control_1$Date <- as.yearmon(Control_1$Date,format = "%B %Y")

Control_1$Ban_Date <- as.yearmon("Nov 2010" ,format = "%B %Y")
Control_1$Ban_Date<- as.yearmon(Control_1$Ban_Date,format = "%B %Y")

path <- 'Descriptive_Plots/EDS#4/National_Averages/Control_Example/Rosiglitazone/ASU.jpeg'
jpeg(file = path,width = 8,height = 8,units = "in",res = 800,pointsize = 1/800)

ggplot() +   
  geom_line(data=Control_1, aes(x=Date,y= Total, color=Sub.Group),size=1.2) +
  labs(x="Date",y="ASU (in mg)") +
  geom_vline(data = Control_1,mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "red")  +
  geom_text(data = Control_1,aes(x = Ban_Date,label = 'BAN',y = 2*10^8), angle = 0,size=3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" ) + 
  theme_economist() + scale_colour_economist()

dev.off()

path <- 'Descriptive_Plots/EDS#4/National_Averages/Control_Example/Rosiglitazone/Price.jpeg'
jpeg(file = path,width = 8,height = 8,units = "in",res = 800,pointsize = 1/800)

ggplot() +   
  geom_line(data=Control_1, aes(x=Date,y= Price, color=Sub.Group),size=1.2) +
  labs(x="Date",y="Price per mg") +
  geom_vline(data = Control_1,mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "red")  +
  geom_text(data = Control_1,aes(x = Ban_Date,label = 'BAN',y = 1), angle = 0,size=3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" ) + ylim(c(0,2.5)) + 
  theme_economist() + scale_colour_economist()

dev.off()


# 2> SKU's
ASU_SIbutramine <- Sibutramine %>%  filter(ASU_mg>0)  %>%group_by(Sub.Group,Date)  %>% summarise(Total = n_distinct(SKU))
ASU_Orlistat <- Orlistat %>% filter(ASU_mg>0) %>%  group_by(Sub.Group,Date) %>% summarise(Total = n_distinct(SKU))
Control_1 <- rbind(ASU_Orlistat,ASU_SIbutramine)
Control_1$Date <- as.yearmon(Control_1$Date,format = "%B %Y")

Control_1$Ban_Date <- as.yearmon("Nov 2010" ,format = "%B %Y")
Control_1$Ban_Date<- as.yearmon(Control_1$Ban_Date,format = "%B %Y")

path <- 'Descriptive_Plots/EDS#4/National_Averages/Control_Example/Rosiglitazone/SKU.jpeg'
jpeg(file = path,width = 8,height = 8,units = "in",res = 800,pointsize = 1/800)

ggplot() +   
  geom_line(data=Control_1, aes(x=Date,y= Total, color=Sub.Group),size=1.2) +
  labs(x="Date",y="No. of SKU's") +
  geom_vline(data = Control_1,mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "red")  +
  geom_text(data = Control_1,aes(x = Ban_Date,label = 'BAN',y = 50), angle = 0,size=3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" ) + ylim(c(0,150)) + 
  theme_economist() + scale_colour_economist()

dev.off()

#3> HHI Index 

ASU_SIbutramine <- Sibutramine %>% group_by(Sub.Group,Brand,Date) %>% summarise(Total = sum(ASU_mg,na.rm = T)) %>%
  group_by(Sub.Group,Date) %>% summarise(HHI = sum((Total/sum(Total,na.rm = T)*100)^2))
ASU_Orlistat <- Orlistat %>% filter(ASU_mg>0) %>%  group_by(Sub.Group,Brand,Date) %>% summarise(Total = sum(ASU_mg,na.rm = T)) %>%
  group_by(Sub.Group,Date) %>% summarise(HHI = sum((Total/sum(Total,na.rm = T)*100)^2))

Control_1 <- rbind(ASU_Orlistat,ASU_SIbutramine)
Control_1$Date <- as.yearmon(Control_1$Date,format = "%B %Y")

Control_1$Ban_Date <- as.yearmon("Nov 2010" ,format = "%B %Y")
Control_1$Ban_Date<- as.yearmon(Control_1$Ban_Date,format = "%B %Y")

path <- 'Descriptive_Plots/EDS#4/National_Averages/Control_Example/Rosiglitazone/HHI.jpeg'
jpeg(file = path,width = 8,height = 8,units = "in",res = 800,pointsize = 1/800)

ggplot() +   
  geom_line(data=Control_1, aes(x=Date,y= HHI, color=Sub.Group),size=1.2) +
  labs(x="Date",y="HHI") +
  geom_vline(data = Control_1,mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "red")  +
  geom_text(data = Control_1,aes(x = Ban_Date,label = 'BAN',y = 5), angle = 0,size=3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" ) +  
  theme_economist() + scale_colour_economist()

dev.off()

## Labelling Companies by Domestic vis-a-vis MNC's ##

Companies <- final_df %>% distinct(Company)
Company_Status <- c("M","M","D","D","D","D","D","D","D","D","D","D","M","D","D","D","D","D","D","D",
                    "D","M","D","D","D","D","M","D","D","D","D","D","D","D","D","D","D","D","D","D",
                    "D","D","D","D","D","D","D","D","M","D","D","D","D","D","D","M","D","D","D","D",
                    "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D",
                    "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D",
                    "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D",
                    "D","D","D","D","D","D","D","D","M","M","D","D","D","D","D","D","D","D","D","D",
                    "D","D","M","D","M","D","D","M","D","D","M","D","D","D","D","D","D","D","D","D",
                    "D","D","D","D","D","D","D","D","D","D","D","D","M","D")


Companies$Company_Type <- Company_Status

Companies <- Companies %>% mutate(Company_Type = case_when(Company_Type == "D"~"Domestic", Company_Type=="M"~"MNC"))


final_df_1 <- merge(final_df,Companies,all.x = T,all.y = T,by="Company")

# National_Averages after decomposing into Domestic and MNC's are plotted above.


###### Breaking Down by the States ########################

# Disaggregating the Molecule Level Info by States and then Plotting National Time Trends
State_dis_def <- final_df_1 %>% distinct(State) ## List of all States


# 5.1> Method to merge Sales and Revenue Info on one plot using two y-axis and doing the analysis by States using a for loop

State_df <- final_df_1 %>% filter(Sub.Group %in% List ) %>% group_by(State,Sub.Group,Date,Company_Type) %>% summarise(Total=sum(ASU_mg,na.rm = T)) %>% mutate(Description = "ASU(in mg)")
State_df <- State_df %>% mutate(Ban_Date = case_when(Sub.Group %in% Cisapride ~ Cisa_ban, Sub.Group %in% GATIFLOXACIN ~ Gatiflox_ban,
                                                       Sub.Group %in% ROSIGLITAZONE ~ Rosiglit_ban, Sub.Group == "SIBUTRAMINE"~Sibutramine_ban,
                                                       Sub.Group == "TEGASEROD"~Tega_ban))


State_df$Ban_Date<- as.yearmon(State_df$Ban_Date,format = "%B %Y")




for( i in (State_dis_def$State)){
  
  data <- subset(State_df,State ==i)

  
  path <- paste('Descriptive_Plots/EDS#4/By_States/2007-13/ASU/ASU_Revenue',i,".jpeg")
  jpeg(file = path,width = 14, height = 10, units = "in", res = 700, pointsize = 1/700)
  if(data$Sub.Group %notin% List){
  g<- ggplot(data, aes(x=Date, y=Total)) +
    geom_line(aes(color=Company_Type),size=0.8) +
    facet_wrap(~ Sub.Group,scales =  "free") +
    labs(title = paste("Evolution of total sales (in mg) for molecules in state",i))+
    theme_economist_white(base_size = 6) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_yearmon(format ="%b %y" )
  }
  else{
    g <- ggplot(data, aes(Date, Total)) +
      geom_line(aes(color=Company_Type),size=0.8) +
      geom_vline(mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "green4") + 
      geom_text(aes(x = Ban_Date,label = 'BAN',y = 500000), angle = 0,size=3) + 
      labs(title = paste("Evolution of total sales (in mg) for molecules in state",i)) +
      facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
    
  }
  
  print(g)
  dev.off()
}

  

#5.2) Method to plot only Price(in mg) Info by states seperately using for loop

State_df <- final_df_1 %>% filter(Sub.Group %in% List ) %>% group_by(State,Sub.Group,Date,Company_Type) %>% summarise(Total=mean(Price_per_mg,na.rm = T)) %>% mutate(Description = "ASU(in mg)")
State_df <- State_df %>% mutate(Ban_Date = case_when(Sub.Group %in% Cisapride ~ Cisa_ban, Sub.Group %in% GATIFLOXACIN ~ Gatiflox_ban,
                                                     Sub.Group %in% ROSIGLITAZONE ~ Rosiglit_ban, Sub.Group == "SIBUTRAMINE"~Sibutramine_ban,
                                                     Sub.Group == "TEGASEROD"~Tega_ban))


State_df$Ban_Date<- as.yearmon(State_df$Ban_Date,format = "%B %Y")

State_df$Total[is.nan(State_df$Total)] <- NA



for( i in (State_dis_def$State)){
  
  data <- subset(State_df,State ==i)
  
  
  path <- paste('Descriptive_Plots/EDS#4/By_States/2007-13/Price/Price',i,".jpeg")
  jpeg(file = path,width = 14, height = 10, units = "in", res = 700, pointsize = 1/700)
  if(data$Sub.Group %notin% List){
    g<- ggplot(data, aes(x=Date, y=Total)) +
      geom_line(aes(color=Company_Type),size=0.8) +
      facet_wrap(~ Sub.Group,scales =  "free") +
      labs(title = paste("Evolution of price per mg for molecules in state",i))+
      theme_economist_white(base_size = 6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
  }
  else{
    g <- ggplot(data, aes(Date, Total)) +
      geom_line(aes(color=Company_Type),size=0.8) +
      geom_vline(mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "green4") +       labs(title = paste("Evolution of price per mg for molecules in state",i)) +
      facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
    
  }
  
  print(g)
  dev.off()
}

#5.3) Method to plot only SKU Info by states seperately using for loop

State_df <- final_df_1 %>% filter(Sub.Group %in% List ) %>% group_by(State,Sub.Group,Date,Company_Type) %>% filter(ASU_mg>0) %>%  summarise(Total=n_distinct(SKU))
State_df <- State_df %>% mutate(Ban_Date = case_when(Sub.Group %in% Cisapride ~ Cisa_ban, Sub.Group %in% GATIFLOXACIN ~ Gatiflox_ban,
                                                     Sub.Group %in% ROSIGLITAZONE ~ Rosiglit_ban, Sub.Group == "SIBUTRAMINE"~Sibutramine_ban,
                                                     Sub.Group == "TEGASEROD"~Tega_ban))
State_df$Ban_Date <- as.yearmon(State_df$Ban_Date,format = "%B %Y")
State_df$Date  <- as.yearmon(State_df$Date,format = "%B %Y")

for( i in (State_dis_def$State)){
  
  data <- subset(State_df,State==i)
  
  path <- paste('Descriptive_Plots/EDS#4/By_States/2007-13/SKU/SKU',i,".jpeg")
  jpeg(file = path,width = 14, height = 10, units = "in", res = 700, pointsize = 1/700)
  if(data$Sub.Group %notin% List){
    g<- ggplot(data, aes(x=Date, y=Total)) +
      geom_line(aes(color=Company_Type),size=0.8) +
      facet_wrap(~ Sub.Group,scales =  "free") +
      labs(title = paste("Variety in molecules in state",i))+
      theme_economist_white(base_size = 6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
  }
  else{
    g <- ggplot(data, aes(Date, Total)) +
      geom_line(aes(color=Company_Type),size=0.8) +
      geom_vline(mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "green4") +
      labs(title = paste("Variety in molecules in state",i)) +
      facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
    
  }
  print(g)
  dev.off()
}


# 5.4)Method to plot only HHI Info by states seperately using for loop

State_df <- final_df_1 %>% filter(Sub.Group %in% List ) %>% group_by(State,Sub.Group,Brand,Date,Company_Type) %>% summarise(Total = sum(ASU_mg,na.rm = T)) %>%
  group_by(State,Sub.Group,Date,Company_Type) %>% summarise(HHI = sum((Total/sum(Total,na.rm = T)*100)^2,na.rm = T)) 

State_df <- State_df %>% mutate(Ban_Date = case_when(Sub.Group %in% Cisapride ~ Cisa_ban, Sub.Group %in% GATIFLOXACIN ~ Gatiflox_ban,
                                                     Sub.Group %in% ROSIGLITAZONE ~ Rosiglit_ban, Sub.Group == "SIBUTRAMINE"~Sibutramine_ban,
                                                     Sub.Group == "TEGASEROD"~Tega_ban))

State_df$Date  <- as.yearmon(State_df$Date,format = "%B %Y")
State_df$Ban_Date <- as.yearmon(State_df$Ban_Date,format = "%B %Y")

for( i in (State_dis_def$State)){
  
  data <- subset(State_df,State==i)
  
  path <- paste('Descriptive_Plots/EDS#4/By_States/2007-13/HHI/HHI',i,".jpeg")
  jpeg(file = path,width = 14, height = 10, units = "in", res = 700, pointsize = 1/700)
  
  if(data$Sub.Group %notin% List){
    g<- ggplot(State_df, aes(x=Date, y=HHI)) +
      geom_line(aes(color=Company_Type),size=0.8) +
      facet_wrap(~ Sub.Group,scales =  "free") +
      labs(title = paste("HHI of molecule in state",i))+
      theme_economist_white(base_size = 6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
  }
  else{
    g <- ggplot(data, aes(Date, HHI)) +
      geom_line(aes(color=Company_Type),size=0.8) +
      geom_vline(mapping = aes(xintercept = Ban_Date),linetype = 4,size = 1,colour = "green4") +
      labs(title = paste("HHI of molecule in state",i)) +
      facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
    
  }
  print(g)
  dev.off()
}


################### ANALYSIS ENDS ############################################



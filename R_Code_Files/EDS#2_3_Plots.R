############  Exploratory Data Analysis-2: For CELECOXIB Molecule  ##############################

#----------Importing Libraries-----------------#

lapply(c("XLConnect","GGally","tidyverse","visNetwork","magrittr","DiagrammeR","data.table",
         "plotly","intergraph","networkD3","optrees","disparityfilter","network","Matrix","igraph"
         ,"CINNA","ggplot2","poweRlaw","devtools","ForceAtlas2","remotes","netdiffuseR","lfe","stargazer","foreign","MASS","lubridate","ggthemes","readtext","zoo"), require,character.only = TRUE)

#---- Setting WD--------------------------------#

library(tidyverse)
library(ggthemes)
library(data.table)

setwd("F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma")

#---- Importing the Data------------------------#

Aggregate_COXIB <- read.csv("F:/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/Aggregate_COXIB_2.csv", row.names=1)

final_df <- data.table(Aggregate_COXIB)
final_df <- final_df[!(final_df$State=="ALL INDIA ONLY")]

#---- Creating National Averages----------------#

# 1> Molecule Aggregate Sale: Yearly Trends ##############3

Molecule_Agg_Sale <- final_df %>% group_by(Sub.Group,Date) %>% summarise(Total = sum(ASU_mg,na.rm = T))
Molecule_Agg_Sale$Date  <- as.yearmon(Molecule_Agg_Sale$Date,format = "%B %Y")


path <- '/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/ASU_Monthly.jpeg'
jpeg(file = path,width = 10, height = 8.5, units = "in", res = 1200, pointsize = 1/1200)


ggplot(data = Molecule_Agg_Sale, aes(Date, Total)) +
  geom_line(color = "grey", size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Evolution of Aggregate Sale Units(in mg) for Sub-groups within COX-2 Inhibitors",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )
dev.off()

# 2> Molecule Aggregate Revenue: Yearly Trends ###########3


Molecule_Agg_Revenue <- final_df %>% group_by(Sub.Group,Date) %>% summarise(Total = sum(Revenue,na.rm = T))
Molecule_Agg_Revenue$Date  <- as.yearmon(Molecule_Agg_Revenue$Date,format = "%B %Y")


path <- '/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/AR_Monthly.jpeg'
jpeg(file = path,width = 10, height = 8.5, units = "in", res = 1200, pointsize = 1/1200)


ggplot(data = Molecule_Agg_Revenue, aes(Date, Total)) +
  geom_line(color = "grey", size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Evolution of Aggregate Revenue for Sub-groups within COX-2 Inhibitors",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()

# 3> Molecule Average Price: Yearly Trends ###########3

Molecule_Agg_Price<- final_df %>% group_by(Sub.Group,Year) %>% summarise(Total = mean(Price_per_mg,na.rm=T))

path <- '/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/Average_Price_by_SC.jpeg'
jpeg(file = path,width = 10, height = 8.5, units = "in", res = 1200, pointsize = 1/1200)


ggplot(data = Molecule_Agg_Price, aes(Year, Total)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Evolution of Average Price (per mg)for Sub-groups within COX-2 Inhibitors",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6)

dev.off()

# 3> Molecule Average SKU's: Yearly Trends ###########3

Molecule_Agg_SKU <- final_df %>% group_by(Sub.Group,Date) %>% filter(ASU_mg > 0 )  %>% summarise(Total = n_distinct(SKU,na.rm = T))
Molecule_Agg_SKU$Date <- as.yearmon(Molecule_Agg_SKU$Date,format = "%B %Y")



path <- '/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/SKU_Monthly.jpeg'
jpeg(file = path,width = 10, height = 8.5, units = "in", res = 1200, pointsize = 1/1200)


ggplot(data = Molecule_Agg_SKU, aes(Date, Total)) +
  geom_line(color = "grey", size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Evolution of distinct SKU's for Sub-groups within COX-2 Inhibitors",
       y = " ", x = "")  +
  facet_wrap(~ Sub.Group,scales =  "free") + theme_economist_white(base_size = 6) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_yearmon(format ="%b %y" )

dev.off()


# 4> Disaggregating the Molecule Level Info by Firms and then Plotting National Time Trends
Company_dis_df <- final_df %>% distinct(Company) ## List of all companies

#4.1) Method to plot Sales Info by companies seperately using for loop

Company_df <- final_df %>% group_by(Company,Sub.Group,Year) %>% summarise(Total=sum(ASU_mg,na.rm = T),Revenue = sum(Revenue,na.rm = T),Price=mean(Price_per_mg, na.rm=T))


for( i in (Company_dis_df$Company)){
  
  data <- subset(Company_df,Company==i)
  
  
  path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_Firms/Total_Sales/',i,".jpeg")
  jpeg(file = path,width = 10, height = 8.5, units = "in", res = 700, pointsize = 1/700)
  
  g<- ggplot(data = data,aes(Year, Total)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color="steelblue") + 
    labs(title = paste("Evolution of total sales (in mg) for molecules in company",i),y = " ", x = "")  +
    facet_wrap(~ Sub.Group,scales =  "free") + 
    theme_economist_white(base_size = 6) 
  print(g)
  dev.off()
  
}

#4.2) Method to plot only Revenue Info by companies seperately using for loop

for( i in (Company_dis_df$Company)){
  
  data <- subset(Company_df,Company==i)
  
  path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_Firms/Total_Revenue/',i,".jpeg")
  jpeg(file = path,width = 10, height = 8.5, units = "in", res = 700, pointsize = 1/700)
  
  g<- ggplot(data = data,aes(Year, Revenue)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color="steelblue") + 
    labs(title = paste("Evolution of total revenue  for molecules in company",i),y = " ", x = "")  +
    facet_wrap(~ Sub.Group,scales =  "free") + 
    theme_economist_white(base_size = 6) 
  print(g)
  dev.off()
  
}

#4.3) Method to merge Sales and Revenue Info on one plot using two y-axis and doing the analysis by companies using a for loop

Company_df <- final_df %>% group_by(Company,Sub.Group,Date) %>% summarise(Total=sum(ASU_mg,na.rm = T)) %>% mutate(Description = "ASU(in mg)")
Company_df_1 <- final_df %>% group_by(Company,Sub.Group,Date) %>% summarise(Total=sum(Revenue,na.rm = T)) %>% mutate(Description = "Revenue")

max_ASU_sales <- max(Company_df$Total)
max_Revenue <- max(Company_df_1$Total)
prop <- max_Revenue/max_ASU_sales
Company_df_1$Total <- Company_df_1$Total/prop
Company_df_2 <- rbind(Company_df,Company_df_1)

Company_df_2$Date  <- as.yearmon(Company_df_2$Date,format = "%B %Y")


for( i in (Company_dis_df$Company)){
  
  data <- subset(Company_df_2,Company==i)
  mycolors <- c("ASU(in mg)"="blue", "Revenue"="red")
  
  
  path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_Firms/Monthly_Plots/Total_Sales_Revenue/',i,".jpeg")
  jpeg(file = path,width = 10, height = 8.5, units = "in", res = 700, pointsize = 1/700)
  
  g<- ggplot(data, aes(x=Date, y=Total, group=Description, color=Description)) +
    geom_line(size=0.7) +
    geom_point(size=0.7) +
    scale_y_continuous(name="ASU(in mg)", sec.axis = sec_axis(trans = ~ .*(prop), name="Revenue")) +
    scale_color_manual(name="Description", values = mycolors) +
    facet_wrap(~ Sub.Group,scales =  "free") +
    labs(title = paste("Evolution of total sales (in mg) and Revenue for molecules in company",i))+
    theme(
      axis.title.y = element_text(color = mycolors["ASU(in mg)"]),
      axis.text.y = element_text(color = mycolors["ASU(in mg)"]),
      axis.title.y.right = element_text(color = mycolors["Revenue"]),
      axis.text.y.right = element_text(color = mycolors["Revenue"]),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
   
  print(g)
  dev.off()
}



#4.4) Method to merge Average Price(in mg) and SKU's Info on one plot using two y-axis and doing the analysis by companies using a for loop

# Company_df <- final_df %>% group_by(Company,Sub.Group,Year) %>% summarise(Total=mean(Price_per_mg,na.rm = T)) %>% mutate(Description = "Price(in mg)")
# Company_df_1 <- final_df %>% group_by(Company,Sub.Group,Year) %>% summarise(Total = n_distinct(SKU)) %>% mutate(Description = "SKU's")
# 
# Company_df$Total[is.nan(Company_df$Total)] <- NA
# 
# max_ASU_sales <- max(Company_df$Total,na.rm = T)
# max_Revenue <- max(Company_df_1$Total)
# prop <- max_Revenue/max_ASU_sales
# Company_df_1$Total <- Company_df_1$Total/prop
# Company_df_2 <- rbind(Company_df,Company_df_1)
# 
# for( i in (Company_dis_df$Company)){
#   
#   data <- subset(Company_df_2,Company==i)
#   mycolors <- c("Price(in mg)"="blue", "SKU's"="red")
#   
#   
#   path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_Firms/Total_SKU_Price/',i,".jpeg")
#   jpeg(file = path,width = 10, height = 8.5, units = "in", res = 700, pointsize = 1/700)
#   
#   g<- ggplot(data, aes(x=Year, y=Total, group=Description, color=Description)) +
#     geom_path() +
#     geom_point() +
#     scale_y_continuous(name="Price(in mg)", sec.axis = sec_axis(trans = ~ .*(prop), name="SKU's")) +
#     scale_color_manual(name="Description", values = mycolors) +
#     facet_wrap(~ Sub.Group,scales =  "free") +
#     labs(title = paste("Evolution of Price(in mg) and SKU's for molecules in company",i))+
#     theme(
#       axis.title.y = element_text(color = mycolors["Price(in mg)"]),
#       axis.text.y = element_text(color = mycolors["Price(in mg)"]),
#       axis.title.y.right = element_text(color = mycolors["SKU's"]),
#       axis.text.y.right = element_text(color = mycolors["SKU's"])
#     ) 
#   print(g)
#   dev.off()
# }


#4.4) Method to plot only Price(in mg) Info by companies seperately using for loop

Company_df <- final_df %>% group_by(Company,Sub.Group,Date) %>% summarise(Total=mean(Price_per_mg,na.rm = T))
Company_df$Total[is.nan(Company_df$Total)] <- NA
Company_df$Date  <- as.yearmon(Company_df$Date,format = "%B %Y")


for( i in (Company_dis_df$Company)){
  
  data <- subset(Company_df,Company==i)
  
  path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_Firms/Monthly_Plots/Total_Price/',i,".jpeg")
  jpeg(file = path,width = 10, height = 8.5, units = "in", res = 700, pointsize = 1/700)
  
  g<- ggplot(data = data,aes(Date, Total)) +
    geom_line(color = "grey", size = 0.7) +
    geom_point(color="steelblue",size=0.7) + 
    labs(title = paste("Evolution of Price(in mg)  for molecules in company",i),y = " ", x = "")  +
    facet_wrap(~ Sub.Group,scales =  "free") + 
    theme_economist_white(base_size = 6)  +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_yearmon(format ="%b %y" )
  print(g)
  dev.off()
}

#4.5) Method to plot only SKU (info) Info by companies seperately using for loop

Company_df <- final_df %>% group_by(Company,Sub.Group,Date) %>% filter(ASU_mg > 0) %>% summarise(Total=n_distinct(SKU))
Company_df$Date  <- as.yearmon(Company_df$Date,format = "%B %Y")


for( i in (Company_dis_df$Company)){
  
  data <- subset(Company_df,Company==i)
  
  path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_Firms/Monthly_Plots/Total_SKU/',i,".jpeg")
  jpeg(file = path,width = 6, height = 6, units = "in", res = 700, pointsize = 1/700)
  
  g<- ggplot(data = data , aes(Date, Total,group=1)) +
    geom_line(color = "steelblue", size = 0.6) +
    geom_point(color="blue",size = 0.7) +  
    labs(title = paste("Yearly variation over number of SKU's for molecules in company",i),y = " ", x = "")  +
    facet_wrap(~ Sub.Group,scales =  "free") + 
    theme_economist_white(base_size = 6) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_yearmon(format ="%b %y" )
  
    
  print(g)
  dev.off()
}


# 5> Disaggregating the Molecule Level Info by States and then Plotting National Time Trends
State_dis_def <- final_df %>% distinct(State) ## List of all States


# 5.1> Method to merge Sales and Revenue Info on one plot using two y-axis and doing the analysis by States using a for loop

State_df <- final_df %>% group_by(State,Sub.Group,Date) %>% summarise(Total=sum(ASU_mg,na.rm = T)) %>% mutate(Description = "ASU(in mg)")
State_df_1 <- final_df %>% group_by(State,Sub.Group,Date) %>% summarise(Total=sum(Revenue,na.rm = T)) %>% mutate(Description = "Revenue")

max_ASU_sales <- max(State_df$Total)
max_Revenue <- max(State_df_1$Total)
prop <- max_Revenue/max_ASU_sales
State_df_1$Total <- State_df_1$Total/prop
State_df_2 <- rbind(State_df,State_df_1)
State_df_2$Date  <- as.yearmon(State_df_2$Date,format = "%B %Y")


for( i in (State_dis_def$State)){
  
  data <- subset(State_df_2,State ==i)
  mycolors <- c("ASU(in mg)"="blue", "Revenue"="red")
  
  
  path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_States/Monthly_Plots/Total_Sales_Revenue/',i,".jpeg")
  jpeg(file = path,width = 14, height = 10, units = "in", res = 700, pointsize = 1/700)
  
  g<- ggplot(data, aes(x=Date, y=Total, group=Description, color=Description)) +
    geom_line(size=0.7) +
    geom_point(size=0.7) +
    scale_y_continuous(name="ASU(in mg)", sec.axis = sec_axis(trans = ~ .*(prop), name="Revenue")) +
    scale_color_manual(name="Description", values = mycolors) +
    facet_wrap(~ Sub.Group,scales =  "free") +
    labs(title = paste("Evolution of total sales (in mg) and Revenue for molecules in state",i))+
    theme(
      axis.title.y = element_text(color = mycolors["ASU(in mg)"]),
      axis.text.y = element_text(color = mycolors["ASU(in mg)"]),
      axis.title.y.right = element_text(color = mycolors["Revenue"]),
      axis.text.y.right = element_text(color = mycolors["Revenue"]),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_x_yearmon(format ="%b %y" )
  print(g)
  dev.off()
}

#5.2) Method to plot only Price(in mg) Info by states seperately using for loop

State_df <- final_df %>% group_by(State,Sub.Group,Date) %>% summarise(Total=mean(Price_per_mg,na.rm = T))
State_df$Total[is.nan(State_df$Total)] <- NA
State_df$Date  <- as.yearmon(State_df$Date,format = "%B %Y")

for( i in (State_dis_def$State)){
  
  data <- subset(State_df,State==i)
  
  path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_States/Monthly_Plots/Total_Price/',i,".jpeg")
  jpeg(file = path,width = 10, height = 8.5, units = "in", res = 700, pointsize = 1/700)
  
  g<- ggplot(data = data,aes(Date, Total)) +
    geom_line(color = "grey", size = 0.7) +
    geom_point(color="steelblue",size=0.7) + 
    labs(title = paste("Evolution of Price(in mg)  for molecules in state",i),y = " ", x = "")  +
    facet_wrap(~ Sub.Group,scales =  "free") + 
    theme_economist_white(base_size = 6) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_yearmon(format ="%b %y" )
  print(g)
  dev.off()
}

#5.3) Method to plot only SKU Info by states seperately using for loop

State_df <- final_df %>% group_by(State,Sub.Group,Date) %>% filter(ASU_mg>0) %>%  summarise(Total=n_distinct(SKU))
State_df$Date  <- as.yearmon(State_df$Date,format = "%B %Y")

for( i in (State_dis_def$State)){
  
  data <- subset(State_df,State==i)
  
  path <- paste('/IIM-Ahmedabad/CMHS/Project_Innovation_Pharma/EDS#3/By_States/Monthly_Plots/Total_SKU/',i,".jpeg")
  jpeg(file = path,width = 10, height = 8.5, units = "in", res = 700, pointsize = 1/700)
  
  g<- ggplot(data = data,aes(Date, Total)) +
    geom_line(color = "grey", size = 0.7) +
    geom_point(color="steelblue",size=0.7) + 
    labs(title = paste("Yearly variation over number of SKU's for molecules in state",i),y = " ", x = "")  +
    facet_wrap(~ Sub.Group,scales =  "free") + 
    theme_economist_white(base_size = 6) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_yearmon(format ="%b %y" )
  print(g)
  dev.off()
}


################## Analysis Ends ########################################################
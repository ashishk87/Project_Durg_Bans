A1 <- tibble( vars = c("All India","Women in Non-disadvantaged HH in LPS","Women in disadvantaged HH in HPS","All women in LPS"),
              coeff = c(0.04,0.05,0.01,0.04),
              conf.low = c(0.03,0.04,-0.006,0.02),
              conf.high = c(0.05,0.06,0.03, 0.06 ))


A2 <- tibble(vars = c("No Effect","Downward Level Shift","Upward Level Shift",
                      "Zero Sales","Downward Dynamic Shift","Upward Dynamic Shift","Sales Rebound","No Effect","Downward Level Shift","Upward Level Shift",
                      "Zero Sales","Downward Dynamic Shift","Upward Dynamic Shift","Sales Rebound"),
             Percentage = c(34.2,26.7,4.3,26.5,3.7,3.1,1.5,25,18.75,0,56.25,0,0,0),
             Type = c("Domestic","Domestic","Domestic","Domestic","Domestic","Domestic","Domestic",
                      "MNC","MNC","MNC","MNC","MNC","MNC","MNC"))



ggplot(A2, aes(x=vars, y=Percentage, fill=vars)) +
  geom_bar(stat="identity",width=0.5)+
  scale_fill_manual(values  = c("palegreen4","salmon2","lightsteelblue3","skyblue1","tan2","palegreen3","lightgreen")) + 
  theme_bw(base_size = 16) + facet_wrap(~Type) + theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     legend.position="bottom",
                     legend.title = element_blank()) + labs(y = "Percentage", x = " ")

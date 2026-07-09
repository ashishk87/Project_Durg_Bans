library(stringdist)
library(RecordLinkage)
library(microbenchmark)


#--------------------  Reading the CSV Files--------------------------------------------##
All_M <- read.csv("C:/Users/Ashish/Desktop/Project_Banned_Drugs/Project_Banned_Drugs/Data/Data/CSV_Format/All_M.csv")
Banned_M <- read.csv("C:/Users/Ashish/Desktop/Project_Banned_Drugs/Project_Banned_Drugs/Data/Data/CSV_Format/Banned_M.csv", row.names=1)

#----------------- Cleaning the Data----------------------------------#####

Banned_M$Sub <- toupper(Banned_M$Sub.Groups)
Banned_M$Sub1 <- gsub("\\+"," ",Banned_M$Sub)


All_M$Sub <- gsub("\\+","" , All_M$SUBGROUP)

#--------- Partitioning the Banned Molecule Files into: ----------------#
#---------1> Individual Molecules: No single Moleculess banned --------------------------------------#
#---------2> Double Molecules --------------------------------------#
#---------3> Triplets          --------------------------------------#
#---------4> Quadruples


Banned_M <- Banned_M %>% separate(col = "Sub1",into = c("1","2","3","4","5","6","7","8"),remove = F)

All_M    <- All_M %>% separate(col = "SUBGROUP",into = c("1","2","3","4","5","6","7","8"),remove = F)
All_M <- data.table(All_M)
All_M$`2`[All_M$`2`== ""] <- NA
All_M$`3`[All_M$`3`== ""] <- NA
All_M$`4`[All_M$`4`== ""] <- NA
All_M$`5`[All_M$`5`== ""] <- NA
All_M$`6`[All_M$`6`== ""] <- NA
All_M$`7`[All_M$`7`== ""] <- NA
All_M$`8`[All_M$`8`== ""] <- NA

#------ Single Molecules-Treatment : 0-----------------------------------#
#------ Single Molecules-Master : 1253-----------------------------------#

Banned_1 <- Banned_M %>% filter( !is.na(`1`) & is.na(`2`) & is.na(`3`)& is.na(`4`)& is.na(`5`)& is.na(`6`)& is.na(`7`)& is.na(`8`))
All_1  <- All_M %>%   filter( !is.na(`1`) & is.na(`2`) & is.na(`3`)& is.na(`4`)& is.na(`5`)& is.na(`6`)& is.na(`7`)& is.na(`8`))


#------ Double Molecules: 58 -----------------------------------#
#------ Double Molecules-Master : 1253-----------------------------------#

Banned_2 <- Banned_M %>% filter( !is.na(`1`) & !is.na(`2`) & is.na(`3`)& is.na(`4`)& is.na(`5`)& is.na(`6`)& is.na(`7`)& is.na(`8`))
All_2  <- All_M %>%   filter( !is.na(`1`) & !is.na(`2`) & is.na(`3`)& is.na(`4`)& is.na(`5`)& is.na(`6`)& is.na(`7`)& is.na(`8`))

All_2 <- All_2[c(280:289),]
Banned_2 <- Banned_2[c(1:3),]
Banned_2[c(7),] <- c("AYURVEDIC MEDICINE","AYURVEDIC MEDICINE","AYURVEDIC MEDICINE","MEDICINE","AYURVEDIC",NA,NA,NA,NA,NA,NA)

for (item in 1:nrow(Banned_2)) {
  Word1 <- Banned_2$`1`[item]
  print(Word1)
  Word2 <- Banned_2$`2`[item]
    
    ## If the first word matches
    if(All_2$`1` == Word1 ){
      
      ## Then see if the second word matches
      
      if(All_2$`2` == Word2){
        
        All_2$Treatment[All_2$`1` == Word1] = 1 ## If both word matches then create a Treatment dummy and assign value 1
        Banned_2$Treatment[item] = 1 ## Also do that in the Ban data
      }else{
        ## This space corresponds to when Word1 gets a match but Word2 does not get a match:
        # So, Treatment Status will be NA in case of Ban data but not for master file (bcoz of possibility of future match)
         Banned_2$Treatment[item] = 0
       }
    
    }else if(All_2$`2` == Word1){
## Or see if the word matches with the word in the second column of master file
      
      ## Then see if the second word matches in the first column
      if(All_2$`1` ==Word2){
        
        All_2$Treatment[All_2$`1` == Word2] = 1 ## If both word matches then create a Treatment dummy and assign value 1
        Banned_2$Treatment[item] = 1 ## Also do that in the Ban data
      }else{
      # Again bcoz there is a single match
      
        Banned_2$Treatment[item] = 0 
      }
      
    }else{
  
  ### Case3: Word 1 Matches neither first column or second column- which means there is a no match
       Banned_2$Treatment[item] = 0}
    
}



#------------------------- The ABOVE FUNCTION DID'NT WORK.With the help of a function in python, I matched values and below I'll be-----------#
#----- importing the files and creating Treatment Status in both the Master and the Banned molecule file.-------------------------------------#

#------------------ Importing Library -------------------------------------------------##

library(stringdist)
library(RecordLinkage)
library(microbenchmark)
library(tidyverse)


#--------------------  Reading the CSV Files --------------------------------------------##
All_M <- read.csv("C:/Users/Ashish/Desktop/Project_Banned_Drugs/Project_Banned_Drugs/Data/Data/CSV_Format/All_M.csv")
Banned_M <- read.csv("C:/Users/Ashish/Desktop/Project_Banned_Drugs/Project_Banned_Drugs/Data/Data/CSV_Format/Banned_M.csv", row.names=1)

## Importing Matched File from Python: The cutoff of matching was 60%., so, need some cleaning
Matched_M <- read.csv("C:/Users/Ashish/Desktop/Project_Banned_Drugs/Project_Banned_Drugs/Data/Data/CSV_Format/Matched_Molecules.csv")
Matched_M <- Matched_M[-c(166),c(2,4)]
All_M_1 <- All_M %>% left_join(., Matched_M) %>% filter(Treatment==1) 
All_M_1 <- unique(All_M_1,by = c(1,2))


All_M_1 <- All_M_1 %>% group_by(SG.CODE) %>% mutate(A = sum(Treatment)) %>% group_by(SG.CODE) %>% distinct(A,.keep_all = T)
All_M_1 <- All_M_1[,-c(4)]
#-------------Saving the Master File with Treatment Status. AAround 100 drugs got matched -----------------------3

write.csv(All_M_1,"Banned_Drugs_List_Identifier.csv")

#----------------------------------------------------------------------------------------------------#

#----------------- Matching the Treatment Status to the Banned Drugs File: 88 unique combinations got mathed ---------------#
Matched_M <- Matched_M[-c(166),c(3,4)]
Matched_M <- Matched_M %>% rename("Sub.Groups" = "Banned_Molecule")
Banned_M$ID <- c(1:344) 
Banned_M_1 <- Banned_M %>% left_join(.,Matched_M)

Banned_M_1 <-  Banned_M_1 %>% 
  group_by(ID) %>% 
  filter(Treatment==max(Treatment)) 

Banned_M_2 <- merge(Banned_M,Banned_M_1,all.x = T) %>% distinct(ID,.keep_all = T)

write.csv(Banned_M_2,"Banned_M_Updated.csv")


#----------------- End of Do File-------------------------------------------------------------------------------#















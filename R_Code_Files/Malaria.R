
#-----------------------------------------------------------------------------------------------------------------------------#
#----------------------------------    Importing Libraries -------------------------------------------------------------------#
lapply(c("tidyverse","kableExtra","here","knitr","ggthemes","lfe","gt","did","xaringan","miceadds","sandwich","lmtest","patchwork",
         "bacondecomp","multcomp","fastDummies","magrittr","MCPanel","gganimate","gifski","zoo","remotes","data.table","fs"), require,character.only = TRUE)

`%notin%` <- Negate(`%in%`)


#--------------------------- Loading the Data -------------------------------------------------------------------------------#
df <- read_excel("antimalaria_jan15_jun20.xlsx",sheet = "Sheet1")
df <- data.table(df)

df$`Month-Year` <- as.yearmon(df$`Month-Year`,format = "%b-%y")
df$DATE <- as.yearmon("March 2020", format = "%B %Y")


#-------------------------- Generating Relevant Vriables --------------------------------------------------------------------#
#-- Treatment_H  = 1; when treatment group is HC, and 0 otherwise.
#-- Treatment_C = 1; when treatment group is C, and 0 otherwise.
#-- Treatment_H_C = 1; when treatment group is both HC and C; and 0 otherwise.
#-- Post = 1; if date variable is greater than Feb 2020; and 0 otherwise.
#-- Log_Revenue = log conversion of Sales_Value + 1 variable.

df <- df %>% mutate(Treatment_H = ifelse(`SG CODE`=="P1D2",1,0),
                    Treatment_C = ifelse(`SG CODE`=="P1D1",1,0),
                    Treatment_H_C = ifelse(`SG CODE` %in% c("P1D1","P1D2"),1,0),
                    Post = ifelse(`Month-Year`< DATE, 0,1),
                    Log_Revenue = log(`Sales Value` + 1))



#-- When HC is the only treatment molecule, I will exclude C from the control group, and dataset is df_H
df_H <- df[df$`SG CODE` != "P1D1"]


#- wHEN c is the only treatment molecule, I will exclude HC from the control group, and dataset is df_C
df_C <- df[df$`SG CODE` != "P1D2"]


#- When both HC and C are treated then original df will be the main dataset.



#-------------------------------------------------------------------------------------------------------------------------- #
#------------------------- Regressions at the National Level ---------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

# 1> When HC is the only treated molecule

df_H_N <- df_H %>% group_by(`SG CODE`,`Month-Year`) %>%
                  summarise(Post = mean(Post),Treatment = mean(Treatment_H), Log_Revenue = sum(Log_Revenue,na.rm = T) ,N_Firms = n_distinct(COMPANY)) %>%
                  mutate(Interaction = Post*Treatment)

# 2> When C is the only treated molecule

df_C_N <- df_C %>% group_by(`SG CODE`,`Month-Year`) %>%
  summarise(Post = mean(Post),Treatment = mean(Treatment_C), Log_Revenue = sum(Log_Revenue,na.rm = T) ,N_Firms = n_distinct(COMPANY)) %>%
  mutate(Interaction = Post*Treatment)


# 3> When C and HC both are treated molecules

df_N <- df  %>% group_by(`SG CODE`,`Month-Year`) %>%
  summarise(Post = mean(Post),Treatment = mean(Treatment_H_C), Log_Revenue = sum(Log_Revenue,na.rm = T) ,N_Firms = n_distinct(COMPANY)) %>%
  mutate(Interaction = Post*Treatment)


#------- Regressions Models at the National Level -------------------#

# 1> When HC is the only treated molecule
model_1 <-  felm(Log_Revenue ~ Interaction  | `SG CODE` + `Month-Year` |0 |`SG CODE`, 
                 data = df_H_N ,cmethod = "reghdfe")
summary(model_1)


# 2> When C is the only treated molecule
model_2 <-  felm(Log_Revenue ~ Interaction  | `SG CODE` + `Month-Year` |0 |`SG CODE`, 
                 data = df_C_N ,cmethod = "reghdfe")
summary(model_2)


# 3> When both C and HC are treated molecules
model_3 <-  felm(Log_Revenue ~ Interaction  | `SG CODE` + `Month-Year` |0 |`SG CODE`, 
                 data = df_N ,cmethod = "reghdfe")
summary(model_3)


#------------ Printing the LATEX Output --------------------------------------#

stargazer::stargazer(model_1,model_2,model_3,
                     title = "How U.S. President's promotion of CHLOROQUINE and HYDROXYCHLOROQUINE affected Indian Market",
                     table.placement = "H",column.labels = c("Panel A: HC","Panel B: C","Panel C: HC & C"),
                     covariate.labels = c("Treatment*Post"),font.size = "footnotesize",
                     add.lines = list(c("Molecule FE","Y","Y","Y","Y"),c("Time FE","Y","Y","Y","Y")),header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes=c("Robust clustered standard errors at the molecule level are provided in parentheses"))



#-------------------------------------------------------------------------------------------------------------------------- #
#------------------------- Regressions at the Sub-National Level ---------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#


# 1> When HC is the only treated molecule

df_H_R <- df_H %>% group_by(`SG CODE`,`Month-Year`,STATE) %>%
  summarise(Post = mean(Post),Treatment = mean(Treatment_H), Log_Revenue = sum(Log_Revenue,na.rm = T) ,N_Firms = n_distinct(COMPANY)) %>%
  mutate(Interaction = Post*Treatment)

df_H_R$molecule_geo <- (as.numeric(as.factor(df_H_R$`SG CODE`):as.factor(df_H_R$STATE)))


# 2> When C is the only treated molecule

df_C_R <- df_C %>% group_by(`SG CODE`,`Month-Year`,STATE) %>%
  summarise(Post = mean(Post),Treatment = mean(Treatment_C), Log_Revenue = sum(Log_Revenue,na.rm = T) ,N_Firms = n_distinct(COMPANY)) %>%
  mutate(Interaction = Post*Treatment)

df_C_R$molecule_geo <- (as.numeric(as.factor(df_C_R$`SG CODE`):as.factor(df_C_R$STATE)))


# 3> When C and HC both are treated molecules

df_R <- df  %>% group_by(`SG CODE`,`Month-Year`,STATE) %>%
  summarise(Post = mean(Post),Treatment = mean(Treatment_H_C), Log_Revenue = sum(Log_Revenue,na.rm = T) ,N_Firms = n_distinct(COMPANY)) %>%
  mutate(Interaction = Post*Treatment)

df_R$molecule_geo <- (as.numeric(as.factor(df_R$`SG CODE`):as.factor(df_R$STATE)))



#------- Regressions Models at the National Level -------------------#

#----- 1> When HC is the only treated molecule

#- (a) baseline model
model_1 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) |0 |molecule_geo, 
                 data = df_H_R ,cmethod = "reghdfe")
summary(model_1)

#- (b) with Interaction of Molecule*Geography FE's
model_1_1 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) + as.factor(molecule_geo) |0 |molecule_geo, 
                 data = df_H_R ,cmethod = "reghdfe")
summary(model_1_1)


#- (c) with Interaction of Interaction of Geography*Time FE's
model_1_2 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) + as.factor(STATE):as.factor(`Month-Year`) |0 |molecule_geo, 
                   data = df_H_R ,cmethod = "reghdfe")
summary(model_1_2)



#--- Printing LATEX Output ---------------#

stargazer::stargazer(model_1,model_1_1,model_1_2,
                     title = "How U.S. President's promotion of  HYDROXYCHLOROQUINE affected Indian Market",
                     table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","N_Firms"),font.size = "footnotesize", keep = c(1:2),
                     add.lines = list(c("Treatment*Time","Y","Y","Y"),c("Time FE","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y"),c("Geography Dummy","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N"),c("Geography*Time","N","N","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses")


# 2> When C is the only treated molecule
#- (a) baseline model
model_2 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) |0 |molecule_geo, 
                 data = df_C_R ,cmethod = "reghdfe")
summary(model_1)

#- (b) with Interaction of Molecule*Geography FE's
model_2_1 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) + as.factor(molecule_geo) |0 |molecule_geo, 
                   data = df_C_R ,cmethod = "reghdfe")
summary(model_2_1)


#- (c) with Interaction of Interaction of Geography*Time FE's
model_2_2 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) + as.factor(STATE):as.factor(`Month-Year`) |0 |molecule_geo, 
                   data = df_C_R ,cmethod = "reghdfe")
summary(model_2_2)



#--- Printing LATEX Output ---------------#

stargazer::stargazer(model_2,model_2_1,model_2_2,
                     title = "How U.S. President's promotion of  CHLOROQUINE affected Indian Market",
                     table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","N_Firms"),font.size = "footnotesize", keep = c(1:2),
                     add.lines = list(c("Treatment*Time","Y","Y","Y"),c("Time FE","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y"),c("Geography Dummy","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N"),c("Geography*Time","N","N","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses")

# 3> When both C and HC are treated molecules
#- (a) baseline model
model_3 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) |0 |molecule_geo, 
                 data = df_R ,cmethod = "reghdfe")
summary(model_3)

#- (b) with Interaction of Molecule*Geography FE's
model_3_1 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) + as.factor(molecule_geo) |0 |molecule_geo, 
                   data = df_R ,cmethod = "reghdfe")
summary(model_3_1)


#- (c) with Interaction of Interaction of Geography*Time FE's
model_3_2 <-  felm(Log_Revenue ~ Interaction + N_Firms + Treatment*as.factor(`Month-Year`) |   STATE + (`SG CODE`) + as.factor(STATE):as.factor(`Month-Year`) |0 |molecule_geo, 
                   data = df_C_R ,cmethod = "reghdfe")
summary(model_3_2)



#--- Printing LATEX Output ---------------#

stargazer::stargazer(model_3,model_3_1,model_3_2,
                     title = "How U.S. President's promotion of  HYDROXYCHLOROQUINE and CHLOROQUINE affected Indian Market",
                     table.placement = "H",
                     covariate.labels = c("Treatment*Post Ban","N_Firms"),font.size = "footnotesize", keep = c(1:2),
                     add.lines = list(c("Treatment*Time","Y","Y","Y"),c("Time FE","Y","Y","Y"),
                                      c("Molecule FE","Y","Y","Y"),c("Geography Dummy","Y","Y","Y"),
                                      c("Molecule*Geography","N","Y","N"),c("Geography*Time","N","N","Y")),
                     header=FALSE,no.space = TRUE, column.sep.width = "1pt",
                     notes = "Robust clustered standard errors at the molecule-geography level are provided in parentheses")



#-------------- End ------------------------------------#


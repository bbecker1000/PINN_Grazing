library(tidyverse)
library(readr)
library(readxl)
library(vegan)
library(ggordiplots)
library(ggrepel)
library(cowplot)
library(lme4)
library(glmmTMB)
library(sjPlot)
library(broom)
library(stringr)
library(ggord)
Wide_3 <- read_csv("Data/Wide 4_qaqc.csv")
(Wide_3)
names(Wide_3)



#split out the Substreatment into 2 columns
Wide_3$SubTrt <-substring(Wide_3$Subtreatment, 2)
#move left
Wide_3 <- Wide_3 %>% relocate(SubTrt, .after = Treatment)

#remove the subtreatments with UM and UMS per Team
# Wide_3 <- Wide_3 %>% filter(Subtreatment != "UM")
# Wide_3 <- Wide_3 %>% filter(Subtreatment != "UMS")


#fix names
unique(Wide_3$SubTrt)
Wide_3$SubTrt <- ifelse(Wide_3$SubTrt == "C", "Control",
                           ifelse(Wide_3$SubTrt == "S", "Scraped + Seeded",
                                  ifelse(Wide_3$SubTrt == "M", "Mowed",
                                         ifelse(Wide_3$SubTrt == "MS", "Mowed + Scraped + Seeded",
                                                Wide_3$SubTrt))))


Wide_3$Treatment <- ifelse(Wide_3$Treatment == "G", "Grazed",
                           ifelse(Wide_3$Treatment == "S", "Seasonal Graze",
                                  "Ungrazed"))
unique(Wide_3$Subtreatment)

Wide_3$Subtreatment <- ifelse(Wide_3$Subtreatment == "GC", "Grazed",
                           ifelse(Wide_3$Subtreatment == "GS", "Grazed + Scraped + Seeded",
                                  ifelse(Wide_3$Subtreatment == "SC", "Seasonal Graze",
                                         ifelse(Wide_3$Subtreatment == "SS", "Seasonal Graze + Scraped + Seeded",
                                                ifelse(Wide_3$Subtreatment == "UC", "Ungrazed",
                                                       ifelse(Wide_3$Subtreatment == "UM", "Ungrazed + Mowed",
                                                              ifelse(Wide_3$Subtreatment == "UMS", "Ungrazed + Mowed + Scraped + Seeded",
                                                                     ifelse(Wide_3$Subtreatment == "US", "Ungrazed + Scraped + Seeded",
                                                                            Wide_3$Subtreatment))))))))



#Wide_3 <- Wide_3 %>% filter(Year != 2021)


#Wide_3 <- Wide_3 %>% filter(Subtreatment != "Ungrazed+Mowed")
#Wide_3 <- Wide_3 %>% filter(Subtreatment != "Ungrazed+Mowed+Seeded")

#remove if any columns that sum to zero
Wide_3 <- Wide_3 %>% 
  select_if(negate(function(col) is.numeric(col) && sum(col) < 1))

#check that all rows sum > 0
(Wide_3[-c(1:5)] %>% filter(if_all(everything(.), ~. != 0)))

##Combine all dead material from previous year into a single column and remove the _d columns

#sum dead cover
DeadSum <- Wide_3 %>% 
  select(contains("_d")) %>%
    reframe(DeadSum = rowSums(across())) 

#remove old _d columns and add deadsum
Wide_4 <- Wide_3 %>%
  select(-contains("_d"))

Wide_5 <- bind_cols(Wide_4, DeadSum)

#rename the columns to groupings
names(Wide_5)

SpeciesCode <- read_excel("Data/Species_qaqc3.xlsx") #updated to "3" 2025-08-04  
(SpeciesCode)
#View(SpeciesCode)
#remove the _d
SpeciesCode <- SpeciesCode %>% 
  filter(!str_detect(Species, '_d'))

unique(SpeciesCode$FxlGrp)  # = 8 groups

SpeciesCode$FxlGrp <- 
  ifelse(SpeciesCode$FxlGrp == "YAF", "Native-AF",
            ifelse(SpeciesCode$FxlGrp == "YPG", "Native-PG",
                  ifelse(SpeciesCode$FxlGrp == "YPF", "Native-PF",
                 #      ifelse(SpeciesCode$FxlGrp == "YPG", "Native-PG",
                              ifelse(SpeciesCode$FxlGrp == "NAG", "NonNative-AG",
                                     ifelse(SpeciesCode$FxlGrp == "NPF", "NonNative-PF",
                                            ifelse(SpeciesCode$FxlGrp == "NAF", "NonNative-AF",
                                              SpeciesCode$FxlGrp))))))


#files for MDS
Wide_6.env <- Wide_5[,c(1:5)]

Wide_6     <- Wide_5[,-c(1:5)]


#add rainfall to .env file
RAIN <- read_csv("Data/PINN_PPT_rainfall.csv")
head(RAIN)
colnames(RAIN)[colnames(RAIN) == 'Water_Year'] <- 'Year'

Wide_6.env <- left_join(Wide_6.env, RAIN, by = "Year")
hist(Wide_6.env$PPT_CM)

Wide_6.env$Rain.f <- ifelse(Wide_6.env$PPT_CM < 30, "Low", 
                                ifelse(Wide_6.env$PPT_CM > 60, "High", 
                                       "Med"))
                                  
Wide_6.env$Rain.f <- factor(Wide_6.env$Rain.f,
                               levels = c("Low", "Med", "High"))



Wide_6.env$PrePost.f <- ifelse(Wide_6.env$Year == 2021, "Pre", "Post")

Wide_6.env$PrePost.f <- factor(Wide_6.env$PrePost.f,
                            levels = c("Pre", "Post"))

                           




#new RAIN#new wide file to assign FxlGrps to species codes.
#get vector of 


####-  if grouping -------------------------------------
lookup = data.frame(SpeciesCode[,c(1,6)])
head(lookup)

#add unique trailing number to FxlGrp so it will play nice with tibble
lookup$FxlGrp <- paste0(lookup$FxlGrp, "_", 1:length(lookup$FxlGrp))


#keep the species of interest
lookup$FxlGrp <- ifelse(lookup$Species == "HIIN", "HIIN",
                                 ifelse(lookup$Species == "CESO", "CESO",
                                        ifelse(lookup$Species == "ESCA", "ESCA",
                                               ifelse(lookup$Species == "DELO", "DELO",
                                                      ifelse(lookup$Species == "CAEX", "CAEX",
                                                             ifelse(lookup$Species == "LAGR", "LAGR",
                                                                  ifelse(lookup$Species == "DeadSum", "DeadSum", 
                                                                         ifelse(lookup$Species == "BG", "BareGround",         
                                                                    lookup$FxlGrp))))))))



#check
unique(lookup$FxlGrp)

######## #extra file
Wide_6.FxlGrp <- tibble(Wide_6)

#base R new header names
Wide_6.FxlGrp.names <- lookup$FxlGrp[match(names(Wide_6.FxlGrp), lookup$Species)]

#add name to row 43
Wide_6.FxlGrp.names[43] <- "DeadSum"

#add names to 
Wide_6.FxlGrp

colnames(Wide_6.FxlGrp) <- c(Wide_6.FxlGrp.names)


# sum across
Wide_7.FxlGrp <- Wide_6.FxlGrp %>%
  rowwise() %>% 
  summarize("NonNative-AF" = sum(c_across(starts_with("NonNative-AF")), na.rm = T),
            "Native-PG" = sum(c_across(starts_with("Native-PG")), na.rm = T),
           # "Native-PF" = sum(c_across(starts_with("Native-PF")), na.rm = T),
            "CAEX" = sum(c_across(starts_with("CAEX")), na.rm = T),
            "CESO" = sum(c_across(starts_with("CESO")), na.rm = T),
            "Native-AF" = sum(c_across(starts_with("Native-AF")), na.rm = T),
            "DELO" = sum(c_across(starts_with("DELO")), na.rm = T),
            "NonNative-AG" = sum(c_across(starts_with("NonNative-AG")), na.rm = T),
            "ESCA" = sum(c_across(starts_with("ESCA")), na.rm = T),
            "Other" = sum(c_across(starts_with("XXX")), na.rm = T),
            "HIIN" = sum(c_across(starts_with("HIIN")), na.rm = T),
            "LAGR" = sum(c_across(starts_with("LAGR")), na.rm = T),
           # "NonNative-PG" = sum(c_across(starts_with("NonNative-PG")), na.rm = T),
            "DeadSum" = sum(c_across(starts_with("DeadSum")), na.rm = T),
            "BareGround" = sum(c_across(starts_with("BareGround")), na.rm = T))

 head(Wide_7.FxlGrp)                     

 
 
 #SET SOME FACTORS
 Wide_6.env$Year.f <- as.factor(Wide_6.env$Year)
 Wide_6.env$PrePost <- as.factor(ifelse(Wide_6.env$Year < 2022, "A", "B"))
 
 Wide_6.env$Treatment <- factor(Wide_6.env$Treatment, levels=c('Ungrazed', 'Grazed', 'Seasonal Graze'))
 Wide_6.env$SubTrt <- factor(Wide_6.env$SubTrt, levels=c('Control', 'Scraped + Seeded', 'Mowed', 'Mowed + Scraped + Seeded'))
 unique(Wide_6.env$Subtreatment)
 Wide_6.env$Subtreatment <- factor(Wide_6.env$Subtreatment, levels = c("Ungrazed", "Ungrazed + Scraped + Seeded",
                                                                       "Ungrazed + Mowed", "Ungrazed + Mowed + Scraped + Seeded",
                                                                       "Grazed", "Grazed + Scraped + Seeded", 
                                                                       "Seasonal Graze", "Seasonal Graze + Scraped + Seeded"))
 Wide_6.env$Rain.f <- factor(Wide_6.env$Rain.f , levels = c("Low", "Med","High"))
 
 unique(Wide_6.env$Subtreatment)
 
 
 Wide_7.env <- Wide_6.env
 
 
 ####- End grouping
 
#########





################ old

# SpeciesCode$Native <- ifelse(SpeciesCode$Native == "N", "NN", 
#                              ifelse(SpeciesCode$Native == "Y", "N", SpeciesCode$Native))
# 
# SpeciesCode$Duration <- ifelse(SpeciesCode$Duration == "Y", "P", SpeciesCode$Duration)
# 
# SpeciesCode$NDL <- paste0(SpeciesCode$Native, SpeciesCode$Duration, SpeciesCode$Lifeform)
# unique(SpeciesCode$NDL) 
# 
# 
# lookup <- c(NAF = "ACAM",        
#             XXG = "AG",           NNAG = "AIPR",        NAF =  "AMSIN",        XXX = "ASTER",        NNAG="AVBA",        
#             XXX="AVEN",        NNAG= "AVFA",        NNAG = "BRBI",         NNAG = "BRBO",         NNAG = "BRDI",        
#             NNAG ="BRDO",        NNAG = "BRHO",       NNAG =  "BRMA",        NNAG = "BRMAr",       NNAG = "BRNI",        
#             NNAG ="BROM",        NNAG = "BROMUS",     NNAG =  "BRRU",         XXX= "BUBR",         NNAF="CABU",        
#             NAF= "CACO",        NAF= "CAEX",         NAF="CAME",         NNAF="CEGL",        NNAF= "CESO",        
#             NAF="CLPU",        NNPF= "COAR",        NAF= "CRSE",         NAF="DELO",        XXG= "DICA",        
#             NAG="ELTR",        NNAF= "ERBO",        NNAF= "ERCI",        NNAF= "ERIC",        NNAF= "ERMO",        
#             NPF="ESCA",        NAF= "EUCH",         NNAG="FEBR",       NNAG=  "FEMY",       NNAG=  "FESTUCA",     
#             XXX="GAPH",       NAF=  "GIAC",        XXX= "GR",         UNK=  "H",          NNPF=  "HIIN",        
#             XXX="HL",       NNAG=  "HOMA",         NNAG="HOMU",         NNAF="HYGL",        NNAF= "LAAM",        
#             NAF="LAGR",       NNAF=  "LASE",       XXX=  "LETR",        NAF= "LOFI",        NAF= "LUBI",        
#             NNAF="MEPO",       UNK=  "NA",         UNK = "NHL",          XXX="ONAG",       NPG=  "PEGRAS",      
#             NAG="PLCA",         XXX="PLTA",       XXX=  "POAC",        NNPX= "RUCR",        NNAF= "SAAP",        
#             NAF="STME",        NAF= "TRGR",         NAF="TRIFO",       XXF ="UNKF",       NAF=  "VEPE",        
#             XXX="VUBR"
# )
# 







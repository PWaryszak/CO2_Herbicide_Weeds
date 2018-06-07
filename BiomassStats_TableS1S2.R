#LOAD libraries and data====
library("tidyverse")
library("Rmisc")
library("officer")
library("flextable")
library("magrittr")

#computing Species biomass response to elevated CO2:
#we need to remove Olea as it was mis-sprayed:
data <- read.csv("CO2survival.csv")
biomass<-subset(data, data=="biomass.data") # only biomass data
biomass<-biomass[!biomass$genus=="Olea",] #remove mis-sprayed species (wrong herbicide concentration)

#This script  below reproduces "dry weight" values 
#as presented in Table S1 & S2 in Weed Manuscript:

#TOTAL BIOMASS SummarySE Table:========
co2.biomass<-summarySE(biomass, measurevar="total.biomass", groupvars=c("genus","species","CO2"))
head(co2.biomass)
c<-round(co2.biomass[,5:8], digits=2)
d <- co2.biomass [,1:4]
cd<-cbind(d,c)

e<- cd[ ,c("genus", "species" ,"CO2","N", "total.biomass", "se")]

eTable <- unite(e, "Biomass_SE", c("total.biomass", "se"), sep = " ± " ) %>%
  unite ("Biomass_SE_N", c("Biomass_SE", "N"), sep = " (")
eTable
eTable$Biomass_SE_N <- factor(paste0(eTable$Biomass_SE_N ,")",sep=""))
eTable
e.wide <- spread(eTable,CO2, Biomass_SE_N)
e.wide
#Output 4 Biomass Table in Manuscript
##########genus       species          ambient         elevated
1     Ageratina    adenophora   1.85 ± 0.3 (4)  2.02 ± 0.16 (4)
2      Anredera    cordifolia  4.34 ± 1.26 (6)  7.84 ± 2.86 (6)
3     Asparagus   aethiopicus  2.33 ± 0.24 (5)   3.32 ± 0.4 (5)
4         Avena       barbata  5.08 ± 0.61 (4)  6.48 ± 0.75 (4)
5        Bromus   catharticus  3.19 ± 0.64 (4)  3.79 ± 0.44 (4)
6      Cenchrus  clandestinum  2.76 ± 0.09 (4)   3.54 ± 0.2 (4)
7       Chloris        gayana  3.52 ± 0.51 (4)  3.46 ± 0.27 (4)
8   Cotoneaster glaucophyllus   3.1 ± 0.21 (6)  4.09 ± 0.43 (6)
9      Ehrharta        erecta  1.28 ± 0.31 (4)  1.43 ± 0.73 (4)
10      Ipomoea        indica 13.72 ± 0.92 (4) 16.39 ± 0.86 (4)
11      Lantana        camara 12.74 ± 1.46 (4) 12.07 ± 1.71 (4)
12        Senna       pendula  1.39 ± 0.06 (4)  2.29 ± 0.62 (4)
13 Tradescantia   fluminensis  6.75 ± 0.65 (4)  9.05 ± 1.03 (4)
14      Verbena   bonariensis  2.63 ± 0.37 (4)  2.53 ± 0.11 (4)

#ROOTS BIOMASS SummarySE Table========
co2.biomassRoot<-summarySE(biomass, measurevar="root.biomass", groupvars=c("genus","species","CO2"))
co2.biomassRoot
a<-round(co2.biomassRoot[,5:8], digits=2)
b <- co2.biomassRoot [,1:4]
ab<-cbind(b,a)

f<- ab[ ,c( "genus", "species" ,"CO2","N", "root.biomass", "se")]
fTable <- unite(f, "Biomass_SE", c("root.biomass", "se"), sep = " ± " ) %>%
  unite ("Biomass_SE_N", c("Biomass_SE", "N"), sep = " (")
fTable
fTable$Biomass_SE_N <- factor(paste0(fTable$Biomass_SE_N ,")",sep=""))
fTable
f.wide <- spread(fTable,CO2, Biomass_SE_N)
f.wide
###########genus       species          ambient         elevated
1     Ageratina    adenophora 1.49 ± 0.23 (4) 1.53 ± 0.12 (4)
2      Anredera    cordifolia     NA ± NA (6)     NA ± NA (6)
3     Asparagus   aethiopicus 1.18 ± 0.13 (5) 1.48 ± 0.17 (5)
4         Avena       barbata 1.04 ± 0.12 (4) 1.28 ± 0.12 (4)
5        Bromus   catharticus 2.02 ± 0.49 (4) 2.47 ± 0.25 (4)
6      Cenchrus  clandestinum 1.77 ± 0.15 (4) 2.32 ± 0.14 (4)
7       Chloris        gayana 2.75 ± 0.42 (4) 2.24 ± 0.22 (4)
8   Cotoneaster glaucophyllus 2.44 ± 0.19 (6) 3.17 ± 0.35 (6)
9      Ehrharta        erecta 0.92 ± 0.25 (4)   1.1 ± 0.6 (4)
10      Ipomoea        indica 5.26 ± 0.63 (4) 5.95 ± 0.63 (4)
11      Lantana        camara 2.94 ± 0.53 (4) 2.73 ± 0.44 (4)
12        Senna       pendula 0.43 ± 0.02 (4)  0.8 ± 0.28 (4)
13 Tradescantia   fluminensis 0.51 ± 0.09 (4) 0.56 ± 0.07 (4)
14      Verbena   bonariensis 1.77 ± 0.33 (4) 1.45 ± 0.07 (4)


mass<- cbind(e.wide,f.wide)
#write.csv(mass, file = "BiomasTotal_and_Roots_Mean_SE.csv")

#STATS on Total Biomass:=========
#Use for loop to run lm models for each weed species
#and place output into a dataframe called Output:

Output <- NULL #we need to set an empty shelf for data from for loop below:

for ( i in unique(biomass$genus.species) ){
  #create a subset data 
  data_sub <- subset(biomass, genus.species== i)
  #create the linear model,   #then the model name will be lm1
  
  datalm <- assign(paste0("lm.", i), round(coef(summary(lm(total.biomass~CO2,data=data_sub))), digits =3))
  namelm <-  i
  
  saveoutput <- data.frame(datalm, Species=namelm)
  Output <- rbind(Output, saveoutput)
}
Output

#write.csv(Output, file="Biomass_Stats_Output.csv")

See.PValue<- rownames_to_column(Output)%>%
  filter(Pr...t.. <0.05) 
See.PValue #only  Sp.Cenchrus records significant change between ambient (intercept) and elevated CO2: 

#Produce Stats Table for TOTAL DRY WEIGHT:
BioTotal <- select(Output, 
                   Weed.Species = Species,
                   Coef = Estimate,
                   SE = Std..Error,
                   t = t.value,
                   P = Pr...t.. ) %>%
  mutate(CO2_factor = rownames(Output),
         CO2_level = ifelse(grepl ("CO2elevated", CO2_factor), "elevated","ambient" )) %>%
  mutate(P_value = ifelse (P < 0.05, "<0.05", P ), Biomass = "Total") %>%
  mutate (Model = rep(1:14,times = 1, each = 2)) %>%
  select(Model, Weed.Species, Coef, SE, t, P_value, CO2_level, Biomass)


head(BioTotal)

#STATS on Root Mass Only:=========
data <- read.csv("CO2survival.csv")
biomass<-subset(data, data=="biomass.data") # only biomass data
str(biomass)#data.frame':  132 obs. of  17 variables:

Output.Root <- NULL 
for ( i in unique(biomass$genus.species) ){
  #create a subset data 
  data_sub <- subset(biomass, genus.species== i)
  #create the linear model,   #then the model name will be lm1
  
  datalm <- assign(paste0("lm.", i), round(coef(summary(lm(root.biomass~CO2,data=data_sub))), digits =3))
  
  namelm <- i #this is a full name of genus.species
  
  saveoutput <- data.frame(datalm, Species=namelm)
  Output.Root <- rbind(Output.Root, saveoutput)
}
Output.Root

#write.csv(Output.Root, file="Biomass_RootsOnly_Stats_Output.csv") #Saves the output in a excel file
See.PValue.Roots<- rownames_to_column(Output.Root)%>%
  filter(Pr...t.. <0.05)
See.PValue.Roots

#Produce Stats Table for TOTAL DRY WEIGHT:
BioRoot <- select(Output.Root, 
                  Weed.Species = Species,
                  Coef = Estimate,
                  SE = Std..Error,
                  t = t.value,
                  P = Pr...t.. ) %>%
  mutate(CO2_factor = rownames(Output.Root),
         CO2_level = ifelse(grepl ("CO2elevated", CO2_factor), "elevated","ambient" )) %>%
  mutate(P_value = ifelse (P < 0.05, "<0.05", P ), Biomass = "Root") %>%
  mutate (Model = rep(15:28,times = 1, each = 2)) %>%
  select(Model, Weed.Species, Coef, SE, t, P_value, CO2_level, Biomass)


# Create Word Table S2 for Weeds Manuscript=====
#join BioTotal and BioRoot together:
BioModels <- rbind(BioTotal,BioRoot)

#Produce flextable:
ft <- flextable(data = BioModels) %>% 
  theme_vanilla %>% 
  autofit

#Make significant coeficients bold:
myft <- bold(ft, ~ P_value == "<0.05", ~ Coef, bold = TRUE)

# Create a temporary Word file:
tmp <- tempfile(fileext = ".docx")

# Create a permanent Word file"
read_docx() %>% 
  body_add_flextable(myft) %>% 
  print(target = tmp)

# open word document
browseURL(tmp)

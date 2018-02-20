#load libraries of R-functions - if not downloaded on your comp
#use function: install.packages() first and then:
library(Rmisc)
library(MASS)
library(tidyverse)
#BIOMASS DATA========
#setwd("C:/Users/poles/OneDrive/Documents/Murdoch/Tanja")
data <- read.csv("CO2survival.csv")
biomass<-subset(data, data=="biomass.data") # only biomass data
str(biomass)#data.frame':  132 obs. of  17 variables:
#OLEA must go as it was misprayed:
biomass<-biomass[!biomass$genus=="Olea",]

co2.biomass<-summarySE(biomass, measurevar="total.biomass", groupvars=c("genus","species","CO2"))
range(co2.biomass$N)# 4 6

#LET us code the species by plant functional type they fall in (PFT):
co2.biomass$PFT<-co2.biomass$genus
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Tradescantia"]<-"herb"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Verbena"   ] <- "herb"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Ageratina" ] <- "herb"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Anredera"  ] <- "vine"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Asparagus"] <-  "shrub"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Avena"     ]<-  "C3grass"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Bromus"    ] <- "C3grass"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Chloris"   ] <- "C4grass"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Cotoneaster"] <-"shrub"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Ehrharta"  ] <- "C3grass"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Ipomoea"   ] <- "vine"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Lantana"   ] <- "shrub"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Olea"      ] <- "tree"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Cenchrus"] <- "C4grass"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Lantana"   ] <- "shrub"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Senna"     ] <- "shrub"

levels(droplevels(co2.biomass$PFT))#"C3grass" "vine"    "shrub"   "C4grass" ,"herb"
co2.biomass$PFT<-factor(co2.biomass$PFT,levels= c("C3grass","C4grass","herb","shrub","vine" ))
head(co2.biomass)

#PAPER FIGURE of ALL Species TWO PANELS :==========
#Changing levels so that PFT are grouped together:
co2.biomass$genus<-factor(co2.biomass$genus,
                          levels= c("Avena","Bromus","Ehrharta","Cenchrus","Chloris",
                                    "Anredera","Ipomoea","Ageratina","Tradescantia",
                                    "Asparagus","Cotoneaster","Lantana","Senna",
                                   "Verbena"))

#The CO2 factors should be abbreviated too to look nicer and more compact so:
co2.biomass$CO2ae<-co2.biomass$CO2
co2.biomass$CO2ae<-as.factor(ifelse(co2.biomass$CO2=="ambient", "A", "E"))

#SPlitting co2.biomass into two data.frames to produce two panels:
dim(co2.biomass)#28 10
half1<- co2.biomass[co2.biomass$PFT == "C3grass" | co2.biomass$PFT == "C4grass" | co2.biomass$PFT == "vine",]
half2<- co2.biomass[co2.biomass$PFT=="shrub" | co2.biomass$PFT== "herb",]
dim(half1)#14 10
dim(half2)#14 10
pd <- position_dodge(.5)
#GGPLOT:
pd <- position_dodge(.5)
Half1.Plot<-ggplot(half1, aes(x=CO2ae, y=total.biomass, shape=PFT)) +
  geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)+
  geom_point(position=pd,size=4)+ geom_line(position=pd) + 
  scale_shape_manual(values=c(0,15,2))+
  facet_grid(.~genus + species )+theme_bw()+
  theme(axis.text.x=element_text(size=15),
                    axis.text.y=element_text(size=15),
                    axis.title.y=element_text(angle=90,size=16),
                    axis.title.x=element_blank(),
                    panel.grid.minor.x = element_blank(),
                    strip.text=element_text(size=12,face = "italic"),
                    legend.position = "none") +
  scale_y_continuous("Mean Total Biomass (g) with 95% CI",limits = c(-1, 20))

Half1.Plot

####
half2$genus<-factor(half2$genus, levels = c("Ageratina" ,"Tradescantia" , "Verbena" ,"Asparagus", "Cotoneaster","Lantana" ,"Senna"))
Half2.Plot <- ggplot(half2, aes(x=CO2ae, y=total.biomass, shape=PFT)) +
  geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)+
  geom_point(position=pd,size=4) +
  geom_line(position=pd) + scale_shape_manual(values=c(8,17))+
  facet_grid(.~genus + species )+theme_bw()+
  theme(axis.text.x=element_text(size=15),
                    axis.text.y=element_text(size=15),
                    axis.title.y=element_text(angle=90,size=16),
                    axis.title.x=element_blank(),
                    panel.grid.minor.x = element_blank(),
                    strip.text=element_text(size=12,face = "italic"),
                    legend.position = "none") + 
  scale_y_continuous("Mean Total Biomass (g) with 95% CI",limits = c(-1, 20))

Half2.Plot

library(gridExtra)
Two.Halves<-grid.arrange(Half1.Plot,Half2.Plot, ncol=1)
ggsave(Two.Halves,filename = "Biomass2Panels2018b.jpeg", width = 280, height = 240, units = "mm" )  


#TOTAL BIOMASS:========
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


#ROOTS BIOMASS========
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

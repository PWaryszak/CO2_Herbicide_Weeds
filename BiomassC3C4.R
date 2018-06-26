#LOAD DATA & LIBRARIES:
#load libraries of R-functions - if not downloaded on your comp
#use function: install.packages() first and then:
library(Rmisc)
library(MASS)
library(tidyverse)
library(gridExtra)

#BIOMASS DATA========
#setwd("C:/Users/poles/OneDrive/Documents/Murdoch/Tanja")
data <- read.csv("CO2survival.csv")
biomass<-subset(data, data=="biomass.data") # only biomass data
str(biomass)#data.frame':  132 obs. of  17 variables:
#OLEA must go as it was misprayed:
biomass<-biomass[!biomass$genus=="Olea",]
biomass$genus <- factor(biomass$genus)
str(biomass)

#Recode genus into plant functional type (Growth_Type):=====
biomass$Growth_Type<-biomass$genus
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Tradescantia"]<-"herb"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Verbena"   ] <- "herb"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Ageratina" ] <- "herb"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Anredera"  ] <- "vine"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Asparagus"] <-  "shrub" 
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Avena"     ]<-  "C3grass"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Bromus"    ] <- "C3grass"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Chloris"   ] <- "C4grass"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Cotoneaster"] <-"shrub"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Ehrharta"  ] <- "C3grass"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Ipomoea"   ] <- "vine"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Lantana"   ] <- "shrub"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Cenchrus"] <- "C4grass"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Lantana"   ] <- "shrub"
levels(biomass$Growth_Type)[levels(biomass$Growth_Type)=="Senna"     ] <- "shrub"

biomass$Propagation<-biomass$genus
levels(biomass$Propagation)[levels(biomass$Propagation)=="Tradescantia"]<-"cutting"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Verbena"   ] <- "seed"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Ageratina" ] <- "seed"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Anredera"  ] <- "tuber"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Asparagus" ] <-  "seed" 
levels(biomass$Propagation)[levels(biomass$Propagation)=="Avena"     ]<-  "seed"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Bromus"    ] <- "seed"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Chloris"   ] <- "seed"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Cotoneaster"] <-"seed"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Ehrharta"  ] <- "seed"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Ipomoea"   ] <- "cutting"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Lantana"   ] <- "cutting"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Cenchrus"  ] <- "seed"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Lantana"   ] <- "cutting"
levels(biomass$Propagation)[levels(biomass$Propagation)=="Senna"     ] <- "seed"


#check and re-arragne new factor levels:
levels(droplevels(biomass$Growth_Type))#"C3grass" "vine"    "shrub"   "C4grass" ,"herb"
levels(droplevels(biomass$Propagation))#"seed"    "tuber"   "cutting"
biomass$Growth_Type<-factor(biomass$Growth_Type,levels= c("C3grass","C4grass","herb","shrub","vine" ))
biomass$Propagation<-factor(biomass$Propagation,levels= c("cutting","seed","tuber"))

#Biomass Accumulation Rate FIGURE per Propagation Types and CO2 levels=====
biomass$BiomassRate <- biomass$total.biomass/biomass$Age
BiomassRate<-summarySE(biomass, measurevar="BiomassRate", groupvars=c("genus","species","CO2","Growth_Type", "Propagation"))
str(BiomassRate)# 28 obs. of  10 variables:

#PAPER FIGURE of ALL Species TWO PANELS :==========
#Changing levels so that Growth_Type are grouped together:
BiomassRate$genus<-factor(BiomassRate$genus,
                          levels= c("Avena","Bromus","Ehrharta","Cenchrus","Chloris",
                                    "Anredera","Ipomoea","Ageratina","Tradescantia",
                                    "Asparagus","Cotoneaster","Lantana","Senna",
                                    "Verbena"))

#The CO2 factors should be abbreviated too to look nicer and more compact so:
BiomassRate$CO2ae<-BiomassRate$CO2
BiomassRate$CO2ae<-as.factor(ifelse(BiomassRate$CO2=="ambient", "A", "E"))

#SPlitting BiomassRate into two data.frames to produce two panels:
dim(BiomassRate)#28 11
half1<- BiomassRate[BiomassRate$Growth_Type == "C3grass" | BiomassRate$Growth_Type == "C4grass" | BiomassRate$Growth_Type == "vine",]
half2<- BiomassRate[BiomassRate$Growth_Type=="shrub" | BiomassRate$Growth_Type== "herb",]
dim(half1)#14 11
dim(half2)#14 11
pd <- position_dodge(.5)
#GGPLOT:
pd <- position_dodge(.5)
Half1.Plot<-ggplot(half1, aes(x=CO2ae, y=BiomassRate, shape=Growth_Type, color = Propagation)) +
  geom_errorbar(aes(ymin=BiomassRate-ci, ymax=BiomassRate+ci),width=.35,position=pd,size=.9)+
  geom_point(position=pd,size=4)+ geom_line(position=pd) + 
  scale_shape_manual(values=c(0,15,2))+
  scale_color_manual(values = c("red","green","blue"))+
  facet_grid(.~genus + species )+theme_bw()+
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(angle=90,size=16),
        axis.title.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=12,face = "italic"),
        legend.position = "right") +
  scale_y_continuous("Mean biomass accumulation rate (g/day)")

Half1.Plot

####
half2$genus<-factor(half2$genus, levels = c("Ageratina" ,"Tradescantia" , "Verbena" ,"Asparagus", "Cotoneaster","Lantana" ,"Senna"))
Half2.Plot <- ggplot(half2, aes(x=CO2ae, y=BiomassRate, shape=Growth_Type, color = Propagation)) +
  geom_errorbar(aes(ymin=BiomassRate-ci, ymax=BiomassRate+ci),width=.35,position=pd,size=.9)+
  geom_point(position=pd,size=4) +
  geom_line(position=pd) + 
  scale_shape_manual(values=c(8,9))+
  scale_color_manual(values = c("red","green"))+
  facet_grid(.~genus + species )+theme_bw()+
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(angle=90,size=16),
        axis.title.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=12,face = "italic"),
        legend.position = "right") + 
  scale_y_continuous("Mean biomass accumulation rate (g/day)")

Half2.Plot

Two.Halves<-grid.arrange(Half1.Plot,Half2.Plot, ncol=1)
ggsave(Two.Halves,filename = "BiomassRate2Panels_26jun2018.jpeg", width = 320, height = 240, units = "mm" )  


#Biomass in C3 and C4 under elevated CO2:=============
c3c4 <- filter(biomass, Growth_Type == "C3grass" | Growth_Type == "C4grass",
               CO2 == "elevated") %>%
  mutate (BiomassRate = total.biomass/as.numeric(Age))

dim(c3c4)#20 21
fitc3c4 <- (lm (total.biomass ~ Growth_Type, data = c3c4))
fitc3c4.summary <- summary (fitc3c4)
fitc3c4.summary
############## Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)   3.9012     0.5608   6.957 1.69e-06 ***
#  Growth_TypeC4grass   -0.3993     0.8866  -0.450    0.658   
sum(resid(fitc3c4 )) #centered on 0 = good!
fitc3c4.summary$coefficients[1,1] #Growth_Type = C3
fitc3c4.summary$coefficients[2,1] #Growth_Type = C4
#check the difference in biomass between C3 and C4 grasses under elevated CO2
round(fitc3c4.summary$coefficients[2,1]/fitc3c4.summary$coefficients[1,1] *100, 1)#10.2 % lower biomass for C4 under elevated CO2



#Biomass accumualtion (Biomass/Age) in C3 and C4 under elevated CO2:=========
c3c4 <- filter(biomass, Growth_Type == "C3grass" | Growth_Type == "C4grass",
               CO2 == "elevated") %>%
  mutate (BiomassRate = total.biomass/as.numeric(Age))

fitc3c4.RATE <- (lm (BiomassRate ~ Growth_Type, data = c3c4))
fitc3c4.RATE.summary <- summary (fitc3c4.RATE)
fitc3c4.RATE.summary
############## Estimate Std. Error t value Pr(>|t|)    
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.50576    0.07667   6.597  3.4e-06 ***
#Growth_TypeC4grass   0.02144    0.12122   0.177    0.862
sum(resid(fitc3c4.RATE )) #centered on 0 = good!
fitc3c4.RATE.summary$coefficients[1,1] #Growth_Type = C3
fitc3c4.RATE.summary$coefficients[2,1] #Growth_Type = C4
#check the difference in biomass between C3 and C4 grasses under elevated CO2
round(fitc3c4.RATE.summary$coefficients[2,1]/fitc3c4.RATE.summary$coefficients[1,1] *100, 1)#4.2 % higher BiomassRate for C4 under elevated CO2

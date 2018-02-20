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
names(biomass)
#OLEA must go as it was misprayed:
biomass<-biomass[!biomass$genus=="Olea",]

co2.biomass<-summarySE(biomass, measurevar="total.biomass", groupvars=c("genus","species","CO2"))
head(co2.biomass)
range(co2.biomass$N)# 4 6
#LET us code the species by plant functional type they fall in (pft):
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
data <- read.csv("CO2survival.csv")
biomass<-subset(data, data=="biomass.data") # only biomass data
str(biomass)#data.frame':  132 obs. of  17 variables:
#OLEA must go as it was misprayed:
biomass<-biomass[!biomass$genus=="Olea",]

co2.biomass<-summarySE(biomass, measurevar="total.biomass", groupvars=c("genus","species","CO2"))
head(co2.biomass)

#Changing levels so that PFT are grouped together:
co2.biomass$genus<-factor(co2.biomass$genus,
                          levels= c("Avena","Bromus","Ehrharta","Cenchrus","Chloris",
                                    "Ageratina","Tradescantia",
                                    "Asparagus","Cotoneaster","Lantana","Senna",
                                    "Anredera","Ipomoea","Verbena"))



#The CO2 factors should be abbreviated too to look nicer and more compact so:
co2.biomass$CO2ae<-co2.biomass$CO2
co2.biomass$CO2ae<-as.factor(ifelse(co2.biomass$CO2=="ambient", "A", "E"))
#Reloading PFTs:
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

#SPlitting co2.biomass into two data.frames to produce two panels:
dim(co2.biomass)#28 10
half1<- co2.biomass[co2.biomass$PFT=="C3grass" | co2.biomass$PFT=="C4grass" | co2.biomass$genus=="Ageratina"| co2.biomass$genus=="Tradescantia",]
half2<- co2.biomass[co2.biomass$PFT=="shrub" | co2.biomass$PFT=="vine" | co2.biomass$genus=="Verbena" ,]
dim(half1)#14 10
dim(half2)#14 10

pd <- position_dodge(.5)
#GGPLOT:
pd <- position_dodge(.5)
HalfOne1<-ggplot(half1, aes(x=CO2ae, y=total.biomass, shape=PFT)) 
HalfOne2<-HalfOne1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)+ geom_point(position=pd,size=4)
HalfOne3<-HalfOne2 + geom_line(position=pd) + scale_shape_manual(values=c(0,15,8))# + scale_colour_manual(values = c("green", "red")) 
HalfOne4<-HalfOne3+facet_grid(.~genus + species )+theme_bw()
#with nicer theme:
HalfOne5<- HalfOne4 + theme(axis.text.x=element_text(size=15),
                    axis.text.y=element_text(size=15),
                    axis.title.y=element_text(angle=90,size=16),
                    axis.title.x=element_blank(),
                    panel.grid.minor.x = element_blank(),
                    strip.text=element_text(size=12,face = "italic"),
                    legend.position = "none")
HalfOne6<- HalfOne5 +  scale_y_continuous("Mean Total Biomass (g) with 95% CI",limits = c(-1, 20))
HalfOne6

half2$PFT<-factor(half2$PFT)#Reshuffling the levels for plot to look good.
HalfTwo1<-ggplot(half2, aes(x=CO2ae, y=total.biomass, shape=PFT)) 
HalfTwo2<-HalfTwo1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)+ geom_point(position=pd,size=4)
HalfTwo3<-HalfTwo2 + geom_line(position=pd) + scale_shape_manual(values=c(8,17,2,8))# + scale_colour_manual(values = c("green", "red")) 
HalfTwo4<-HalfTwo3+facet_grid(.~genus + species )+theme_bw()
#with nicer theme:
HalfTwo5<- HalfTwo4 + theme(axis.text.x=element_text(size=15),
                    axis.text.y=element_text(size=15),
                    axis.title.y=element_text(angle=90,size=16),
                    axis.title.x=element_blank(),
                    panel.grid.minor.x = element_blank(),
                    strip.text=element_text(size=12,face = "italic"),
                    legend.position = "none")
HalfTwo6<- HalfTwo5 +  scale_y_continuous("Mean Total Biomass (g) with 95% CI",limits = c(-1, 20))
#HalfTwo7 <-HalfTwo6  + guides(shape = guide_legend("Growth Forms: "))
HalfTwo6

library(gridExtra)
Pan2<-grid.arrange(HalfOne6,HalfTwo6, ncol=1)
ggsave(Pan2,filename = "Biomass2Panels6.jpeg", width = 280, height = 240, units = "mm" )  


#ALLTOGETHER OLD:
pd <- position_dodge(.5)
#GGPLOT:
pd <- position_dodge(.5)
all1<-ggplot(co2.biomass, aes(x=CO2ae, y=total.biomass, shape=PFT)) 
all2<-all1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)+ geom_point(position=pd,size=4)
all3<-all2 + geom_line(position=pd) + scale_shape_manual(values=c(0,15,8,17,2))# + scale_colour_manual(values = c("green", "red")) 
all4<-all3+facet_grid(.~genus + species )+theme_bw()
#with nicer theme:
all5<- all4 + theme(axis.text.x=element_text(size=15),
                    axis.text.y=element_text(size=15),
                    axis.title.y=element_text(angle=90,size=22),
                    axis.title.x=element_blank(),
                    panel.grid.minor.x = element_blank(),
                    strip.text=element_text(size=10,face = "italic"),
                    legend.text = element_text(size = 13),
                    legend.position = "bottom",
                    legend.title = element_text(face = "italic",size=13))
all6<- all5 +  scale_y_continuous("Mean Total Biomass (g) with 95% CI")
all7 <-all6  + guides(shape = guide_legend("Growth Forms: "))
all7


#FIGURE of ALL Species MULTIPLE PANELS:============
#Subsetting 5 major PFTs to split the plot and make it look nicer:
C3grass<-co2.biomass[co2.biomass$PFT=="C3grass",]
C4grass<-co2.biomass[co2.biomass$PFT=="C4grass",]
herb<-co2.biomass[co2.biomass$PFT=="herb",]
shrub<-co2.biomass[co2.biomass$PFT=="shrub",]
vine<-co2.biomass[co2.biomass$PFT=="vine",]

#C3grass:
pd <- position_dodge(.5)
C3grass.1<-ggplot(C3grass, aes(x=CO2, y=total.biomass, shape=CO2)) 
C3grass.2<-C3grass.1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)
C3grass.3<-C3grass.2 + geom_point(position=pd,size=4)
C3grass.3a<-C3grass.3 + geom_line(position=pd)# +scale_shape_manual(values=c(0,15)) + scale_colour_manual(values = c("green", "red")) 
C3grass.4<-C3grass.3a+facet_grid(PFT~genus + species )+theme_bw()
C3grass.4
#with grid:
C3grass.5<- C3grass.4 +theme(axis.text.x=element_text(size=15),
                   axis.text.y=element_text(size=15),
                   axis.title.y=element_text(angle=90,size=22),
                   axis.title.x=element_blank(),
                   panel.grid.minor.x = element_blank(),
                   strip.text=element_text(size=13,face = "italic"),
                   legend.text = element_text(size = 13),
                   legend.position = "none",
                   legend.title = element_text(face = "italic",size=13))
C3grass.5
C3grass.7<- C3grass.5 +  scale_y_continuous("Mean Biomass (g) with 95% CI",limits = c(0, 20))
C3grass.7 

#C4grass.:
pd <- position_dodge(.5)
C4grass.1<-ggplot(C4grass, aes(x=CO2, y=total.biomass, shape=CO2)) 
C4grass.2<-C4grass.1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)
C4grass.3<-C4grass.2 + geom_point(position=pd,size=4)
C4grass.3a<-C4grass.3 + geom_line(position=pd)# +scale_shape_manual(values=c(0,15)) + scale_colour_manual(values = c("green", "red")) 
C4grass.4<-C4grass.3a + facet_grid(PFT~genus + species )+theme_bw()
C4grass.4
#with grid:
C4grass.5<- C4grass.4 +theme(axis.text.x=element_text(size=15),
                             axis.text.y=element_text(size=15),
                             axis.title.y=element_blank(),
                             axis.title.x=element_blank(),
                             panel.grid.minor.x = element_blank(),
                             strip.text=element_text(size=13,face = "italic"),
                             legend.text = element_text(size = 13),
                             legend.position = "none",
                             legend.title = element_text(face = "italic",size=13))
C4grass.5
C4grass.6<- C4grass.5 +  scale_y_continuous(limits = c(0, 20))
C4grass.6 

#herb.:
pd <- position_dodge(.5)
herb.1<-ggplot(herb, aes(x=CO2, y=total.biomass, shape=CO2)) 
herb.2<-herb.1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)
herb.3<-herb.2 + geom_point(position=pd,size=4)
herb.3a<-herb.3 + geom_line(position=pd)# +scale_shape_manual(values=c(0,15)) + scale_colour_manual(values = c("green", "red")) 
herb.4<-herb.3a + facet_grid(PFT~genus + species )+theme_bw()
herb.4
#with grid:
herb.5<- herb.4 +theme(axis.text.x=element_text(size=15),
                             axis.text.y=element_text(size=15),
                             axis.title.y=element_blank(),
                             axis.title.x=element_blank(),
                             panel.grid.minor.x = element_blank(),
                             strip.text=element_text(size=13,face = "italic"),
                             legend.text = element_text(size = 13),
                             legend.position = "none",
                             legend.title = element_text(face = "italic",size=13))
herb.5
herb.6<- herb.5 +  scale_y_continuous(limits = c(0, 20))
herb.6 


#shrub.:
pd <- position_dodge(.5)
shrub.1<-ggplot(shrub, aes(x=CO2, y=total.biomass, shape=CO2)) 
shrub.2<-shrub.1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)
shrub.3<-shrub.2 + geom_point(position=pd,size=4)
shrub.3a<-shrub.3 + geom_line(position=pd)# +scale_shape_manual(values=c(0,15)) + scale_colour_manual(values = c("green", "red")) 
shrub.4<-shrub.3a + facet_grid(PFT~genus + species )+theme_bw()
shrub.4
#with grid:
shrub.5<- shrub.4 +theme(axis.text.x=element_text(size=15),
                       axis.text.y=element_text(size=15),
                       axis.title.y=element_blank(),
                       axis.title.x=element_blank(),
                       panel.grid.minor.x = element_blank(),
                       strip.text=element_text(size=13,face = "italic"),
                       legend.text = element_text(size = 13),
                       legend.position = "none",
                       legend.title = element_text(face = "italic",size=13))
shrub.5
shrub.6<- shrub.5 +  scale_y_continuous(limits = c(0, 20))
shrub.6 

#vine.:
pd <- position_dodge(.5)
vine.1<-ggplot(vine, aes(x=CO2, y=total.biomass, shape=CO2)) 
vine.2<-vine.1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)
vine.3<-vine.2 + geom_point(position=pd,size=4)
vine.3a<-vine.3 + geom_line(position=pd)# +scale_shape_manual(values=c(0,15)) + scale_colour_manual(values = c("green", "red")) 
vine.4<-vine.3a + facet_grid(PFT~genus + species )+theme_bw()
vine.4
#with grid:
vine.5<- vine.4 +theme(axis.text.x=element_text(size=15),
                         axis.text.y=element_text(size=15),
                         axis.title.y=element_blank(),
                         axis.title.x=element_blank(),
                         panel.grid.minor.x = element_blank(),
                         strip.text=element_text(size=13,face = "italic"),
                         legend.text = element_text(size = 13),
                         legend.position = "none",
                         legend.title = element_text(face = "italic",size=13))
vine.5
vine.6<- vine.5 +  scale_y_continuous(limits = c(0, 20))
vine.6 

#Multiplot function:
library(ggplot2)
library(grid)
library(gridExtra)
grid.arrange(C3grass.7 , C4grass.6 , herb.6 , shrub.6 ,vine.6 , ncol=3)


#FIGURE of ALL Species ONE PANEL:==========
#Changing levels so that PFT are grouped together:
data <- read.csv("CO2survival.csv")
biomass<-subset(data, data=="biomass.data") # only biomass data
str(biomass)#data.frame':  132 obs. of  17 variables:
names(biomass)
#OLEA must go as it was misprayed:
biomass<-biomass[!biomass$genus=="Olea",]

co2.biomass<-summarySE(biomass, measurevar="total.biomass", groupvars=c("genus","species","CO2"))
head(co2.biomass)

co2.biomass$genus<-factor(co2.biomass$genus,
                          levels= c("Ageratina","Avena","Bromus","Ehrharta",
                                    "Chloris","Cenchrus" ,
                                    "Tradescantia","Verbena",
                                    "Asparagus","Cotoneaster","Lantana","Senna",
                                    "Anredera","Ipomoea"))

#The CO2 factors should be abbreviated too to look nicer and more compact so:
co2.biomass$CO2ae<-co2.biomass$CO2
co2.biomass$CO2ae<-as.factor(ifelse(co2.biomass$CO2=="ambient", "A", "E"))
#REloading PFTs:
co2.biomass$PFT<-co2.biomass$genus
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Tradescantia"]<-"herb"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Verbena"   ] <- "herb"
levels(co2.biomass$PFT)[levels(co2.biomass$PFT)=="Ageratina" ] <- "C3grass"
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

pd <- position_dodge(.5)
all1<-ggplot(co2.biomass, aes(x=CO2ae, y=total.biomass, shape=PFT)) 
all2<-all1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)
all2
all3<-all2+ geom_point(position=pd,size=4)
all3
all3a<-all3 + geom_line(position=pd)# +scale_shape_manual(values=c(0,15)) + scale_colour_manual(values = c("green", "red")) 
all3a
all4<-all3a+facet_grid(.~genus + species )+theme_bw()
all4
#with grid:
all5<- all4 +theme(axis.text.x=element_text(size=15),
                   axis.text.y=element_text(size=15),
                   axis.title.y=element_text(angle=90,size=22),
                   axis.title.x=element_blank(),
                   panel.grid.minor.x = element_blank(),
                   strip.text=element_text(size=13,face = "italic"),
                   legend.text = element_text(size = 13),
                   legend.position = "bottom",
                   legend.title = element_text(face = "italic",size=13))
all5
all6<- all5 +  scale_y_continuous("Mean Total Biomass (g) with 95% CI")+scale_shape_manual(values=c(0,15,16,17,2)) 
all6 + guides(shape = guide_legend("Plant Functional Types: "))
#ggsave(filename="AE.jpg", dpi=600)


#Mean +- SE of Total Biomass and Root Biomass====
#TOTAL BIOMASS:
co2.biomass<-summarySE(biomass, measurevar="total.biomass", groupvars=c("genus","species","CO2"))
head(co2.biomass)
c<-round(co2.biomass[,5:8], digits=2)
d <- co2.biomassRoot [,1:4]
cd<-cbind(d,c)
#write.csv(cd, file = "BiomasTotal_MeansSE3.csv")

e<- cd[ ,c("genus", "species" ,"CO2","N", "total.biomass", "se")]

eTable <- unite(e, "Biomass_SE", c("total.biomass", "se"), sep = " ± " ) %>%
  unite ("Biomass_SE_N", c("Biomass_SE", "N"), sep = " (")
eTable
eTable$Biomass_SE_N <- factor(paste0(eTable$Biomass_SE_N ,")",sep=""))
eTable
e.wide <- spread(eTable,CO2, Biomass_SE_N)
e.wide
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
write.csv(mass, file = "BiomasTotal_and_Roots_Mean_SE.csv")

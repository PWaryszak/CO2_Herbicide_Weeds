#LOAD Packages=======
#load libraries of R-functions - if not downloaded on your comp
#install.packages("Rmisc")#instal first if not on your comp:
library(Rmisc)
library(MASS)
library(tidyverse)

#LOAD BIOMASS DATA=========
#setwd("C:/Users/poles/OneDrive/Documents/Murdoch/Tanja")#Home laptop
data <- read.csv("CO2survival.csv")
biomass<-subset(data, data=="biomass.data") # only biomass data
str(biomass)#data.frame':  132 obs. of  17 variables:
names(biomass)
#OLEA must go as it was misprayed:
biomass<-biomass[!biomass$genus=="Olea",]

co2.biomass<-summarySE(biomass, measurevar="total.biomass", groupvars=c("genus","species","CO2"))
head(co2.biomass)

#Let us code the species by plant functional type they fall in (pft):
#Paul asked to update/rename Cenchrus (previously Pennisetum)
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

levels(droplevels(co2.biomass$PFT))#"C3grass" "vine"    "shrub"   "C4grass" ,"herb"
co2.biomass$PFT<-factor(co2.biomass$PFT,levels= c("C3grass","C4grass","herb","shrub","vine" ))
co2.biomass<-co2.biomass[order(co2.biomass$PFT),] #ORder alphabeticaly so they look good on figures.
#write.table(co2.biomass, file="co2.biomass.csv",sep=",") #saves data in a excel file
#FIGURE of BIOMASS 4 PAPER:=========
#Compute Means +-95% CI
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

#GGPLOT:
pd <- position_dodge(.5)
all1<-ggplot(co2.biomass, aes(x=CO2ae, y=total.biomass, shape=PFT)) 
all2<-all1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)+ geom_point(position=pd,size=4)
all3<-all2 + geom_line(position=pd) + scale_shape_manual(values=c(0,15,8,17,2))# + scale_colour_manual(values = c("green", "red")) 
all3
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
#ggsave(filename="AE.jpg", dpi=600)

#Let us split the biomass data into heavy and light weight now:
x<-arrange(co2.biomass, genus,total.biomass)#Sorted the data by total.biomass and keeps the species together

light<-x[1:16,]
heavy<-x[17:30,]

#FIGURE light :
library(ggplot2)
pd <- position_dodge(.5)
light1<-ggplot(light, aes(x=CO2, y=total.biomass, shape=CO2, colour=CO2)) 
light2<-light1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)
light2 <-light2 + geom_point(position=pd,size=4) + geom_line(position=pd,width=10)  
light2 
light3<-light2 + scale_colour_manual(values=c("green","red"))
light3
light4<-light3+facet_grid(.~genus + species)+theme_bw()
light4
light5<- light4 +theme(axis.text.x=element_blank(),
                       axis.text.y=element_text(size=35),
                       axis.title.y=element_text(angle=90,size=32),
                       axis.title.x=element_blank(),
                       panel.grid.minor.x = element_blank(),
                       strip.text=element_text(size=25,face = "italic"),
                       legend.text = element_text(size = 33),
                       legend.position = "bottom",
                       legend.title = element_text(face = "italic",size=23))
light5
light6<- light5 +  scale_y_continuous("Weed Biomass (g) with 95% CI")
light6 

#FIGURE heavy :
library(ggplot2)
pd <- position_dodge(.5)
heavy1<-ggplot(heavy, aes(x=CO2, y=total.biomass, shape=CO2, colour=CO2)) 
heavy2<-heavy1 +geom_errorbar(aes(ymin=total.biomass-ci, ymax=total.biomass+ci),width=.35,position=pd,size=.9)
heavy2 <-heavy2 + geom_point(position=pd,size=4) + geom_line(position=pd,width=10)  
heavy2 
heavy3<-heavy2 + scale_colour_manual(values=c("green","red"))
heavy3
heavy4<-heavy3+facet_grid(.~genus + species)+theme_bw()
heavy4
heavy5<- heavy4 +theme(axis.text.x=element_blank(),
                        axis.text.y=element_text(size=35),
                        axis.title.y=element_text(angle=90,size=32),
                        axis.title.x=element_blank(),
                        panel.grid.minor.x = element_blank(),
                        strip.text=element_text(size=25,face = "italic"),
                        legend.text = element_text(size = 33),
                        legend.position = "bottom",
                        legend.title = element_text(face = "italic",size=23))
heavy5
heavy6<- heavy5 +  scale_y_continuous("Weed Biomass (g) with 95% CI")
heavy6 
#Computing % survival:===========
#survival<-read.csv(url("https://sites.google.com/site/herbicideandelevatedco2/data/survival.csv?attredirects=0&d=1"))#load data
survival<-read.csv("survival.csv")
str(survival)#1125 obs. of  13 variables:

perc.all<-summarySE(survival, measurevar="survival", groupvars=c("genus","CO2","herbicide"))
perc.all #looking at the n numbe of replicates. Mean reflects %
perc.all$Perc<-perc.all$survival*100 #turning means into percentages

#Plotting Percentages off perc ALL:
names(perc.all) #I need to change levels layout for nice plotting:
levels(perc.all$herbicide)#empty levels stil exist in Rs memory :/ but it is OK.
perc.all$herbicide <- factor(perc.all$herbicide, levels=c("recom.glyphosate","double.glyphosate",
                                                          "recom.starane","double.starane" ,
                                                          "double.brush.off","quadruble.brush.off",  "Control" ))           
#Fig of 2 herbicides (Glyhosata & Starane):========
#Brush off to be removed as only one species treated, not looking good on a plot:                                                          "double.brush.off","quadruble.brush.off","Control" ))
perc.all<-perc.all[!perc.all$herbicide=="double.brush.off",] #removing brush-off
perc.all<-perc.all[!perc.all$herbicide=="quadruble.brush.off",] #removing brush-off
levels(droplevels(perc.all$herbicide))#5 only
library(ggplot2)
library(scales) 
#PLOT:
perc.all1<-ggplot(perc.all, aes(x=CO2, y=Perc/100, colour=herbicide)) 
perc.all2<-perc.all1 + geom_boxplot(size=2) + scale_colour_manual(values=c("blue","red", "brown","orange", "darkgreen" ))#"grey40", "grey90"
perc.all2 
perc.all3<-perc.all2+facet_grid(.~herbicide)+theme_bw() + guides(color="none", shape="none")
perc.all4<- perc.all3 + scale_y_continuous(labels=percent) +ylab("Survival (%)")+ scale_x_discrete(labels=abbreviate)
perc.all4
perc.all5<- perc.all4 + theme(axis.text.x=element_text(size=25),
                            axis.text.y=element_text(size=35),
                            axis.title.y=element_text(angle=90,size=32),
                            axis.title.x=element_text(size=32),
                            panel.grid.minor.x = element_blank(),
                            strip.text=element_text(size=20,face = "italic"))

perc.all5

## Computing % survival of species X CO2 X glyphosate only=====
library(dplyr)
s<-select(survival,genus,species, CO2, herbicide,glyphosate, survival )
g <- filter(s, glyphosate=="glyphosate")#we look only on glyphosate-treated
g
g2 <- group_by(g,genus,species, CO2, herbicide)# we group
g2
g3 <- mutate(g2, Nall=length(survival))#we count number of rows(obs)
head(g3)
datout2 <- mutate(g3, Nalive=sum(survival), Perc=(Nalive/Nall)*100)# We create a column Perc
datout2
datout3 <-distinct(datout2,genus,species, CO2, herbicide) #we keep only the distinct rows:
datout3
perc<-as.data.frame(datout3)
perc #84 rows #write.table(perc, file="percSurvival.csv",sep=",")

#Plotting Percentages off perc of ALL:=======
names(perc) #I need to change levels layout for nice plotting:
levels(perc$herbicide)#dead levels stil exist in Rs memory :/ but it is OK.
perc$herbicide <- factor(perc$herbicide, levels=c("recom.glyphosate","double.glyphosate","Control" ))
library(ggplot2)
library(scales) 
perc1<-ggplot(perc, aes(x=CO2, y=Perc/100, colour=herbicide, shape=CO2)) 
perc2<-perc1 + geom_jitter(size=6)# +scale_colour_manual(values=c("blue","red", "darkgreen"))
perc2 
perc3<-perc2+facet_grid(.~herbicide)+theme_bw() + guides(color="none")
perc4<- perc3 + scale_y_continuous(labels=percent) +ylab("Survival (%)") 
perc4
perc5<- perc4 +theme(axis.text.y=element_text(size=35),
                      axis.title.y=element_text(angle=90,size=32),
                      axis.title.x=element_blank(),
                      panel.grid.minor.x = element_blank(),
                      strip.text=element_text(size=25,face = "bold"),
                      legend.text = element_text(size = 33),
                      legend.position = "bottom",
                      legend.title = element_text(face = "italic",size=23))
perc5


#Plotting Percentages (gluphosate-treated only) in arranged way:====
z<-perc[ ! perc$herbicide=="Control",] # removing Control as it is always 100%
z<-arrange(z, genus,Perc)#Sorted the data by total.biomass and keeps the species together
dim(z)#56  9
low<-z[1:28,]
high<-z[29:56,]
low1<-ggplot(low, aes(x=CO2, y=Perc/100, colour=herbicide, shape=CO2)) 
low2<-low1 + geom_jitter(size=6) +scale_colour_manual(values=c("blue","red", "darkgreen"))
low2 
low3<-low2+facet_grid(.~herbicide)+theme_bw() + guides(color="none")
low4<- low3 + scale_y_continuous(labels=percent) +ylab("Survival (%)")
low4

#High Perc:
high1<-ggplot(high, aes(x=CO2, y=Perc/100, colour=herbicide, shape=CO2)) 
high2<-high1 + geom_jitter(size=6) +scale_colour_manual(values=c("blue","red", "darkgreen"))
high2 
high3<-high2+facet_grid(.~herbicide)+theme_bw() + guides(color="none")
high4<- high3 + scale_y_continuous(labels=percent) +ylab("Survival (%)")
high4


#Adding species to plot of low4:
low1<-ggplot(low, aes(x=herbicide, y=Perc/100, shape=CO2, colour=herbicide)) 
low2<-low1 + geom_jitter(size=6) +scale_colour_manual(values=c("blue","red", "darkgreen"))
low2 
low3<-low2+facet_grid(.~genus)+theme_bw()  + guides(color="none")#turning off colour part of legend for clarity
low4<- low3 + scale_y_continuous(labels=percent) +ylab("Survival (%)")+ scale_x_discrete(labels=abbreviate)
low4 
low5<-low4 + theme(axis.text.x=element_text(size=20),
                     axis.text.y=element_text(size=35),
                     axis.title.y=element_text(angle=90,size=32),
                     axis.title.x=element_blank(),
                     panel.grid.minor.x = element_blank(),
                     strip.text=element_text(size=25,face = "italic"),
                   legend.text = element_text(size = 33),
                   legend.position = "bottom",
                   legend.title = element_text(face = "italic",size=33))
low5 

##Adding species to plot high4 Perc:
high1<-ggplot(high, aes(x=herbicide, y=Perc/100, colour=herbicide, shape=CO2)) 
high2<-high1 + geom_jitter(size=6) +scale_colour_manual(values=c("blue","red", "darkgreen"))
high2 
high3<-high2+facet_grid(.~genus)+theme_bw() + guides(color="none")
high4<- high3 + scale_y_continuous(labels=percent) +ylab("Survival (%)") + scale_x_discrete(labels=abbreviate)
high4 
high5<-high4 + theme(axis.text.x=element_text(size=30),
       axis.text.y=element_text(size=35),
       axis.title.y=element_text(angle=90,size=32),
       axis.title.x=element_blank(),
       panel.grid.minor.x = element_blank(),
       strip.text=element_text(size=25,face = "italic"),
       legend.text = element_text(size = 33),
       legend.position = "bottom",
       legend.title = element_text(face = "italic",size=33))
high5

#Plotting PEnnisetum only (the only one with significant response to elevated CO2):========
penni<-z[z$genus=="Pennisetum",]
penni1<-ggplot(penni, aes(x=herbicide, y=Perc/100, colour=herbicide, shape=CO2)) 
penni2<-penni1 + geom_point(size=9) +scale_colour_manual(values=c("blue","red", "darkgreen"))
penni2 
penni3<-penni2+facet_grid(.~genus +species)+theme_bw() #+ guides(color="none")
penni4<- penni3 + scale_y_continuous(labels=percent) +ylab("Survival (%)")
penni4 
penni5<- penni4 +theme(axis.text.y=element_text(size=15),
                   axis.title.y=element_text(angle=90,size=22),
                   panel.grid.minor.x = element_blank(),
                   strip.text=element_text(size=13,face = "bold"),
                   legend.text = element_text(size = 15),
                   legend.position = "bottom",
                   legend.background = element_rect(fill="gray90", size=.6, linetype="dotted"),
                   legend.title = element_text(size=17))
penni5

#STATS on biomass:=========
#computing Species biomass response to elevated CO2:
levels(biomass$genus)
#we need to remove Olea as it was mis-sprayed:
biomass<-biomass[!biomass$genus=="Olea",]

#SUPER COOL WAY using for loop:
#TOTAL DRY WEIGHT: it puts the outcome of lm into a dataframe called Output:
#The code below reproduces "dry weight" values in Table 2 in Manuscript:
#"
Output <- NULL #we need to set an empty shelf for data called Output

for ( i in unique(biomass$genus) ){
  #create a subset data 
  data_sub <- subset(biomass, genus== i)
  #create the linear model,   #then the model name will be lm1
  
  datalm <- assign(paste0("lm.", i), round(coef(summary(lm(total.biomass~CO2,data=data_sub))), digits =3))
  namelm <- paste0("Sp.", i)
  
  saveoutput <- data.frame(datalm, Species=namelm)
  Output <- rbind(Output, saveoutput)
  }
Output
#write.csv(Output, file="Output.csv")


#STATS on Root Mass Only:=========
#it puts the outcome into a dataframe called Output:
#The code belowe reproduces "root mass weight" values in Table 2 in Tanja's Manuscript:
#"
Output.Root <- NULL #we need to set an empty shelf for data called Output
str(biomass)
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
summary(lm(root.biomass~CO2,data=biomass))
#write.csv(Output.Root, file="OutputRootMass.csv") #Saves the output in a excel file


#STATS on Root/Total Biomass Ratio===========
Output.RootTotalRatio <- NULL #we need to set an empty shelf for data called Output
str(biomass)#data.frame':  132 obs. of  17 variables:
#adding ratio colum:
biomass$ratio<- biomass$root.biomass / biomass$total.biomass
#Computing ANOVA-like coef-s using lm on each species seperatly:

for ( i in unique(biomass$genus.species) ){
  #create a subset data 
  data_sub <- subset(biomass, genus.species== i)
  #create the linear model,   #then the model name will be lm1
  
  datalm <- assign(paste0("lm.", i), round(coef(summary(lm(ratio~CO2,data=data_sub))), digits =3))
  
  namelm <- i #this is a full name of genus.species
  
  saveoutput <- data.frame(datalm, Species=namelm)
  Output.RootTotalRatio <- rbind(Output.RootTotalRatio, saveoutput)
}
Output.RootTotalRatio
#write.csv(Output.RootTotalRatio, file="Output.RootTotalRatio.csv") #Saves the output in a excel file

#Survival lm regression for glyphosate only for all species separetly:=====
library(MASS)#contains functions to run glm
str(survival) #data.frame':  1125 obs. of  13 variables:
names(survival)
glyph<- survival[survival$glyphosate=="glyphosate",]
glyph$herbicide<-factor(glyph$herbicide)#it dropes the unused levels
glyph<-glyph[!is.na(glyph$genus.species),] #it removes the NA-s that make for loop jam
glyph

names(glyph)

Output.Glyph <- NULL #we need to set an empty shelf for data called Output
for ( i in unique(glyph$genus.species) ){
  #create a subset data 
  data_sub <- subset(glyph, genus.species== i)
  #create the linear model,   #then the model name will be lm1
  
  datalm <- assign(paste0("lm.", i), round(coef(summary(glm(survival~CO2+herbicide,data=data_sub, family="binomial"))), digits =3))
  
  namelm <- i #this is a full name of genus.species
  
  saveoutput <- data.frame(datalm, Species=namelm)
  Output.Glyph <- rbind(Output.Glyph, saveoutput)
}
Output.Glyph
#write.csv(Output.Glyph, file="OutputGlyphSurvival.csv") #Saves the output in a excel file


#Survival for Starane-treated only:=========
names(survival)
starane<- survival[survival$starane=="starane",]
starane$herbicide<-factor(starane$herbicide)#it dropes the unused levels
starane<-starane[!is.na(starane$genus.species),] #it removes the NA-s that make for loop jam
starane

Output.starane <- NULL #we need to set an empty shelf for data called Output
for ( i in unique(starane$genus.species) ){
  #create a subset data 
  data_sub <- subset(starane, genus.species== i)
  #create the linear model,   #then the model name will be lm1
  
  datalm <- assign(paste0("lm.", i), round(coef(summary(glm(survival~CO2+herbicide,data=data_sub, family="binomial"))), digits =3))
  
  namelm <- i #this is a full name of genus.species
  
  saveoutput <- data.frame(datalm, Species=namelm)
  Output.starane <- rbind(Output.starane, saveoutput)
}
Output.starane
#write.csv(Output.starane, file="OutputstaraneSurvival.csv") #Saves the output in a excel file

#Conclusion: None of the higher doses in herbicides were more effective


#SPRAY Effect on overall survival:============
#Computing binomial survival with herbicide on:
survival<-read.csv("survival.csv")
str(survival)#1125 obs. of  13 variables:

names(survival) #I need to remove brush.off as it was misprayed.
s<-survival[!survival$herbicide=="double.brush.off",] #removing brush-off
s<-s[!s$herbicide=="quadruble.brush.off",] #removing brush-off
levels(droplevels(s$herbicide))#5 only: "Control", "double.glyphosate" "double.starane", "recom.glyphosate"  "recom.starane"
starane.only<- survival[survival$starane=="starane", ]
str(starane.only)
s.glm<- glm(survival ~ herbicide + CO2, family="binomial", data = starane.only)
summary(s.glm)

s1<- glm(survival ~ herbicide + CO2, family="binomial", data = s)
s1
summary(s1)
#OUTPUT BELOW:
Call:  glm(formula = survival ~ herbicide + CO2, family = "binomial", data = s)

Coefficients:
###########################Estimate Std. Error z value Pr(>|z|)
(Intercept)                 18.5196   373.3848   0.050    0.960
herbicidedouble.glyphosate -18.5046   373.3848  -0.050    0.960
herbicidedouble.starane    -18.8055   373.3848  -0.050    0.960
herbiciderecom.glyphosate  -17.8072   373.3848  -0.048    0.962
herbiciderecom.starane     -19.0889   373.3848  -0.051    0.959
CO2elevated                  0.0956     0.1479   0.646    0.518

(Dispersion parameter for binomial family taken to be 1)
Null deviance: 1352.4  on 1084  degrees of freedom
Residual deviance: 1032.3  on 1079  degrees of freedom
AIC: 1044.3, Number of Fisher Scoring iterations: 17

#so we lump the herbicides into one spray column
#Computing the logistic regression fit for survival - all herbicide sprays clumped together:
#As we saw a lot of noise in response to herbicide let us clump all (except brush-off) herbicides together:

str(s) #data.frame':  1085 obs. of  13 variables
s$spray<-as.factor(ifelse(s$herbicide=="Control", "off", "on"))

g1<- glm(survival ~ spray + CO2 , family="binomial", data = s)
g1
summary(g1)
#STATS Output below:
#Call:  glm(formula = s ~ spray + CO2, family = "binomial", data = s)

Deviance Residuals: 
  Min        1Q    Median        3Q       Max  
-1.29509  -1.24858   0.00013   1.06411   1.10787  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)  18.5143   373.3486   0.050    0.960
sprayon     -18.3485   373.3486  -0.049    0.961
CO2elevated   0.1067     0.1406   0.759    0.448

#Definietly an effect of elevated CO2 but very tiny!!!
#Spray of Herbicide did not really affect the species.
#I think one conclusion might go towards the technique - we sprayed 10ml/plant regardless its size.


#PFT EFFECT:=========
#adding PFT (plant functional type) as an extra category and see how this affects survival:
data <- read.csv("CO2survival.csv")
survival<-subset(data, data=="survival.data")
str(survival)#1125 obs. of  17 variables:
levels(survival$genus)
survival$pft.<-survival$genus
#Changing the level names to pft.-s:
levels(survival$pft.)[levels(survival$pft.)=="Tradescantia"]<-"herb"
levels(survival$pft.)[levels(survival$pft.)=="Verbena"   ] <- "herb"
levels(survival$pft.)[levels(survival$pft.)=="Ageratina" ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Anredera"  ] <- "vine"
levels(survival$pft.)[levels(survival$pft.)=="Asparagus"] <-  "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Avena"     ]<-  "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Bromus"    ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Chloris"   ] <- "C4grass"
levels(survival$pft.)[levels(survival$pft.)=="Cotoneaster"] <-"shrub"
levels(survival$pft.)[levels(survival$pft.)=="Ehrharta"  ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Ipomoea"   ] <- "vine"
levels(survival$pft.)[levels(survival$pft.)=="Lantana"   ] <- "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Olea"      ] <- "tree"
levels(survival$pft.)[levels(survival$pft.)=="Pennisetum"] <- "C4grass"
levels(survival$pft.)[levels(survival$pft.)=="Lantana"   ] <- "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Senna"     ] <- "shrub"

levels(survival$pft.)#"C3grass" "vine"    "shrub"   "C4grass" "tree","herb"

#Adding additional Spray column:
survival$spray<-as.factor(ifelse(survival$herbicide=="Control", "off", "on"))
new.survival<-survival[!survival$pft.=="tree",] #removing tree that is all Olea-s as they were misprayed.
levels(new.survival$pft.)
new.survival$pft.<-factor(new.survival$pft.,levels= c("C3grass","herb","C4grass","shrub","vine" ))

g2<- glm(survival ~ spray + CO2 + pft. , family="binomial", data = new.survival)
g2.summary<-summary(g2)#all significant when compared with C3 grasses!!!
output.pft<- as.data.frame(round(coef(g2.summary), digits= 3))
output.pft
#write.csv(output.pft, file="OutputSprayPFT.csv") #Saves the output in a excel file

#STATS OUTPUT:
Call:  glm(formula = survival ~ spray + CO2 + pft., family = "binomial", 
      data = new.survival)

Deviance Residuals: 
  Min        1Q    Median        3Q       Max  
-1.66611  -0.65001   0.00007   0.81664   1.83835  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  18.36551  576.78115   0.032    0.975    
sprayon     -19.85123  576.78116  -0.034    0.973    
CO2elevated   0.03854    0.15988   0.241    0.809    
pft.herb      1.36499    0.27390   4.983 6.24e-07 ***
  pft.C4grass   1.71779    0.30303   5.669 1.44e-08 ***
  pft.shrub     2.37409    0.24774   9.583  < 2e-16 ***
  pft.vine      2.54802    0.26495   9.617  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1337.03  on 1064  degrees of freedom
Residual deviance:  921.03  on 1058  degrees of freedom
AIC: 935.03
Number of Fisher Scoring iterations: 18
coef(g2)


#Analysis without Olea (misprayed) and Anredera======
#(long time in glasshouse, lack of week6 survival data - something went wrong here)
data <- read.csv("CO2survival.csv")
survival<-subset(data, data=="survival.data")
str(survival)#1125 obs. of  17 variables:
levels(survival$genus)
survival$pft.<-survival$genus
#Changing the level names to pft.-s:
levels(survival$pft.)[levels(survival$pft.)=="Tradescantia"]<-"herb"
levels(survival$pft.)[levels(survival$pft.)=="Verbena"   ] <- "herb"
levels(survival$pft.)[levels(survival$pft.)=="Ageratina" ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Anredera"  ] <- "vine"
levels(survival$pft.)[levels(survival$pft.)=="Asparagus"] <-  "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Avena"     ]<-  "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Bromus"    ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Chloris"   ] <- "C4grass"
levels(survival$pft.)[levels(survival$pft.)=="Cotoneaster"] <-"shrub"
levels(survival$pft.)[levels(survival$pft.)=="Ehrharta"  ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Ipomoea"   ] <- "vine"
levels(survival$pft.)[levels(survival$pft.)=="Lantana"   ] <- "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Olea"      ] <- "tree"
levels(survival$pft.)[levels(survival$pft.)=="Pennisetum"] <- "C4grass"
levels(survival$pft.)[levels(survival$pft.)=="Lantana"   ] <- "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Senna"     ] <- "shrub"

levels(survival$pft.)#"C3grass" "vine"    "shrub"   "C4grass" "tree","herb"

#Adding additional Spray column:
survival$spray<-as.factor(ifelse(survival$herbicide=="Control", "off", "on"))
new.survival<-survival[!survival$pft.=="tree",]
new.survival$pft.<-factor(new.survival$pft.,levels= c("C3grass","herb","C4grass","shrub","vine" ))
levels(new.survival$pft.)#"C3grass" "herb"    "C4grass" "shrub"   "vine" 

#Removing Olea and Anredera:
#OLEA must go as it was misprayed:
new.survival<-new.survival[!new.survival$genus=="Olea",]
new.survival<-new.survival[!new.survival$genus=="Anredera",]

#Running binomial model on survival:
g2<- glm(survival ~ spray + CO2 + pft. , family="binomial", data = new.survival)
g2.summary<-summary(g2)#all significant when compared with C3 grasses!!!
output.pft<- as.data.frame(round(coef(g2.summary), digits= 3))
output.pft
#write.csv(output.pft, file="OutputSprayPFT.csv") #Saves the output in a excel file
#Output when both Olea and Anredera were removed:
Call:
  glm(formula = survival ~ spray + CO2 + pft., family = "binomial", 
      data = new.survival)

Deviance Residuals: 
  Min        1Q    Median        3Q       Max  
-1.60522  -0.66102   0.00007   0.80322   1.85570  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)   18.4864   597.3018   0.031    0.975    
sprayon      -19.8963   597.3018  -0.033    0.973    
CO2elevated   -0.1150     0.1807  -0.637    0.524    
pft.herb       1.3645     0.2740   4.980 6.34e-07 ***
  pft.C4grass    1.7189     0.3031   5.670 1.43e-08 ***
  pft.shrub      2.3756     0.2479   9.585  < 2e-16 ***
  pft.vine      20.5387  1359.0266   0.015    0.988  #weird  
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1137.7  on 896  degrees of freedom
Residual deviance:  714.9  on 890  degrees of freedom
AIC: 728.9

#Analysis as per suggestion of PAUL:========
#Removing: A. aethiopicus, C. glaucophyllus, I. indica and T. fluminensis:
data <- read.csv("CO2survival.csv")
survival<-subset(data, data=="survival.data")
str(survival)#1125 obs. of  17 variables:
levels(survival$genus)
survival$pft.<-survival$genus
#Changing the level names to pft.-s:
levels(survival$pft.)[levels(survival$pft.)=="Tradescantia"]<-"herb"
levels(survival$pft.)[levels(survival$pft.)=="Verbena"   ] <- "herb"
levels(survival$pft.)[levels(survival$pft.)=="Ageratina" ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Anredera"  ] <- "vine"
levels(survival$pft.)[levels(survival$pft.)=="Asparagus"] <-  "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Avena"     ]<-  "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Bromus"    ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Chloris"   ] <- "C4grass"
levels(survival$pft.)[levels(survival$pft.)=="Cotoneaster"] <-"shrub"
levels(survival$pft.)[levels(survival$pft.)=="Ehrharta"  ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Ipomoea"   ] <- "vine"
levels(survival$pft.)[levels(survival$pft.)=="Lantana"   ] <- "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Olea"      ] <- "tree"
levels(survival$pft.)[levels(survival$pft.)=="Pennisetum"] <- "C4grass"
levels(survival$pft.)[levels(survival$pft.)=="Lantana"   ] <- "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Senna"     ] <- "shrub"

levels(survival$pft.)#"C3grass" "vine"    "shrub"   "C4grass" "tree","herb"

#Adding additional Spray column:
survival$spray<-as.factor(ifelse(survival$herbicide=="Control", "off", "on"))
new.survival<-survival[!survival$pft.=="tree",] #removing tree that is all Olea-s as they were misprayed.
new.survival$pft.<-factor(new.survival$pft.,levels= c("C3grass","herb","C4grass","shrub","vine" ))
levels(new.survival$pft.)

#Removing 4 Sp:
str(new.survival)#1065 obs. of  19 variables:
paul<-new.survival[!new.survival$genus=="Asparagus",] #removing 4 species with high survival as suggested by Paul
paul<-paul[!paul$genus=="Cotoneaster",]
paul<-paul[!paul$genus=="Ipomoea",]
paul<-paul[!paul$genus=="Tradescantia" & paul$glyphosate=="glyphosate",]
str(paul)#779 obs. of  19 variables:



paul.glm<- glm(survival ~ spray + CO2 + pft. , family="binomial", data = paul)
paul.glm.summary<-summary(paul.glm)#all significant when compared with C3 grasses!!!
paul.glm.summary
output.pft.paul<- as.data.frame(round(coef(paul.glm.summary), digits= 3))
#OUTPUT:
Call:
  glm(formula = survival ~ spray + CO2 + pft., family = "binomial", data = paul)

Deviance Residuals: 
  Min        1Q    Median        3Q       Max  
-1.52014  -0.63715   0.00004   0.86974   2.26317  

Coefficients:
#############Estimate Std. Error z value Pr(>|z|)    
(Intercept)  18.99798  688.72553   0.028    0.978    
sprayon     -20.48942  688.72553  -0.030    0.976    
CO2elevated   0.04982    0.22367   0.223    0.824    
pft.herb     -0.98915    0.63481  -1.558    0.119    
pft.C4grass   1.71788    0.30304   5.669 1.44e-08 ***
pft.shrub     2.14109    0.31137   6.876 6.14e-12 ***
pft.vine      2.21881    0.32431   6.842 7.83e-12 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 843.30  on 626  degrees of freedom
Residual deviance: 477.52  on 620  degrees of freedom
(152 observations deleted due to missingness)
AIC: 491.52, Number of Fisher Scoring iterations: 18

#Analysis as per suggestion of Tanja = looking at Co2*Herbicide interaction:=========
#adding PFT (plant functional type) as an extra category and see how this affects survival:
data <- read.csv("CO2survival.csv")
survival<-subset(data, data=="survival.data")
str(survival)#1125 obs. of  17 variables:
levels(survival$genus)
survival$pft.<-survival$genus
#Changing the level names to pft.-s:
levels(survival$pft.)[levels(survival$pft.)=="Tradescantia"]<-"herb"
levels(survival$pft.)[levels(survival$pft.)=="Verbena"   ] <- "herb"
levels(survival$pft.)[levels(survival$pft.)=="Ageratina" ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Anredera"  ] <- "vine"
levels(survival$pft.)[levels(survival$pft.)=="Asparagus"] <-  "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Avena"     ]<-  "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Bromus"    ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Chloris"   ] <- "C4grass"
levels(survival$pft.)[levels(survival$pft.)=="Cotoneaster"] <-"shrub"
levels(survival$pft.)[levels(survival$pft.)=="Ehrharta"  ] <- "C3grass"
levels(survival$pft.)[levels(survival$pft.)=="Ipomoea"   ] <- "vine"
levels(survival$pft.)[levels(survival$pft.)=="Lantana"   ] <- "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Olea"      ] <- "tree"
levels(survival$pft.)[levels(survival$pft.)=="Pennisetum"] <- "C4grass"
levels(survival$pft.)[levels(survival$pft.)=="Lantana"   ] <- "shrub"
levels(survival$pft.)[levels(survival$pft.)=="Senna"     ] <- "shrub"

levels(survival$pft.)#"C3grass" "vine"    "shrub"   "C4grass" "tree","herb"

#Adding additional Spray column:
survival$spray<-as.factor(ifelse(survival$herbicide=="Control", "off", "on"))
new.survival<-survival[!survival$pft.=="tree",] #removing tree that is all Olea-s as they were misprayed.
levels(new.survival$pft.)
new.survival$pft.<-factor(new.survival$pft.,levels= c("C3grass","herb","C4grass","shrub","vine" ))

inter<- glm(survival ~ spray*CO2 , family="binomial", data = new.survival)
inter.summary<-summary(inter)#all significant when compared with C3 grasses!!!
output.inter<- as.data.frame(round(coef(inter.summary), digits= 3))
output.inter
#OUTPUT:
Estimate Std. Error z value Pr(>|z|)
(Intercept)           18.566    541.676   0.034    0.973
sprayon              -18.360    541.676  -0.034    0.973
CO2elevated            0.000    772.854   0.000    1.000
sprayon:CO2elevated    0.080    772.854   0.000    1.000

inter.pft<- glm(survival ~ spray*CO2 +pft. , family="binomial", data = new.survival)
inter.pft.summary<-summary(inter.pft)#all significant when compared with C3 grasses!!!
output.inter.pft<- as.data.frame(round(coef(inter.pft.summary), digits= 3))
output.inter.pft
#OUTPUT:
Estimate Std. Error z value Pr(>|z|)
(Intercept)           18.382    808.614   0.023    0.982
sprayon              -19.868    808.614  -0.025    0.980
CO2elevated            0.004   1153.792   0.000    1.000
pft.herb               1.365      0.274   4.983    0.000
pft.C4grass            1.718      0.303   5.669    0.000
pft.shrub              2.374      0.248   9.583    0.000
pft.vine               2.548      0.265   9.617    0.000
sprayon:CO2elevated    0.034   1153.792   0.000    1.000


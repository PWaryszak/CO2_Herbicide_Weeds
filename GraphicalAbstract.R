#PFT + Plant Functional Groups=======
#Analysis without Olea (misprayed)
#CO2 X Herbicde X PFT analysis of data from glasshouse:
#LOAD DATA:
data <- read.csv("CO2survival.csv")
survival<-subset(data, data=="survival.data")
#Removing Olea = OLEA must go as it was misprayed:
survival<-survival[!survival$genus=="Olea",]
survival$genus<- factor(survival$genus) #way to recategorize genus to delete Olea.
dim(survival)#1064 obs. of  20 variables:
levels(survival$genus)
survival$PFT<-as.factor(survival$genus)

#Changing the level names to PFT-s:
levels(survival$PFT)[levels(survival$PFT)=="Tradescantia"]<-"herb"
levels(survival$PFT)[levels(survival$PFT)=="Verbena"   ] <- "herb"
levels(survival$PFT)[levels(survival$PFT)=="Ageratina" ] <- "herb"
levels(survival$PFT)[levels(survival$PFT)=="Anredera"  ] <- "vine"
levels(survival$PFT)[levels(survival$PFT)=="Asparagus"] <-  "herb"
levels(survival$PFT)[levels(survival$PFT)=="Avena"     ]<-  "C3grass"
levels(survival$PFT)[levels(survival$PFT)=="Bromus"    ] <- "C3grass"
levels(survival$PFT)[levels(survival$PFT)=="Chloris"   ] <- "C4grass"
levels(survival$PFT)[levels(survival$PFT)=="Cotoneaster"] <-"shrub"
levels(survival$PFT)[levels(survival$PFT)=="Ehrharta"  ] <- "C3grass"
levels(survival$PFT)[levels(survival$PFT)=="Ipomoea"   ] <- "vine"
levels(survival$PFT)[levels(survival$PFT)=="Lantana"   ] <- "shrub"
levels(survival$PFT)[levels(survival$PFT)=="Cenchrus"] <- "C4grass"
levels(survival$PFT)[levels(survival$PFT)=="Lantana"   ] <- "shrub"
levels(survival$PFT)[levels(survival$PFT)=="Senna"     ] <- "shrub"

#Adding additional Herbicide column as herbicide rate showed not significan difference:
survival$Herbicide<-as.factor(ifelse(survival$herbicide=="Control", "Off", "On"))
survival$PFT<-factor(survival$PFT,levels= c("C3grass","C4grass","herb","shrub","vine" ))
survival$PFT<-factor(survival$PFT,levels= c("C3grass","shrub","herb","C4grass","vine" ))
levels(survival$PFT)#"C3grass" "herb" "C4grass" "shrub"   "vine" 
#LOAD Libraries:
library(nlme)
library(sjmisc)
library(tidyverse)
library(lme4)
library("sjPlot")

#G2 Binomial model to produce graphical abstract for JEM:======
#Keep spray on and elevated CO2 only for simplicity reasons:
abstract<-survival[survival$Herbicide=="On" & survival$CO2=="elevated",]
#run simple model on PFT-s only:
abstract.model<- glmer(survival ~  PFT + (1 | glasshouse), 
                       family="binomial", data = abstract)
summary(abstract.model)

sjp.glmer(abstract.model, type = "fe")#Simple plot of effect sizes.

#Choose theme for your plot:
set_theme(geom.outline.color = "antiquewhite4", 
          geom.label.color = "grey70",
          title.color = "red", 
          title.size = 1.3, 
          title.align = "center",
          axis.textcolor = "black", 
          axis.textsize = 1.5,
          base = theme_classic())

#Plot effect sizes of abstract.model:
sjp.glmer(abstract.model, type = "pred",vars = c("PFT"),show.scatter = T,
          show.ci =  T, facet.grid = T, 
          title = "Survival (±95%CI) of five growth types (14 sp.) grown under elevated CO2 and treated with herbicide") 

#Many functions are not available but you can access data of sjp plot to manipulated it:
https://stackoverflow.com/questions/40620644/changing-line-colors-and-types-of-a-spj-glmer-probability-plots-of-covariates-by


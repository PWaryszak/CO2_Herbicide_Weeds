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

#LOAD Libraries:
library(nlme)
library(sjmisc)
library(tidyverse)
library(lme4)
library("sjPlot")

#Changing the level names to PFT-s:
levels(survival$PFT)[levels(survival$PFT)=="Avena"     ]<-  "C3grass"
levels(survival$PFT)[levels(survival$PFT)=="Bromus"    ] <- "C3grass"
levels(survival$PFT)[levels(survival$PFT)=="Ehrharta"  ] <- "C3grass"
levels(survival$PFT)[levels(survival$PFT)=="Chloris"   ] <- "C4grass"
levels(survival$PFT)[levels(survival$PFT)=="Cenchrus"  ] <- "C4grass"
levels(survival$PFT)[levels(survival$PFT)=="Tradescantia"]<-"herb"
levels(survival$PFT)[levels(survival$PFT)=="Verbena"   ] <- "herb"
levels(survival$PFT)[levels(survival$PFT)=="Ageratina" ] <- "herb"
levels(survival$PFT)[levels(survival$PFT)=="Asparagus"] <-  "shrub"
levels(survival$PFT)[levels(survival$PFT)=="Cotoneaster"] <-"shrub"
levels(survival$PFT)[levels(survival$PFT)=="Lantana"   ] <- "shrub"
levels(survival$PFT)[levels(survival$PFT)=="Senna"     ] <- "shrub"
levels(survival$PFT)[levels(survival$PFT)=="Ipomoea"   ] <- "vine"
levels(survival$PFT)[levels(survival$PFT)=="Anredera"  ] <- "vine"

#Adding additional Herbicide column as herbicide rate showed not significan difference:
survival$Herbicide<-as.factor(ifelse(survival$herbicide=="Control", "Off", "On"))
survival$PFT<-factor(survival$PFT,levels= c("C3grass","herb","C4grass","shrub","vine" ))
levels(survival$PFT)#"C3grass" "herb" "C4grass" "shrub"   "vine" 

#G2 Binomial model to produce graphical abstract for JEM:======
#Keep spray on and elevated CO2 only for simplicity reasons:
abstract <- filter(survival, CO2=="elevated" & glyphosate=="glyphosate" &
                    Herbicide=="On")
#run simple model on PFT-s only:
abstract.model<- glm(survival ~  PFT ,family="binomial", data = abstract)
summary(abstract.model)

#Choose theme for your plot:
set_theme(geom.outline.color = "black",
          title.color = "black", 
          title.size = 1.5, 
          axis.title.size = 1.5,
          title.align = "center",
          axis.textcolor = "black", 
          axis.textsize = 1.5,
          axis.title.color = "black",
          base = theme_classic())
#Plot effect sizes of abstract.model:
sjp.glm(abstract.model, type = "pred",vars = c("PFT"),
          show.ci =  T,geom.colors  = "black",
         show.scatter = F, axis.lim = c(0,1),
        title = "Weeds grown under elevated CO  and treated with glyphosate",
        axis.title = c( "", "Survival (±95% CI)"))
        
#Many functions are not available but you can access data of sjp plot to manipulated it:
#https://stackoverflow.com/questions/40620644/changing-line-colors-and-types-of-a-spj-glmer-probability-plots-of-covariates-by
ggsave(filename="p4.pdf", width=10, height=4,bg = "transparent")

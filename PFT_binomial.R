#PFT + Plant Functional Groups=======
#Analysis without Olea (misprayed)
#CO2 X Herbicde X PFT analysis of data from glasshouse:
#LOAD DATA:
data <- read.csv("CO2survival.csv")
survival<-subset(data, data=="survival.data")
#Removing Olea = OLEA must go as it was misprayed:
survival<-survival[!survival$genus=="Olea",]
survival$genus<- factor(survival$genus) #way to recategorize genus to delete Olea.
str(survival)#1064 obs. of  18 variables:
levels(survival$genus)
survival$PFT<-as.factor(survival$genus)

#Changing the level names to PFT-s:
levels(survival$PFT)[levels(survival$PFT)=="Tradescantia"]<-"herb"
levels(survival$PFT)[levels(survival$PFT)=="Verbena"   ] <- "herb"
levels(survival$PFT)[levels(survival$PFT)=="Ageratina" ] <- "herb"
levels(survival$PFT)[levels(survival$PFT)=="Anredera"  ] <- "vine"
levels(survival$PFT)[levels(survival$PFT)=="Asparagus"] <-  "shrub"
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

#Adding additional Herbicide column:
survival$Herbicide<-as.factor(ifelse(survival$herbicide=="Control", "Off", "On"))
survival$PFT<-factor(survival$PFT,levels= c("C3grass","herb","C4grass","shrub","vine" ))
levels(survival$PFT)#"C3grass" "herb" "C4grass" "shrub"   "vine" 
names(survival)
#LOAD Libraries:
library(sjstats)
#install.packages(c("sjPlot", "nlme", "sjmisc", "tidyverse"))
library(sjPlot)
library(nlme)
library(sjmisc)
library(tidyverse)
library(lme4)


#GLMER Binomial model: ALL DATA:======
g1<- glmer(survival ~ Herbicide + CO2 + PFT + (1 | glasshouse), family="binomial", data = survival)
g1.summary<-summary(g1)#nearly all significant when compared with C3 grasses!!!
g1.summary
#STATS Output:
Coefficients:
##########Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   18.5212   589.2061   0.031    0.975    
#Herbicide.On -19.6371   589.2061  -0.033    0.973    
#CO2.elevated   0.0344     0.1575   0.218    0.827    
#PFTherb       0.4992     0.2685   1.859    0.063 .  
#PFTC4grass    1.3500     0.3086   4.374 1.22e-05 ***
#PFTshrub      2.0063     0.2545   7.882 3.22e-15 ***
#PFTvine       2.1805     0.2713   8.036 9.27e-16 ***
  ---
(Dispersion parameter for binomial family taken to be 1)
Null deviance: 1337.03  on 1064  degrees of freedom
Residual deviance:  943.74  on 1058  degrees of freedom
AIC: 957.74

#SAVE as:
output.pft<- as.data.frame(round(coef(g1.summary), digits= 2))
output.pft
#write.csv(output.pft, file="Output_GLMER_HerbicidePFTcsv") #Saves the output in a excel file



#SJ-mplot of coeficients: g1 model:========
sjp.glmer(g1,sort.est = FALSE,type = "fe")
?sjp.glmer

#G2 Binomial model: Herbicide=ON DATA:======
survival2<-survival[survival$Herbicide=="On",]
g2<- glmer(survival ~ Dose * CO2 + PFT + (1 | glasshouse), 
           family="binomial", data = survival2)
g2.summary<-summary(g2)#nearly all significant when compared with C3 grasses!!!
g2.summary
#STATS Output:



#SAVE as:
output.pft2<- as.data.frame(round(coef(g2.summary), digits= 2))
output.pft2
#write.csv(output.pft2, file="Output_GLMER_Herbicide_ON_only_PFTcsv") #Saves the output in a excel file



#PLOT g2:
sjp.glmer(g2, type = "fe")#Simple plot of effect sizes.

#Choose theme for your plot:
sjp.setTheme(base = theme_light())

set_theme(geom.outline.color = "antiquewhite4", 
          geom.outline.size = 1, 
          geom.label.size = 2,
          geom.label.color = "grey70",
          title.color = "red", 
          title.size = 1.5, 
          title.align = "center",
          axis.textcolor = "blue", 
          base = theme_bw())
sjp.glmer(g2, type = "pred",vars = c("PFT","Dose","CO2"),show.scatter = F,
           show.ci =  T, facet.grid = T, 
          title = "Predicted survival under ambient & elevated CO2")

#Many functions are not available but you can access data of sjp plot to manipulated it:
https://stackoverflow.com/questions/40620644/changing-line-colors-and-types-of-a-spj-glmer-probability-plots-of-covariates-by





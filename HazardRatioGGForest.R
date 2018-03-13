#Kaplan Meier (KM) Curves:
#Multiplot function:
library("tidyverse")
library("glyph")
library("survminer")
library("survival")
#Assumption TEST: http://www.sthda.com/english/wiki/cox-model-assumptions

#Data for 2 Herbicide Treatments (Glyphosate & Starane)==========
#START FROM HERE:
df<-read.csv("WeedsBinary.csv")
#Glyphosate only subset:
glyph<-df[df$herbicide=="glyphosate",]
glyph$genus<-factor(glyph$genus)
str(glyph)#2930 obs. of  9 variables:
glyph$CO2.Herbicide <- glyph$CO2_Dose_
glyph$CO2.Herbicide<- factor(glyph$CO2.Herbicide, levels = c("ambient.recom","ambient.double","elevated.recom","elevated.double"))
#STARANE only subset
star<-df[df$herbicide=="starane",] #subset species that were treated with starane
star$genus<-factor(star$genus)#makes sure these species are factors to ease running Cox model
str(star)#data.frame':760 obs. of  9 variables:
levels(star$Dose)#"double" "recom"  - LEt us relevel the Dose factor to show double in stats table below:
star$Dose<- factor(star$Dose, levels = c("recom","double"))
levels(star$genus)#3 only: "Anredera","Lantana", "Tradescantia"
star$CO2.Herbicide <- star$CO2_Dose_
star$CO2.Herbicide<- factor(star$CO2.Herbicide, levels = c("ambient.recom","ambient.double","elevated.recom","elevated.double"))


#These plots to be arranged by species as in KM plot:
#1Ageratina ============ 
Ageratina <- glyph[glyph$genus=="Ageratina", ]
fit.Ageratina <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ageratina)
summary(fit.Ageratina) # Access to the sort summary table
#Plot:
plot.Ageratina <- ggforest(fit.Ageratina, main = "Ageratina adenophora (glyphosate)", fontsize = 0.9)
#plot.Ageratina
# test for the proportional-hazards (PH) assumption, type this:
test.ph1 <- cox.zph(fit.Ageratina)
test.ph1
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.

#2Anredera under glyphosate============ 
Anredera <- glyph[glyph$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
plot.Anredera <- ggforest(fit.Anredera,fontsize = 0.9,main = "Anredera cordifolia (glyphosate)")
#plot.Anredera
test.ph2 <- cox.zph(fit.Anredera)
test.ph2
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.

#3Anredera under Starane ============ 
Anredera <- star[star$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
plot.Anredera.starane <- ggforest(fit.Anredera, main = "Anredera cordifolia (starane)",fontsize = 0.9)
#plot.Anredera.starane
test.ph3 <- cox.zph(fit.Anredera)
test.ph3
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.

#4Avena ============ 
Avena<-glyph[glyph$genus=="Avena", ]
fit.Avena <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Avena)
summary(fit.Avena)# Access to the sort summary table
#Plot:
plot.Avena  <- ggforest(fit.Avena, main = "Avena barbata (glyphosate)",fontsize=0.9)
#plot.Avena
#5Bromus =================
Bromus<-glyph[glyph$genus=="Bromus", ]
fit.Bromus <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Bromus)
summary(fit.Bromus)# Access to the sort summary table
#Plot:
plot.Bromus  <- ggforest(fit.Bromus, main = "Bromus catharticus (glyphosate)",fontsize=0.9)
#plot.Bromus
test.ph5 <- cox.zph(fit.Bromus)
test.ph5
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.

#6Cenchrus  ============ 
Pennisetum <- glyph[glyph$genus=="Pennisetum", ]
fit.Pennisetum <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Pennisetum)
summary(fit.Pennisetum) # Access to the sort summary table
#Plot:
plot.Pennisetum  <- ggforest(fit.Pennisetum, main = "Cenchrus clandestinum (glyphosate)",fontsize=0.9)
#plot.Pennisetum
test.ph6 <- cox.zph(fit.Pennisetum )
test.ph6
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.

#7Chloris  ============ 
Chloris <- glyph[glyph$genus=="Chloris", ]
fit.Chloris <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Chloris)
summary(fit.Chloris) # Access to the sort summary table
#Plot:
plot.Chloris <- ggforest(fit.Chloris, main = "Chloris gayana (glyphosate)",fontsize = 0.9)
#plot.Chloris
test.ph7 <- cox.zph(fit.Chloris)
test.ph7
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.

#8Ehrharta  ============ 
Ehrharta<- glyph[glyph$genus=="Ehrharta", ]
fit.Ehrharta<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ehrharta)
summary(fit.Ehrharta) # Access to the sort summary table
#Plot:
plot.Ehrharta <- ggforest(fit.Ehrharta, main ="Ehrharta erecta (glyphosate)",fontsize=0.9) 
#plot.Ehrharta
test.ph8 <- cox.zph(fit.Ehrharta)
test.ph8
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.


#9Lantana under Starane============ 
Lantana<- star[star$genus=="Lantana", ]
fit.Lantana<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Lantana)
summary(fit.Lantana) # Access to the sort summary table
#Plot:
plot.Lantana.starane <- ggforest(fit.Lantana, main = "Lantana camara (starane)", fontsize = 0.9)
#plot.Lantana.starane
test.ph9 <- cox.zph(fit.Lantana)
test.ph9
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.

#10Senna ============ 
Senna <- glyph[glyph$genus=="Senna", ]
fit.Senna <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Senna)
summary(fit.Senna) # Access to the sort summary table
#Plot:
plot.Senna <- ggforest(fit.Senna,main = "Senna pendula (glyphosate)",fontsize = 0.9) 
#plot.Senna
test.ph10 <- cox.zph(fit.Senna)
test.ph10
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.

#11Tradescantia under Starane  ============ 
Tradescantia <- star[star$genus=="Tradescantia", ]
fit.Tradescantia <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Tradescantia)
summary(fit.Tradescantia) # Access to the sort summary table
#Plot:
plot.Tradescantia.starane <- ggforest(fit.Tradescantia,
                                      main = "Tradescantia fluminensis (starane)",
                                      fontsize=0.9) 
test.ph11 <- cox.zph(fit.Tradescantia)
test.ph11

#A violations!!!
#A violations of proportional hazards assumption can be resolved by:
# 1) Adding covariate*time interaction
fit.Tradescantia2 <- coxph( Surv(Week, Status) ~  CO2.Herbicide*Week, data = Tradescantia)
summary(fit.Tradescantia2) # Access to the sort summary table
#HR TEST:
test.ph11b <- cox.zph(fit.Tradescantia2)
test.ph11b
#In tradescantia there was significant interaction with time


#12Verbena ============ 
Verbena <- glyph[glyph$genus=="Verbena", ]
fit.Verbena <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Verbena)
summary(fit.Verbena) # Access to the sort summary table
#Plot:
plot.Verbena <- ggforest(fit.Verbena, main = "Verbena bonariensis (glyphosate)", fontsize = 0.9)
#plot.Verbena
test.ph12 <- cox.zph(fit.Verbena)
test.ph12
#The test is not statistically significant for each of the covariates,
#and the global test is also not statistically significant. 
#Therefore, we can assume the proportional hazards.



#GRID-Arrange ALL PLOTS========
g1 <- ggarrange(plot.Ageratina, plot.Anredera, plot.Anredera.starane,
                plot.Avena, plot.Bromus , plot.Pennisetum ,
                plot.Chloris , plot.Ehrharta, plot.Lantana.starane,
                plot.Senna, plot.Tradescantia.starane, plot.Verbena, ncol = 3, nrow = 2)
ggsave(g1, filename = "HR_Mulitplot12species.jpg", width = 25, height = 20 )  



grasses <- ggarrange(plot.Avena, plot.Bromus , plot.Pennisetum ,
                plot.Chloris , plot.Ehrharta, plot.Verbena, ncol = 2, nrow = 3)
ggsave(grasses, filename = "10.GrassesPlusVerbena.jpg", width = 15, height = 15 )  

weedsRest <- ggarrange( plot.Anredera, plot.Anredera.starane,
                        plot.Ageratina,plot.Lantana.starane, plot.Senna, plot.Tradescantia.starane, ncol = 2, nrow = 3)
ggsave(weedsRest, filename = "01.HR_WeedsRest.jpg", width = 15, height = 15 )  

#It works after updating the pckges as per recommendation of Przemek:
#https://github.com/kassambara/survminer/issues/114#issuecomment-371729599
devtools::session_info() #check whehter your pckages are updated.




#Asparagus NO HAZARD, No Plot ============ 
Asparagus<-glyph[glyph$genus=="Asparagus", ]
fit.Asparagus <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Asparagus)
summary(fit.Asparagus)# Access to the sort summary table
#Plot:
plot.Asparagus <- ggforest(fit.Asparagus,main = "Asparagus aethiopicus (glyphosate)", fontsize = 0.9)
#plot.Asparagus

#Cotoneaster NO HAZARD, No Plot =================
Cotoneaster<-glyph[glyph$genus=="Cotoneaster", ]
fit.Cotoneaster <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Cotoneaster)
summary(fit.Cotoneaster)# Access to the sort summary table
#Plot:
plot.Cotoneaster <- ggforest(fit.Cotoneaster, main = "Cotoneaster glaucophyllus (glyphosate)",fontsize = 0.9)
#plot.Cotoneaster

#Lantana NO HAZARD, No Plot ============ 
Lantana<- glyph[glyph$genus=="Lantana", ]
fit.Lantana<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Lantana)
summary(fit.Lantana) # Access to the sort summary table
#Plot:
plot.Lantana <- ggforest(fit.Lantana, main = "Lantana camara (glyphosate)",fontsize = 0.9)
#plot.Lantana #NO HAZARD - remove from the multiplot

#Ipomea NO HAZARD, No Plot============ 

#PFT##############
levels(glyph$genus)
glyph$PFT<-as.factor(glyph$genus)

levels(glyph$PFT)[levels(glyph$PFT)=="Tradescantia"]<-"herb"
levels(glyph$PFT)[levels(glyph$PFT)=="Verbena"   ] <- "herb"
levels(glyph$PFT)[levels(glyph$PFT)=="Ageratina" ] <- "herb"
levels(glyph$PFT)[levels(glyph$PFT)=="Anredera"  ] <- "vine"
levels(glyph$PFT)[levels(glyph$PFT)=="Asparagus"] <-  "herb"
levels(glyph$PFT)[levels(glyph$PFT)=="Avena"     ]<-  "C3grass"
levels(glyph$PFT)[levels(glyph$PFT)=="Bromus"    ] <- "C3grass"
levels(glyph$PFT)[levels(glyph$PFT)=="Chloris"   ] <- "C4grass"
levels(glyph$PFT)[levels(glyph$PFT)=="Cotoneaster"] <-"shrub"
levels(glyph$PFT)[levels(glyph$PFT)=="Ehrharta"  ] <- "C3grass"
levels(glyph$PFT)[levels(glyph$PFT)=="Ipomoea"   ] <- "vine"
levels(glyph$PFT)[levels(glyph$PFT)=="Lantana"   ] <- "shrub"
levels(glyph$PFT)[levels(glyph$PFT)=="Pennisetum"] <- "C4grass"
levels(glyph$PFT)[levels(glyph$PFT)=="Lantana"   ] <- "shrub"
levels(glyph$PFT)[levels(glyph$PFT)=="Senna"     ] <- "shrub"
levels(glyph$PFT)#"herb"    "vine"    "C3grass" "C4grass" "shrub"

herb <- glyph[glyph$PFT=="herb", ]
fit.herb <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = herb)
summary(fit.herb) # Access to the sort summary table
#Plot:
plot.herb <- ggforest(fit.herb, main = "herb (glyphosate)", fontsize = 0.9)
plot.herb

vine <- glyph[glyph$PFT=="vine", ]
fit.vine <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = vine)
summary(fit.vine) # Access to the sort summary table
#Plot:
plot.vine <- ggforest(fit.vine, main = "Vines (glyphosate)", fontsize = 0.9)
plot.vine

C3grass <- glyph[glyph$PFT=="C3grass", ]
fit.C3grass <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = C3grass)
summary(fit.C3grass) # Access to the sort summary table
#Plot:
plot.C3grass <- ggforest(fit.C3grass, main = "C3grass (glyphosate)", fontsize = 0.9)
plot.C3grass

C4grass <- glyph[glyph$PFT=="C4grass", ]
fit.C4grass <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = C4grass)
summary(fit.C4grass) # Access to the sort summary table
#Plot:
plot.C4grass <- ggforest(fit.C4grass, main = "C4grass (glyphosate)", fontsize = 0.9)
plot.C4grass

Allgrass <- glyph[glyph$PFT=="C4grass"|glyph$PFT=="C3grass", ]
fit.Allgrass <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Allgrass)
summary(fit.Allgrass) # Access to the sort summary table
#Plot:
plot.Allgrass <- ggforest(fit.C4grass, main = "Grasses (glyphosate)", fontsize = 0.9)
plot.Allgrass

shrub <- glyph[glyph$PFT=="shrub", ]
fit.shrub <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = shrub)
summary(fit.shrub) # Access to the sort summary table
#Plot:
plot.shrub <- ggforest(fit.shrub, main = "Shrubs (glyphosate)", fontsize = 0.9)
plot.shrub

g3 <- ggarrange(plot.Allgrass, plot.shrub, plot.vine, ncol = 3, nrow = 1)
ggsave(g3, filename = "04.HR_GraphicalAbstract.jpg", width = 24, height = 6 )  




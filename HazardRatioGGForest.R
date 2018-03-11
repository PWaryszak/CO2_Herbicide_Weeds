#Kaplan Meier (KM) Curves:
#Multiplot function:
library("tidyverse")
library("survival")
library("survminer")

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

#2Anredera under glyphosate============ 
Anredera <- glyph[glyph$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
plot.Anredera <- ggforest(fit.Anredera,fontsize = 0.9,main = "Anredera cordifolia (glyphosate)")
#plot.Anredera

#3Anredera under Starane ============ 
Anredera <- star[star$genus=="Anredera", ]
fit.Anredera <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Anredera)
summary(fit.Anredera) # Access to the sort summary table
#Plot:
plot.Anredera.starane <- ggforest(fit.Anredera, main = "Anredera cordifolia (starane)",fontsize = 0.9)
#plot.Anredera.starane

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

#6Cenchrus  ============ 
Pennisetum <- glyph[glyph$genus=="Pennisetum", ]
fit.Pennisetum <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Pennisetum)
summary(fit.Pennisetum) # Access to the sort summary table
#Plot:
plot.Pennisetum  <- ggforest(fit.Pennisetum, main = "Cenchrus clandestinum (glyphosate)",fontsize=0.9)
#plot.Pennisetum

#7Chloris  ============ 
Chloris <- glyph[glyph$genus=="Chloris", ]
fit.Chloris <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Chloris)
summary(fit.Chloris) # Access to the sort summary table
#Plot:
plot.Chloris <- ggforest(fit.Chloris, main = "Chloris gayana (glyphosate)",fontsize = 0.9)
#plot.Chloris

#8Ehrharta  ============ 
Ehrharta<- glyph[glyph$genus=="Ehrharta", ]
fit.Ehrharta<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ehrharta)
summary(fit.Ehrharta) # Access to the sort summary table
#Plot:
plot.Ehrharta <- ggforest(fit.Ehrharta, main ="Ehrharta erecta (glyphosate)",fontsize=0.9) 
#plot.Ehrharta


#9Lantana under Starane============ 
Lantana<- star[star$genus=="Lantana", ]
fit.Lantana<- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Lantana)
summary(fit.Lantana) # Access to the sort summary table
#Plot:
plot.Lantana.starane <- ggforest(fit.Lantana, main = "Lantana camara (starane)", fontsize = 0.9)
#plot.Lantana.starane

#10Senna ============ 
Senna <- glyph[glyph$genus=="Senna", ]
fit.Senna <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Senna)
summary(fit.Senna) # Access to the sort summary table
#Plot:
plot.Senna <- ggforest(fit.Senna,main = "Senna pendula (glyphosate)",fontsize = 0.9) 
#plot.Senna

#11Tradescantia under Starane  ============ 
Tradescantia <- star[star$genus=="Tradescantia", ]
fit.Tradescantia <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Tradescantia)
summary(fit.Tradescantia) # Access to the sort summary table
#Plot:
plot.Tradescantia.starane <- ggforest(fit.Tradescantia,
                                      main = "Tradescantia fluminensis (starane)",
                                      fontsize=0.9) 
#plot.Tradescantia.starane

#12Verbena ============ 
Verbena <- glyph[glyph$genus=="Verbena", ]
fit.Verbena <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Verbena)
summary(fit.Verbena) # Access to the sort summary table
#Plot:
plot.Verbena <- ggforest(fit.Verbena, main = "Verbena bonariensis (glyphosate)", fontsize = 0.9)
#plot.Verbena

#Arrange ALL PLOTS========
g1 <- ggarrange(plot.Ageratina, plot.Anredera, plot.Anredera.starane,
                plot.Avena, plot.Bromus , plot.Pennisetum ,
                plot.Chloris , plot.Ehrharta, plot.Lantana.starane,
                plot.Senna, plot.Tradescantia.starane, plot.Verbena, ncol = 3, nrow = 4)
ggsave(g1, filename = "HR_Mulitplot12species.jpg", width = 25, height = 20 )  

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



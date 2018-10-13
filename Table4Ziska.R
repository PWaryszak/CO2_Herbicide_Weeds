#Script for Lewis Z. as per his last request to:
#create  a brief and simple table basically listing the species, 
#and a simple "yes" or "no" as to whether CO2 affected herbicide efficacy.

#Install these packages if not done so yet:
library("tidyverse")
library("glyph")
library("survminer")
library("survival")

#Data for 2 Herbicide Treatments (Glyphosate & Starane)==========
#START FROM HERE:
df<-read.csv("WeedsBinary2.csv")
str(df)#6180 obs. of  18 variables:

#GLUPHOSATE-only subset:
glyph<-df[df$herbicide=="glyphosate",]
glyph$genus<-factor(glyph$genus)
str(glyph)#3456 obs. of  18 variables:
glyph$CO2.Herbicide<- factor(glyph$CO2.Herbicide, levels = c("ambient.recom","ambient.double","elevated.recom","elevated.double"))


#Test on 01Ageratina ============ 
Ageratina <- glyph[glyph$genus=="Ageratina", ]
fit.Ageratina <- coxph( Surv(Week, Status) ~  CO2.Herbicide, data = Ageratina)
fit.Ageratina.summary <- summary(fit.Ageratina) # Access to the sort summary table
HazardRatio <- fit.Ageratina.summary  $ coefficients [ 2,2 ] #location of Coef value for Elevated CO2 effect on herbicide efficacy (recommended level only)
PValue <- fit.Ageratina.summary $ coefficients [ 2,5 ] #Location of PValue for above /\
Weed01 <- data.frame(HR = HazardRatio, P = PValue, Species = "Ageratina", CO2 = "elevated")
Weed01

#Compute & Extract coef for elevated CO2 and Recommended level of herbicide:
df1 <- glyph %>% select(genus, species, CO2.Herbicide, Week, Status) %>%
  group_by(genus)  %>%
  mutate (HR = summary(coxph( Surv(Week, Status) ~  CO2.Herbicide)) $ coefficients[2,2],
          P  = summary(coxph( Surv(Week, Status) ~  CO2.Herbicide)) $ coefficients[2,5]) %>%
  select(genus,species, HR, P)

df2 <- as.data.frame(df1) #covnert to data.frame to continue. Pipeline got blown-up by coxph function above.
df3 <- df2 %>% group_by(genus,species) %>% summarize( HazardRatio_ElevatedCO2 = mean(HR),
                                              PValue_ElevatedCO2 = mean (P)) %>%
  mutate (CO2_affected_herbicide_efficacy = ifelse(PValue_ElevatedCO2 > 0.055 | PValue_ElevatedCO2 =="NaN", "No", "Yes")) 


#Recode genus into plant functional type (Growth_Type):=====
df3$Growth_Type<-df3$genus
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Tradescantia"]<-"herb"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Verbena"   ] <- "herb"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Ageratina" ] <- "herb"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Anredera"  ] <- "vine"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Asparagus"] <-  "shrub" 
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Avena"     ]<-  "C3grass"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Bromus"    ] <- "C3grass"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Chloris"   ] <- "C4grass"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Cotoneaster"] <-"shrub"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Ehrharta"  ] <- "C3grass"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Ipomoea"   ] <- "vine"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Lantana"   ] <- "shrub"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Pennisetum"] <- "C4grass" #Name of Pennisetum has changed to Cenchrus (as in paper)
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Lantana"   ] <- "shrub"
levels(df3$Growth_Type)[levels(df3$Growth_Type)=="Senna"     ] <- "shrub"

#Data available from:
https://data.mendeley.com/datasets/894y2x52sw/1


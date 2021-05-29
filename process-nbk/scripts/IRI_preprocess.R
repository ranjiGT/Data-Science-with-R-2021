#Objective-3 (draft - would be merged to Rmarkdown)

library(dplyr)
library(foreign)
library(ggplot2)
library(haven) 
library(jtools)
library(lavaan)
library(lme4) 
library(lmerTest) 
library(patchwork) 
library(performance)
library(qgraph)
library(reshape2)
library(sjmisc)
library(sm)
library(readr)


#Convert sav to csv

# sav <- read_sav("DATA_COVID19_TrustInformation.sav")
# getwd() # this is the folder it will save into unless you specify otherwise in the path below
# write_csv(x=sav, path="DATA_COVID19_TrustInformation.csv")

# Data loading
data_12 <- read.spss("DATA_COVID19_TrustInformation.sav",
                     use.value.labels = FALSE,
                     to.data.frame = TRUE)
names(data_12) <- tolower(names(data_12))

# Data pre-processing pipeline

#Converting for 'z' scores / standardization transforms your data such that the resulting distribution has a mean of 0 and a standard deviation of 1
data_12 <- 
  data_12 %>% 
  mutate(avoidactions_avgZ = (avoid_avg - mean(avoid_avg, na.rm=T))/sd(avoid_avg, na.rm=T))%>% 
  mutate(safetyprecautions_avgZ = (safetyprecautions_avg - mean(safetyprecautions_avg, na.rm=T))/sd(safetyprecautions_avg, na.rm=T))%>% 
  mutate(otheravoid_avgZ = (otheravoid_avg - mean(otheravoid_avg, na.rm=T))/sd(otheravoid_avg, na.rm=T))

data_12 <- 
  data_12 %>% 
  mutate(actselfZ = (actions_selfcare - mean(actions_selfcare, na.rm=T))/sd(actions_selfcare, na.rm=T))%>% 
  mutate(actnegZ = (actions_negout - mean(actions_negout, na.rm=T))/sd(actions_negout, na.rm=T))%>% 
  mutate(actdistZ = (actions_distance - mean(actions_distance, na.rm=T))/sd(actions_distance, na.rm=T))%>% 
  mutate(actmaskZ = (actions_masking - mean(actions_masking, na.rm=T))/sd(actions_masking, na.rm=T))

data_12 <- 
  data_12 %>% 
  mutate(risk2Z = (risk2_avg - mean(risk2_avg, na.rm=T))/sd(risk2_avg, na.rm=T))%>%
  mutate(concernZ = (concerned_quant - mean(concerned_quant, na.rm=T))/sd(concerned_quant, na.rm=T))%>% 
  mutate(feelinginformedZ = (feelinginformed_avg - mean(feelinginformed_avg, na.rm=T))/sd(feelinginformed_avg, na.rm=T))%>% 
  mutate(conspiracyZ = (virus_natart - mean(virus_natart, na.rm=T))/sd(virus_natart, na.rm=T))%>% 
  mutate(threatglobalZ = (severe_globalsociety - mean(severe_globalsociety, na.rm=T))/sd(severe_globalsociety, na.rm=T))

data_12 <- 
  data_12 %>% 
  mutate(trustgov_localZ = (trustgov_country - mean(trustgov_country, na.rm=T))/sd(trustgov_country, na.rm=T))%>% 
  mutate(trustgov_globalZ = (trustgov_global - mean(trustgov_global, na.rm=T))/sd(trustgov_global, na.rm=T))%>% 
  mutate(trustscient_localZ = (trustscient_country - mean(trustscient_country, na.rm=T))/sd(trustscient_country, na.rm=T))%>% 
  mutate(trustscient_globalZ = (trustscient_global - mean(trustscient_global, na.rm=T))/sd(trustscient_global, na.rm=T))%>% 
  mutate(trustpers_localZ = (trustpers_country - mean(trustpers_country, na.rm=T))/sd(trustpers_country, na.rm=T))%>% 
  mutate(trustpers_globalZ = (trustpers_global - mean(trustpers_global, na.rm=T))/sd(trustpers_global, na.rm=T))%>% 
  mutate(trustscientistsZ = (trust_scientists - mean(trust_scientists, na.rm=T))/sd(trust_scientists, na.rm=T))%>% 
  mutate(trust_peopleZ = (trust_people - mean(trust_people, na.rm=T))/sd(trust_people, na.rm=T))%>% 
  mutate(trustgov_nonZ = (trustgovnon - mean(trustgovnon, na.rm=T))/sd(trustgovnon, na.rm=T))%>% 
  mutate(trustgov_popZ = (trustgovpop - mean(trustgovpop, na.rm=T))/sd(trustgovpop, na.rm=T))

data_12 <- 
  data_12 %>%
  mutate(fakenewsZ = (fakenews - mean(fakenews, na.rm=T))/sd(fakenews, na.rm=T))%>% 
  mutate(media_underoverZ = (media_underover - mean(media_underover, na.rm=T))/sd(media_underover, na.rm=T))%>% 
  mutate(trust_whoZ = (trust_who - mean(trust_who, na.rm=T))/sd(trust_who, na.rm=T))%>%
  mutate(trust_nhsZ = (trust_nhs - mean(trust_nhs, na.rm=T))/sd(trust_nhs, na.rm=T))%>%
  mutate(trust_govZ = (trust_gov - mean(trust_gov, na.rm=T))/sd(trust_gov, na.rm=T))%>%
  mutate(trust_institZ = (trust_institwebsites - mean(trust_institwebsites, na.rm=T))/sd(trust_institwebsites, na.rm=T))%>%
  mutate(trust_newspaperZ = (trust_newspaper - mean(trust_newspaper, na.rm=T))/sd(trust_newspaper, na.rm=T))%>%
  mutate(trust_fbZ = (trust_fb - mean(trust_fb, na.rm=T))/sd(trust_fb, na.rm=T))%>%
  mutate(trust_twZ = (trust_tw - mean(trust_tw, na.rm=T))/sd(trust_tw, na.rm=T))%>%
  mutate(trust_igZ = (trust_ig - mean(trust_ig, na.rm=T))/sd(trust_ig, na.rm=T))%>%
  mutate(trust_mapsZ = (trust_maps - mean(trust_maps, na.rm=T))/sd(trust_maps, na.rm=T))%>%
  mutate(trust_googleZ = (trust_google - mean(trust_google, na.rm=T))/sd(trust_google, na.rm=T))


data_non <- data_12[data_12$populistcountry==0,]
data_pop <- data_12[data_12$populistcountry==1,]


# Data preparation (recipe for scatter plot (objective 3))

#Gender 
demo_gender <- factor(data_12$demo_gender)
data <- data_12[data_12$demo_gender!=3,] 

#Countries
data_usa <- data[data$countryres==187,]
data_uk <-data[data$countryres==185,]
data_ita <-data[data$countryres==84,]
data_bra <-data[data$countryres==24,]
data_aus <-data[data$countryres==9,]
data_nld <-data[data$countryres==122,]
data_por <-data[data$countryres==138,]
data_ger <-data[data$countryres==65,]
data_fra <-data[data$countryres==61,]
data_fin <-data[data$countryres==60,]
data_cro <-data[data$countryres==42,]
data_nz <-data[data$countryres==123,]

# Karl-Pearson correlation (product-moment) test for `protective behaviors`

cor.test(data_12$concerned_quant, data_12$safetyprecautions_avg, 
         method = "pearson")

cor.test(data_12$risk2_avg, data_12$safetyprecautions_avg, 
         method = "pearson")

cor.test(data_12$concerned_quant, data_12$otheravoid_avg, 
         method = "pearson")

cor.test(data_12$risk2_avg, data_12$otheravoid_avg, 
         method = "pearson")

cor.test(data_12$risk2_avg, data_12$otheravoid_avg, 
         method = "pearson")

# preprocess for SPlot trust in gov vs. scientist (mean transform)
#References
#Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.

govloc.mean_usa <-  mean(data_usa$trustgov_country,na.rm = TRUE)
govloc.mean_uk <-  mean(data_uk$trustgov_country,na.rm = TRUE)
govloc.mean_ita <-  mean(data_ita$trustgov_country,na.rm = TRUE)
govloc.mean_bra <-  mean(data_bra$trustgov_country,na.rm = TRUE)
govloc.mean_aus <-  mean(data_aus$trustgov_country,na.rm = TRUE)
govloc.mean_nld <-  mean(data_nld$trustgov_country,na.rm = TRUE)
govloc.mean_por <-  mean(data_por$trustgov_country,na.rm = TRUE)
govloc.mean_ger <-  mean(data_ger$trustgov_country,na.rm = TRUE)
govloc.mean_fra <-  mean(data_fra$trustgov_country,na.rm = TRUE)
govloc.mean_fin <-  mean(data_fin$trustgov_country,na.rm = TRUE)
govloc.mean_cro <-  mean(data_cro$trustgov_country,na.rm = TRUE)
govloc.mean_nz <-  mean(data_nz$trustgov_country,na.rm = TRUE)

govglob.mean_usa <-  mean(data_usa$trustgov_global,na.rm = TRUE)
govglob.mean_uk <-  mean(data_uk$trustgov_global,na.rm = TRUE)
govglob.mean_ita <-  mean(data_ita$trustgov_global,na.rm = TRUE)
govglob.mean_bra <-  mean(data_bra$trustgov_global,na.rm = TRUE)
govglob.mean_aus <-  mean(data_aus$trustgov_global,na.rm = TRUE)
govglob.mean_nld <-  mean(data_nld$trustgov_global,na.rm = TRUE)
govglob.mean_por <-  mean(data_por$trustgov_global,na.rm = TRUE)
govglob.mean_ger <-  mean(data_ger$trustgov_global,na.rm = TRUE)
govglob.mean_fra <-  mean(data_fra$trustgov_global,na.rm = TRUE)
govglob.mean_fin <-  mean(data_fin$trustgov_global,na.rm = TRUE)
govglob.mean_cro <-  mean(data_cro$trustgov_global,na.rm = TRUE)
govglob.mean_nz <-  mean(data_nz$trustgov_global,na.rm = TRUE)

scientloc.mean_usa <-  mean(data_usa$trustscient_country,na.rm = TRUE)
scientloc.mean_uk <-  mean(data_uk$trustscient_country,na.rm = TRUE)
scientloc.mean_ita <-  mean(data_ita$trustscient_country,na.rm = TRUE)
scientloc.mean_bra <-  mean(data_bra$trustscient_country,na.rm = TRUE)
scientloc.mean_aus <-  mean(data_aus$trustscient_country,na.rm = TRUE)
scientloc.mean_nld <-  mean(data_nld$trustscient_country,na.rm = TRUE)
scientloc.mean_por <-  mean(data_por$trustscient_country,na.rm = TRUE)
scientloc.mean_ger <-  mean(data_ger$trustscient_country,na.rm = TRUE)
scientloc.mean_fra <-  mean(data_fra$trustscient_country,na.rm = TRUE)
scientloc.mean_fin <-  mean(data_fin$trustscient_country,na.rm = TRUE)
scientloc.mean_cro <-  mean(data_cro$trustscient_country,na.rm = TRUE)
scientloc.mean_nz <-  mean(data_nz$trustscient_country,na.rm = TRUE)

scientglob.mean_usa <-  mean(data_usa$trustscient_global,na.rm = TRUE)
scientglob.mean_uk <-  mean(data_uk$trustscient_global,na.rm = TRUE)
scientglob.mean_ita <-  mean(data_ita$trustscient_global,na.rm = TRUE)
scientglob.mean_bra <-  mean(data_bra$trustscient_global,na.rm = TRUE)
scientglob.mean_aus <-  mean(data_aus$trustscient_global,na.rm = TRUE)
scientglob.mean_nld <-  mean(data_nld$trustscient_global,na.rm = TRUE)
scientglob.mean_por <-  mean(data_por$trustscient_global,na.rm = TRUE)
scientglob.mean_ger <-  mean(data_ger$trustscient_global,na.rm = TRUE)

scientglob.mean_fra <-  mean(data_fra$trustscient_global,na.rm = TRUE)
scientglob.mean_fin <-  mean(data_fin$trustscient_global,na.rm = TRUE)
scientglob.mean_cro <-  mean(data_cro$trustscient_global,na.rm = TRUE)
scientglob.mean_nz <-  mean(data_nz$trustscient_global,na.rm = TRUE)

#basic scatter plot 

column1gov <- c("Australia","Brazil","Croatia","Finland","France","Germany","Italy","Netherlands", "New Zealand","Portugal","United Kingdom","United States")
column2gov <- c(govloc.mean_aus,govloc.mean_bra,govloc.mean_cro,govloc.mean_fin,govloc.mean_fra,govloc.mean_ger,govloc.mean_ita,govloc.mean_nld, govloc.mean_nz, govloc.mean_por,govloc.mean_uk,govloc.mean_usa)
column3gov <- c(govglob.mean_aus,govglob.mean_bra,govglob.mean_cro,govglob.mean_fin,govglob.mean_fra,govglob.mean_ger,govglob.mean_ita,govglob.mean_nld, govglob.mean_nz, govglob.mean_por,govglob.mean_uk,govglob.mean_usa)

data_scatter_gov <- cbind(column1gov,column2gov,column3gov)
colnames(data_scatter_gov) <- c("country", "govloc", "govglob")

write.csv(data_scatter_gov,'data_scatter_gov.csv')
data_scatterplot_gov <- read.csv("data_scatter_gov.csv")

# par(pty = "s")
# plot(govloc~govglob, col="lightblue", pch=19, cex=2, xlim=c(-10,10), ylim=c(-10,10), xlab="trust in global governments", ylab="trust in country's government", data=data_scatterplot_gov)
# text(govloc~govglob, labels=country,data=data_scatterplot_gov, cex=0.9, font=2)

ggplot(data_scatterplot_gov, aes(x=govglob, y=govloc)) +
  geom_point(col="blue", alpha = 0.9) + 
  geom_text(label=data_scatterplot_gov$country)+
  xlab("Trust in global governments")+
  ylab("Trust in country's government")+
  ggtitle("Plot for trust among citizens in Country's government for 12 different countries")
  

column1scient <- c("Australia","Brazil","Croatia","Finland","France","Germany","Italy","Netherlands", "New Zealand","Portugal","United Kingdom","United States")
column2scient <- c(scientloc.mean_aus,scientloc.mean_bra,scientloc.mean_cro,scientloc.mean_fin,scientloc.mean_fra,scientloc.mean_ger,scientloc.mean_ita,scientloc.mean_nld, scientloc.mean_nz, scientloc.mean_por,scientloc.mean_uk,scientloc.mean_usa)
column3scient <- c(scientglob.mean_aus,scientglob.mean_bra,scientglob.mean_cro,scientglob.mean_fin,scientglob.mean_fra,scientglob.mean_ger,scientglob.mean_ita,scientglob.mean_nld, scientglob.mean_nz, scientglob.mean_por,scientglob.mean_uk,scientglob.mean_usa)

data_scatter_scient <- cbind(column1scient,column2scient,column3scient)
colnames(data_scatter_scient) <- c("country", "scientloc", "scientglob")

write.csv(data_scatter_scient,'data_scatter_scient.csv')
data_scatterplot_scient <- read.csv("data_scatter_scient.csv")

# par(pty = "s")
# plot(scientloc~scientglob, col="lightblue", pch=19, cex=2, xlim=c(-10,10), ylim=c(-10,10), xlab="trust in scientists globally", ylab="trust in country's scientists", data=data_scatterplot_scient)
# text(scientloc~scientglob, labels=country,data=data_scatterplot_scient, cex=0.9, font=2)

ggplot(data_scatterplot_scient, aes(x=scientglob, y=scientloc)) +
  geom_point(col="blue", alpha = 0.9) + 
  geom_text(label=data_scatterplot_scient$country)+
  xlab("Trust in country's scientists")+
  ylab("Trust in scientists globally")+
  ggtitle("Plot for trust among citizens in Country's scientists for 12 different countries")

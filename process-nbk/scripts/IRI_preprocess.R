#Objective-3 (draft - would be merged to Rmarkdown)

#Taken from the transparencies of
#R. Gallotti, N. Castaldo, F. Valle, P. Sacco and M. De Domenico, COVID19 Infodemics Observatory (2020). DOI: 10.17605/OSF.IO/N6UPX

library(pacman)

p_load(dplyr, foreign, ggplot2, haven, jtools, lavaan, lme4, lmerTest, patchwork, performance, qgraph, reshape2, tidyr, sjmisc, sm, readr, ggpubr, plotly, RColorBrewer, scales, ggsci, mgcv, countrycode, ggrepel, anytime)

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

# Karl-Pearson correlation (product-moment) test for `protective behaviors`between paired samples

cor.test(data_12$concerned_quant, data_12$safetyprecautions_avg, 
         method = "pearson")

#Weak positive
ggscatter(data_12, x = "concerned_quant", y = "safetyprecautions_avg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Concerned quantity", ylab = "Average of saftey precautions")+
  ggtitle("Correlation bewtween quantity of concerned vs. Average saftey precautions")+
  geom_smooth(method = "lm")


#Weak positive
cor.test(data_12$risk2_avg, data_12$safetyprecautions_avg, 
         method = "pearson")

ggscatter(data_12, x = "risk2_avg", y = "safetyprecautions_avg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Average of risk", ylab = "Average of saftey precautions")+
  ggtitle("Correlation bewtween Average risk vs. Average saftey precautions")+
  geom_smooth()

#risk2_avg

#weak positive
cor.test(data_12$concerned_quant, data_12$otheravoid_avg, 
         method = "pearson")

ggscatter(data_12, x = "concerned_quant", y = "otheravoid_avg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Concerned quantity", ylab = "Average of other avoids")+
  ggtitle("Correlation bewtween quantity of concerned vs. Average other avoids")


#weak positive
cor.test(data_12$risk2_avg, data_12$otheravoid_avg, 
         method = "pearson")


ggscatter(data_12, x = "risk2_avg", y = "otheravoid_avg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Average of risk", ylab = "Average of other avoids")+
  ggtitle("Correlation bewtween Average risk vs. Average other avoids")


# ggplot()+
#   geom_scatter(data_12, aes(x = risk2_avg, y = otheravoid_avg))


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

#Shows a negative trend
ggplot(data_scatterplot_gov, aes(x=govglob, y=govloc)) +
  geom_point(col="blue", alpha = 0.9) + 
  geom_text(label=data_scatterplot_gov$country)+
  xlab("Trust in global governments")+
  ylab("Trust in country's government")+
  ggtitle("Plot for trust among citizens in Country's government for 12 different countries")+
  geom_smooth(method = "lm")
  

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

#Shows positive trend 
ggplot(data_scatterplot_scient, aes(x=scientglob, y=scientloc)) +
  geom_point(col="blue", alpha = 0.5) + 
  geom_text(label=data_scatterplot_scient$country)+
  xlab("Trust in country's scientists")+
  ylab("Trust in scientists globally")+
  ggtitle("Plot for trust among citizens in Country's scientists for 12 different countries")+
  geom_smooth(method = "lm")


### Table-2 regression analysis by `lme4`

full_conspir_non = lmer(conspiracyZ ~ trustgov_localZ + trustgov_globalZ + trustscient_localZ + trustscient_globalZ + factor(demo_gender) + demo_age + demo_education + demo_income + (1 | countryres),
                        data = data_non)
summary(full_conspir_non)
confint(full_conspir_non)
r2(full_conspir_non)
icc(full_conspir_non)
AIC(full_conspir_non)
BIC(full_conspir_non)


full_conspir_pop = lmer(conspiracyZ ~ trustgov_localZ + trustgov_globalZ + trustscient_localZ + trustscient_globalZ +  factor(demo_gender) + demo_age + demo_education + demo_income + (1 | countryres),
                        data = data_pop)
summary(full_conspir_pop)
confint(full_conspir_pop)
r2(full_conspir_pop)
icc(full_conspir_pop)
AIC(full_conspir_pop)
BIC(full_conspir_pop)

full_percknowl_non = lmer(feelinginformedZ ~ trustgov_localZ + trustgov_globalZ + trustscient_localZ + trustscient_globalZ  + factor(demo_gender) + demo_age + demo_education + demo_income + (1 | countryres),
                          data = data_non)
summary(full_percknowl_non)
confint(full_percknowl_non)
r2(full_percknowl_non)
icc(full_percknowl_non)
AIC(full_percknowl_non)
BIC(full_percknowl_non)


full_percknowl_pop = lmer(feelinginformedZ ~ trustgov_localZ + trustgov_globalZ + trustscient_localZ + trustscient_globalZ  + factor(demo_gender) + demo_age + demo_education + demo_income + (1 | countryres),
                          data = data_pop)
summary(full_percknowl_pop)
confint(full_percknowl_pop)
r2(full_percknowl_pop)
icc(full_percknowl_pop)
AIC(full_percknowl_pop)
BIC(full_percknowl_pop)


### Figure 3 ###

trustgov_localZ <- data_non$trustgov_localZ
feelinginformedZ<- data_non$feelinginformedZ
conspiracyZ<- data_non$conspiracyZ
safetyprecautions_avgZ<- data_non$safetyprecautions_avgZ
otheravoid_avgZ<- data_non$otheravoid_avgZ


trustgov_localZ <- data_pop$trustgov_localZ
feelinginformedZ<- data_pop$feelinginformedZ
conspiracyZ<- data_pop$conspiracyZ
safetyprecautions_avgZ<- data_pop$safetyprecautions_avgZ
otheravoid_avgZ<- data_pop$otheravoid_avgZ


meddata<-data.frame(cbind(trustgov_localZ, feelinginformedZ, conspiracyZ, safetyprecautions_avgZ, otheravoid_avgZ))


myModel <- '
feelinginformedZ ~ a1*trustgov_localZ
conspiracyZ ~ a2*trustgov_localZ
safetyprecautions_avgZ ~ b1*feelinginformedZ + b3*conspiracyZ + c1*trustgov_localZ
otheravoid_avgZ ~ b2*feelinginformedZ + b4*conspiracyZ + c2*trustgov_localZ
## indirects
indirect1 := a1 * b1
indirect2 := a2 * b3
indirect3 := a1 * b2
indirect4 := a2 * b4
## contrasts
con1 := (a1*b1) - (a2*b3)
con2 := (a1*b2) - (a2*b4)
## covariates
feelinginformedZ ~~ conspiracyZ
safetyprecautions_avgZ ~~ otheravoid_avgZ
'

fit <- sem(myModel, data=meddata, se = "bootstrap", bootstrap = 5000) 
summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)
parameterEstimates(fit, boot.ci.type="bca.simple")


trustscientistsZ <- data_12$trustscientistsZ
feelinginformedZ<- data_12$feelinginformedZ
conspiracyZ<- data_12$conspiracyZ
safetyprecautions_avgZ<- data_12$safetyprecautions_avgZ
otheravoid_avgZ<- data_12$otheravoid_avgZ


meddata<-data.frame(cbind(trustscientistsZ, feelinginformedZ, conspiracyZ, safetyprecautions_avgZ, otheravoid_avgZ))

myModel <- '
feelinginformedZ ~ a1*trustscientistsZ
conspiracyZ ~ a2*trustscientistsZ
safetyprecautions_avgZ ~ b1*feelinginformedZ + b3*conspiracyZ + c1*trustscientistsZ
otheravoid_avgZ ~ b2*feelinginformedZ + b4*conspiracyZ + c2*trustscientistsZ
## indirects
indirect1 := a1 * b1
indirect2 := a2 * b3
indirect3 := a1 * b2
indirect4 := a2 * b4
## contrasts
con1 := (a1*b1) - (a2*b3)
con2 := (a1*b2) - (a2*b4)
## covariates
feelinginformedZ ~~ conspiracyZ
safetyprecautions_avgZ ~~ otheravoid_avgZ
'

fit <- sem(myModel, data=meddata, se = "bootstrap", bootstrap = 5000) #
summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)
parameterEstimates(fit, boot.ci.type="bca.simple")



### Trust in media ###

cor.test(data_12$virus_natart, data_12$media_underover, 
         method = "pearson")

cor.test(data_12$virus_natart, data_12$fakenews, 
         method = "pearson")

cor.test(data_12$feelinginformed_avg, data_12$media_underover, 
         method = "pearson")

cor.test(data_12$feelinginformed_avg, data_12$fakenews, 
         method = "pearson")


table_socmed_conspir_non = lmer(conspiracyZ ~ trust_fbZ + trust_igZ + trust_govZ  + trust_twZ + trust_nhsZ + trust_newspaperZ + trust_whoZ+ (1 | countryres), 
                                data = data_non)
summary(table_socmed_conspir_non)
confint(table_socmed_conspir_non)
r2(table_socmed_conspir_non)
icc(table_socmed_conspir_non)
AIC(table_socmed_conspir_non)
BIC(table_socmed_conspir_non)


table_socmed_conspir_pop = lmer(conspiracyZ ~ trust_fbZ + trust_igZ + trust_govZ  + trust_twZ + trust_nhsZ + trust_newspaperZ + trust_whoZ+ (1 | countryres), 
                                data = data_pop)
summary(table_socmed_conspir_pop)
confint(table_socmed_conspir_pop)
r2(table_socmed_conspir_pop)
icc(table_socmed_conspir_pop)
AIC(table_socmed_conspir_pop)
BIC(table_socmed_conspir_pop)

table_socmed_knowl_non = lmer(feelinginformedZ ~ trust_fbZ + trust_igZ + trust_govZ  + trust_twZ + trust_nhsZ + trust_newspaperZ + trust_whoZ+ (1 | countryres), 
                              data = data_non)
summary(table_socmed_knowl_non)
confint(table_socmed_knowl_non)
r2(table_socmed_knowl_non)
icc(table_socmed_knowl_non)
AIC(table_socmed_knowl_non)
BIC(table_socmed_knowl_non)


table_socmed_knowl_pop = lmer(feelinginformedZ ~ trust_fbZ + trust_igZ + trust_govZ  + trust_twZ + trust_nhsZ + trust_newspaperZ + trust_whoZ+ (1 | countryres), 
                              data = data_pop)
summary(table_socmed_knowl_pop)
confint(table_socmed_knowl_pop)
r2(table_socmed_knowl_pop)
icc(table_socmed_knowl_pop)
AIC(table_socmed_knowl_pop)
BIC(table_socmed_knowl_pop)

multiregressiontest <- plot_summs(table_socmed_conspir_non, table_socmed_conspir_pop, table_socmed_knowl_non, table_socmed_knowl_pop, omit.coefs = c("(Intercept)", "Intercept"),
                                  model.names = c("Conspiracy belief (non-populist)", "Conspiracy belief (populist)", "Perceived Knowledge (non-populist)", "Perceived Knowledge (populist)"))


multi_plot_con <- multiregressiontest  + theme_apa() 

multi_plot_con + theme(legend.position="top") + scale_y_discrete(labels=c("World Health Organisation",
                                                                           "Newspaper websites",
                                                                           "National health institutions",
                                                                           "Twitter", 
                                                                           "National government", 
                                                                           "Instagram", 
                                                                           "Facebook"
))+xlab("Estimate")+ylab("Information sources")+ggtitle("Trust in information sources during the COVID-19 among citizens")

#############################IRI-evolution##############################

#### File paths definition ####

# Input folder and files

RESOURCES_DIR_PATH <- getwd()

INFODEMIC_REDUCED_FILE_PATH <- file.path(RESOURCES_DIR_PATH, "infodemics_reduced.csv")
WORLD_RISK_INDEX_FILE_PATH <- file.path(RESOURCES_DIR_PATH, "world_risk_index.csv")


# Output folder

PLOTS_DIR_PATH <- "./plots"

#### Data loading ####

dat.red <- read.table(INFODEMIC_REDUCED_FILE_PATH, header = T, sep = ";" )
dat.red$date <- as.Date(dat.red$date)

dat.iri.world <- read.table(WORLD_RISK_INDEX_FILE_PATH, header = T, sep = ";")


#### IRI evolution by Country ####

# Plot function pipeline

getIRIBarPlot <- function(COUNTRY){
  
  cat(paste("  IRI Country:", COUNTRY, "\n"))
  
  dat.red.country.tmp <- dat.red[
    which(dat.red$iso3 == COUNTRY), 
    c("date", "IRI_UNVERIFIED", "IRI_VERIFIED")
  ]
  
  dat.red.country <- data.frame(
    date = dat.red.country.tmp$date, 
    Unverified = (dat.red.country.tmp$IRI_UNVERIFIED),
    Verified = (dat.red.country.tmp$IRI_VERIFIED)
  )
  
  tmp.cum.mean <- dplyr::cummean(rowSums(dat.red.country[order(dat.red.country$date), 2:3]))
  
  dat.red.country <- melt(dat.red.country, id.vars = "date")
  
  dat.red.country.epi <- data.frame(
    date = dat.red[which(dat.red$iso3 == COUNTRY), ]$date, 
    epi.new = c(0, diff(dat.red[which(dat.red$iso3 == COUNTRY), ]$EPI_CONFIRMED)) 
  )
  
  dat.red.country.epi[which(dat.red.country.epi$epi.new == 0), ]$epi.new <- NA
  
  dat.red.country.cummean <- data.frame(
    date = dat.red[which(dat.red$iso3 == COUNTRY), ]$date, 
    Cum.Mean = tmp.cum.mean
  )
  
  pj <- ggplot() + 
    theme_bw() + 
    theme(panel.grid = element_blank()) + 
    geom_point(data = dat.red.country.epi, 
               aes(date, size = epi.new), 
               y = 0.9, alpha = 0.5, 
               color = "tomato") + 
    geom_histogram(data = dat.red.country, 
                   aes(date, value, fill = variable), 
                   stat = "identity", 
                   type = "stacked", 
                   position = position_stack(reverse = TRUE)) + 
    scale_fill_manual(name = "", 
                      values = c('#4DBBD5FF', '#3C5488FF')) + 
    ylab("IRI") + 
    xlab("Timeline")  +
    ylim(c(0, 1)) + 
    guides(size = guide_legend(title = "New Cases")) + 
    geom_text(data = dat.red.country.epi, 
              aes(x = date, label = epi.new), 
              angle = 90, 
              y = 0.97, 
              size = 2, 
              color = "grey30") + 
    geom_line(data = dat.red.country.cummean, 
              aes(date, Cum.Mean), 
              linetype = "dashed", 
              color = "grey30")
  
  return(pj)
}


# Code execution per country

CountrytoPrint <- c('ITA', 'USA', 'CAN', 'RUS', 'VEN', 'KOR')
for(CC in CountrytoPrint){
  
  plot_file_path <- file.path(PLOTS_DIR_PATH, paste0("IRI_", CC, ".png"))
  
  png(plot_file_path, height = 1024, width = 1024*2, res = 250)
  
  dev.off()
  
}

# Code execution overall

col <- brewer.pal(9, "YlGnBu") 

idxs.sub <- which(dat.red$TWI_VOLUME > 2000 & dat.red$EPI_CONFIRMED > 100)
country.sub <- as.character(unique(dat.red[idxs.sub, ]$iso3))
dat.red.sub <- dat.red[which(dat.red$iso3 %in% country.sub), c('date' ,'iso3', 'IRI_ALL')]


tile.iri <- ggplot(dat.red.sub, aes(x = date, y = reorder(iso3, IRI_ALL), fill = IRI_ALL)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_gradientn(colors = col, limits = c(0, 1), name = "IRI") +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), legend.position = 'top') +
  ylab('Country') +
  xlab('Timeline') +
  geom_hline(yintercept = c(seq(1.5, 21, 1)), color = 'grey70') +
  scale_x_date(expand = c(0, 0))+ggtitle("IRI evolution across different countries")


plot_file_path <- file.path(PLOTS_DIR_PATH, "IRI_evolution_overall.png")

png(plot_file_path, height = 1024, width = 1024*0.75, res = 200)
print(tile.iri)
dev.off()

#### IRI World evolution ####

dat.epi.chn <- aggregate(EPI_CONFIRMED ~ date, dat.red[dat.red$iso3 == "CHN", ], sum)
dat.epi.chn$daily <- c(0, diff(dat.epi.chn$EPI_CONFIRMED))

dat.epi.nonchn <- aggregate(EPI_CONFIRMED ~ date, dat.red[dat.red$iso3 != "CHN", ], sum)
dat.epi.nonchn$daily <- c(0, diff(dat.epi.nonchn$EPI_CONFIRMED))


 ggplot(dat.iri.world, aes(as.Date(date), world_risk_index)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 90)) + 
  scale_x_date(date_breaks = "3 days", 
               labels = date_format("%d %b")) + 
  xlab("Timeline") + 
  ylab("IRI") + 
  ylim(c(0, 0.5))  + 
  geom_histogram(stat = "identity", 
                 aes(fill = world_risk_index), 
                 color = NA) + 
  geom_point(color = "grey70") +
  scale_fill_gradientn(colours = colorRampPalette(brewer.pal(9, "YlGnBu"))(10), 
                       limits = c(0, 1)) +
  stat_smooth(color = "grey60", 
              alpha = 0.3) +
  geom_point(data = dat.epi.chn, 
             aes(as.Date(date), size = daily), 
             y = 0.5, 
             alpha = 0.5, 
             color = "tomato") + 
  geom_point(data = dat.epi.nonchn,
             aes(as.Date(date), size = daily), 
             y = 0.4, 
             alpha = 0.5, 
             color = "steelblue")+ggtitle("IRI evolution worldwide")


#### IRI reduction ####

dat.corr2 <- data.frame()
dat.corr <- dat.red[, c("date", "iso3", "EPI_CONFIRMED", "IRI_ALL")]

for(cc in unique(dat.corr$iso3)){
  print(cc)
  tmp <- dat.corr[which(dat.corr$iso3 == cc), ]
  tmp <- tmp[order(tmp$date), ]
  tmp$EPI_CONFIRMED_DAILY <- c(0, diff(tmp$EPI_CONFIRMED))
  tmp$IRI_ALL_CUMMEAN <- dplyr::cummean(tmp$IRI_ALL)
  dat.corr2 <- rbind(dat.corr2, tmp)
}

dat.corr2 <- dat.corr2[! is.na(dat.corr2$IRI_ALL), ]
dat.corr2 <- dat.corr2[- which(dat.corr2$EPI_CONFIRMED == 0), ]

bin <- rep(0, nrow(dat.corr2))
bin[which(dat.corr2$EPI_CONFIRMED <= 2 )] <- 0
bin[which(3 <= dat.corr2$EPI_CONFIRMED & dat.corr2$EPI_CONFIRMED < 8)] <- 1
bin[which(8 <= dat.corr2$EPI_CONFIRMED & dat.corr2$EPI_CONFIRMED < 16)] <- 2
bin[which(16 <= dat.corr2$EPI_CONFIRMED & dat.corr2$EPI_CONFIRMED < 51)] <- 3
bin[which(51 <= dat.corr2$EPI_CONFIRMED & dat.corr2$EPI_CONFIRMED < 10001)] <- 4
bin[which(10001 <= dat.corr2$EPI_CONFIRMED & dat.corr2$EPI_CONFIRMED < 81000)] <- 5

dat.corr2$bin <- bin

labels.min <- dat.corr2 %>% group_by(bin) %>% summarise_at(vars(EPI_CONFIRMED), min)
labels.max <- dat.corr2 %>% group_by(bin) %>% summarise_at(vars(EPI_CONFIRMED), max)

lab <- paste0(labels.min$EPI_CONFIRMED, '-', labels.max$EPI_CONFIRMED)
lab[5:6] <- c('51-9999', '10000+')

ggplot(dat.corr2, aes(as.factor(bin), IRI_ALL_CUMMEAN)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none") + 
  geom_boxplot(aes(fill = as.numeric(bin)), 
               size = 0.15, 
               outlier.color = "grey70", 
               color = "grey70") + 
  xlab("Cumulative Number of Reported Cases") + 
  ylab("IRI Cumulative Mean") + 
  scale_fill_viridis_c() + 
  scale_x_discrete(labels = lab)+ggtitle("Cumulative IRI vs. Epidemic per index confirmed")


#### IRI vs Confirmed COVID-19 cases ####

# Infodemics and Epidemics data aggregation by Country (iso3 code)

x0 <- aggregate(TWI_VOLUME ~ iso3, dat.red, mean)
colnames(x0) <- c("Country", "Message.Volume") 
x1 <- aggregate( EPI_CONFIRMED ~ iso3, dat.red, max)
colnames(x1) <- c("Country", "Infected")
x2a <- aggregate( IRI_UNVERIFIED ~ iso3, dat.red, mean)
colnames(x2a) <- c("Country", "Risk Unverified")
x2b <- aggregate( IRI_VERIFIED ~ iso3, dat.red, mean)
colnames(x2b) <- c("Country", "Risk Verified")

tab <- merge(x0, x1, by = "Country")
tab <- merge(tab, x2a, by = "Country")
tab <- merge(tab, x2b, by = "Country")

tab$Info.Risk <- tab[, "Risk Verified"] + tab[, "Risk Unverified"]
tab$Continent <- countrycode(tab$Country, 'iso3c', 'continent')

# reorder by IRI value   
tab <- tab[order(tab$Info.Risk),]
tab <- tab[! is.na(tab$Continent), ]
rownames(tab) <- NULL

Infect.thres <- 0
idxs <- which(tab$Infected > Infect.thres & ! tab$Country %in% c("CHN", "TWN", "IRN"))

ggplotly(ggplot(tab[idxs, ], aes(Info.Risk, Infected, color = Continent, size = Message.Volume))  + 
           theme_bw() + 
           theme(panel.grid = element_blank()) + 
           stat_smooth(method = 'lm', 
                       linetype = "solid", 
                       color = "red", 
                       alpha = 0.2, 
                       size = 0.25, 
                       se = T) + 
           geom_point(alpha = 0.7) + 
           scale_color_npg() + 
           geom_text_repel(aes(label = Country), 
                           show.legend = F, 
                           seed = 786) + 
           scale_y_log10() + 
           geom_vline(xintercept = median(tab$Info.Risk[idxs], na.rm = T), 
                      linetype = "dashed", 
                      color = "#dadada") + 
           geom_hline(yintercept = median(tab$Infected[idxs], na.rm = T), 
                      linetype = "dashed", 
                      color = "#dadada") + 
           xlab("IRI")  + 
           ylab("Confirmed COVID19 Cases") +  
           stat_smooth(linetype = "dashed", 
                       color = "black", 
                       alpha = 0.2, 
                       size = 0.35, 
                       se = F ) + 
           labs(size = 'Volume')+
           ggtitle("Showing confirmed COVID-19 cases across countries and the IRI score"))
 

#
# Code reproducing parts of the figures of the Nature Human Behavior paper
# "Assessing the risks of Infodemics in response to COVID-19 epidemics"
# Link: https://doi.org/10.1038/s41562-020-00994-6
#
# - Mapping infodemic risk worldwide (Fig. 3)
# - Reduction of infodemic risk after COVID-19 reaches countries (Fig. 4)
# - Infodemic evolution is country dependent (Fig. 5)

#References:
# Written by Manlio De Domenico (@manlius) and Francesco Valle (fravalle93@gmail.com)
# Revised by Nicola Castaldo (@ncastaldo)
# Reused: for this analysis project
# Version 1.0
#


#### Library imports ####

library(ggplot2)
library(anytime)
library(ggsci)
require(mgcv)
library(countrycode)
library(ggrepel)
library(reshape2)
library(scales)
library(dplyr)
library(RColorBrewer)


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

# Plot function 

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
  getIRIBarPlot(CC)
  #plot_file_path <- file.path(PLOTS_DIR_PATH, paste0("IRI_", CC, ".png"))
  
  #png(plot_file_path, height = 1024, width = 1024*2, res = 250)
  #print(getIRIBarPlot(CC))
  #dev.off()
  
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
  ylab('') +
  xlab('') +
  geom_hline(yintercept = c(seq(1.5, 21, 1)), color = 'grey70') +
  scale_x_date(expand = c(0, 0))


plot_file_path <- file.path(PLOTS_DIR_PATH, "IRI_evolution_overall.png")

png(plot_file_path, height = 1024, width = 1024*0.75, res = 200)
print(tile.iri)
dev.off()


#### IRI World evolution ####
                            
dat.epi.chn <- aggregate(EPI_CONFIRMED ~ date, dat.red[dat.red$iso3 == "CHN", ], sum)
dat.epi.chn$daily <- c(0, diff(dat.epi.chn$EPI_CONFIRMED))

dat.epi.nonchn <- aggregate(EPI_CONFIRMED ~ date, dat.red[dat.red$iso3 != "CHN", ], sum)
dat.epi.nonchn$daily <- c(0, diff(dat.epi.nonchn$EPI_CONFIRMED))


p.iri.world.evo <- ggplot(dat.iri.world, aes(as.Date(date), world_risk_index)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_text(angle = 90)) + 
  scale_x_date(date_breaks = "3 days", 
               labels = date_format("%d %b")) + 
  xlab("") + 
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
             color = "steelblue")

plot_file_path <- file.path(PLOTS_DIR_PATH, "IRI_world_evolution.png")
                            
png(plot_file_path, height = 1024, width = 1024*0.75, res = 250)
print(p.iri.world.evo)
dev.off()


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

pmiriepi <- ggplot(dat.corr2, aes(as.factor(bin), IRI_ALL_CUMMEAN)) + 
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
  scale_x_discrete(labels = lab)


plot_file_path <- file.path(PLOTS_DIR_PATH, "cumIRI_vs_epi_boxplot.pdf")                            
                            
pdf(plot_file_path, height = 3, width = 6)
print(pmiriepi)
dev.off()

                            
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

 
ggplot(tab[idxs, ], aes(Info.Risk, Infected, color = Continent, size = Message.Volume))  + 
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
  ggtitle("Showing confirmed COVID-19 cases across countries and the IRI score")

plot_file_path <- file.path(PLOTS_DIR_PATH, "info_vs_epi.pdf")                                               

pdf(plot_file_path, height = 4, width = 8)
print(pinfoepi_overall)
dev.off()

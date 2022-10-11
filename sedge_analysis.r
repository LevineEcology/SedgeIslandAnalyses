##---------------------------------------------------------------
## Jacob's code
##---------------------------------------------------------------


source("SedgwickWaterCompetition/code/utility/plot_utility.R")
library(ggplot2)

beach <- read.csv("beach.csv")
beach <- beach[!is.na(beach$Transect_no),]
beach$AbsStart <- beach$Start + (beach$Transect_no - 1)*30
beach$AbsEnd <- beach$End + (beach$Transect_no - 1)*30
beach[beach$Species == "Beach Plum", "Species"] <- "Plum"
beach[beach$Species == "Beach heather", "Species"] <- "Heather"
beach[beach$Species == "Beach grass", "Species"] <- "Beachgrass"

freq <- data.frame(x = seq(0, max(beach$AbsEnd), by = 0.05))

for (i in 1:nrow(freq)) {
  for (sp in unique(beach$Species)) {
    if (sp %in% beach[freq$x[i] > beach$AbsStart & freq$x[i] < beach$AbsEnd, "Species"]) {
      freq[i, paste0(sp, "_occ")] <- 1
    }
    else freq[i, paste0(sp, "_occ")] <- 0
  }
}

for (i in 1:nrow(freq)) {
  for (sp in unique(beach$Species)) {
    freq[i, paste0(sp, "_freq")] <- sum(freq[freq$x < freq$x[i]+30 & freq$x > freq$x[i]-30,
                                             paste0(sp, "_occ")]) / (60*20)
  }
}


common_spp <- c("Beachgrass", "Bayberry", "Plum", "Heather", "Bluestem", "Smilax")
colors <- c("red", "green", "blue", "orange", "purple", "gray")

plot <- second_axis(ggplot(freq, aes(x = x, y = Beachgrass_freq)) +
  geom_line(size = 1, color = colors[1], se = FALSE) +
  scale_y_continuous(expand = c(0.02,0.02)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_jabo())

for (i in 2:length(common_spp)) {

  eval(str2expression(paste0("plot <- plot + geom_line(aes(y =", common_spp[i],
                    "_freq),size = 1, se = FALSE, color = '", colors[i], "')" )))

}

beach$length <- beach$End - beach$Start
beach[beach$length == 0, "length"] <- 0.1
cover <- data.frame(transect = rep(1:max(beach$Transect_no),
                                   times = length(unique(beach$Species))),
                    species = rep(unique(beach$Species), each = max(beach$Transect_no)))
for (i in 1:nrow(cover)) {
  cover[i, "cover"] <- sum(beach[beach$Transect_no == cover[i, "transect"] &
                             beach$Species == cover[i, "species"], "length"]) / 30

}

sumlength <- aggregate(formula = length ~ Species, data = beach, FUN = sum)
sumlength <- sumlength[order(sumlength$length),]
common <- sumlength[nrow(sumlength):(nrow(sumlength)-6), "Species"]
cover <- cover[cover$species %in% common,]
cover$transect = as.numeric(cover$transect)

forest <- read.csv("Forest.csv")
forest$AbsPos <- (forest$Transect_no - 2) * 30
for_cover <- data.frame(transect = rep(unique(forest$AbsPos), times = length(unique(forest$Species))),
                        species = rep(unique(forest$Species), each = length(unique(forest$AbsPos))))

for (i in 1:nrow(for_cover)) {

  if (for_cover[i, "species"] %in%forest[forest$AbsPos == for_cover[i, "transect"], "Species"]) {
    for_cover[i, "cover"] <- 1
  }
  else for_cover[i, "cover"] <- 0

}

cover$transect <- (cover$transect-1)*30

ag <- aggregate(cover ~ species, data = for_cover, FUN = sum)
spp_keep <- ag[ag$cover > 1, "species"]
for_cover <- for_cover[for_cover$species %in% spp_keep, ]
for_cover[for_cover$species == "Cedar" & for_cover$transect == 210, "cover"] <- 1
for_cover[for_cover$species == "Smilax" & for_cover$transect == 210, "cover"] <- 0
for_cover[for_cover$species == "Maple" & for_cover$transect == 210, "cover"] <- 0

colors <- c("maroon", "orange", "yellow", "darkgreen", "skyblue", "purple",
            "red", "darkblue", "chocolate", "turquoise", "yellowgreen", "plum")

unique_spp <- unique(c(for_cover$species, cover$species))

cover$species <- factor(cover$species, levels = unique_spp)
for_cover$species <- factor(for_cover$species, levels = unique_spp)


n_colors = colors[c(9:12,5,2,6)]
plot <- second_axis(ggplot() +
                    geom_smooth(data = cover, aes(x = 0, y = 0, color = species),
                                size = 1, se = FALSE) +
                    geom_vline(aes(xintercept = 210), size = 0.75) +
                    scale_color_manual(values = n_colors) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(NA, NA)) +
  ylab("% cover") +
  xlab("Transect #") +
  theme_jabo())
plot

unique(cover$species)
j = 1
for (i in unique(cover$species)) {

  n_colors = colors[c(3,8,6,2,1,12,5)]
  mod <- loess(cover ~ transect, data = cover[cover$species == i,])
  x = data.frame(transect = seq(0, 210, by = 1))
  x$y = predict(mod, newdata = x)
  plot <- plot + geom_ribbon(data = x,
                   aes(x = transect, ymin = 0, ymax = y),
                    alpha = 0.4, fill = n_colors[j])
  j = j+1
}
plot

plot <- second_axis(ggplot() +
                    ##geom_smooth(data = cover, aes(x = transect, color = species, y = cover),
                    ##size = 1, se = FALSE) +
                    ##geom_smooth(data = for_cover, aes(x = transect, y = cover, color = species),
                    ##            size = 1, se = FALSE) +
                    scale_color_manual(values = colors) +
                    geom_text(aes(x = 160, y = 0.98,label = "slow transects")) +
                    geom_text(aes(x = 260, y = 0.98,label = "fast transects")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(NA, NA)) +
  ylab("% cover") +
  xlab("Transect #") +
  theme_jabo())

j = 1
for (i in unique(cover$species)) {

  n_colors = colors[c(3,8,6,2,1,12,5)]
  mod <- loess(cover ~ transect, data = cover[cover$species == i,])
  x = data.frame(transect = seq(1, 210, by = 1))
  x$y = predict(mod, newdata = x)
  plot <- plot + geom_ribbon(data = x,
                   aes(x = transect, ymin = 0, ymax = y),
                    alpha = 0.4, fill = n_colors[j])
  j = j+1
}

j = 1
for (i in unique(for_cover$species)) {
 n_colors = colors[c(7,12,9,4,1,5,10,11)]
  mod <- loess(cover ~ transect, data = for_cover[for_cover$species == i,])
  x = data.frame(transect = seq(210, 690, by = 1))
 x$y = predict(mod, newdata = x)
 x[x$y > 1, "y"] <- 1
  plot <- plot + geom_ribbon(data = x,
                   aes(x = transect, ymin = 0, ymax = y),
                    alpha = 0.4, fill = n_colors[j])
 j = j+1
}
plot <- plot + geom_vline(aes(xintercept = 210), size = 0.75)

plot

marsh <- read.csv("marsh.csv")

library(cowplot)

plot_list <- list()
marsh_agg <- aggregate(cover ~ position + species, data = marsh, FUN = mean)

marsh_agg$species <- factor(marsh_agg$species, levels = unique(marsh$species))
leg <- second_axis(ggplot(marsh_agg, aes(x = position, y = cover, fill = species)) +
  geom_col(position = "stack") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_jabo())
legend <- get_legend(leg)


for (i in 1:4) {

  data <- marsh[marsh$transect_no == unique(marsh$transect_no)[i],]
  data$species <- factor(data$species, levels = unique(marsh$species))
  plot_list[[i]] <- second_axis(ggplot(data = data,
                                       aes(x = position, y = cover, fill = species)) +
              geom_col(position = "stack") +
              scale_x_continuous(expand = c(0,0)) +
              scale_y_continuous(expand = c(0,0)) +
              theme_jabo())
}

for (i in 1:4) {

  plot_list[[i]] <- plot_list[[i]] + theme(legend.position = "none", axis.title.x = element_blank())
}

pg <- plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 1)
pg <- plot_grid(pg, legend, ncol = 2, rel_widths = c(8, 1))
pg


for (i in unique(marsh$elevation)) {

  for (j in unique(marsh$transect_no)) {

    for (k in unique(marsh$species)) {

      if ( nrow(marsh[marsh$elevation == i & marsh$transect_no == j, ]) != 0) {
        if (!(k %in% marsh[marsh$elevation == i & marsh$transect_no == j, "species"])) {

          ndata <- marsh[marsh$elevation == i & marsh$transect_no == j, ]
          ndata <- ndata[1,]
          ndata$species <- k
          ndata$cover <- 0
          marsh <- rbind(marsh, ndata)
        }

      }

    }

  }

}

for (i in unique(marsh$transect_no)) {

  marsh[marsh$transect_no == i, "AbsElevation"] <- min(marsh[marsh$transect_no == i, "elevation"])  - marsh[marsh$transect_no == i, "elevation"]

}
marsh$AbsElevation <- marsh$AbsElevation - min(marsh$AbsElevation)

int_spp <- c("phragmites", "spartina a", "spartina p", "iva", "bare", "dist")
second_axis(ggplot(data = marsh[marsh$species %in% int_spp,],
                   aes(x = AbsElevation, y = cover, color = species)) +
            geom_point() +
            geom_smooth(se = FALSE, method = "gam") +
              scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
            xlab("Elevation (cm)") +
            theme_jabo() +
            theme(legend.position = "none") +
            facet_wrap(~species))

second_axis(ggplot(data = marsh[marsh$AbsElevation > 0,],
                   aes(x = AbsElevation, y = salinity)) +
            geom_point() +
            geom_smooth(se = FALSE, method = "gam", formula = y ~s(x, k = 5)) +
              scale_x_continuous(expand = c(0,0), limits = c(22, NA)) +
            scale_y_continuous(expand = c(0,0), limits = c(NA, NA)) +
            xlab("Elevation (cm)") +
            theme_jabo() +
            theme(legend.position = "none"))

for (i in 1:4) {

  data <- marsh[marsh$transect_no == unique(marsh$transect_no)[i],]
  data$species <- factor(data$species, levels = unique(marsh$species))
  plot_list[[i]] <- second_axis(ggplot(data = data,
                                       aes(x = elevation, y = cover, fill = species)) +
              geom_col(position = "stack") +
              scale_x_continuous(expand = c(0,0)) +
              scale_y_continuous(expand = c(0,0)) +
              theme_jabo())
}

for (i in 1:4) {

  plot_list[[i]] <- plot_list[[i]] + theme(legend.position = "none", axis.title.x = element_blank())
}

pge <- plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], ncol = 1)
pge <- plot_grid(pg, legend, ncol = 2, rel_widths = c(8, 1))
pge


##---------------------------------------------------------------
## Eve's code
##---------------------------------------------------------------


## Sedge data analysis

#install.packages(dplyr)
#install.packages(ggplot2)
library(dplyr)
library(ggplot2)


# Make sure excel sheet tabs have been exported as individal CSVs

# Load data
beach = read.csv("Beach.csv")
# Take a look at the data to make sure it loaded correctly
glimpse(beach)
# Fix some scientific names
beach$Species[beach$Species=="plum"] = "Beach plum"
beach$Species[beach$Species=="Plum"] = "Beach plum"
beach$Species[beach$Species=="rumex"] = "Rumex"

# How many different species did we observe during the intensive transect sampling?
unique(beach$Species) # 14
sort(table(beach$Species))
# Most commonly observed species = beach grass, heater, sedge
# convert transect measurements to percent cover?
beach = beach %>% mutate(AbsStart = Start +(Transect_no - 1)*30,
                         AbsEnd = End + (Transect_no - 1)*30)
beach %>% mutate(Dist = ((End-Start)/30)*100)
beach %>% ggplot(aes(x=AbsStart, xend=AbsEnd, y=reorder(Species, AbsStart), yend=Species,  col=Species)) +
  geom_segment(size=8) + theme_bw() + xlab("Distance from beach (m)") +
  theme(text=element_text(size=15), legend.position = "none")

beach %>% group_by(Transect_no, Species) %>% summarise(NHits = n())
beach %>% group_by(Species) %>%
  summarise(Density = sum(((End-Start)+1))/(30*7)*100) %>%
  ggplot(aes(reorder(Species, Density), Density)) + geom_point(size=5) +
  theme_bw() + ylab("Percent cover (100%)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=15)) +
  xlab("Species") + theme(text=element_text(size=15))


## Less intensive forest transects
forest = read.csv("Forest.csv")
head(forest)
unique(forest$Species)
sort(table(forest$Species))
# Fix some names
forest$Species[forest$Species=="ceder"] = "Cedar"
forest$Species[forest$Species=="poison ivy"] = "Poison ivy"
forest = forest %>% mutate(AbsStart = Position +(Transect_no - 1)*30,
                         AbsEnd = Position + (Transect_no - 1)*30)
forest %>% ggplot(aes(x=AbsStart, xend=AbsEnd, y=Species, yend=Species)) +
  geom_segment(size=4) + theme_bw()
forest %>% group_by(Transect_no) %>% summarise(Richness = n_distinct(Species)) %>%
  ggplot(aes(Transect_no, Richness)) + geom_point(size=3) + theme_bw() +
  geom_smooth()
forest %>% group_by(Transect_no, Density) %>%
  summarise(Richness = n_distinct(Species)) %>%
  filter(!is.na(Density)) %>%
  ggplot(aes(Richness, (Density)*4*1.04)) + geom_point() + geom_smooth(method="lm", se=FALSE) +
  theme_bw() + theme(text=element_text(size=15)) +
  ggtitle("Forest samples") + ylab("Canopy cover (%)")

# Merge data to look at transects together
both = rbind(beach %>% select(Transect_no, Species) %>% distinct(),
             forest %>% select(Transect_no, Species) %>% distinct())
both %>% group_by(Transect_no) %>% summarise(Richness = n_distinct(Species)) %>%
  ggplot(aes((Transect_no-1)*30, Richness)) + geom_point(size=3) + theme_bw() +
  geom_smooth() +
  xlab("Distance from beach (m)") + ylab("Number of species") +
  theme(text=element_text(size=15)) +
  geom_vline(xintercept = 240, col="red")
glimpse(forest)

beach %>% group_by(Species) %>% summarise(AbsStart = min(AbsStart),
                                          AbsEnd = max(AbsEnd)) %>%
  ggplot(aes(x=AbsStart, xend=AbsEnd, y=reorder(Species, AbsStart), yend=Species, col=Species)) +
  geom_segment(size=6) + theme_bw() + xlab("Distance from beach (m)") +
  theme(text=element_text(size=15), legend.position = "none")
beach %>% group_by(Species) %>% summarise(AbsStart = min(AbsStart),
                                          AbsEnd = max(AbsEnd)) %>%
  ggplot(aes(x=AbsStart, xend=AbsEnd, y=reorder(Species, AbsStart), yend=Species, col=Species)) +
  geom_segment(size=6) + theme_bw() + xlab("Distance from beach (m)") +
  theme(text=element_text(size=15), legend.position = "none") +
  geom_segment(data = beach, aes(x=AbsStart, xend=AbsEnd, y=Species, yend=Species),
               col="black", inherit.aes = FALSE, size=3) +
  ylab("Species")

both = rbind(beach %>% group_by(Species) %>% summarise(AbsStart = min(AbsStart),
                                                       AbsEnd = max(AbsEnd)),
             forest %>% group_by(Species) %>% summarise(AbsStart = min(AbsStart),
                                                        AbsEnd = max(AbsEnd))) %>%
  group_by(Species) %>% summarise(AbsStart = min(AbsStart),
                                  AbsEnd = max(AbsEnd)) %>%
  filter(!is.na(Species))
both$Species[both$Species=="Ceder"] <- "Cedar"
both %>% ggplot(aes(x=AbsStart, xend=AbsEnd, y=reorder(Species, AbsStart), yend=Species, col=Species)) +
  geom_segment(size=6) + theme_bw() + xlab("Distance from beach (m)") +
  theme(text=element_text(size=15), legend.position = "none") +
  ylab("Species")

## Explore forest inventory plots
dbh=read.csv("Forest2.csv")
head(dbh)
tail(dbh)
dbh %>% filter(!is.na(height_total), species %in% c("cedar", "cherry", "holly", "oak", "pine")) %>%
  ggplot(aes(dbh, height_total, col=species, group=species)) +
  geom_point(size=4) + theme_bw() +
  xlab("DBH") + ylab("Height") +
  theme(text=element_text(size=15))
# proportion of different kinds
# sizes
dbh %>% filter(!is.na(dbh), species %in% c("holly", "oak", "pine")) %>%
  ggplot(aes(dbh, fill=species, group=species)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  ylab("Number of trees") + xlab("DBH")
dbh %>% filter(!is.na(dbh)) %>%
  ggplot(aes(dbh, fill=species)) + geom_histogram() +
  facet_wrap(~species) + theme_bw()
table(dbh$species)
dbh = dbh %>% mutate(Type =ifelse(canopy_class=="sapling", "sapling", "adult"))
table(dbh$Type, dbh$transect_no)
dbh %>% group_by(transect_no, Type) %>% summarise(ntrees = n()) %>%
 pivot_wider(names_from = Type, values_from = ntrees, values_fill = 0) %>%
  ggplot(aes(adult, sapling)) + geom_point(size=5) + theme_bw() +
  xlab("Number of adult trees per transect") +
  ylab("Number of saplings")
# n saplings ~ sum DBH
dbh %>% filter(species=="oak",Type=="sapling") %>% group_by(transect_no) %>%
  summarise(Saplings = n()) %>%
  left_join(dbh %>% filter(species=="oak") %>%
              group_by(transect_no) %>%
            summarise(DBH = sum(dbh, na.rm=T))) %>%
  ggplot(aes(DBH, Saplings)) + geom_point(size=5) + theme_bw()
dbh %>% ggplot(aes(height_total)) + geom_histogram(bins=10, col="black", fill="seagreen") +
  theme_bw() + xlab("Height") + ylab("Number of trees") +
  theme(text=element_text(size=15))
dbh %>% filter(species %in% c("oak", "holly")) %>%
  ggplot(aes(height_total, fill=species)) + geom_density(alpha=0.5) +
  theme_bw() + xlab("Height") + ylab("Number of trees") +
  theme(text=element_text(size=15))
dbh %>% filter(species %in% c("oak", "holly")) %>%
  ggplot(aes(height_total, fill=species)) + geom_histogram(bins=20) +
  theme_bw() + xlab("Height") + ylab("Number of trees") +
  theme(text=element_text(size=15))
dbh %>%
  filter(!species %in% c("snag", "sumac")) %>%
  ggplot(aes(height_total, fill=species)) + geom_histogram(bins=20, col="black") +
  theme_bw() + xlab("Height") + ylab("Number of trees") +
  theme(text=element_text(size=15))

dbh %>% filter(species %in% c("oak", "holly")) %>%
  ggplot(aes(height_total, fill=species)) + geom_histogram() +
  theme_bw() + xlab("Height") + ylab("Number of trees") +
  theme(text=element_text(size=15)) + facet_wrap(~species)
table(dbh$Type, dbh$species)

dbh %>%
  filter(!species %in% c("snag", "sumac")) %>%
  ggplot(aes(dbh, fill=species)) + geom_histogram(bins=20, col="black") +
  theme_bw() + xlab("DBH") + ylab("Number of trees") +
  theme(text=element_text(size=15))

forest %>% mutate(Distance = tran)

## Canopy density
den = read.csv("density.csv")
head(den)
den %>% ggplot(aes(Distance, (Density)*4*1.04)) +
  geom_point() + geom_smooth() +
  theme_bw() +
  xlab("Distance from beach (m)") + ylab("Canopy cover (%)") +
  theme(text=element_text(size=15)) +
  geom_vline(xintercept = 240, col="red")

## Marsh data collection
marsh = read.csv("marsh.csv")
head(marsh)
marsh %>%
  filter(!species %in% c("bare", "unk", "water")) %>%
  ggplot(aes(salinity, cover, col=species)) + geom_point() +
  geom_smooth(se=F) +
  theme_bw()
marsh%>%
  filter(!species %in% c("bare", "unk", "water")) %>%
  ggplot(aes(species, cover)) + geom_boxplot() +
  theme_bw()

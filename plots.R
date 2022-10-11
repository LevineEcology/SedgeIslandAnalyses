## Sedge data analysis

#install.packages(dplyr)
#install.packages(ggplot2)
library(dplyr)
library(ggplot2)

# Set working directory to location of the files
setwd("~/Dropbox/Princeton/Sedge")

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

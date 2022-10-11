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

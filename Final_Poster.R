# Loads in the packages neeeded to run this code.
# install.packages("stringr")
library(stringr)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("maps")
library(maps)
# install.packages("mapproj")
library(mapproj)
# install.packages("ggmap")
library(ggmap)
# install.packages("rworldmap")
library(rworldmap)
# install.packages("plyr")
library(plyr)
# library(grDevices)

# Reads in the eruption data set. 
eruption <- read.csv("database.csv", sep = ",", header = TRUE)

# Removes the NAs from the data. 
eruption[eruption == ""] <- NA
eruption <- na.omit(eruption)

# For 588 of the remaining observations, their last known eruption is listed as
# "Unknown." For this homework, I decided to remove them. 
unknown <- eruption[eruption$Last.Known.Eruption == "Unknown",]
eruption <- eruption[eruption$Last.Known.Eruption != "Unknown",]

# There were 5 observations whose eveidence was not concrete. These were also
# removed.
eruption <- eruption[eruption$Activity.Evidence != "Evidence Credible",]
eruption <- eruption[eruption$Activity.Evidence != "Evidence Uncertain",]

# Type column cleanup
table(eruption$Type)
# The following line was copied from Stack Overflow
# https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r
# Removes all instances of "(s)" and "(es)" from the Type column for the sake of 
# simplicity
eruption$Type <- gsub("\\s*\\([^\\)]+\\)", "", eruption$Type)
eruption$Type <- gsub("\\?$", "", eruption$Type)
table(eruption$Type)

# This codes cleans the "Tectonic Setting" Column
table(eruption$Tectonic.Setting)
# tectonic <- gsub("\\s*\\([^\\)]+\\)", "", eruption$Tectonic.Setting)
tectonic <- 
  str_split_fixed(eruption$Tectonic.Setting, " / ", 2)
eruption$Plate <- tectonic[,1]
eruption$Crust <- tectonic[,2]

# Removes 52 observations where the crust thickness is unknown
eruption <- eruption[eruption$Crust != "Crust Thickness Unknown",]



# This code was taken from the following URL
# https://ryouready.wordpress.com/2009/02/17/r-good-practice-adding-footnotes-to-graphics/
# It creates a function to place a footnote at the bottom right of each of the 
# plots.
makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= "white")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

# Plots ##########################################

# Question 1: Where are the volcanoes located?

# Plot 1 (World Map)
world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "black", size = 0.15
  ) +
  geom_point(
    data = eruption,
    aes(Longitude, Latitude
        , color = Activity.Evidence),
    alpha = 0.5, size = 2
  ) +
  scale_color_manual(
    values = c("orange", "red")
  ) +
  theme_void() + labs(title = "Volcano Locations") +
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 24
                                  , color = "white"
                                  , face = "bold"
                                  , family = "Times"),
        legend.title = element_text(size = 12
                                    , color = "white"
                                    , family = "Times"),
        legend.text = element_text(size = 12
                                   , color = "white"
                                   , family = "Times"))

makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot1.pdf", width = 16, height = 12, units = "in")

# Plot 2 (Japan and Russia)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "black", size = 0.15
  ) + xlim(125,200) + ylim(25,70) +
  geom_point(
    data = eruption,
    aes(Longitude, Latitude
        , color = Activity.Evidence),
    alpha = 0.5, size = 2
  ) +
  scale_color_manual(
    values = c("orange", "red")
  ) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 24
                                  , color = "white"
                                  , face = "bold"
                                  , family = "Times"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none")
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot2.pdf", width = 6, height = 6, units = "in")


# Plot 3 (Oceania)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "black", size = 0.15) +
  xlim(90,175) + ylim(-25,25) +
  geom_point(
    data = eruption,
    aes(Longitude, Latitude
        , color = Activity.Evidence),
    alpha = 0.5, size = 2
  ) +
  scale_color_manual(
    values = c("orange", "red")
  ) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 24
                                  , color = "white"
                                  , face = "bold"
                                  , family = "Times"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none")
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot3.pdf", width = 6, height = 6, units = "in")

# Plot 4 (North America)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "black", size = 0.15) +
  xlim(-180,-110) + ylim(25,75) +
  geom_point(
    data = eruption,
    aes(Longitude, Latitude
        , color = Activity.Evidence),
    alpha = 0.5, size = 2
  ) +
  scale_color_manual(
    values = c("orange", "red")
  ) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 24
                                  , color = "white"
                                  , face = "bold"
                                  , family = "Times"),
        legend.position = "none")
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot4.pdf", width = 6, height = 6, units = "in")

# Plot 5 (Patagonia)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "black", size = 0.15) +
  xlim(-100,-30) + ylim(-60,-15) +
  geom_point(
    data = eruption,
    aes(Longitude, Latitude
        , color = Activity.Evidence),
    alpha = 0.5, size = 2
  ) +
  scale_color_manual(
    values = c("orange", "red")
  ) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 24
                                  , color = "white"
                                  , face = "bold"
                                  , family = "Times"),
        legend.position = "none")
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot5.pdf", width = 6, height = 6, units = "in")

# Plot 6 (Eruptions by Region)
region <- table(eruption$Region, eruption$Activity.Evidence)
region <- as.data.frame(region)

# Rename Columns
region <- plyr::rename(region, c("Var1" = "Region", 
                                 "Var2" = "Evidence",
                                 "Freq"="Count"))

# Code to make pyramid plot was borrowed from the website below
# https://www.statology.org/population-pyramid-in-r/ 
ggplot(region, aes(x = Region, fill = Evidence,
       y = ifelse(test = Evidence == "Eruption Dated",
                  yes = -Count, no = Count))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(region$Count) * c(-1,1)) +
  coord_flip() + scale_fill_manual(values = c("orange", "red")) + 
  ggtitle("Eruptions by Region") + ylab("Count") +
  theme(
    panel.background = element_rect(fill = "black",
                                    color = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                    color = "white"), 
    panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
                                    color = "white"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()) +
  theme(plot.background = element_rect(fill = "black")) +
  theme(legend.background = element_rect(fill = "black")) +
  theme(plot.title = element_text(size = 24
                                  , color = "white"
                                  , face = "bold"
                                  , family = "Times"),
  axis.text = element_text(size = 12
                                  , color = "white"
                                  , face = "bold"
                                  , family = "Times"),
  axis.title.x = element_text(size = 12
                                  , color = "white"
                                  , face = "bold"
                                  , family = "Times"),
  axis.title.y = element_blank(),
  legend.title = element_text(size = 12
                                    , color = "white"
                                    , family = "Times"),

  legend.text = element_text(size = 12
                                   , color = "white"
                                   , family = "Times"),
  axis.ticks = element_line(color = "white"))
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot6.pdf", width = 10, height = 8, units = "in")


# Question 2: Under what geological conditions did these eruptions occur?

# Plot 7 (Elevation Boxplot)

# Summary statistics for the elevation column
mean(eruption$Elevation..Meters.)
summary(eruption$Elevation..Meters.)

ggplot(eruption, aes(y = Elevation..Meters.)) + 
  geom_boxplot(color = "white", fill = "red") + 
  ggtitle("Volcano Elevations") + labs(y = "Elevation (m)") +
  geom_hline(yintercept = 0, color = "orange") +
  geom_hline(yintercept = mean(eruption$Elevation..Meters.), color = "orange") +
  theme(
    panel.background = element_rect(fill = "black",
                                    color = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    plot.title = element_text(size = 24
                              , color = "white"
                              , face = "bold"
                              , family = "Times"),
    axis.text.y = element_text(size = 12
                               , color = "white"
                               , face = "bold"
                               , family = "Times"),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 12
                                , color = "white"
                                , face = "bold"
                                , family = "Times")
  )
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot7.pdf", width = 8, height = 8, units = "in")


# Plot 8 (Barplot of Rock Type) 
# Defines a new data frame to make an ordered barplot
vrock <- sort(table(eruption$Dominant.Rock.Type), decreasing = T)
vrock <- as.data.frame(vrock)

# Rename Columns
vrock <- plyr::rename(vrock, c("Var1" = "Type", 
                             "Freq"="Count"))

ggplot(vrock) + aes(x = Type, y = Count) + 
  geom_bar(stat = "identity", fill = "red") + 
  coord_flip() + ggtitle("Volcanoes by Rock Type") + 
  labs(x = "Dominant Rock Type", y = "Count") +
  theme(
    panel.background = element_rect(fill = "black",
                                    color = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 24
                              , color = "white"
                              , face = "bold"
                              , family = "Times"),
    axis.text = element_text(size = 12
                             , color = "white"
                             , face = "bold"
                             , family = "Times"),
    axis.title.x = element_text(size = 12
                                , color = "white"
                                , face = "bold"
                                , family = "Times"),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "white")
  )
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot8.pdf", width = 8, height = 8, units = "in")

# Plot 9 (Tectonic Setting Barplot)
ggplot(eruption) + aes(x = Plate, fill = Crust) + 
  geom_bar(position = position_dodge()) + 
  scale_fill_manual(values = c("red","yellow", "orange")) +
  ggtitle("Tectonic Setting") + 
  labs(x = "Plate Type", y = "Count") +
  theme(
    panel.background = element_rect(fill = "black"
                                    , color = "black"
                                    , size = 0.5
                                    , linetype = "solid"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    plot.title = element_text(size = 24
                              , color = "white"
                              , face = "bold"
                              , family = "Times"),
    axis.text = element_text(size = 12
                             , color = "white"
                             , family = "Times"),
    axis.title = element_text(size = 12
                              , color = "white"
                              , face = "bold"
                              , family = "Times"),
    legend.title = element_text(size = 12
                                , color = "white"
                                , family = "Times"),
    
    legend.text = element_text(size = 12
                               , color = "white"
                               , family = "Times"),
    axis.ticks.y = element_line(color = "white"),
    axis.ticks.x = element_blank())
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot9.pdf", width = 8, height = 8, units = "in")


# Question 3: When did the latest eruptions for these volcanoes occur?

# Plot 10 (Eruption Time Series)

# The "Last.Known.Eruptions" column is a character string vector written with 
# both BCE/CE dates. This code splits the string, separating the date from the 
# era. 
esplit <- 
  str_split_fixed(eruption$Last.Known.Eruption, " ", 2)

# This code takes the to parts of the split string and makes them new columns in
# the data frame. 
eruption$Date <- as.integer(esplit[,1])
eruption$Era <- esplit[,2]

# This for loop converts all of the "BCE" dates to negative numbers to make it 
# easier to graph.
for (x in 1:length(eruption$Date)) {
  if (eruption$Era[x] == "BCE") {
    eruption$Date[x] = eruption$Date[x] * (-1)
  }
}

# Plots each volcano by the date of its last eruption 
table(eruption$Date)

# Defines a new data frame
date <- table(eruption$Date)
date <- as.data.frame(date)
date

# Rename Columns
date <- plyr::rename(date, c("Var1" = "Year", 
                             "Freq"="Count"))

ggplot(date) + aes(x = Year, y = Count) + geom_point(color = "red") +
  ggtitle("Date of Latest Eruption") +
  expand_limits(x = 400) + expand_limits(x = -10) +
  scale_x_discrete(breaks = levels(date$Year)[floor(seq(1, 
                                                        nlevels(date$Year), 
                                                        length.out = 10))]) +
  xlab("Year (Negative = BCE, Positive = CE)") + ylab("Count") +
theme(
  panel.background = element_rect(fill = "black",
                                  color = "black",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major.x = element_blank(), 
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.background = element_rect(fill = "black"),
  plot.title = element_text(size = 24
                            , color = "white"
                            , face = "bold"
                            , family = "Times"),
  axis.text = element_text(size = 12
                           , color = "white"
                           , face = "bold"
                           , family = "Times"),
  axis.title.x = element_text(size = 12
                              , color = "white"
                              , face = "bold"
                              , family = "Times"),
  axis.title.y = element_text(size = 12
                               , color = "white"
                               , face = "bold"
                               , family = "Times"),
  axis.ticks = element_line(color = "white")
)
makeFootnote("Source: Smithsonian Institute (via Kaggle)")
# ggsave("Plot10.pdf", width = 12, height = 8, units = "in")



# Sources ##################################################

# Smithsonian Institute (via Kaggle)

# https://www.statology.org/population-pyramid-in-r/

# https://datavizpyr.com/how-to-make-world-map-with-ggplot2-in-r/

# http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements

# https://stackoverflow.com/questions/55789198/how-to-reduce-number-of-ticks-for-ggplot-with-axis-derived-from-basecut-func

# geology.com

# https://www.usgs.gov/special-topics/subduction-zone-science/science/introduction-subduction-zones-amazing-events


# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
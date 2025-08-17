# COMBINED BOXPLOT IN ONE PLOT ################################################

# LOAD PACKAGES ###############################################################

library(ggplot2) # Main plotting system , customizable graphics
library(dplyr) # For data manipulation, part of tidyverse, for cultivar_pathogen factor

# LOAD DATA ###################################################################
data <- read.csv(file.choose())

## Define cultivar order ======================================================
cultivar_order <- c("CA001", "CA002", "CA003", "CA004", "CA005", "CA006",
                    "CA007", "CA017", "CA019", "CA058", "CA059",
                    "CA062", "Control")


### Create combined factor for x-axis -----------------------------------------
# This groups cultivars under each pathogen group
# Each pathogen group appear seperately
data <- data %>%
  mutate(Cultivar_Pathogen = factor(paste(Pathogen, Cultivar, sep = "_"),
                                    levels = c(
                                      paste("SL1931", cultivar_order, sep = "_"),
                                      paste("HWA", cultivar_order, sep = "_"),
                                      paste("AD", cultivar_order, sep = "_"),
                                      paste("SL3505", cultivar_order, sep = "_"),
                                      paste("SL341", cultivar_order, sep = "_")
                                    )))

## Create x-axis labels: blank for the gap ====================================
# In the x axis under each pathogen cultivar names should appear
# There 5 levels in pathogens
# So the cultivar order should repeat 5 times
x_labels <- c(cultivar_order, cultivar_order,
              cultivar_order, cultivar_order,
              cultivar_order)

# PLOT ########################################################################

# for x axis both cultivar and pathogen group considered
# y axis - wilt rate measurements as %
# box colors will be assigned based on pathogen type
ggplot(data, aes(x = Cultivar_Pathogen, y = Wilt_Rate, fill = Pathogen)) +
  # geom_boxplot - draws boxplot shapes
  # outliers.shape = NA, hides outlier points to make plots cleaner
  # alpha = 0.8 - makes boxes slightly transparent
  # width = 0.5 - controls box width (narrower boxes if <1)
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.5) +
  # scale_fill_manual() - manually assigns specific colors to pathogen groups 
  # instead of using default ggplot colors
  scale_fill_manual(values = c("SL1931" = "#FF0000", 
                               "HWA" = "#FFA500", 
                               "AD" = "#59DA50",
                               "SL3505" = "#368aff",
                               "SL341" = "#6B66FF")) +
  # labs() - sets the plot title and axis labels
  labs(title = "Wilt rate (%) of 13 pepper cultivars inoculated with 5 Ral strains",
       x = "Cultivar_ID",
       y = "Wilt Rate (%)") +
  scale_y_continuous(limits = c(0, 100)) +   # force y-axis from 0–100
  # theme_classic - no background, only axis lines, base_size - text size for whole plot
  theme_classic(base_size = 12) +
  theme(
    # angle - rotate x axis label vertically 
    # hjust - adjust label alignment
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    # axis lines black
    axis.line = element_line(color = "black"),
    # major y axis horizontal lines visible, add color
    panel.grid.major.y = element_line(color = "grey80"),
    # removes minor y axis horizontal lines
    panel.grid.minor.y = element_blank(),
    # add title
    plot.title = element_text(face = "bold", hjust = 0.5),
    # position legend
    legend.position = "right",
    legend.direction = "vertical"
  ) +
  # replaces factor names on the x-axis with custom labels
  scale_x_discrete(labels = x_labels)


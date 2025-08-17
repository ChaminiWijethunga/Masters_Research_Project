# SINGLE BOX PLOT DIAGRAMS ####################################################

# LOAD PACKAGES ###############################################################
library(ggplot2) # Plotting
library(dplyr) # data manipulation

# manually mention data frame
data_SL1931 <- data.frame(
  Cultivar = c("CA001", "CA001", "CA001",
               "CA002", "CA002", "CA002",
               "CA003", "CA003", "CA003",
               "CA004", "CA004", "CA004",
               "CA005", "CA005", "CA005",
               "CA006", "CA006", "CA006",
               "CA007", "CA007", "CA007",
               "CA017", "CA017", "CA017",
               "CA019", "CA019", "CA019",
               "CA058", "CA058", "CA058",
               "CA059", "CA059", "CA059",
               "CA062", "CA062", "CA062",
               "Control", "Control", "Control"),
  Wilt_Rate = c(0.0000,	0.0000,	0.0000,
                0.0000,	0.0000,	0.0000,
                0.0000,	0.0000,	0.0000,
                0.0000,	0.0000,	0.0000,
                0.0000,	0.0000,	0.0000,
                0.0000,	0.0000,	0.0000,
                0.0000,	0.0000,	0.0000,
                20.0000,	13.3333,	0.0000,
                13.3333,	20.0000,	6.6667,
                80.0000,	93.3333,	66.6667,
                73.3333,	80.0000,	93.3333,
                100.0000,	100.0000,	86.6667,
                100.0000,	83.3333,	93.3333),
  Pathogen = c("SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931",
               "SL1931", "SL1931", "SL1931"),
  Replicate = c("R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3",
                "R1", "R2", "R3")
)


# Define the cultivar order ###################################################
cultivar_order <- c("CA001", "CA002", "CA003", "CA004", "CA005", "CA006",
                    "CA007", "CA017", "CA019", "CA058", "CA059",
                    "CA062", "Control")


# Plot ########################################################################
ggplot(data_SL1931, aes(x = Cultivar, y = Wilt_Rate, fill = Pathogen)) + # assign axis
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.4) + # create boxplot
  scale_fill_manual(values = c("SL1931" = "#FF0000")) +  # custom colors
  labs(title = "Wilt Rate % of cultivars vs SL1931 strain", # labels
       x = "Cultivar",
       y = "Wilt Rate (%)") +
  scale_y_continuous(limits = c(0, 100)) +   # force y-axis from 0–100
  theme_classic(base_size = 14) + # how the plot appear
  theme(
    axis.line = element_line(color = "black"),               # Axis lines in black
    panel.grid.major.x = element_blank(),                    # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),                    # Remove vertical minor grid lines
    panel.grid.major.y = element_line(color = "grey80"),      # Horizontal grid lines in grey
    panel.grid.minor.y = element_blank(),                     # Light grey minor horizontal lines
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # vertical labels
  )


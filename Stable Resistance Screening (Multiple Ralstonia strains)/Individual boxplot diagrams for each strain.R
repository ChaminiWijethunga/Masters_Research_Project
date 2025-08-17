# INDIVIDUAL BOXPLOT DIAGRAMS FOR EACH STRAIN ################################

# LOAD PACKAGES ###############################################################

library(ggplot2) # Plotting
library(dplyr) # For data manipulation

# LOAD DATA ###################################################################
data <- read.csv(file.choose())

## Define cultivar order ======================================================
cultivar_order <- c("CA001", "CA002", "CA003", "CA004", "CA005", "CA006",
                    "CA007", "CA017", "CA019", "CA058", "CA059",
                    "CA062", "Control")


# Separate datasets ===========================================================
data_SL1931 <- data %>%
  filter(Pathogen == "SL1931") %>%
  mutate(Cultivar = factor(Cultivar, levels = cultivar_order))

data_HWA <- data %>%
  filter(Pathogen == "HWA") %>%
  mutate(Cultivar = factor(Cultivar, levels = cultivar_order))

data_AD <- data %>%
  filter(Pathogen == "AD") %>%
  mutate(Cultivar = factor(Cultivar, levels = cultivar_order))

data_SL3505 <- data %>%
  filter(Pathogen == "SL3505") %>%
  mutate(Cultivar = factor(Cultivar, levels = cultivar_order))

data_SL341 <- data %>%
  filter(Pathogen == "SL341") %>%
  mutate(Cultivar = factor(Cultivar, levels = cultivar_order))


# PLOT ########################################################################

# Plot ########################################################################
p1 <- ggplot(data_SL1931, aes(x = Cultivar, y = Wilt_Rate, fill = Pathogen)) + # assign axis
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.4) + # create boxplot
  scale_fill_manual(values = c("SL1931" = "#FF0000")) +  # custom colors
  labs(title = "Wilt Rate % of cultivar vs SL1931 strain", # labels
       x = "Cultivar",
       y = "Wilt Rate (%)") +
  scale_y_continuous(limits = c(0, 100)) +   # force y-axis from 0–100
  theme_classic(base_size = 12) + # how the plot appear
  theme(
    axis.line = element_line(color = "black"),               # Axis lines in black
    panel.grid.major.x = element_blank(),                    # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),                    # Remove vertical minor grid lines
    panel.grid.major.y = element_line(color = "grey80"),      # Horizontal grid lines in grey
    panel.grid.minor.y = element_blank(),                     # Light grey minor horizontal lines
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1) # vertical labels
  )

p2 <- ggplot(data_HWA, aes(x = Cultivar, y = Wilt_Rate, fill = Pathogen)) + # assign axis
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.4) + # create boxplot
  scale_fill_manual(values = c("HWA" = "#FFA500")) +  # custom colors
  labs(title = "Wilt Rate % of cultivar vs HWA strain", # labels
       x = "Cultivar",
       y = "Wilt Rate (%)") +
  scale_y_continuous(limits = c(0, 100)) +   # force y-axis from 0–100
  theme_classic(base_size = 12) + # how the plot appear
  theme(
    axis.line = element_line(color = "black"),               # Axis lines in black
    panel.grid.major.x = element_blank(),                    # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),                    # Remove vertical minor grid lines
    panel.grid.major.y = element_line(color = "grey80"),      # Horizontal grid lines in grey
    panel.grid.minor.y = element_blank(),                     # Light grey minor horizontal lines
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1) # vertical labels
  )

p3 <- ggplot(data_AD, aes(x = Cultivar, y = Wilt_Rate, fill = Pathogen)) + # assign axis
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.4) + # create boxplot
  scale_fill_manual(values = c("AD" = "#59DA50")) +  # custom colors
  labs(title = "Wilt Rate % of cultivars vs AD strain", # labels
       x = "Cultivar",
       y = "Wilt Rate (%)") +
  scale_y_continuous(limits = c(0, 100)) +   # force y-axis from 0–100
  theme_classic(base_size = 12) + # how the plot appear
  theme(
    axis.line = element_line(color = "black"),               # Axis lines in black
    panel.grid.major.x = element_blank(),                    # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),                    # Remove vertical minor grid lines
    panel.grid.major.y = element_line(color = "grey80"),      # Horizontal grid lines in grey
    panel.grid.minor.y = element_blank(),                     # Light grey minor horizontal lines
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1) # vertical labels
  )

p4 <- ggplot(data_SL3505, aes(x = Cultivar, y = Wilt_Rate, fill = Pathogen)) + # assign axis
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.4) + # create boxplot
  scale_fill_manual(values = c("SL3505" = "#368aff")) +  # custom colors
  labs(title = "Wilt Rate % of cultivars vs SL3505 strain", # labels
       x = "Cultivar",
       y = "Wilt Rate (%)") +
  scale_y_continuous(limits = c(0, 100)) +   # force y-axis from 0–100
  theme_classic(base_size = 12) + # how the plot appear
  theme(
    axis.line = element_line(color = "black"),               # Axis lines in black
    panel.grid.major.x = element_blank(),                    # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),                    # Remove vertical minor grid lines
    panel.grid.major.y = element_line(color = "grey80"),      # Horizontal grid lines in grey
    panel.grid.minor.y = element_blank(),                     # Light grey minor horizontal lines
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1) # vertical labels
  )

p5 <- ggplot(data_SL341, aes(x = Cultivar, y = Wilt_Rate, fill = Pathogen)) + # assign axis
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.4) + # create boxplot
  scale_fill_manual(values = c("SL341" = "#6B66FF")) +  # custom colors
  labs(title = "Wilt Rate % of cultivars vs SL341 strain", # labels
       x = "Cultivar",
       y = "Wilt Rate (%)") +
  scale_y_continuous(limits = c(0, 100)) +   # force y-axis from 0–100
  theme_classic(base_size = 12) + # how the plot appear
  theme(
    axis.line = element_line(color = "black"),               # Axis lines in black
    panel.grid.major.x = element_blank(),                    # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),                    # Remove vertical minor grid lines
    panel.grid.major.y = element_line(color = "grey80"),      # Horizontal grid lines in grey
    panel.grid.minor.y = element_blank(),                     # Light grey minor horizontal lines
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1) # vertical labels
  )
# Load patchwork
library(patchwork)

# Combine the two plots side by side
combined_plot <- p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 2)

# Display the combined plot
combined_plot

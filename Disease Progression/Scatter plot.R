# LOAD PACKAGES ##############################################################

library(tidyr) # reshaping data wide -> long
library(dplyr) # data manipulation
library(ggplot2) # for plotting

# LOAD AND PREPARE DATA ######################################################

# Load your data
data_wide <- read.csv(file.choose(), row.names = 1, check.names = FALSE)

# Convert row names to a new column
data_wide <- data_wide %>%
  mutate(Cultivar = rownames(data_wide)) # # assign row names to a new column

# CONVERT WIDE TO LONG FORMAT ################################################

# Convert wide to long format
data_long <- data_wide %>%
  pivot_longer( # turns columns for each day into rows
    cols = -Cultivar,
    names_to = "Day", # column name for days
    values_to = "Score" # values for each cultivar at the day
  ) %>%
  mutate(Day = as.numeric(Day)) # converts day days to numeric plotting

# DEFINE GROUPS ##############################################################

# Define groups for lines and shapes
# Groups are defined based on phenotype category of each cultivar
group1 <- c("CA001","CA002","CA003","CA004","CA005","CA006","CA007")
group2 <- c("CA008","CA009","CA010","CA011","CA012","CA013","CA014","CA015",
            "CA016","CA017","CA018","CA019","CA020","CA021","CA022","CA023",
            "CA024","CA025","CA026","CA027","CA028","CA029","CA030")
group3 <- c("CA031", "CA032", "CA033", "CA034", "CA035", "CA036", "CA037",
            "CA038", "CA039", "CA040", "CA041", "CA042", "CA043", "CA044",
            "CA045", "CA046", "CA047", "CA048", "CA049", "CA050", "CA051",
            "CA052", "CA053")
group4 <- c("CA054","CA055","CA056","CA057","CA058","CA059","CA060", "CA061")

# Assign groups to cultivars
# create new column group
data_long <- data_long %>%
  mutate(Group = case_when(
    Cultivar %in% group1 ~ "Group1",
    Cultivar %in% group2 ~ "Group2",
    Cultivar %in% group3 ~ "Group3",
    Cultivar %in% group4 ~ "Group4",
    TRUE ~ "Other"
  ))

# DEFINE LINE TYPES AND SHAPES ################################################

# Assign unique shapes within each group
group_list <- list(group1, group2, group3, group4)
shapes <- c(0,1,2,3,4,5)  # shapes to choose from (0=square, 1=circle, 2=triangle, etc.).

# Cultivar + shape for all groups
shape_map <- lapply(group_list, function(grp) {
  n <- length(grp) # no of cultivar in each group
  # repeat shapes if more cultivars than shapes available, but no overlaps
  assigned_shapes <- rep(shapes, length.out = n) # assigns shapes uniquely
  data.frame(Cultivar = grp,
             Shape = assigned_shapes) # create small tble for each group
}) %>% bind_rows() # combine all group mappings

# Join with main dataset
data_long <- data_long %>%
  left_join(shape_map, by = "Cultivar") %>% # add shape column to main data
  mutate(Shape = factor(Shape))  # make factor for ggplot

# PLOT #######################################################################

# plot points only
ggplot(data_long, aes(x = Day, y = Score,
                      group = Cultivar,
                      color = Cultivar,
                      shape = Cultivar)) +  # map shape to cultivar for legend
  geom_point(size = 2, stroke = 0.7) +       # only points, no lines, stroke - shape outline
  scale_color_manual(values = c( # manually assign colors
    "CA001" = "#0046ab", "CA002" = "#0046ab", "CA003" = "#0046ab",
    "CA004" = "#0046ab", "CA005" = "#0046ab", "CA006" = "#0046ab",
    "CA007" = "#0046ab", "CA008" = "#3be7fe", "CA009" = "#3be7fe",
    "CA010" = "#3be7fe", "CA011" = "#3be7fe", "CA012" = "#3be7fe",
    "CA013" = "#3be7fe", "CA014" = "#3be7fe", "CA015" = "#3be7fe",
    "CA016" = "#3be7fe", "CA017" = "#3be7fe", "CA018" = "#3be7fe",
    "CA019" = "#3be7fe", "CA020" = "#3be7fe", "CA021" = "#3be7fe",
    "CA022" = "#3be7fe", "CA023" = "#3be7fe", "CA024" = "#3be7fe",
    "CA025" = "#3be7fe", "CA026" = "#3be7fe", "CA027" = "#3be7fe",
    "CA028" = "#3be7fe", "CA029" = "#3be7fe", "CA030" = "#3be7fe",
    "CA031" = "#ffc100", "CA032" = "#ffc100", "CA033" = "#ffc100",
    "CA034" = "#ffc100", "CA035" = "#ffc100", "CA036" = "#ffc100",
    "CA037" = "#ffc100", "CA038" = "#ffc100", "CA039" = "#ffc100",
    "CA040" = "#ffc100", "CA041" = "#ffc100", "CA042" = "#ffc100",
    "CA043" = "#ffc100", "CA044" = "#ffc100", "CA045" = "#ffc100",
    "CA046" = "#ffc100", "CA047" = "#ffc100", "CA048" = "#ffc100",
    "CA049" = "#ffc100", "CA050" = "#ffc100", "CA051" = "#ffc100",
    "CA052" = "#ffc100", "CA053" = "#ffc100", "CA054" = "#FF0000",
    "CA055" = "#FF0000", "CA056" = "#FF0000", "CA057" = "#FF0000", 
    "CA058" = "#FF0000", "CA059" = "#FF0000", "CA060" = "#FF0000",
    "CA061" = "#FF0000"
  )) +
  scale_shape_manual(               # custom color and shapes
    values = setNames(              # maps shape numbers to cultivar
      as.numeric(data_long$Shape),
      data_long$Cultivar)) +
  guides(color = guide_legend(ncol = 3),    # legend format
         shape = guide_legend(ncol = 3)) +  # both color+shape in legend, 3 columns
  theme_classic(base_size = 12) +
  theme(                                  # theme and labels
    legend.position = "right",
    legend.title = element_blank(),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(title = "Disease progression in 61 pepper germplasms to Rals strain",
       x = "Days After Inoculation", y = "Disease Severity Index")

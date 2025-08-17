# LOAD PACKAGES ##############################################################

library(tidyr) # reshaping data wide -> long
library(dplyr) # data manipulation
library(ggplot2) # for plotting

# LOAD AND PREPARE DATA ######################################################

# Load your data
data_wide <- read.csv(file.choose(),
                      row.names = 1, # cultivar IDs in first column becomes row names
                      check.names = FALSE) # column names stays the same

# Convert row names to a column "cultivar"
data_wide <- data_wide %>%
  mutate(Cultivar = rownames(data_wide)) # assign row names to a new column

# CONVERT WIDE TO LONG FORMAT ################################################

# Convert wide to long format
data_long <- data_wide %>%
  pivot_longer( # turns columns for each day into rows
    cols = -Cultivar, # gather all column except cultivar ID column
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

# Define available line types & shapes
linetypes <- c("solid","dashed","dotted","dotdash","longdash","twodash")
shapes <- c(0,1,2,3,4,5)  # squares, circles, triangles, etc.

# Create all possible line type-shape pairs
combo_tbl <- expand.grid(Linetype = linetypes, # creates all combinations
                         Shape = shapes,       # of line types and shapes
                         stringsAsFactors = FALSE) # give unique line-shape combo

# Function to assign unique pairs within each group
# create a new function
assign_combos <- function(cultivars) { # picks first N combos for each group
  needed <- length(cultivars) # no of cultivars in each group
  combos <- combo_tbl[1:needed, ]  # take first N unique combos
  data.frame(Cultivar = cultivars,
             Linetype = combos$Linetype,
             Shape = combos$Shape,
             stringsAsFactors = FALSE)
}

# Apply for each group
# create 4 maps for 4 groups
map1 <- assign_combos(group1)
map2 <- assign_combos(group2)
map3 <- assign_combos(group3)
map4 <- assign_combos(group4)

# Combine mapping
# style_map - tbl assigning unique line-shape combo to each cultivar
style_map <- bind_rows(map1, map2, map3, map4)

# Join style map with main dataset
# left_join - adds ploting info to cultivar in main data
data_long <- data_long %>%
  left_join(style_map, by = "Cultivar")

# line and shape has to be categorical
# in ggplot2 aesthtics has to be converted to factors
data_long <- data_long %>%
  mutate(
    Shape = factor(Shape),
    Linetype = factor(Linetype)
  )


# PLOT #######################################################################

ggplot(data_long, aes(x = Day, y = Score,
                      group = Cultivar, # lines connect points per cultivar
                      color = Cultivar, # each cultivar has assigned colors
                      linetype = Cultivar,   # map linetype to Cultivar
                      shape = Cultivar)) +   # map shape to Cultivar
  geom_line(linewidth = 0.6) + # draws line connecting value points
  geom_point(size = 2) + # draws points at each day
  scale_color_manual(values = c( # maps each cultivar to assigned color
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
  scale_linetype_manual(values = setNames(data_long$Linetype,
                                          data_long$Cultivar)) + # map each cultivar to unique linetype
  scale_shape_manual(values = setNames(as.numeric(data_long$Shape),
                                       data_long$Cultivar)) + # map each cultivar to unique shape
  labs(title = "Disease progression in 61 pepper germplasms to Rals strain",
       x = "Days After Inoculation", y = "Disease Severity Index") +
  guides(color = guide_legend(ncol = 3)) +  # legend in 3 columns
  theme_classic(base_size = 12) + # clean plot, minimal styling
  theme(
    legend.position = "right", # position legend to right side
    legend.title = element_blank(),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# ============================================================
# Layered Design Space Network
# ============================================================

library(tidyverse)
library(janitor)
library(igraph)

# ============================================================
# Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# Select dimensions
# ============================================================

design_space <- data %>%
  select(
    platform,
    display,
    scenario,
    measurements
  )

# ============================================================
# Create edges between layers
# ============================================================

edges1 <- design_space %>%
  count(platform, scenario)

edges2 <- design_space %>%
  count(scenario, measurements)

edges <- bind_rows(
  edges1 %>% rename(from = platform, to = scenario),
  edges2 %>% rename(from = scenario, to = measurements)
)

# ============================================================
# Create graph
# ============================================================

graph <- graph_from_data_frame(edges, directed = FALSE)

# ============================================================
# Assign node types
# ============================================================

nodes <- V(graph)$name

node_type <- case_when(
  nodes %in% unique(data$platform) ~ "Experimental Setup",
  nodes %in% unique(data$scenario) ~ "Study Focus",
  nodes %in% unique(data$measurements) ~ "Evaluation",
  TRUE ~ "Other"
)

V(graph)$type <- node_type

# ============================================================
# Assign colors
# ============================================================

node_colors <- case_when(
  node_type == "Experimental Setup" ~ "#4E79A7",
  node_type == "Study Focus" ~ "#59A14F",
  node_type == "Evaluation" ~ "#F28E2B",
  TRUE ~ "grey70"
)

# ============================================================
# Define layered layout
# ============================================================

layer <- ifelse(node_type == "Experimental Setup", 3,
                ifelse(node_type == "Study Focus", 2, 1))

layout <- layout_as_tree(graph, root = which(layer == 3))

# ============================================================
# Plot graph
# ============================================================

plot(
  graph,
  
  layout = layout,
  
  vertex.color = node_colors,
  vertex.size = 28,
  
  vertex.label.cex = 0.9,
  vertex.label.color = "black",
  
  edge.width = edges$n,
  edge.color = "grey70"
)

legend(
  "topleft",
  legend = c(
    "Experimental Setup",
    "Study Focus",
    "Evaluation Measures"
  ),
  col = c("#4E79A7","#59A14F","#F28E2B"),
  pch = 19,
  pt.cex = 2,
  bty = "n"
)

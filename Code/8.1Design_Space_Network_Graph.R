# ============================================================
# Design Space Network Graph
# ============================================================

library(tidyverse)
library(janitor)
library(igraph)
library(ggraph)

# ============================================================
# Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# Select key design-space dimensions
# ============================================================

design_space <- data %>%
  select(
    platform,
    display,
    scenario,
    measurements
  )

# ============================================================
# Convert to long format
# ============================================================

long_data <- design_space %>%
  pivot_longer(
    cols = everything(),
    names_to = "dimension",
    values_to = "value"
  )

# ============================================================
# Create edges between dimensions
# ============================================================

edges <- design_space %>%
  unite("combo", everything(), remove = FALSE) %>%
  separate_rows(combo, sep = "_") %>%
  count(combo)

# simpler pairwise edges

pairs <- design_space %>%
  mutate(study = row_number()) %>%
  pivot_longer(-study) %>%
  select(study, value)

edges <- pairs %>%
  inner_join(pairs, by="study") %>%
  filter(value.x != value.y) %>%
  count(value.x, value.y)

# ============================================================
# Create network
# ============================================================

graph <- graph_from_data_frame(edges, directed = FALSE)

# ============================================================
# Plot network
# ============================================================

network_plot <- ggraph(graph, layout = "fr") +
  
  geom_edge_link(
    aes(width = n),
    alpha = 0.4
  ) +
  
  geom_node_point(
    size = 6,
    color = "#4E79A7"
  ) +
  
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 4
  ) +
  
  theme_void()

network_plot

# ============================================================
# Save figure
# ============================================================

ggsave(
  "figures/design_space_network_graph.pdf",
  network_plot,
  width = 10,
  height = 8
)

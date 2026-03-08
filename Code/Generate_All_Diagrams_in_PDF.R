# ============================================================
# MASTER SCRIPT
# AV Passenger Experience Review - All Graphs to One PDF
# Order: 1, 2, 3, 4, 5, 6, 8.1, 8.2, 9
# ============================================================

# ============================================================
# 0 Install/load packages ONCE
# ============================================================

packages <- c(
  "tidyverse",
  "janitor",
  "viridis",
  "patchwork",
  "scales",
  "gridExtra",
  "cowplot",
  "igraph",
  "ggraph"
)

missing <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))

# ============================================================
# 1 Create output folder
# ============================================================

dir.create("figures", showWarnings = FALSE)

# ============================================================
# 2 Load dataset ONCE
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# 3 Helper: section title page in PDF
# ============================================================

section_title_page <- function(file_name, subtitle = NULL) {
  grid::grid.newpage()
  
  grid::grid.text(
    label = file_name,
    x = 0.5, y = 0.60,
    gp = grid::gpar(fontsize = 24, fontface = "bold")
  )
  
  if (!is.null(subtitle)) {
    grid::grid.text(
      label = subtitle,
      x = 0.5, y = 0.52,
      gp = grid::gpar(fontsize = 14)
    )
  }
}

# ============================================================
# 4 Open ONE PDF for all figures
# ============================================================

output_pdf <- "figures/all_graphs_1_to_9.pdf"
pdf(output_pdf, width = 14, height = 10)

# ============================================================
# FILE 1 - Distribution Analysis
# ============================================================

section_title_page("1.Distribution_Analysis.R", "Distribution Analysis")

exclude_vars <- c("paper", "author", "year")

vars_dist <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
vars_dist <- setdiff(vars_dist, exclude_vars)

plot_distribution <- function(variable) {
  plot_data <- data %>%
    filter(!is.na(.data[[variable]])) %>%
    count(value = as.factor(.data[[variable]]), name = "n")
  
  ggplot(plot_data, aes(
    x = reorder(value, n),
    y = n,
    fill = value
  )) +
    geom_col() +
    scale_fill_viridis_d() +
    coord_flip() +
    labs(
      title = str_to_title(gsub("_", " ", variable)),
      x = NULL,
      y = "Number of Reviewed Studies"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

plots_dist <- lapply(vars_dist, plot_distribution)

plots_per_page_dist <- 4
for (i in seq(1, length(plots_dist), by = plots_per_page_dist)) {
  chunk <- plots_dist[i:min(i + plots_per_page_dist - 1, length(plots_dist))]
  page_plot <- wrap_plots(chunk, ncol = 2) +
    plot_annotation(title = "1.Distribution_Analysis.R")
  print(page_plot)
}

# ============================================================
# FILE 2 - Cross-Dimensions
# Includes BOTH count and percentage graphs
# ============================================================

section_title_page("2.Cross_Dimentions.R", "Cross-Dimension Analysis")

vars_cross <- c(
  "platform",
  "driving_mode",
  "display",
  "scenario",
  "measurements",
  "controlled_sensory_focus",
  "focus_group",
  "participant_seating_position",
  "driver_presence_visibility",
  "cabin_structure",
  "cabin_visibility",
  "motion_dof",
  "comfort_factor",
  "focus_condition"
)

vars_cross <- vars_cross[vars_cross %in% names(data)]

plot_cross_dimension <- function(var1, var2) {
  plot_data <- data %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]])) %>%
    mutate(
      x_var = as.factor(.data[[var1]]),
      fill_var = as.factor(.data[[var2]])
    )
  
  ggplot(plot_data, aes(
    x = x_var,
    fill = fill_var
  )) +
    geom_bar(position = "stack") +
    coord_flip() +
    scale_fill_viridis_d() +
    labs(
      title = paste(
        str_to_title(gsub("_", " ", var1)),
        "vs",
        str_to_title(gsub("_", " ", var2))
      ),
      x = str_to_title(gsub("_", " ", var1)),
      y = "Number of Reviewed Studies",
      fill = str_to_title(gsub("_", " ", var2))
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
}

plot_cross_dimension_percent <- function(var1, var2) {
  plot_data <- data %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]])) %>%
    count(.data[[var1]], .data[[var2]], name = "n") %>%
    group_by(.data[[var1]]) %>%
    mutate(percent = n / sum(n)) %>%
    ungroup() %>%
    mutate(
      x_var = as.factor(.data[[var1]]),
      fill_var = as.factor(.data[[var2]])
    )
  
  ggplot(plot_data, aes(
    x = x_var,
    y = percent,
    fill = fill_var
  )) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_d() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = paste(
        str_to_title(gsub("_", " ", var1)),
        "vs",
        str_to_title(gsub("_", " ", var2)),
        "(Percent)"
      ),
      x = str_to_title(gsub("_", " ", var1)),
      y = "Percentage of Studies",
      fill = str_to_title(gsub("_", " ", var2))
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
}

plots_cross_count <- list()
plots_cross_percent <- list()

for (i in seq_along(vars_cross)) {
  for (j in seq_along(vars_cross)) {
    if (i < j) {
      var1 <- vars_cross[i]
      var2 <- vars_cross[j]
      
      plots_cross_count[[length(plots_cross_count) + 1]] <-
        plot_cross_dimension(var1, var2)
      
      plots_cross_percent[[length(plots_cross_percent) + 1]] <-
        plot_cross_dimension_percent(var1, var2)
    }
  }
}

plots_per_page_cross <- 4

# Count plots
for (i in seq(1, length(plots_cross_count), by = plots_per_page_cross)) {
  chunk <- plots_cross_count[i:min(i + plots_per_page_cross - 1, length(plots_cross_count))]
  page_plot <- wrap_plots(chunk, ncol = 2) +
    plot_annotation(
      title = "2.Cross_Dimentions.R - Counts"
    )
  print(page_plot)
}

# Percent plots
for (i in seq(1, length(plots_cross_percent), by = plots_per_page_cross)) {
  chunk <- plots_cross_percent[i:min(i + plots_per_page_cross - 1, length(plots_cross_percent))]
  page_plot <- wrap_plots(chunk, ncol = 2) +
    plot_annotation(
      title = "2.Cross_Dimentions.R - Percentages"
    )
  print(page_plot)
}

# ============================================================
# FILE 3 - Coverage Matrix
# All possible pairwise coverage matrix combinations
# ============================================================

section_title_page("3.Coverage_Matrix.R", "All Coverage Matrix Combinations")

vars_cov <- c(
  "platform",
  "driving_mode",
  "display",
  "scenario",
  "measurements",
  "measurement_domain",
  "controlled_sensory_focus",
  "focus_group",
  "participant_seating_position",
  "driver_presence_visibility",
  "cabin_structure",
  "cabin_visibility",
  "motion_dof",
  "comfort_factor",
  "focus_condition"
)

vars_cov <- vars_cov[vars_cov %in% names(data)]

data_cov <- data %>%
  mutate(across(all_of(vars_cov), as.factor))

plot_coverage_matrix <- function(var1, var2) {
  
  plot_data <- data_cov %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]])) %>%
    count(.data[[var1]], .data[[var2]], name = "n") %>%
    complete(
      !!sym(var1),
      !!sym(var2),
      fill = list(n = 0)
    )
  
  ggplot(
    plot_data,
    aes(
      x = .data[[var2]],
      y = .data[[var1]],
      fill = n
    )
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), size = 3, color = "white") +
    scale_fill_viridis_c() +
    labs(
      title = paste(
        str_to_title(gsub("_", " ", var1)),
        "vs",
        str_to_title(gsub("_", " ", var2))
      ),
      x = str_to_title(gsub("_", " ", var2)),
      y = str_to_title(gsub("_", " ", var1)),
      fill = "Number of Studies"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 11)
    )
}

var_pairs_cov <- combn(vars_cov, 2, simplify = FALSE)

coverage_plots <- lapply(var_pairs_cov, function(pair) {
  plot_coverage_matrix(pair[1], pair[2])
})

plots_per_page_cov <- 4

for (i in seq(1, length(coverage_plots), by = plots_per_page_cov)) {
  
  chunk <- coverage_plots[i:min(i + plots_per_page_cov - 1, length(coverage_plots))]
  
  page_plot <- wrap_plots(chunk, ncol = 2) +
    plot_annotation(title = "3.Coverage_Matrix.R - All Pairwise Coverage Matrices")
  
  print(page_plot)
}
# ============================================================
# FILE 4 - Research Landscape Map
# ============================================================

section_title_page("4.Research_Landscape_Map.R", "Research Landscape Map")

data_landscape <- data
data_landscape$platform <- tolower(as.character(data_landscape$platform))

data_landscape$platform <- case_when(
  str_detect(data_landscape$platform, "sim") ~ "Simulator",
  str_detect(data_landscape$platform, "real") ~ "Real-world",
  TRUE ~ "Other"
)

landscape_data <- data_landscape %>%
  filter(!is.na(scenario), !is.na(measurements), !is.na(platform)) %>%
  count(scenario, measurements, platform)

landscape_plot <- ggplot(
  landscape_data,
  aes(
    x = scenario,
    y = measurements,
    size = n,
    color = platform
  )
) +
  geom_point(alpha = 0.8) +
  scale_size(range = c(3, 12)) +
  scale_color_manual(
    values = c(
      "Simulator" = "#4E79A7",
      "Real-world" = "#F28E2B",
      "Other" = "grey60"
    )
  ) +
  coord_flip() +
  labs(
    title = "4.Research_Landscape_Map.R",
    x = "Study Scenario",
    y = "Measurement Type",
    size = "Number of Studies",
    color = "Platform"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position = "bottom"
  )

print(landscape_plot)

# ============================================================
# FILE 5 - Immersive Technology Gap
# ============================================================

section_title_page("5.Immersive_Tech_Gap.R", "Immersive Technology Gap Map")

data_immersive <- data
data_immersive$platform <- tolower(as.character(data_immersive$platform))

data_immersive$platform <- case_when(
  str_detect(data_immersive$platform, "sim") ~ "Simulator",
  str_detect(data_immersive$platform, "real") ~ "Real-world",
  TRUE ~ "Other"
)

immersive_map <- data_immersive %>%
  filter(!is.na(platform), !is.na(display), !is.na(measurement_domain)) %>%
  count(platform, display, measurement_domain)

immersive_plot <- ggplot(
  immersive_map,
  aes(
    x = display,
    y = platform,
    size = n,
    color = platform
  )
) +
  geom_point(alpha = 0.8) +
  facet_wrap(~measurement_domain) +
  scale_size(range = c(3, 12)) +
  scale_color_manual(
    values = c(
      "Simulator" = "#4E79A7",
      "Real-world" = "#F28E2B",
      "Other" = "grey60"
    )
  ) +
  labs(
    title = "5.Immersive_Tech_Gap.R",
    x = "Display / Immersive Technology",
    y = "Experimental Platform",
    size = "Number of Studies",
    color = "Platform"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )

print(immersive_plot)

# ============================================================
# FILE 6 - Design Space Gap All Grid
# ============================================================

section_title_page("6.Design_Space_Gap_AllGrid.R", "Design Space Gap Grid")

experimental_dims <- c(
  "platform",
  "driving_mode",
  "cabin_structure",
  "cabin_visibility",
  "motion_dof",
  "display"
)

focus_dims <- c(
  "focus_group",
  "participant_seating_position",
  "scenario",
  "controlled_sensory_focus",
  "driver_presence_visibility"
)

evaluation_dims <- c(
  "comfort_factor",
  "measurements",
  "measurement_domain",
  "focus_condition"
)

design_cols <- c(experimental_dims, focus_dims, evaluation_dims)
design_cols <- design_cols[design_cols %in% names(data)]

design_space_long <- data %>%
  pivot_longer(
    cols = all_of(design_cols),
    names_to = "dimension",
    values_to = "value"
  ) %>%
  mutate(
    category = case_when(
      dimension %in% experimental_dims ~ "Experimental Setup",
      dimension %in% focus_dims ~ "Study Focus",
      dimension %in% evaluation_dims ~ "Evaluation Measures"
    )
  )

gap_grid <- design_space_long %>%
  filter(!is.na(value)) %>%
  count(category, dimension, value)

gap_grid_plot <- ggplot(
  gap_grid,
  aes(
    x = category,
    y = value,
    fill = n
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 4) +
  facet_wrap(~dimension, scales = "free_y") +
  scale_fill_viridis_c(option = "cividis") +
  labs(
    title = "6.Design_Space_Gap_AllGrid.R",
    x = "Design Space Category",
    y = "Dimension Value",
    fill = "Number of Studies"
  ) +
  theme_minimal(base_size = 7) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold")
  )

print(gap_grid_plot)

# ============================================================
# FILE 8.1 - Design Space Network Graph
# ============================================================

section_title_page("8.1Design_Space_Network_Graph.R", "Design Space Network Graph")

design_space_net <- data %>%
  select(any_of(c("platform", "display", "scenario", "measurements"))) %>%
  drop_na()

pairs <- design_space_net %>%
  mutate(study = row_number()) %>%
  pivot_longer(-study) %>%
  select(study, value)

edges_net <- pairs %>%
  inner_join(pairs, by = "study") %>%
  filter(value.x != value.y) %>%
  count(value.x, value.y, name = "n")

graph_net <- graph_from_data_frame(edges_net, directed = FALSE)

network_plot <- ggraph(graph_net, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.4) +
  geom_node_point(size = 6, color = "#4E79A7") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  labs(title = "8.1Design_Space_Network_Graph.R") +
  theme_void()

print(network_plot)

# ============================================================
# FILE 8.2 - Layered Network
# ============================================================

section_title_page("8.2Layered_Neetwork.R", "Layered Design Space Network")

design_space_layer <- data %>%
  select(any_of(c("platform", "display", "scenario", "measurements"))) %>%
  drop_na()

edges1 <- design_space_layer %>%
  count(platform, scenario, name = "n")

edges2 <- design_space_layer %>%
  count(scenario, measurements, name = "n")

edges_layer <- bind_rows(
  edges1 %>% rename(from = platform, to = scenario),
  edges2 %>% rename(from = scenario, to = measurements)
)

graph_layer <- graph_from_data_frame(edges_layer, directed = FALSE)

nodes_layer <- V(graph_layer)$name

node_type <- case_when(
  nodes_layer %in% unique(design_space_layer$platform) ~ "Experimental Setup",
  nodes_layer %in% unique(design_space_layer$scenario) ~ "Study Focus",
  nodes_layer %in% unique(design_space_layer$measurements) ~ "Evaluation",
  TRUE ~ "Other"
)

node_colors <- case_when(
  node_type == "Experimental Setup" ~ "#4E79A7",
  node_type == "Study Focus" ~ "#59A14F",
  node_type == "Evaluation" ~ "#F28E2B",
  TRUE ~ "grey70"
)

layer <- ifelse(
  node_type == "Experimental Setup", 3,
  ifelse(node_type == "Study Focus", 2, 1)
)

layout_layer <- layout_as_tree(graph_layer, root = which(layer == 3))

plot(
  graph_layer,
  layout = layout_layer,
  vertex.color = node_colors,
  vertex.size = 28,
  vertex.label.cex = 0.9,
  vertex.label.color = "black",
  edge.width = edges_layer$n,
  edge.color = "grey70",
  main = "8.2Layered_Neetwork.R"
)

legend(
  "topleft",
  legend = c("Experimental Setup", "Study Focus", "Evaluation Measures"),
  col = c("#4E79A7", "#59A14F", "#F28E2B"),
  pch = 19,
  pt.cex = 2,
  bty = "n"
)

# ============================================================
# FILE 9 - Design Space Gap
# ============================================================

section_title_page("9.Design_Space_Gap.R", "Design Space Gap Map")

design_space_gap <- data %>%
  select(any_of(c("platform", "scenario", "measurements")))

coverage <- design_space_gap %>%
  count(platform, scenario, measurements, name = "n")

full_space <- coverage %>%
  complete(
    platform,
    scenario,
    measurements,
    fill = list(n = 0)
  )

gap_plot <- ggplot(
  full_space,
  aes(
    x = scenario,
    y = platform,
    fill = n
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 4) +
  facet_wrap(~measurements) +
  scale_fill_viridis_c(option = "cividis") +
  labs(
    title = "9.Design_Space_Gap.R",
    x = "Scenario",
    y = "Experimental Platform",
    fill = "Number of Studies"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(gap_plot)

# ============================================================
# CLOSE PDF
# ============================================================

dev.off()

cat("Done. PDF saved at:", output_pdf, "\n")
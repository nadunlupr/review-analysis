# ============================================================
# All Possible Research Landscape Graph Combinations
# ============================================================

# ============================================================
# 0 Install/load packages
# ============================================================

packages <- c("tidyverse", "janitor", "patchwork", "scales")

missing <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))

# ============================================================
# 1 Create output folder
# ============================================================

dir.create("figures_landscape_all", showWarnings = FALSE)

# ============================================================
# 2 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# 3 Choose variables for landscape combinations
#    Edit this list as needed
# ============================================================

vars <- c(
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

# Keep only columns that exist
vars <- vars[vars %in% names(data)]

# Convert to factor for cleaner plotting
data <- data %>%
  mutate(across(all_of(vars), as.factor))

print(vars)

# ============================================================
# 4 Function to build one landscape graph
# ============================================================

plot_landscape_graph <- function(x_var, y_var, color_var) {
  
  plot_data <- data %>%
    filter(
      !is.na(.data[[x_var]]),
      !is.na(.data[[y_var]]),
      !is.na(.data[[color_var]])
    ) %>%
    count(
      .data[[x_var]],
      .data[[y_var]],
      .data[[color_var]],
      name = "n"
    )
  
  ggplot(
    plot_data,
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      size = n,
      color = .data[[color_var]]
    )
  ) +
    geom_point(alpha = 0.8) +
    scale_size(range = c(3, 12)) +
    coord_flip() +
    labs(
      title = paste(
        stringr::str_to_title(gsub("_", " ", y_var)),
        "vs",
        stringr::str_to_title(gsub("_", " ", x_var)),
        "| Colored by",
        stringr::str_to_title(gsub("_", " ", color_var))
      ),
      x = stringr::str_to_title(gsub("_", " ", x_var)),
      y = stringr::str_to_title(gsub("_", " ", y_var)),
      size = "Number of Studies",
      color = stringr::str_to_title(gsub("_", " ", color_var))
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1),
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
}

# ============================================================
# 5 Generate all possible 3-variable combinations
#    x, y, and color must all be different
# ============================================================

landscape_combinations <- expand.grid(
  x_var = vars,
  y_var = vars,
  color_var = vars,
  stringsAsFactors = FALSE
) %>%
  filter(
    x_var != y_var,
    x_var != color_var,
    y_var != color_var
  )

# Optional: remove mirrored duplicates for x/y
# Keeps (platform, scenario, measurements) but removes
# (scenario, platform, measurements)
landscape_combinations <- landscape_combinations %>%
  rowwise() %>%
  mutate(
    xy_key = paste(sort(c(x_var, y_var)), collapse = "__")
  ) %>%
  ungroup() %>%
  distinct(xy_key, color_var, .keep_all = TRUE) %>%
  select(-xy_key)

print(head(landscape_combinations))
cat("Total landscape graphs to generate:", nrow(landscape_combinations), "\n")

# ============================================================
# 6 Create all plots
# ============================================================

landscape_plots <- vector("list", nrow(landscape_combinations))

for (i in seq_len(nrow(landscape_combinations))) {
  
  x_var <- landscape_combinations$x_var[i]
  y_var <- landscape_combinations$y_var[i]
  color_var <- landscape_combinations$color_var[i]
  
  landscape_plots[[i]] <- plot_landscape_graph(x_var, y_var, color_var)
}

# ============================================================
# 7 Save all plots into one PDF
#    One graph per page for readability
# ============================================================

pdf(
  "figures_landscape_all/all_landscape_graph_combinations.pdf",
  width = 12,
  height = 8
)

for (p in landscape_plots) {
  print(p)
}

dev.off()

# ============================================================
# 8 Optional: save a contact-sheet style overview PDF
#    Multiple charts per page
# ============================================================

pdf(
  "figures_landscape_all/all_landscape_graph_combinations_overview.pdf",
  width = 14,
  height = 10
)

plots_per_page <- 4

for (i in seq(1, length(landscape_plots), by = plots_per_page)) {
  
  chunk <- landscape_plots[i:min(i + plots_per_page - 1, length(landscape_plots))]
  
  page_plot <- patchwork::wrap_plots(chunk, ncol = 2) +
    patchwork::plot_annotation(
      title = "All Research Landscape Graph Combinations"
    )
  
  print(page_plot)
}

dev.off()

# ============================================================
# 9 Optional preview in R
# ============================================================

landscape_plots[[1]]
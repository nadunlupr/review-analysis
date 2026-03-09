# ============================================================
# All Possible Coverage Matrix Combinations
# ============================================================

# ============================================================
# 0 Install/load packages
# ============================================================

packages <- c("tidyverse", "janitor", "viridis", "patchwork")

missing <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))

# ============================================================
# 1 Create output folder
# ============================================================

dir.create("figures_coverage_all", showWarnings = FALSE)

# ============================================================
# 2 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# 3 Choose variables for coverage matrices
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

# Keep only variables that actually exist
vars <- vars[vars %in% names(data)]

# Convert selected variables to factor
data <- data %>%
  mutate(across(all_of(vars), as.factor))

print(vars)

# ============================================================
# 4 Coverage matrix plotting function
# ============================================================

plot_coverage_matrix <- function(var1, var2) {
  
  plot_data <- data %>%
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
      title = NULL,
      x = stringr::str_to_title(gsub("_", " ", var2)),
      y = stringr::str_to_title(gsub("_", " ", var1)),
      fill = "Number of Studies"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

# ============================================================
# 5 Generate all pairwise combinations
# ============================================================

var_pairs <- combn(vars, 2, simplify = FALSE)

coverage_plots <- lapply(var_pairs, function(pair) {
  plot_coverage_matrix(pair[1], pair[2])
})

plot_coverage_matrix("driver_presence_visibility", "scenario")

# ============================================================
# 6 Save all matrices into one PDF
#    Multiple charts per page
# ============================================================

plots_per_page <- 4
ncol_layout <- 2
pdf_width <- 14
pdf_height <- 10

pdf("figures_coverage_all/all_coverage_matrix_combinations.pdf",
    width = pdf_width,
    height = pdf_height)

for (i in seq(1, length(coverage_plots), by = plots_per_page)) {
  
  chunk <- coverage_plots[i:min(i + plots_per_page - 1, length(coverage_plots))]
  
  page_plot <- wrap_plots(chunk, ncol = ncol_layout) +
    plot_annotation(
      title = "All Possible Coverage Matrix Combinations"
    )
  
  print(page_plot)
}

dev.off()

# ============================================================
# 7 Optional: save each matrix separately
# ============================================================

for (pair in var_pairs) {
  
  var1 <- pair[1]
  var2 <- pair[2]
  
  p <- plot_coverage_matrix(var1, var2)
  
  ggsave(
    filename = paste0(
      "figures_coverage_all/coverage_",
      var1,
      "_vs_",
      var2,
      ".pdf"
    ),
    plot = p,
    width = 8,
    height = 6
  )
}

# ============================================================
# 8 Optional preview in R
# ============================================================

if (length(coverage_plots) >= 4) {
  wrap_plots(coverage_plots[1:4], ncol = 2)
} else {
  wrap_plots(coverage_plots, ncol = 2)
}
# ======================================
#   Distribution Analysis
# ======================================

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
# ======================================
# 1 Load dataset
# ======================================

data <- read.csv("Data/TableLiterature.csv")

# Clean column names
data <- clean_names(data)

# Inspect data
str(data)
summary(data)

# ======================================
# 2 Identify variables to plot
# ======================================

# Remove non-categorical columns if needed
# (example: paper titles or IDs)

exclude_vars <- c("paper", "author", "year")

vars <- setdiff(names(data), exclude_vars)

print(vars)

# ======================================
# 3 Create output folder
# ======================================

dir.create("figures", showWarnings = FALSE)

# ======================================
# 4 Distribution plotting function
# ======================================

plot_distribution <- function(variable){
  
  p <- data %>%
    count(.data[[variable]]) %>%
    ggplot(aes(
      x = reorder(.data[[variable]], n),
      y = n,
      fill = .data[[variable]]
    )) +
    scale_fill_viridis_d()+
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      x = stringr::str_to_title(gsub("_", " ", variable)),
      y = "Number of Reviewed Studies"
    )+
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ======================================
# 5 Generate plots for all variables
# ======================================

plots <- lapply(vars, plot_distribution)

# ======================================
# 6 Save plots automatically
# ======================================

for(v in vars){
  
  p <- plot_distribution(v)
  
  ggsave(
    filename = paste0("figures/dist_", v, ".pdf"),
    plot = p,
    width = 6,
    height = 4
  )
  
}

# ======================================
# 7 Optional: show plots in R
# ======================================

plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]]
plots[[11]]
plots[[12]]
plots[[13]]
plots[[14]]
# plots[[15]]
# plots[[16]]
plots[[17]]
# plots[[18]]
# plots[[19]]
# plots[[20]]
plots[[21]]
plots[[22]]

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

exclude_vars <- c("study", "year_of_publication")

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
    mutate(
      label = stringr::str_wrap(as.character(.data[[variable]]), width = 18)
    ) %>%
    ggplot(aes(
      x = reorder(label, n),
      y = n,
      fill = label
    )) +
    scale_fill_viridis_d() +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = NULL,
      x = stringr::str_to_title(gsub("_", " ", variable)),
      y = "Number of Reviewed Studies"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(
        color = "black",
        size = 14,
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(t = 2)
      ),
      
      axis.text.y = element_text(
        color = "black",
        size = 14,
        hjust = 0,
        vjust = 0.5,
        margin = margin(r = -2)
      ),
      
      axis.title.x = element_text(
        color = "black",
        face = "bold",
        size = 16,
        hjust = 0.5,
        margin = margin(t = 12)
      ),
      
      axis.title.y = element_text(
        color = "black",
        face = "bold",
        size = 16,
        hjust = 0.5,
        margin = margin(r = 12)
      ),
      
      panel.border = element_rect(
        color = "grey40",
        fill = NA,
        linewidth = 0.8
      ),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
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

plots[[1]]

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
 plots[[15]]
 plots[[16]]
plots[[17]]
# plots[[18]]
# plots[[19]]
# plots[[20]]
plots[[21]]
plots[[22]]

# ======================================
# Distribution Analysis
# ======================================

# Load libraries
library(tidyverse)
library(janitor)

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

exclude_vars <- c("study", "year_of_publication")
vars <- setdiff(names(data), exclude_vars)

print(vars)

# ======================================
# 3 Create output folder
# ======================================

dir.create("figures", showWarnings = FALSE)

# ======================================
# 4 Variables where "+" means combined categories
# ======================================

split_plus_vars <- c("display", "scenario")

# ======================================
# 5 Color gradients for different plots
#    Use exact cleaned column names here
# ======================================

plot_gradients <- list(
  platform = c("#DCEAF4", "#7FA6C9", "#2F4B7C"),          # blue
  interaction = c("#E3F3EC", "#8FC9AE", "#2F6F4F"),       # green
  display = c("#F3E8FF", "#B999E5", "#5B3C88"),           # purple
  scenario = c("#FDEBE2", "#F2A97E", "#C65D2E"),          # orange
  modality = c("#FBE4EC", "#E78FB3", "#A63D6E"),          # pink
  feedback = c("#FFF4D6", "#E7C768", "#A67C00"),          # gold
  task = c("#E6F4F1", "#7FC8BE", "#2F7F77"),              # teal
  environment = c("#ECEFF4", "#97A6BA", "#4A5A70")        # slate
)

# Default gradient if a variable is not listed above
default_gradient <- c("#E5ECF6", "#8FA9D6", "#345E9A")

# ======================================
# 6 Distribution plotting function
# ======================================

plot_distribution <- function(variable) {
  
  if (variable %in% split_plus_vars) {
    
    plot_data <- data %>%
      select(all_of(variable)) %>%
      filter(!is.na(.data[[variable]]), .data[[variable]] != "") %>%
      separate_rows(all_of(variable), sep = "\\s*\\+\\s*") %>%
      mutate(
        !!variable := str_trim(.data[[variable]])
      ) %>%
      count(.data[[variable]], name = "n") %>%
      mutate(
        label = stringr::str_wrap(as.character(.data[[variable]]), width = 12)
      )
    
  } else {
    
    plot_data <- data %>%
      filter(!is.na(.data[[variable]]), .data[[variable]] != "") %>%
      count(.data[[variable]], name = "n") %>%
      mutate(
        label = stringr::str_wrap(as.character(.data[[variable]]), width = 12)
      )
  }
  
  gradient_cols <- plot_gradients[[variable]]
  if (is.null(gradient_cols)) {
    gradient_cols <- default_gradient
  }
  
  p <- ggplot(
    plot_data,
    aes(
      x = reorder(label, n),
      y = n,
      fill = n
    )
  ) +
    geom_bar(stat = "identity", color = NA) +
    scale_fill_gradientn(colours = gradient_cols) +
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
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
      
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
        color = "grey55",
        fill = NA,
        linewidth = 0.7
      ),
      
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
    )
  
  return(p)
}

# ======================================
# 7 Generate plots for all variables
# ======================================

plots <- lapply(vars, plot_distribution)

# ======================================
# 8 Save plots automatically
# ======================================

for (v in vars) {
  
  p <- plot_distribution(v)
  
  ggsave(
    filename = paste0("figures/dist_", v, ".pdf"),
    plot = p,
    width = 6,
    height = 4
  )
}

# ======================================
# 9 Optional: show plots in R
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

# ======================================
# 10 Save selected PNG files
# ======================================

ggsave(
  "figures/PlatformCount.png",
  plots[[2]],
  width = 7,
  height = 5,
  dpi = 300
)
ggsave(
  "figures/PlatformCount.pdf",
  plots[[2]],
  width = 7,
  height = 5,
  dpi = 600
)
#-----------------
ggsave(
  "figures/DisplayCount.png",
  plots[[7]],
  width = 7,
  height = 5,
  dpi = 300
)
ggsave(
  "figures/DisplayCount.pdf",
  plots[[7]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------
ggsave(
  "figures/ScenarioCount.png",
  plots[[10]],
  width = 7,
  height = 5,
  dpi = 300
)
ggsave(
  "figures/ScenarioCount.pdf",
  plots[[10]],
  width = 7,
  height = 5,
  dpi = 600
)

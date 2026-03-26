# ============================================================
# Faceted Count Matrix / Heatmap
# Platform × (Driver Presence & Visibility × Motion DOF)
# Aggregated across ALL cabin structures
# ============================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(grid)
library(janitor)

# ============================================================
# 1 Load dataset
# ============================================================

data <- read_csv("Data/TableLiterature.csv", show_col_types = FALSE)
data <- clean_names(data)

# ============================================================
# 2 Helpers
# ============================================================

norm_str <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\r\n]+", " ")
  x <- str_squish(x)
  na_if(x, "")
}

wrap_value <- function(x, width = 16) {
  str_wrap(as.character(x), width = width, whitespace_only = FALSE)
}

get_text_color_from_fill <- function(fill_hex) {
  rgb <- grDevices::col2rgb(fill_hex) / 255
  
  channel_luminance <- function(c) {
    ifelse(c <= 0.03928, c / 12.92, ((c + 0.055) / 1.055)^2.4)
  }
  
  r <- channel_luminance(rgb[1])
  g <- channel_luminance(rgb[2])
  b <- channel_luminance(rgb[3])
  
  luminance <- 0.2126 * r + 0.7152 * g + 0.0722 * b
  
  if (luminance > 0.5) "black" else "white"
}

# ============================================================
# 3 Clean
#    Cabin Structure is NOT filtered and NOT grouped
#    so counts are aggregated across all cabin structures
# ============================================================

df <- data %>%
  mutate(across(where(is.character), norm_str)) %>%
  distinct(study, .keep_all = TRUE) %>%
  mutate(
    motion_dof_num = suppressWarnings(as.integer(str_extract(motion_dof, "\\d+"))),
    platform_clean = norm_str(platform),
    driver_presence_clean = norm_str(driver_presence_visibility)
  ) %>%
  filter(
    !is.na(platform_clean),
    !is.na(motion_dof_num),
    !is.na(driver_presence_clean)
  )

# ============================================================
# 4 Count combinations
#    Counts are summed across all cabin structures
# ============================================================

dof_levels <- c(0, 1, 4, 6)

driver_presence_levels <- c(
  "Present-Visible",
  "Participant Driving",
  "Present-Concealed",
  "Not Present"
)

driver_presence_label_levels <- rev(wrap_value(driver_presence_levels, width = 18))

counts <- df %>%
  filter(motion_dof_num %in% dof_levels) %>%
  count(platform_clean, driver_presence_clean, motion_dof_num, name = "n") %>%
  mutate(
    platform_clean = fct_infreq(platform_clean),
    driver_presence_clean = factor(
      driver_presence_clean,
      levels = driver_presence_levels
    ),
    motion_dof_num = factor(motion_dof_num, levels = dof_levels)
  ) %>%
  complete(
    platform_clean,
    driver_presence_clean,
    motion_dof_num,
    fill = list(n = 0)
  ) %>%
  mutate(
    driver_presence_label = factor(
      wrap_value(driver_presence_clean, width = 18),
      levels = driver_presence_label_levels
    )
  )

max_n <- max(counts$n, na.rm = TRUE)

# Use the same muted family, but not the blue used for platform
heatmap_cols <- c("#E6F4F1", "#7FC8BE", "#2F7F77")


fill_lookup <- grDevices::colorRampPalette(heatmap_cols)(max(max_n, 1) + 1)

counts <- counts %>%
  mutate(
    fill_hex = fill_lookup[n + 1],
    text_color = vapply(fill_hex, get_text_color_from_fill, character(1))
  )

# ============================================================
# 5 Plot
# ============================================================

p <- ggplot(
  counts,
  aes(
    x = motion_dof_num,
    y = driver_presence_label,
    fill = n
  )
) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(
      label = n,
      color = I(text_color)
    ),
    size = 5,
    fontface = "bold"
  ) +
  facet_wrap(~ platform_clean, nrow = 1) +
  scale_fill_gradientn(
    colours = heatmap_cols,
    limits = c(0, max_n),
    breaks = 0:max_n
  ) +
  labs(
    title = NULL,
    x = "Motion DOF",
    y = "Driver Presence & Visibility",
    fill = "Number of Studies"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    
    strip.text = element_text(
      color = "black",
      face = "bold",
      size = 14
    ),
    strip.background = element_rect(
      fill = "#ECEFF4",
      color = "grey40",
      linewidth = 0.8
    ),
    
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
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.justification = "center",
    legend.title.position = "left",
    
    legend.title = element_text(
      color = "black",
      face = "bold",
      size = 14,
      vjust = 0.7,
      margin = margin(r = 12, b = 8)
    ),
    
    legend.text = element_text(
      color = "black",
      size = 12
    ),
    
    panel.border = element_rect(
      color = "grey40",
      fill = NA,
      linewidth = 0.8
    ),
    
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "left",
      title.hjust = 0,
      title.vjust = 0.7,
      direction = "horizontal",
      barwidth = unit(4, "cm"),
      barheight = unit(0.45, "cm")
    )
  )

p

# ============================================================
# 6 Save
# ============================================================

ggsave(
  "figures/platform_driver_presence_motion_dof_count_matrix.png",
  p,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/platform_driver_presence_motion_dof_count_matrix.pdf",
  p,
  width = 8,
  height = 5,
  dpi = 300
)


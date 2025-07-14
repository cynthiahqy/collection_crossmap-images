source(here::here("src/diagram_no-coverage.R"))
source(here::here("src/diagram_missing-val-bigraph.R"))

library(patchwork)
no_cover <- wrap_plots(gg_pm_mtx +
  # guides(fill = "none") +
  theme(legend.position = "none", panel.grid = element_blank()) +
  ggtitle("")) +
  wrap_table(gt_x_bad, panel = "full", space = "free_x")

save_height <- 3.5
save_width <- 6
save_path <- file.path("src","output",
  paste0("diagram_no-coverage_", save_width, "x", save_height, ".png"))
ggsave(save_path, plot = no_cover, width = 6, height = 3.5)

# wrap_table(gt_missing, space = "fixed_y", panel = "full") +
bigraph <- gg_pm_BA + guides(fill = "none") + ggtitle("")
ggsave("src/output/diagram_missing-val-bigraph-only.png", plot = bigraph, width = 5, height = 3.5)

input_array <- wrap_table(gt_missing, panel = "full")
ggsave("src/output/diagram_missing-val-input-array.png", plot = input_array, width = 1.5, height = 3.5)

no_cover_missing <- (gg_pm_mtx) + wrap_table(gt_x_bad2, panel = "body", space = "free_x")
  
save_height <- 3.5
save_width <- 6
save_path <- file.path("src","output",
  paste0("diagram_no-coverage_missing", save_width, "x", save_height, ".png"))
ggsave(save_path, plot = no_cover_missing, width = 6, height = 3.5)


#gg_pm_mtx

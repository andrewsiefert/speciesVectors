library(tidyverse)


# GloVe results -------------------------------------------------------------------

# Read in model performance results
globalb <- read_csv("results/glove_global_test_results_w_bias.csv")
xmax <- read_csv("results/glove_xmax_test_results.csv")

# Combine results into a single dataframe
all <- bind_rows(list(globalb, xmax)) %>%
  as_tibble() %>%
  filter(test == "emb", dim %in% c(16, 32)) %>%
  mutate(rmse_lo = rmse-rmse_se,
         rmse_hi = rmse+rmse_se,
         r2_lo = r2-r2_se,
         r2_hi = r2+r2_se) |>
  arrange(dim)

# Get y-axis limits
rmse_lim <- range(c(all$rmse_lo, all$rmse_hi), na.rm = T)
r2_lim <- range(c(all$r2_lo, all$r2_hi), na.rm = T)

# Get colors
cols <- c("#E69F00", "#56B4E9")

# Plot GloVe RMSE

rmse <- all %>%
  ggplot(aes(x = xmax, y = rmse, color = factor(dim)), group = dim) +
  geom_line(position = position_dodge(width = 0.1)) +
  geom_pointrange(aes(ymin = rmse_lo, ymax = rmse_hi), size = 0.4, linewidth = 0.4, position = position_dodge(width = 0.1)) +
  scale_x_log10() +
  scale_y_continuous(limits = rmse_lim) +
  labs(x = bquote(italic(x[max])), y = "RMSE", color = "Vector dimension") +
  theme_bw() +
  theme(panel.grid = element_blank(), aspect.ratio = 0.8) +
  scale_color_manual(values = cols)


# Plot GloVe R2
r2 <- all %>%
  ggplot(aes(x = xmax, y = r2, color = factor(dim)), group = dim) +
  geom_line(position = position_dodge(width = 0.1)) +
  geom_pointrange(aes(ymin = r2_lo, ymax = r2_hi), size = 0.4, linewidth = 0.4, position = position_dodge(width = 0.1)) +
  scale_x_log10() +
  scale_y_continuous(limits = r2_lim) +
  labs(x = bquote(italic(x[max])), y = bquote(italic(R)^2), color = "Vector dimension") +
  theme_bw() +
  theme(panel.grid = element_blank(), aspect.ratio = 0.8) +
  scale_color_manual(values = cols)


# Combine and save plots
ggpubr::ggarrange(rmse, r2, nrow = 2, common.legend = T, legend = "right")
ggsave("results/figures/xmax_performance.jpg", height = 6, width = 5, units = "in")

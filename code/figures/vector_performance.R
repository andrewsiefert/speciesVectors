library(tidyverse)


# GloVe results -------------------------------------------------------------------

# Read in model performance results
global <- read_csv("results/glove_global_test_results.csv") %>% mutate(data = "Global", bias = "No")           # global, no bias terms
globalb <- read_csv("results/glove_global_test_results_w_bias.csv") %>% mutate(data = "Global", bias = "Yes")  # global, with bias terms
local <- read_csv("results/glove_local_test_results.csv") %>% mutate(data = "Local", bias = "No")              # local, no bias terms
localb <- read_csv("results/glove_local_test_results_w_bias.csv") %>% mutate(data = "Local", bias = "Yes")     # local, with bias terms

# Combine results into a single dataframe
all <- bind_rows(list(global, globalb, local, localb)) %>%
  as_tibble() %>%
  mutate(rmse_lo = rmse-rmse_se,
         rmse_hi = rmse+rmse_se,
         r2_lo = r2-r2_se,
         r2_hi = r2+r2_se,
         bias = fct_rev(bias))

# Get performance of trait-only and null models
trait_perf <- global %>% filter(model == "trait")
null_perf <- global %>% filter(model == "null")

# Get y-axis limits
rmse_lim <- range(c(all$rmse_lo, all$rmse_hi), na.rm = T)
r2_lim <- range(c(all$r2_lo, all$r2_hi), na.rm = T)


# Plot GloVe RMSE
ann_text <- tibble(dim = 1,
                   data = "Global",
                   test = "Species vectors",
                   rmse = c(trait_perf$rmse+0.2, null_perf$rmse+0.2),
                   label = c("Traits only", "Intercept only"))

all %>%
  filter(test %in% c("emb", "emb_trait")) %>%
  mutate(test = factor(test, labels = c("Species vectors + traits", "Species vectors")) %>% fct_relevel("Species vectors")) %>%
  ggplot(aes(x = dim, y = rmse, color = factor(xmax))) +
  geom_path(aes(linetype = bias)) +
  geom_pointrange(aes(ymin = rmse_lo, ymax = rmse_hi, alpha = factor(bias)), size = 0.4, linewidth = 0.4) +
  facet_grid(data~test) +
  geom_hline(yintercept = null_perf$rmse, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = trait_perf$rmse, linetype = "dashed", color = "gray40") +
  geom_text(data = ann_text, aes(label = label), size = 3.5, color = "gray40", hjust = 0, vjust = 0) +
  scale_x_continuous(limits = c(0, 68), expand = c(0,0)) +
  scale_y_continuous(limits = rmse_lim) +
  scale_alpha_discrete(range = c(1, 0.5)) +
  labs(x = "Vector dimension", y = "RMSE", color = bquote(italic(x[max])), linetype = "Bias terms", alpha = "Bias terms") +
  theme_bw() +
  theme(panel.grid = element_blank(), aspect.ratio = 0.8) +
  viridis::scale_color_viridis(discrete = T, option = "magma", begin = 0.2, end = 0.8)

ggsave("results/figures/glove_rmse.jpg", height = 5.5, width = 8, units = "in")


# Plot GloVe R2
ann_text <- tibble(dim = 1,
                   data = "Global",
                   test = "Species vectors",
                   r2 = trait_perf$r2-0.002,
                   label = "Traits only")

all %>%
  filter(test %in% c("emb", "emb_trait")) %>%
  mutate(test = factor(test, labels = c("Species vectors + traits", "Species vectors")) %>% fct_relevel("Species vectors")) %>%
  ggplot(aes(x = dim, y = r2, color = factor(xmax))) +
  geom_path(aes(linetype = bias)) +
  geom_pointrange(aes(ymin = r2_lo, ymax = r2_hi, alpha = factor(bias)), size = 0.4, linewidth = 0.4) +
  facet_grid(data~test) +
  geom_hline(yintercept = null_perf$r2, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = trait_perf$r2, linetype = "dashed", color = "gray40") +
  geom_text(data = ann_text, aes(label = label), size = 3.5, color = "gray40", hjust = 0, vjust = 1, nudge_x = 5) +
  scale_x_continuous(limits = c(0, 68), expand = c(0,0)) +
  scale_y_continuous(limits = r2_lim) +
  scale_alpha_discrete(range = c(1, 0.5)) +
  labs(x = "Vector dimension", y = bquote(italic(R)^2), color = bquote(italic(x[max])), linetype = "Bias terms", alpha = "Bias terms") +
  theme_bw() +
  theme(panel.grid = element_blank(), aspect.ratio = 0.8) +
  viridis::scale_color_viridis(discrete = T, option = "magma", begin = 0.2, end = 0.8)

ggsave("results/figures/glove_r2.jpg", height = 5.5, width = 8, units = "in")



# PCA results -------------------------------------------------------------

# Read in test results
p <- read_csv("results/pca_test_results.csv") %>%
  mutate(model = "tb-PCA")

# Select results from best GloVe model
g <- all %>%
  filter(test == "emb", data == "Global", bias == "Yes", xmax == 100) %>%
  mutate(model = "GloVe")

# Combine PCA and best GloVe results
pg <- bind_rows(p, g) %>%
  mutate(rmse_lo = rmse-rmse_se,
         rmse_hi = rmse+rmse_se,
         r2_lo = r2-r2_se,
         r2_hi = r2+r2_se)


## Plot PCA RMSE ----
ann_text <- tibble(dim = 1,
                   rmse = trait_perf$rmse+0.2,
                   label = "Traits only model")

pos <- position_dodge(width = 1)

ggplot(pg, aes(x = dim, y = rmse)) +
  geom_pointrange(aes(ymin = rmse_lo, ymax = rmse_hi, color = model), position = pos) +
  geom_path(aes(color = model, linetype = model), position = pos) +
  geom_hline(yintercept = trait_perf$rmse, linetype = "dotted", color = "gray40") +
  geom_text(data = ann_text, aes(label = label), size = 3.5, color = "gray40", hjust = 0, vjust = 0) +
  scale_x_continuous(limits = c(0, 68), expand = c(0,0)) +
  scale_y_continuous(limits = rmse_lim) +
  labs(x = "Vector dimension", y = "RMSE", color = "") +
  guides(linetype = "none") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 0.9,
        legend.position = c(0.12, 0.87)) +
  viridis::scale_color_viridis(discrete = T, begin = 0.2, end = 0.8)

ggsave("results/figures/pca_rmse.jpg", height = 4, width = 5, units = "in")


## Plot PCA R2 ----
ann_text <- tibble(dim = 1,
                   r2 = trait_perf$r2+0.002,
                   label = "Traits only model")

ggplot(pg, aes(x = dim, y = r2)) +
  geom_pointrange(aes(ymin = r2_lo, ymax = r2_hi, color = model), position = pos) +
  geom_path(aes(color = model, linetype = model), position = pos) +
  geom_hline(yintercept = trait_perf$r2, linetype = "dotted", color = "gray40") +
  geom_text(data = ann_text, aes(label = label), size = 3.5, color = "gray40", hjust = 0, vjust = 0) +
  scale_x_continuous(limits = c(0, 68), expand = c(0,0)) +
  scale_y_continuous(limits = r2_lim) +
  labs(x = "Vector dimension", y = bquote(italic(R)^2), color = "") +
  guides(linetype = "none") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 0.9,
        legend.position = c(0.12, 0.13)) +
  viridis::scale_color_viridis(discrete = T, begin = 0.2, end = 0.8)

ggsave("results/figures/pca_r2.jpg", height = 4, width = 5, units = "in")




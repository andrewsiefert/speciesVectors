library(tidyverse)

global <- read_csv("results/glove_global_test_results.csv") %>% mutate(data = "Global", bias = "No")
globalb <- read_csv("results/glove_global_test_results_w_bias.csv") %>% mutate(data = "Global", bias = "Yes")
local <- read_csv("results/glove_local_test_results.csv") %>% mutate(data = "Local", bias = "No")
localb <- read_csv("results/glove_local_test_results_w_bias.csv") %>% mutate(data = "Local", bias = "Yes")

all <- bind_rows(list(global, globalb, local, localb)) %>%
  as_tibble()

trait_perf <- global %>% filter(model == "trait")
null_perf <- global %>% filter(model == "null")

rmse_lim <- range(all$rmse)
r2_lim <- range(all$r2, na.rm = T)

all %>%
  filter(test %in% c("emb", "emb_trait")) %>%
  mutate(test = factor(test, labels = c("Species vectors + traits", "Species vectors")) %>% fct_relevel("Species vectors")) %>%
  ggplot(aes(x = dim, y = rmse, linetype = factor(bias))) +
  geom_point(aes(color = factor(xmax))) +
  geom_path(aes(color = factor(xmax))) +
  facet_grid(data~test) +
  geom_hline(yintercept = null_perf$rmse, linetype = "dotted", color = "gray30") +
  geom_hline(yintercept = trait_perf$rmse, linetype = "dotted", color = "gray30") +
  scale_x_continuous(limits = c(0, 65)) +
  scale_y_continuous(limits = rmse_lim) +
  labs(x = "Vector dimension", y = "RMSE", color = bquote(italic(x[max])), linetype = "Bias terms") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  viridis::scale_color_viridis(discrete = T, option = "magma", begin = 0.2, end = 0.8)

ggsave("results/figures/glove_rmse.svg", height = 4, width = 6, units = "in")


all %>%
  filter(test %in% c("emb", "emb_trait")) %>%
  mutate(test = factor(test, labels = c("Species vectors + traits", "Species vectors")) %>% fct_relevel("Species vectors")) %>%
  ggplot(aes(x = dim, y = r2, linetype = factor(bias))) +
  geom_point(aes(color = factor(xmax))) +
  geom_path(aes(color = factor(xmax))) +
  facet_grid(data~test) +
  geom_hline(yintercept = trait_perf$r2, linetype = "dotted", color = "gray30") +
  scale_x_continuous(limits = c(0, 65)) +
  scale_y_continuous(limits = r2_lim) +
  labs(x = "Vector dimension", y = bquote(R^2), color = bquote(italic(x[max])), linetype = "Bias terms") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  viridis::scale_color_viridis(discrete = T, option = "magma", begin = 0.2, end = 0.8)


ggsave("results/figures/glove_r2.svg", height = 4, width = 6, units = "in")



# PCA results -------------------------------------------------------------

p <- read_csv("results/pca_test_results.csv") %>%
  mutate(model = "tb-PCA")

g <- all %>%
  filter(test == "emb", data == "Global", bias == "Yes", xmax == 100) %>%
  mutate(model = "GloVe")

pg <- bind_rows(p, g)


## RMSE ----
ggplot(pg, aes(x = dim, y = rmse)) +
  geom_point(aes(color = model)) +
  geom_path(aes(color = model, linetype = model)) +
  geom_hline(yintercept = null_perf$rmse, linetype = "dotted", color = "gray30") +
  geom_hline(yintercept = trait_perf$rmse, linetype = "dotted", color = "gray30") +
  scale_x_continuous(limits = c(0, 65)) +
  scale_y_continuous(limits = rmse_lim) +
  labs(x = "Vector dimension", y = "RMSE", color = "") +
  guides(linetype = "none") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  viridis::scale_color_viridis(discrete = T, begin = 0.2, end = 0.8)

ggsave("results/figures/pca_rmse.svg", height = 3, width = 4.5, units = "in")


## R2 ----
ggplot(pg, aes(x = dim, y = r2)) +
  geom_point(aes(color = model)) +
  geom_path(aes(color = model, linetype = model)) +
  geom_hline(yintercept = trait_perf$r2, linetype = "dotted", color = "gray30") +
  scale_x_continuous(limits = c(0, 65)) +
  scale_y_continuous(limits = r2_lim) +
  labs(x = "Vector dimension", y = bquote(R^2), color = "") +
  guides(linetype = "none") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  viridis::scale_color_viridis(discrete = T, begin = 0.2, end = 0.8)

ggsave("results/figures/pca_r2.svg", height = 3, width = 4.5, units = "in")




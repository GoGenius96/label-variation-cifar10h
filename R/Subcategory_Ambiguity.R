library(readr) # for reading csv files
library(tidyverse) # for data manipulation and visualization
library(RColorBrewer)
library(ggpubr)

theme_set(theme_bw() + theme(axis.text.x = element_text(size = 18),
                             axis.text.y = element_text(size = 18),
                             axis.title = element_text(size = 20),
                             title = element_text(size = 20)))

cifar10h <- read_csv("data/cifar10h.csv")
cifar10h_original <- read_csv("data/cifar10h-raw.csv")
Y_cifar10h <- read_csv("data/Y_cifar10h.csv")
pi <- read_csv("data/pi.csv")
tau <- read_csv("data/tau.csv")
theta <- read_csv("data/theta.csv")
theta_old <- read_csv("data/theta_old.csv")
variance_theta <- read_csv("data/variance_theta.csv")
post_entropy <- read_csv("data/post_entropy.csv")

subcategory_rt <- cifar10h %>%
  filter(reaction_time < 26) %>%
  group_by(subcategory) %>%
  summarise(n = n(),
            avg_rt = mean(reaction_time, na.rm = TRUE),
            median_rt = median(reaction_time, na.rm = TRUE),
            quantile_05 = quantile(reaction_time, 0.05, na.rm = TRUE),
            quantile_95 = quantile(reaction_time, 0.95, na.rm = TRUE)) %>%
  arrange(avg_rt)

subcategory_rt %>%
  ggplot(aes(x = 1:nrow(subcategory_rt), y = avg_rt)) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0.9) + 
  geom_errorbar(aes(ymin = quantile_05, ymax = quantile_95), width = 0.2) +
  labs(
    x = "Subcategories",
    y = "Average Reaction Time (s)"
  ) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20)) +
  scale_y_continuous(limits = c(0, 10), breaks = c(seq(0,10,2), 0.9)) +
  theme(legend.position = "none")

ggsave("figs/subcategory_rt.png", width = 10, height = 6, dpi = 400)

# Create subset S
subcategory_sample <- subcategory_rt %>%
  filter(avg_rt <= quantile(avg_rt, 0.8)) %>%
  pull(subcategory)

S_cifar10h <- cifar10h %>%
  filter(subcategory %in% subcategory_sample)

# Proportion of distinct images
n_distinct(S_cifar10h$image_filename)/10000

# export S_cifar10h
write_csv(S_cifar10h, "data/cifar10h_S.csv")


S_pi <- read_csv("data/pi_S.csv")
S_tau <- read_csv("data/tau_S.csv")
S_theta <- read_csv("data/theta_S.csv")
S_theta_old <- read_csv("data/theta_old_S.csv")
S_variance_theta <- read_csv("data/variance_theta_S.csv")
S_post_entropy <- read_csv("data/post_entropy_S.csv")



# compare confusion matrices
colnames(theta) <- colnames(S_theta) <- Y_cifar10h %>% dplyr::select(-image_filename, -J) %>% names()
rownames(theta) <- rownames(S_theta) <- colnames(theta)

theta_matrix <- function(theta) {
  prop.table(as.table(as.matrix(theta)), margin = 1) %>%
    as.data.frame() %>%
    ggplot(aes(x = Var2, y = Var1, fill = sqrt(Freq))) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "orange") +
    scale_y_discrete(limits = rev) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # add the value of the proportion on the tile
    geom_text(aes(label = scales::percent(Freq, accuracy = 0.01)), size = 6)
}

estimated_S_theta_plot <- theta_matrix(S_theta) +
  labs(title = "Estimated Confusion Matrix on S",
       y = "True Category") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())


estimated_theta_plot <- theta_matrix(theta) +
  labs(title = "Estimated Confusion Matrix") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

relative_diff_plot <- (prop.table(as.table(as.matrix(S_theta)),margin = 1) / prop.table(as.table(as.matrix(theta)), margin = 1)) %>%
  as.data.frame() %>%
  mutate(Freq_color = log(Freq)) %>%
  ggplot(aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", low = "blue", mid = "white", midpoint = 1.0) +
  labs(title = "Relative Difference") +
  scale_y_discrete(limits = rev) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # add the value of the proportion on the tile
  geom_text(aes(label = round(Freq, 3)), size = 6) +
  labs(y = "True Category",
       x = "Chosen Category") +
  theme(legend.position = "none",
        axis.ticks = element_blank())

absolute_diff_plot <- (prop.table(as.table(as.matrix(S_theta)), margin = 1) - prop.table(as.table(as.matrix(theta)), margin = 1)) %>%
  as.data.frame() %>%
  mutate(Freq_color = sign(Freq) * abs(Freq)) %>%
  ggplot(aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0.00) +
  labs(title = "Absolute Difference",
       x = "Chosen Category") + 
  scale_y_discrete(limits = rev) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # add the value of the proportion on the tile
  geom_text(aes(label = round(Freq, 3)), size = 6) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())


complete_plot <- ggarrange(estimated_S_theta_plot, estimated_theta_plot, relative_diff_plot, absolute_diff_plot, ncol = 2, nrow = 2)

ggsave("figs/confusion_matrices_comparison_subcategories.png", complete_plot, width = 20, height = 20, dpi = 600)

## entropy based
subcategory_entropy <- cifar10h %>%
  filter(reaction_time < 26) %>%
  group_by(subcategory) %>%
  summarise(n = n(),
            avg_entropy = mean(entropy, na.rm = TRUE),
            median_entropy = median(entropy, na.rm = TRUE),
            quantile_05 = quantile(entropy, 0.05, na.rm = TRUE),
            quantile_95 = quantile(entropy, 0.95, na.rm = TRUE)) %>%
  arrange(avg_entropy)

subcategory_entropy %>%
  ggplot(aes(x = 1:nrow(subcategory_entropy), y = avg_entropy)) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = quantile_05, ymax = quantile_95), width = 0.2) +
  labs(
    x = "Subcategories",
    y = "Average Prior Entropy"
  ) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 31),
        axis.title = element_text(size = 31)) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0,2,0.2)) +
  theme(legend.position = "none")

ggsave("figs/subcategory_entropy.png", width = 10, height = 6, dpi = 400)

# Create subset S
subcategory_entropy_sample <- subcategory_entropy %>%
  filter(avg_entropy <= quantile(avg_entropy, 0.8)) %>%
  pull(subcategory)

S_entropy_cifar10h <- cifar10h %>%
  filter(subcategory %in% subcategory_entropy_sample)

# Proportion of distinct images
n_distinct(S_entropy_cifar10h$image_filename)/10000

# export S_cifar10h
write_csv(S_cifar10h, "data/cifar10h_Se.csv")

# compute the number of subcategories in S and S_entropy
a <- S_cifar10h$subcategory %>% unique()
b <- S_entropy_cifar10h$subcategory %>% unique()
length(intersect(a,b))/length(a)


S_pi_entropy <- read_csv("data/pi_Se.csv")
S_tau_entropy <- read_csv("data/tau_Se.csv")
S_theta_entropy <- read_csv("data/theta_Se.csv")
S_theta_old_entropy <- read_csv("data/theta_old_Se.csv")
S_variance_theta_entropy <- read_csv("data/variance_theta_Se.csv")
S_post_entropy_entropy <- read_csv("data/post_entropy_Se.csv")



# compare confusion matrices
colnames(theta) <- colnames(S_theta_entropy) <- Y_cifar10h %>% dplyr::select(-image_filename, -J) %>% names()
rownames(theta) <- rownames(S_theta_entropy) <- colnames(theta)

theta_matrix <- function(theta) {
  prop.table(as.table(as.matrix(theta)), margin = 1) %>%
    as.data.frame() %>%
    ggplot(aes(x = Var2, y = Var1, fill = sqrt(Freq))) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "orange") +
    scale_y_discrete(limits = rev) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # add the value of the proportion on the tile
    geom_text(aes(label = scales::percent(Freq, accuracy = 0.01)), size = 6)
}

estimated_S_theta_plot <- theta_matrix(S_theta_entropy) +
  labs(title = "Estimated Confusion Matrix on S_entropy",
       y = "True Category") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())


estimated_theta_plot <- theta_matrix(theta) +
  labs(title = "Estimated Confusion Matrix") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

relative_diff_plot <- (prop.table(as.table(as.matrix(S_theta_entropy)),margin = 1) / prop.table(as.table(as.matrix(theta)), margin = 1)) %>%
  as.data.frame() %>%
  mutate(Freq_color = log(Freq)) %>%
  ggplot(aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", low = "blue", mid = "white", midpoint = 1.0) +
  labs(title = "Relative Difference") +
  scale_y_discrete(limits = rev) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # add the value of the proportion on the tile
  geom_text(aes(label = round(Freq, 3)), size = 6) +
  labs(y = "True Category",
       x = "Chosen Category") +
  theme(legend.position = "none",
        axis.ticks = element_blank())

absolute_diff_plot <- (prop.table(as.table(as.matrix(S_theta_entropy)), margin = 1) - prop.table(as.table(as.matrix(theta)), margin = 1)) %>%
  as.data.frame() %>%
  mutate(Freq_color = sign(Freq) * abs(Freq)) %>%
  ggplot(aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0.00) +
  labs(title = "Absolute Difference",
       x = "Chosen Category") + 
  scale_y_discrete(limits = rev) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # add the value of the proportion on the tile
  geom_text(aes(label = round(Freq, 3)), size = 6) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())


complete_plot <- ggarrange(estimated_S_theta_plot, estimated_theta_plot, relative_diff_plot, absolute_diff_plot, ncol = 2, nrow = 2)

ggsave("figs/confusion_matrices_comparison_subcategories_entropy.png", complete_plot, width = 20, height = 20, dpi = 600)

# compute the difference between the estimated confusion matrix and the true confusion matrix
(prop.table(as.table(as.matrix(S_theta_entropy)),margin = 1) - prop.table(as.table(as.matrix(theta)), margin = 1)) - (prop.table(as.table(as.matrix(S_theta)),margin = 1) - prop.table(as.table(as.matrix(theta)), margin = 1))
(prop.table(as.table(as.matrix(S_theta_entropy)),margin = 1) / prop.table(as.table(as.matrix(theta)), margin = 1)) - (prop.table(as.table(as.matrix(S_theta)),margin = 1) / prop.table(as.table(as.matrix(theta)), margin = 1))

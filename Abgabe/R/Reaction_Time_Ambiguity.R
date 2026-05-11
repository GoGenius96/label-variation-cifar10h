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


pi_R <- read_csv("data/pi_R.csv")
tau_R <- read_csv("data/tau_R.csv")
theta_R <- read_csv("data/theta_R.csv")
theta_old_R <- read_csv("data/theta_old_R.csv")
variance_theta_R <- read_csv("data/variance_theta_R.csv")
post_entropy_R <- read_csv("data/post_entropy_R.csv")

pi_L <- read_csv("data/pi_L.csv")
tau_L <- read_csv("data/tau_L.csv")
theta_L <- read_csv("data/theta_L.csv")
theta_old_L <- read_csv("data/theta_old_L.csv")
variance_theta_L <- read_csv("data/variance_theta_L.csv")
post_entropy_L <- read_csv("data/post_entropy_L.csv")


# compare confusion matrices
colnames(theta_L) <- colnames(theta_R) <- Y_cifar10h %>% dplyr::select(-image_filename, -J) %>% names()
rownames(theta_L) <- rownames(theta_R) <- colnames(theta_L)

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

estimated_theta_L_plot <- theta_matrix(theta_L) +
  labs(title = "Estimated Confusion Matrix for fast RT",
       y = "True Category") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())


estimated_theta_R_plot <- theta_matrix(theta_R) +
  labs(title = "Estimated Confusion Matrix for slow RT") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

relative_diff_plot <- (prop.table(as.table(as.matrix(theta_L)),margin = 1) / prop.table(as.table(as.matrix(theta_R)), margin = 1)) %>%
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

absolute_diff_plot <- (prop.table(as.table(as.matrix(theta_L)), margin = 1) - prop.table(as.table(as.matrix(theta_R)), margin = 1)) %>%
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


complete_plot <- ggarrange(estimated_theta_L_plot, estimated_theta_R_plot, relative_diff_plot, absolute_diff_plot, ncol = 2, nrow = 2)

ggsave("figs/confusion_matrices_RT_comparison.png", complete_plot, width = 20, height = 20, dpi = 600)

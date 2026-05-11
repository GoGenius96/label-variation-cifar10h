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

true_theta <- prop.table(table(cifar10h$true_category, cifar10h$chosen_category), margin = 1)

theta_diff <- theta - true_theta
delta <- as.vector(t(theta_diff[, -ncol(theta_diff)]))

T_statistic = t(delta) %*% solve(variance_theta) %*% delta

p_value <- pchisq(T_statistic, df = length(delta), lower.tail = FALSE)

# add image_filename to the post_entropy table
post_entropy <- data.frame(image_filename = Y_cifar10h$image_filename, entropy = post_entropy$`0`)

entropy_vote_distribution <- cifar10h_original %>%
  left_join(post_entropy, by = "image_filename") %>%
  dplyr::select(image_filename, true_category, entropy) %>%
  left_join(Y_cifar10h, by = "image_filename") %>%
  distinct() %>%
  arrange(desc(entropy))

# find the index of the images with the 10 highest entropies
top10_entropy <- post_entropy %>%
  arrange(desc(entropy)) %>%
  head(10) %>%
  pull(image_filename)

top10_entropy_votes <- cifar10h_original %>%
  filter(image_filename %in% top10_entropy) %>%
  dplyr::select(image_filename, true_category, index = cifar10_test_test_idx) %>%
  distinct()

top10_entropy_votes %>%
  write_csv(file = "data/top10_entropy.csv")

for(i in seq_along(top10_entropy_votes$image_filename)) {
  entropy_vote_distribution %>%
    filter(image_filename %in% top10_entropy_votes$image_filename[i]) %>%
    dplyr::select(-true_category, -image_filename, -entropy, -J) %>%
    pivot_longer(cols = everything(), names_to = "chosen_category") %>%
    ggplot(aes(x = chosen_category, y = value)) +
    geom_col() +
    labs(
      x = "Chosen Category",
      y = "Frequency"
    ) + 
    scale_y_continuous(limits = c(0, 30)) +
    theme(axis.text.x = element_text(size = 30, angle = 60, hjust = 1),
          axis.text.y = element_text(size = 31),
          axis.title = element_text(size = 31))
  
    ggsave(paste0("figs/distribution_post_entropy_", i-1, ".png"), height = 10, width = 10)
}

# weird entropies
images <- c("tabby_cat_s_001838.png", "fallow_deer_s_001050.png")

special_entropy_votes <- cifar10h_original %>%
  filter(image_filename %in% images) %>%
  select(image_filename, true_category, index = cifar10_test_test_idx) %>%
  distinct()

special_entropy_votes %>%
  write_csv(file = "data/special_entropy.csv")

for(i in seq_along(images)) {
  print(entropy_vote_distribution %>%
    filter(image_filename == images[i]) %>%
    select(-true_category, -image_filename, -J, -entropy) %>%
    pivot_longer(cols = everything(), names_to = "chosen_category") %>%
    ggplot(aes(x = chosen_category, y = value)) +
    geom_col() +
    labs(
      x = "Chosen Category",
      y = "Frequency"
    ) + 
    scale_y_continuous(limits = c(0, 30)) +
    theme(axis.text.x = element_text(size = 30, angle = 60, hjust = 1),
          axis.text.y = element_text(size = 31),
          axis.title = element_text(size = 31)))
  
  ggsave(paste0("figs/distribution_post_entropy_special", i-1, ".png"), height = 10, width = 10)
}





# compare confusion matrices
colnames(theta) <- Y_cifar10h %>% dplyr::select(-image_filename, -J) %>% names()
rownames(theta) <- colnames(theta)

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

true_theta_plot <- theta_matrix(true_theta) +
  labs(title = "True Confusion Matrix",
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

relative_diff_plot <- (true_theta / prop.table(as.table(as.matrix(theta)), margin = 1)) %>%
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

absolute_diff_plot <- (true_theta - prop.table(as.table(as.matrix(theta)), margin = 1)) %>%
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


complete_plot <- ggarrange(true_theta_plot, estimated_theta_plot, relative_diff_plot, absolute_diff_plot, ncol = 2, nrow = 2)

ggsave("figs/confusion_matrices_comparison.png", complete_plot, width = 20, height = 20, dpi = 600)

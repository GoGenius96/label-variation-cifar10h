library(readr) # for reading csv files
library(tidyverse) # for data manipulation and visualization
library(lme4)
library(RColorBrewer)

theme_set(theme_bw() + theme(axis.text.x = element_text(size = 18),
                             axis.text.y = element_text(size = 18),
                             axis.title = element_text(size = 20)))

cifar10h <- read_csv("data/cifar10h.csv")
Y_cifar10h <- read_csv("data/Y_cifar10h.csv")

# Check number of unique images in each true category
cifar10h %>%
  group_by(true_category) %>%
  summarise(N_unique_images = length(unique(image_filename)))

# Check number of images per annotator per category
cifar10h %>%
  group_by(annotator_id, true_category) %>%
  summarise(n_trials = length(unique(trial_index))) %>%
  pull(n_trials) %>%
  unique()

# Check how often each image was annotated
cifar10h %>%
  group_by(image_filename) %>%
  summarise(n_annotations = n()) %>%
  ggplot(aes(x = n_annotations)) +
  geom_bar() +
  labs(
    x = "Number of Annotations",
    y = "Frequency"
  )

# Check the number of observations per chosen category
cifar10h %>%
  ggplot(aes(x = chosen_category)) +
  geom_bar() +
  geom_hline(yintercept = unique(table(cifar10h$true_category)), color = "blue") +
  scale_y_continuous(breaks = c(0, 20000, 40000, unique(table(cifar10h$true_category))),
                     labels = c(0, 20, 40, round(unique(table(cifar10h$true_category)), -3)/1000)) +
  labs(
    x = "Chosen Category",
    y = "Frequency (in thousands)"
  )

ggsave("figs/chosen_category.png")

# # Check the proportion of images with consensus per true category
# cifar10h %>%
#   group_by(true_category, image_filename) %>%
#   summarise(Unique_vote = length(unique(chosen_category)) == 1) %>%
#   summarise(Proportion = mean(Unique_vote)) %>%
#   ggplot(aes(x = true_category, y = Proportion)) +
#   geom_bar(stat = "identity") +
#   labs(
#     x = "True Category",
#     y = "Proportion of Consensus"
#   ) +
#   scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
#   theme(axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 14))
# 
# ggsave("figs/consensus_proportion.png")
  
# Check the distribution of the image consensus
cifar10h %>%
  group_by(true_category, image_filename) %>%
  summarise(Unique_vote = length(unique(chosen_category))) %>%
  mutate(Unique_vote = factor(Unique_vote, levels = 10:1)) %>%
  group_by(true_category, Unique_vote) %>%
  summarise(N = n()) %>%
  group_by(true_category) %>%
  mutate(Proportion = N / sum(N)) %>%
  ggplot(aes(x = true_category, y = Proportion, fill = Unique_vote)) +
  geom_col(position = "stack") +
  labs(
    x = "True Category",
    y = "Proportion",
    fill = "Number of\nunique chosen\nlabels"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  # scale_fill_brewer(palette = 2) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Oranges"))(10)) + 
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 60, hjust = 1))

ggsave("figs/consensus_distribution.png")

# Export distributions of 3 (un)ambiguous images
cifar10h %>%
  mutate(chosen_category = factor(chosen_category)) %>%
  filter(image_filename %in% c("dump_truck_s_000114.png")) %>%
  ggplot(aes(x = chosen_category)) +
  geom_bar() +
  labs(
    x = "Chosen Category",
    y = "Frequency"
  ) + 
  scale_y_continuous(limits = c(0, 55)) +
  theme(axis.text.x = element_text(size = 30, angle = 60, hjust = 1),
        axis.text.y = element_text(size = 31),
        axis.title = element_text(size = 31))

ggsave("figs/ambiguous_image1_dist.png", height = 10, width = 10)

cifar10h %>%
  mutate(chosen_category = factor(chosen_category)) %>%
  filter(image_filename %in% c("cassowary_s_000401.png")) %>%
  ggplot(aes(x = chosen_category)) +
  geom_bar() +
  labs(
    x = "Chosen Category",
    y = "Frequency"
  ) + 
  scale_y_continuous(limits = c(0, 55)) +
  theme(axis.text.x = element_text(size = 30, angle = 60, hjust = 1),
        #axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 31),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("figs/ambiguous_image2_dist.png", height = 10, width = 10)

cifar10h %>%
  mutate(chosen_category = factor(chosen_category)) %>%
  filter(image_filename %in% c("abandoned_ship_s_000635.png")) %>%
  group_by(chosen_category, .drop = FALSE) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = chosen_category, y = n)) +
  geom_col() +
  labs(
    x = "Chosen Category",
    y = "Frequency"
  ) + 
  scale_y_continuous(limits = c(0, 55)) +
  theme(axis.text.x = element_text(size = 30, angle = 60, hjust = 1),
        #axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 31),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("figs/unambiguous_image_dist.png", height = 10, width = 10)

# plot the entropies
ggplot(entropies, aes(x = entropy)) +
  geom_histogram(binwidth = 0.12) +
  labs(
    x = "Entropy",
    y = "Number of images"
  ) +
  scale_x_continuous(breaks = seq(0,3,0.3))

ggsave("figs/entropy_distribution.png")

# create confusion matrix
prop.table(table(cifar10h$true_category, cifar10h$chosen_category), margin = 1) %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Var1, fill = sqrt(Freq))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(
    y = "True Category",
    x = "Chosen Category",
    fill = "Proportion"
  ) + 
  scale_y_discrete(limits = rev) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # add the value of the proportion on the tile
  geom_text(aes(label = scales::percent(round(Freq, 3))), size = 5) +
  theme(legend.position = "none")

ggsave("figs/confusion_matrix.png")

# Check the number of unique subcategories for each category
cifar10h %>%
  group_by(true_category) %>%
  summarise(subcategory_unique = n_distinct(subcategory))


# Check the number of unique images per subcategory
cifar10h %>%
  group_by(subcategory) %>%
  summarise(n = n_distinct(image_filename)) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 2) +
  labs(
    x = "Number of unique images in a subcategory",
    y = "Number of subcategories"
  )

ggsave("figs/Subcategory_Distinct_Image_Dist.png")


## Reaction Time

rt_med <- median(cifar10h$reaction_time, na.rm = TRUE)
cifar10h %>%
  ggplot(aes(x = reaction_time)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, color = "black") +
  scale_x_continuous(limits = c(0,6), labels = scales::comma, breaks = c(seq(0, 6, 2), rt_med, 0.75)) +
  #stat_function(fun = dnorm, args = list(mean = mean(log(cifar10h$reaction_time), na.rm = TRUE), sd = 0.3), color = "red") +
  labs(
    x = "Reaction Time (in seconds)",
    y = "Density"
  )

ggsave("figs/Reaction_Time_Dist.png")

# compute the average reaction time and some statistics of it for each annotator and plot
cifar10h %>%
  group_by(annotator_id) %>%
  summarise(avg_rt = mean(reaction_time, na.rm = TRUE)) %>%
  summarise(exp_avg_rt = mean(avg_rt),
            sd_rt = sd(avg_rt, na.rm = TRUE),
            median_rt = median(avg_rt, na.rm = TRUE),
            quantile1_rt  = quantile(avg_rt, 0.01, na.rm = TRUE),
            quantile99_rt = quantile(avg_rt, 0.99, na.rm = TRUE),
            n = n())

# plot the average reaction time distribution
cifar10h %>%
  group_by(annotator_id) %>%
  summarise(avg_rt = mean(reaction_time, na.rm = TRUE)) %>%
  ggplot(aes(x = avg_rt, y = after_stat(density))) +
  geom_histogram(binwidth = 0.1, color = "black") +
  labs(
    x = "Average Reaction Time (in seconds)",
    y = "Density"
  ) +
  scale_x_continuous(limits = c(0, 5))

ggsave("figs/reaction_time_avg.png")

# time_elapsed
et_df <- cifar10h %>%
  group_by(annotator_id) %>%
  summarise(time_elapsed = max(time_elapsed, na.rm = TRUE),
            avg_rt = mean(reaction_time, na.rm = TRUE),
            avg_correct = mean(correct_guess, na.rm = TRUE))

# proportion of observations with time_elapsed < 1500
mean(et_df$time_elapsed > 1500)

et_df %>%
  filter(time_elapsed < 1500) %>%
  summarise(avg_et = mean(time_elapsed),
            sd_et = sd(time_elapsed, na.rm = TRUE),
            median_et = median(time_elapsed, na.rm = TRUE),
            quantile1_et  = quantile(time_elapsed, 0.01, na.rm = TRUE),
            quantile99_et = quantile(time_elapsed, 0.99, na.rm = TRUE),
            avg_cg = mean(avg_correct),
            sd_cg = sd(avg_correct, na.rm = TRUE),
            median_cg = median(avg_correct, na.rm = TRUE),
            quantile1_cg  = quantile(avg_correct, 0.01, na.rm = TRUE),
            quantile99_cg = quantile(avg_correct, 0.99, na.rm = TRUE))


# time_elapsed vs. avg_correct
et_df %>%
  ggplot(aes(x = time_elapsed/60, y = avg_correct)) +
  geom_point(alpha = 0.25) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Total Elapsed Time (in minutes)",
    y = "Proportion of\nCorrect Guesses"
  )

ggsave("figs/elapsed_time_vs_correct.png")

# Entropy vs. Reaction Time
cor_ent_rt <- function(cutoff) {
  cifar10h %>%
    filter(reaction_time < cutoff) %>%
    summarise(cor = cor(entropy, reaction_time, use = "pairwise.complete.obs"))
}

cor_ent_rt_df <- seq(6, 250, 1) %>%
  map_dfr(cor_ent_rt)

cor_ent_rt_df %>% pull(cor) %>% max()
which(abs(cor_ent_rt_df$cor - 0.3) < 0.001)

cor_ent_rt_df %>%
  ggplot(aes(x = seq(6, 250, 1), y = cor)) +
  geom_line() +
  labs(
    x = "Reaction Time Cutoff (in seconds)",
    y = "Pearson Correlation\nbetween Entropy and Reaction Time"
  ) +
  #scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0,250,60))

ggsave("figs/entropy_reaction_time_cor.png")

# Calculate the proportion of reaction times greater than 26 seconds
mean(cifar10h$reaction_time > 6, na.rm = TRUE)

# Relationship between reaction time and correct guess
cifar10h %>%
  ggplot(aes(x = reaction_time, y = correct_guess)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
  scale_x_continuous(labels = scales::number, limits = c(0, 26)) +
  labs(
    x = "Reaction Time (in seconds)",
    y = "Probability of Correct Guesses"
  )

ggsave("figs/Reaction_Time_Correct_Guess.png")
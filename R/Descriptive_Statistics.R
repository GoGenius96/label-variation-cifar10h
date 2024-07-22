library(readr) # for reading csv files
library(tidyverse) # for data manipulation and visualization

cifar10h <- readRDS("cifar-10h/data/cifar10h.rds")

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
  ) +
  # increase the text size
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14))

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
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Blues"))(10)) + 
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))

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
  geom_text(aes(label = scales::percent(round(Freq, 3))), size = 3) +
  theme(legend.position = "none")

            
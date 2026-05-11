library(readr) # for reading csv files
library(tidyverse) # for data manipulation and visualization

# read in the cifar-10h data
cifar10h <- read_csv("data/cifar10h-raw.csv")

# find the index for the images with the highest and lowest entropy
# cifar10h %>%
#   filter(image_filename == "dump_truck_s_000114.png") %>%
#   select(cifar10_test_test_idx)
# 
# cifar10h %>%
#   filter(image_filename == "cassowary_s_000401.png") %>%
#   select(cifar10_test_test_idx)
# 
# cifar10h %>%
#   filter(image_filename == "abandoned_ship_s_000635.png") %>%
#   select(cifar10_test_test_idx)

# remove unnecessary columns, convert annotator_id to character and correct the subcategory column
cifar10h <- cifar10h %>%
  filter(is_attn_check == 0) %>% # remove attention check trials
  select(image_filename, annotator_id, trial_index, true_category, subcategory, chosen_category, true_label, correct_guess, reaction_time, time_elapsed) %>%
  mutate(annotator_id = as.character(annotator_id)) %>%
  # remove _s_ and everything after it from subcategory
  mutate(subcategory = str_remove(subcategory, "_s_.*"))

# create a new table with the Vote distribution for each image as rows and an extra column for the total number of votes
Y_cifar10h <- cifar10h %>%
  mutate(chosen_category = as.factor(chosen_category)) %>%
  group_by(image_filename, chosen_category, .drop = FALSE) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = chosen_category, values_from = n) %>%
  ungroup() %>%
  mutate(J = rowSums(dplyr::select(., -image_filename)))

# negative reaction times are not possible, so we set them to NA and we transform the reaction time into seconds
cifar10h$reaction_time <- cifar10h$reaction_time/1000
cifar10h$reaction_time[cifar10h$reaction_time < 0] <- NA
cifar10h$time_elapsed <- cifar10h$time_elapsed/1000
cifar10h$time_elapsed[cifar10h$time_elapsed < 0] <- NA

# Function to calculate prior entropy
calculate_entropy <- function(label_counts) {
  total_labels <- sum(label_counts)
  probabilities <- label_counts[label_counts > 0] / total_labels
  entropy <- -sum(probabilities * log2(probabilities))
  return(entropy)
}

# Calculate prior entropies for each image
label_counts_list <- Y_cifar10h %>% select(-image_filename, -J) %>% as.matrix()
entropies <- apply(label_counts_list, MARGIN = 1, calculate_entropy)
entropies <- data.frame(image_filename = Y_cifar10h$image_filename, entropy = entropies)

# join cifar10h with the prior entropy
cifar10h <- left_join(cifar10h, entropies, by = "image_filename")

# export files
write.csv(cifar10h, file = "data/cifar10h.csv", row.names = FALSE)
write.csv(Y_cifar10h, file = "data/Y_cifar10h.csv", row.names = FALSE)
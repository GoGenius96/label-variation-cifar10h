library(readr) # for reading csv files
library(dplyr) # for data manipulation and visualization

# read in the cifar-10h data
cifar10h <- read_csv("cifar-10h/data/cifar10h-raw.csv")

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
  mutate(J = rowSums(dplyr::select(., -image_filename))) %>%
  # remove rows with more than 100 votes as those are likely to be errors
  filter(J < 100)

# filter out all the images that have 100 votes or more from the cifar10h data
cifar10h <- cifar10h %>%
  filter(image_filename %in% Y_cifar10h$image_filename)

# export cifar10h as a rds file and Y_cifar10h as a csv file
saveRDS(cifar10h, file = "cifar-10h/data/cifar10h.rds")
write.csv(Y_cifar10h, file = "cifar-10h/data/Y_cifar10h.csv", row.names = FALSE)



# Y_R_cifar10h <- cifar10h %>%
#   mutate(chosen_category = as.factor(chosen_category),
#          Reaction = case_when(
#            reaction_time > 2000 ~ "Slow",
#            TRUE ~ "Fast"
#          )) %>%
#   group_by(Reaction, image_filename, chosen_category, .drop = FALSE) %>%
#   summarise(n = n()) %>%
#   pivot_wider(names_from = chosen_category, values_from = n) %>%
#   ungroup() %>%
#   mutate(J = rowSums(dplyr::select(., -c(Reaction, image_filename)))) %>%
#   filter(J < 100)


#write.csv(Y_R_cifar10h, file = "cifar-10h/data/Y_R_cifar10h.csv", row.names = FALSE)





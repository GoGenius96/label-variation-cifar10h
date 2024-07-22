library(readr) # for reading csv files
library(tidyverse) # for data manipulation and visualization
library(lme4)

theme_set(theme_bw())

cifar10h <- read_csv("cifar-10h/data/cifar10h-raw.csv")

cifar10h %>%
  group_by(true_category) %>%
  summarise(n = length(unique(image_filename)))

cifar10h %>%
  group_by(annotator_id) %>%
  summarise(n_trials = length(unique(trial_index))) %>%
  pull(n_trials) %>%
  unique()

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



reaction_time <- cifar10h %>%
  ggplot(aes(x = reaction_time, y = correct_guess)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_continuous(labels = scales::number, limits = c(0, 5000))

model_data <- cifar10h %>%
  filter(reaction_time > 0, reaction_time < 5000) %>%
  mutate(correct_guess = factor(correct_guess, levels = c(0, 1)))

model_glm <- glm(correct_guess ~ reaction_time + true_category,
             data = model_data,
             family = "binomial")

model_data <- model_data %>%
  mutate(prob = predict(object = model_glm, type = "response"),
         log_odds = predict(object = model_glm, type = "link"),
         odds = exp(predict(object = model_glm, type = "link")))

roc <- pROC::roc(response = model_data$correct_guess, predictor = model_data$log_odds)

roc_surve <- pROC::ggroc(roc, legacy.axes = TRUE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
  linetype = "dashed", color = "black") +
  labs(x = "1 - Spezifität",
       y = "Sensitivität")

AUC <- pROC::auc(roc)


# sort(table(cifar10h$subcategory))
# 
# img <- sample(cifar10h$image_filename, 100)
# 
# test_data <- cifar10h %>%
#   filter(reaction_time > 0, reaction_time < 5000, image_filename %in% img) %>%
#   mutate(correct_guess = factor(correct_guess, levels = c(0, 1)))
# 
# model_glm <- glm(correct_guess ~ reaction_time + true_category, 
#              data = test_data, 
#              family = "binomial")
# 
# model_lmer <- lmer(reaction_time ~ true_category + (1|image_filename), data = test_data)
# summary(model_lmer)
# 
# model_glmer_image <- glmer(correct_guess ~ reaction_time + true_category + (1|image_filename), 
#     data = test_data, 
#     family = "binomial")
# 
# model_glmer_annotator <- glmer(correct_guess ~ reaction_time + true_category + (1|annotator_id),
#                      data = test_data, 
#                      family = "binomial")
# 
# model_glmer_image_annotator <- glmer(correct_guess ~ reaction_time + true_category + (1|image_filename) + (1|annotator_id),
#                      data = test_data, 
#                      family = "binomial")
# 
# summary(model_glm)
# summary(model_glmer_image)
# summary(model_glmer_annotator)
# summary(model_glmer_image_annotator)
# 
# AIC(model_glm, model_glmer_image, model_glmer_annotator, model_glmer_image_annotator)


# Subcategory analysis

cifar10h %>%
  group_by(true_category) %>%
  summarise(subcategory_unique = n_distinct(subcategory))

library(readr) # for reading csv files
library(tidyverse) # for data manipulation and visualization
library(lme4)
library(RColorBrewer)

theme_set(theme_bw() + theme(axis.text.x = element_text(size = 18),
                             axis.text.y = element_text(size = 18),
                             axis.title = element_text(size = 20)))

cifar10h <- readRDS("cifar-10h/data/cifar10h.rds")
Y_cifar10h <- read_csv("cifar-10h/data/Y_cifar10h.csv")

# Function to calculate entropy
calculate_entropy <- function(label_counts) {
  total_labels <- sum(label_counts)
  probabilities <- label_counts[label_counts > 0] / total_labels
  entropy <- -sum(probabilities * log2(probabilities))
  return(entropy)
}

label_counts_list <- Y_cifar10h %>% select(-image_filename, -J) %>% as.matrix()

# Calculate entropies for each image
entropies <- apply(label_counts_list, MARGIN = 1, calculate_entropy)
entropies <- data.frame(image_filename = Y_cifar10h$image_filename, entropy = entropies)

# join cifar10h with the entropy
cifar10h <- left_join(cifar10h, entropies, by = "image_filename")


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

# compute the average reaction time for each annotator and plot
cifar10h %>%
  group_by(annotator_id) %>%
  summarise(avg_rt = mean(reaction_time, na.rm = TRUE)) %>%
  summarise(exp_avg_rt = mean(avg_rt),
            sd_rt = sd(avg_rt, na.rm = TRUE),
            median_rt = median(avg_rt, na.rm = TRUE),
            quantile1_rt  = quantile(avg_rt, 0.01, na.rm = TRUE),
            quantile99_rt = quantile(avg_rt, 0.99, na.rm = TRUE),
            n = n())

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

# create a qqplot
et_df %>%
  filter(time_elapsed < 1500) %>%
  ggplot(aes(sample = time_elapsed)) +
  geom_qq() +
  geom_qq_line() +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

et_df %>% 
  ggplot(aes(x = time_elapsed)) +
  geom_density() +
  labs(
    x = "Elapsed Time (in seconds)",
    y = "Density"
  ) +
  scale_x_continuous(limits = c(0, 1500))


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

lm(time_elapsed/60 ~ avg_correct, data = et_df[et_df$time_elapsed < 1500,]) %>%
  summary()

# Entropy vs. Reaction Time
cifar10h %>%
  ggplot(aes(x = entropy, y = reaction_time)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_y_continuous(limits = c(0, 26)) +
  labs(
    x = "Entropy",
    y = "Reaction Time (in seconds)"
  )

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

# model <- lmer(reaction_time ~ (1|annotator_id) + (1|subcategory)+ entropy, data = cifar10h %>%
#                        mutate(reaction_time = reaction_time) %>%
#                        filter(reaction_time < 60))
# 
# summary(model)
# # how much variance can be explained by the annotator
# model <- lmer(reaction_time ~ entropy + (1|annotator_id), data = cifar10h %>% 
#        mutate(reaction_time = reaction_time) %>% 
#        filter(reaction_time < 10))
# 
# model %>% summary()
# plot(model)
# 
# ggplot() + 
#   geom_density(aes(x = ranef(model)[["annotator_id"]][["(Intercept)"]])) + 
#   stat_function(fun = dnorm, args = list(mean = 0, sd = 0.4))

# Calculate the proportion of reaction times greater than 26 seconds
mean(cifar10h$reaction_time > 6, na.rm = TRUE)

# Relationship between reaction time and correct guess
cifar10h %>%
  ggplot(aes(x = reaction_time, y = correct_guess)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
  scale_x_continuous(labels = scales::number, limits = c(0, 26)) +
  labs(
    x = "Reaction Time (in seconds)",
    y = "Probability of Correct Guesses"
  )

ggsave("figs/Reaction_Time_Correct_Guess.png")


# # Example data
# set.seed(123)
# data <- data.frame(value = rnorm(1000))
# 
# # Function to calculate equal area bins
# equal_area_bins <- function(data, bins) {
#   data <- data %>%
#     arrange(value) %>%
#     mutate(rank = row_number(),
#            bin = cut(value, breaks = quantile(value, probs = seq(0, 1, 1/bins)), include.lowest = TRUE))
#   
#   bin_stats <- data %>%
#     group_by(bin) %>%
#     summarize(n = n(), 
#               min_value = min(value), 
#               max_value = max(value)) %>%
#     mutate(width = max_value - min_value,
#            density = n / width,
#            mid = (min_value + max_value) / 2)
#   
#   return(bin_stats)
# }
# 
# bins <- 30 # Number of bins
# bin_stats <- cifar10h %>%
#   group_by(annotator_id) %>%
#   summarise(avg_rt = mean(reaction_time, na.rm = TRUE) / 1000) %>%
#   filter(avg_rt < 4.5) %>%
#   select(value = avg_rt) %>%
#   equal_area_bins(bins)
# 
# # Plotting the histogram with equal area bins
# ggplot(bin_stats, aes(xmin = min_value, xmax = max_value, ymin = 0, ymax = density)) +
#   geom_rect() +
#   labs(
#     x = "Average Reaction Time (in seconds)",
#     y = "Number of Annotators"
#   ) +
#   scale_x_continuous(limits = c(0, 5)) +
#   theme(axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 14))
# 
# 




# squash reaction time.
cifar10h <- cifar10h %>%
  mutate(reaction_time_squashed = case_when(
    reaction_time > 26 ~ NA,
    reaction_time < 0.75 ~ 0.75 + 1e-6,
    TRUE ~ reaction_time
  ),
  reaction_time_squashed = reaction_time_squashed - 0.75)



model_data <- cifar10h %>%
  filter(time_elapsed < 1500) %>%
  na.omit()

annotator_example <- sample(model_data$annotator_id, 300)
model_data_ex <- model_data %>%
  filter(annotator_id %in% annotator_example)

model1 <- glmer(reaction_time_squashed ~ entropy + true_category + (1|annotator_id), data = model_data_ex, family = Gamma(link = "log"))
model2 <- lmer(reaction_time ~ entropy * true_category + (1|annotator_id), data = model_data)
anova(model1, model2)
plot(model1)
summary(model1)


model_data %>%
  ggplot(aes(x = log(reaction_time))) +
  geom_density() +
  labs(
    x = "Entropy",
    y = "Reaction Time (in seconds)"
  )


library(gamlss)

# fit model1 using gamlss
model1_gamlss <- gamlss(reaction_time_squashed ~ entropy + true_category + re(random = ~1|annotator_id),
                        data = na.omit(model_data_ex),
                        family = GA(mu.link = "log", sigma.link ="log"))

summary(model1_gamlss)
plot(model1_gamlss)

model2_gamlss <- gamlss(reaction_time ~ entropy + true_category + re(random = ~1|annotator_id), 
                        sigma.formula = ~entropy + true_category,
                        data = na.omit(model_data_ex), 
                        family = IG(mu.link = "log", sigma.link ="log"))

summary(model2_gamlss)
plot(model2_gamlss)

# Sample data
actual_data <- model_data_ex[model_data_ex$reaction_time < 6,]$reaction_time_squashed
model_predictions <- predict(model1, type = "response")

# Compute ECDFs
ecdf_actual <- ecdf(actual_data)
ecdf_predictions <- ecdf(model_predictions)

# Plot ECDF of actual data
plot(ecdf_actual, main="ECDF Comparison", xlab="Value", ylab="ECDF", col="blue", lwd=2)

# Add ECDF of model predictions
lines(ecdf_predictions, col="red", lwd=2, lty=2)

# Add legend
legend("bottomright", legend=c("Actual Data", "Model Predictions"), col=c("blue", "red"), lwd=2, lty=c(1, 2))


# create a shiny application where I can construct the ecdf of a gamma distribution and see the ecdf of the actual data
# and the model predictions.
library(shiny)
shinyApp(
  ui = fluidPage(
    titlePanel("ECDF Comparison"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("shape", "Shape", min = 0.1, max = 10, value = 1),
        sliderInput("rate", "Rate", min = 0.1, max = 10, value = 1)
      ),
      mainPanel(
        plotOutput("ecdf_plot")
      )
    )
  ),
  server = function(input, output) {
    output$ecdf_plot <- renderPlot({
      # Generate data
      data <- rgamma(10000, shape = input$shape, rate = input$rate)

      # Compute ECDFs
      ecdf_actual <- ecdf(cifar10h$reaction_time)
      ecdf_gamma <- ecdf(data)

      # Plot ECDF of actual data
      plot(ecdf_actual, main="ECDF Comparison", xlab="Value", ylab="ECDF", col="blue", lwd=2, xlim = c(-0.2, 6))

      # Add ECDF of model predictions
      lines(ecdf_gamma, col="red", lwd=2, lty=2)

      # Add legend
      legend("bottomright", legend=c("Actual Data", "Model Predictions"), col=c("blue", "red"), lwd=2, lty=c(1, 2))
    })
  }
)





Y <- Y_cifar10h %>% select(-image_filename, -J)

true_category_i <- cifar10h %>%
  group_by(image_filename) %>%
  summarise(true_category = unique(true_category))
  

Y_cifar10h <- Y_cifar10h %>% left_join(true_category_i, by = c("image_filename"))

Z <- as.numeric(factor(Y_cifar10h$true_category))

theta = matrix(0, 10, 10)
for(l in 1:10){
  for(k in 1:10){
    numerator <- sum(Y[Z == l, k])
    denominator <- sum(Y[Z == l, ])
    theta[l, k] <- numerator/denominator
  }
}

sum(Y[Z == 1, 1])/sum(Y[Z == 1, ])


list_x <- list()
for(i in 1:10){
  list_x[[i]] <- rnorm(100, 0,1)
}

mean_list_x <- colMeans(do.call(rbind, list_x))
for(i in 1:10){
  (list_x[[i]] - mean_list_x) %*% t(list_x[[i]] - mean_list_x)
}
library(readr) # for reading csv files
library(tidyverse) # for data manipulation and visualization
library(RColorBrewer)
library(ggpubr)
library(lme4)
library(shiny)
library(data.table)
library(MuMIn)


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

# add image_filename to the post_entropy table
post_entropy <- data.frame(image_filename = Y_cifar10h$image_filename, post_entropy = post_entropy$`0`)

model_data <- cifar10h %>%
  left_join(post_entropy, by = "image_filename") %>%
  dplyr::select(reaction_time , annotator_id, prior_entropy = entropy, post_entropy, true_category)

# squashing the reaction time
model_data <- model_data %>%
  filter(reaction_time < 26) %>%
  mutate(reaction_time = case_when(
    reaction_time <= 0.75 ~ 0.751,
    TRUE ~ reaction_time
  ), 
  reaction_time = reaction_time - 0.75)

# create some parameters for the gamma distribution
mode <- 0.48
sd <- 0.5
c <- mode/sd
alpha <- 0.5 * (c + sqrt(c^4 + 4) * c + 2)
beta <- sqrt(alpha) / sd

rt_med <- median(model_data$reaction_time, na.rm = TRUE)
model_data %>%
  ggplot(aes(x = reaction_time)) +
  #stat_density(geom = "line", color = "black", size = 1.5, bounds = c(0, Inf)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, color = "black") +
  stat_function(fun = dgamma, args = list(shape = alpha, rate = beta), color = "red", size = 1) +
  labs(
    x = "Reaction Time (in seconds)",
    y = "Density"
  )

ggsave("figs/reaction_time_gamma.png", width = 10, height = 5)


# Fit the model
model_glmer <- glmer(reaction_time ~ post_entropy + (1|annotator_id), 
                     data = model_data,
                     family = Gamma(link = "log"))
summary(model_glmer)

estnu <- as.data.table(VarCorr(model_glmer))[2,4]
estsig <- as.data.table(VarCorr(model_glmer))[1,4] / estnu 
ICC <- estsig/(estsig + trigamma(1/estnu))

R_squared <- r.squaredGLMM(model_glmer)

# get the random effects
random_effects <- ranef(model_glmer)[["annotator_id"]][["(Intercept)"]]


# Shiny application to fit gamma distribution to reaction time data
# # Define UI
# ui <- fluidPage(
#   titlePanel("Gamma Distribution Fit to Reaction Time Data"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("mode", "Mode:", 
#                   min = 0.1, max = 6, value = 0.73, step = 0.02),
#       sliderInput("sd", "standard dev:", 
#                   min = 0.1, max = 10, value = 1, step = 0.05)
#     ),
#     
#     mainPanel(
#       plotOutput("histPlot")
#     )
#   )
# )
# 
# # Define server logic
# server <- function(input, output) {
#   computedParams <- reactive({
#     mode <- input$mode
#     sd <- input$sd
#     if (mode > 0 && sd > 0) {
#       c <- mode/sd
#       alpha <- 0.5 * (c + sqrt(c^4 + 4) * c + 2)
#       beta <- sqrt(alpha) / sd
#       return(list(alpha = alpha, beta = beta))
#     } else {
#       return(NULL)
#     }
#   })
#   
#   output$histPlot <- renderPlot({
#     params <- computedParams()
#     if (is.null(params)) return(NULL)
#     
#     rt_med <- median(model_data$reaction_time, na.rm = TRUE)
#     
#     model_data %>%
#       ggplot(aes(x = reaction_time)) +
#       geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, color = "black") +
#       scale_x_continuous(limits = c(0,6), labels = scales::comma, breaks = c(seq(0, 6, 2), rt_med)) +
#       stat_function(fun = dgamma, args = list(shape = params$alpha, rate = params$beta), color = "red", size = 1) +
#       labs(
#         x = "Reaction Time (in seconds)",
#         y = "Density"
#       )
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

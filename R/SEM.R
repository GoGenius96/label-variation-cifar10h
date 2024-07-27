library(MASS) # for multinomial distribution
library(gtools) # for dirichlet distribution

MultinomialExpectationMaximizer <- setRefClass(
  "MultinomialExpectationMaximizer",
  fields = list(
    K = "numeric",
    rtol = "numeric",
    max_iter = "numeric",
    restarts = "numeric"
  ),
  methods = list(
    initialize = function(K, rtol = 1e-4, max_iter = 100, restarts = 10) {
      .self$K <- K
      .self$rtol <- rtol
      .self$max_iter <- max_iter
      .self$restarts <- restarts
    },
    
    log_lh = function(Y, pi, theta) {
      mn_probs <- rep(0, nrow(Y))
      for (k in 1:nrow(theta)) {
        mn_probs_k <- pi[k] * .self$multinomial_prob(Y, theta[k, ])
        mn_probs <- mn_probs + mn_probs_k
      }
      mn_probs[mn_probs == 0] <- .Machine$double.eps
      return(sum(log(mn_probs)))
    },
    
    multinomial_prob = function(counts, theta) {
      m <- apply(Y, 1, function(x) dmultinom(x, prob = theta))
      return(m)
    },
    
    e_step = function(Y, pi, theta) {
      N <- nrow(Y)
      K <- length(pi)
      weighted_multi_prob <- matrix(0, nrow = N, ncol = K)
      for (k in 1:K) {
        for(i in 1:N) {
          weighted_multi_prob[i, k] <- pi[k] * dmultinom(Y[i, ], prob = theta[k, ])
        }
      }
      denum <- rowSums(weighted_multi_prob)
      tau <- weighted_multi_prob / denum
      return(tau)
    },
    
    m_step = function(Y, tau) {
      pi <- colSums(tau) / sum(tau)
      weighted_counts <- t(tau) %*% Y
      theta <- weighted_counts / rowSums(weighted_counts)
      return(list(pi = pi, theta = theta))
    },
    
    compute_loss = function(Y, pi, theta, tau) {
      loss <- 0
      for (k in 1:length(pi)) {
        weights <- tau[, k]
        loss <- loss + sum(weights * (log(pi[k]) + log(.self$multinomial_prob(Y, theta[k, ]))))
        loss <- loss - sum(weights * log(weights))
      }
      return(loss)
    },
    
    init_params = function(C) {
      pi <- rep(1 / .self$K, .self$K)
      theta <- rdirichlet(.self$K, rep(2 * C, C))
      return(list(pi = pi, theta = theta))
    },
    
    train_once = function(Y) {
      loss <- Inf
      C <- ncol(Y)
      params <- .self$init_params(C)
      pi <- params$pi
      theta <- params$theta
      
      for (it in 1:.self$max_iter) {
        prev_loss <- loss
        tau <- .self$e_step(Y, pi, theta)
        params <- .self$m_step(Y, tau)
        pi <- params$pi
        theta <- params$theta
        loss <- .self$compute_loss(Y, pi, theta, tau)
        if (it > 1 && abs((prev_loss - loss) / prev_loss) < .self$rtol) {
          break
        }
      }
      return(list(pi = pi, theta = theta, tau = tau, loss = loss))
    },
    
    fit = function(Y) {
      best_loss <- -Inf
      best_pi <- NULL
      best_theta <- NULL
      best_tau <- NULL
      
      for (it in 1:.self$restarts) {
        result <- .self$train_once(Y)
        pi <- result$pi
        theta <- result$theta
        tau <- result$tau
        loss <- result$loss
        if (loss > best_loss) {
          best_loss <- loss
          best_pi <- pi
          best_theta <- theta
          best_tau <- tau
        }
      }
      return(list(best_loss = best_loss, best_pi = best_pi, best_theta = best_theta, best_tau = best_tau))
    }
  )
)

# Example usage
model <- MultinomialExpectationMaximizer$new(K = 10, rtol = 1e-4, max_iter = 100, restarts = 10)
# Fit the model with some data Y
Y_cifar10h <- read_csv("cifar-10h/data/Y_cifar10h.csv")
Y <- Y_cifar10h %>% dplyr::select(-image_filename, -J) %>% as.matrix()
result <- model$fit(Y)

tolerance_sort <- function(array, tolerance, reverse = FALSE) {
  # Sort the array by the first column, then by the second column
  array_sorted <- array[order(array[, 1], array[, 2]), ]
  sort_range <- list(1)
  
  for (i in seq_len(nrow(array) - 1)) {
    if (array_sorted[i + 1, 1] - array_sorted[i, 1] <= tolerance) {
      sort_range <- append(sort_range, i + 1)
    } else {
      sub_arr <- array_sorted[unlist(sort_range), ]
      sub_arr_ord <- sub_arr[order(sub_arr[, 2], sub_arr[, 1]), ]
      array_sorted[unlist(sort_range), ] <- sub_arr_ord
      sort_range <- list(i + 1)
    }
  }
  
  if (reverse) {
    array_sorted <- array_sorted[nrow(array_sorted):1, ]
  }
  
  matched_index <- sapply(1:nrow(array_sorted), function(i) {
    which(apply(array, 1, function(row) all(row == array_sorted[i, ])))[1]
  })
  
  return(list(sorted_array = array_sorted, matched_index = matched_index))
}
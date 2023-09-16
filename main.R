library("survival")
library("plotly")
library("nph")
library("grid")
library("gridExtra")
library("eha")

source("calculate_power_cc.R")
source("calculate_power_le.R")

# Number of observations in each treatment groups
n <- 100
# Level of significance 
alpha <- 0.05
# Number of experiments
N <- 200
# Scale parameter of first (control) group
scale_1 <- 1

# Crossing Curves Data
# Scale parameter of the second (treatment) group
scale_2_cc <- 1.6
# List of shape parameters of Weibull distribution
shape_list <- seq(1,2,0.05)
# List of censoring probabilities
p_cens_list <- seq(0.2,0.4,0.05)

cc_results <- data.frame()
for (shape in shape_list) {
  for (p_cens in p_cens_list) {
    # Parameters of generated data
    params <- list()
    params[[1]] <- c(1, scale_1, p_cens)
    params[[2]] <- c(shape, scale_2_cc, p_cens)
    cc_powers <- calculate_power_cc(params=params)
    logrank_power <- cc_powers$logrank_power
    maxcombo_power <- cc_powers$maxcombo_power
    cc_results <-
      rbind(cc_results,
            data.frame(shape, p_cens, logrank_power, maxcombo_power))
  }
}

# Late Effect Data
# Rate parameter of the first (control) group
rate_1_le <- 1
# Rate parameter of the second (treatment) group after effect_time
rate_2_le <- 0.5
# List of effect times
effect_time_list <- seq(0,0.6,0.05)
# List of censoring probabilities
p_cens_list <- seq(0.2,0.4,0.05)

le_results <- data.frame()
for (effect_time in effect_time_list) {
  for (p_cens in p_cens_list) {
    # Parameters of generated data
    params <- list()
    params[[1]] <- c(rate_1_le, rate_1_le, 0, p_cens)
    params[[2]] <- c(rate_1_le, rate_2_le, effect_time, p_cens)
    le_powers <- calculate_power_le(params=params)
    logrank_power <- le_powers$logrank_power
    maxcombo_power <- le_powers$maxcombo_power
    le_results <-
      rbind(le_results,
            data.frame(effect_time, p_cens, logrank_power, maxcombo_power))
  }
}

# 3D plot of results
fig <- plot_ly(cc_results, x = ~shape, y = ~p_cens, z = ~logrank_power, color = ~p_cens)

fig <-
  fig %>% add_mesh(
    x = c(
      min(shape_list),
      min(shape_list),
      max(shape_list),
      max(shape_list)
    ),
    y = c(
      min(p_cens_list),
      max(p_cens_list),
      min(p_cens_list),
      max(p_cens_list)
    ),
    z = c(0.8, 0.8, 0.8, 0.8),
    opacity = 0.4
  )

fig <- fig %>% add_markers()

fig <- fig %>% layout(scene = list(xaxis = list(title = 'Shape'),

                                   yaxis = list(title = 'p_cens'),

                                   zaxis = list(title = 'Log-rank test power')))

fig

fig <- plot_ly(cc_results, x = ~shape, y = ~p_cens, z = ~maxcombo_power, color = ~p_cens)

fig <-
  fig %>% add_mesh(
    x = c(
      min(shape_list),
      min(shape_list),
      max(shape_list),
      max(shape_list)
    ),
    y = c(
      min(p_cens_list),
      max(p_cens_list),
      min(p_cens_list),
      max(p_cens_list)
    ),
    z = c(0.8, 0.8, 0.8, 0.8),
    opacity = 0.4
  )

fig <- fig %>% add_markers()

fig <- fig %>% layout(scene = list(xaxis = list(title = 'Shape'),
                                   
                                   yaxis = list(title = 'p_cens'),
                                   
                                   zaxis = list(title = 'MaxCombo test power')))

fig

# 2D plots of results
cc_results_LR <- cc_results %>%
  ggplot(aes(x = shape, y = logrank_power, group = p_cens)) +
  geom_point(aes(colour = p_cens), size = 3) +
  geom_smooth(aes(group = p_cens, colour = p_cens),
              method = "lm",
              linewidth = 0.5) +
  geom_hline(yintercept = 0.8) +
  xlab('Shape') + ylab('Test Power') + scale_y_continuous(limits = c(0.4, 1.1)) +
  ggtitle(label = 'Log-rank test')

cc_results_MC <- cc_results %>%
  ggplot(aes(x = shape, y = maxcombo_power, group = p_cens)) +
  geom_point(aes(colour = p_cens), size = 3) +
  geom_smooth(aes(group = p_cens, colour = p_cens),
              method = "lm",
              linewidth = 0.5) +
  geom_hline(yintercept = 0.8) +
  xlab('Shape') + ylab('Test Power') + scale_y_continuous(limits = c(0.4, 1.1)) +
  ggtitle(label = 'Maxcombo test')

le_results_LR <- le_results %>%
  ggplot(aes(x = effect_time, y = logrank_power, group = p_cens)) +
  geom_point(aes(colour = p_cens), size = 3) +
  geom_smooth(aes(group = p_cens, colour = p_cens),
              method = "lm",
              linewidth = 0.5) +
  geom_hline(yintercept = 0.8) +
  xlab('Effect Time') +
  ylab('Test Power') + scale_y_continuous(limits = c(0.4, 1.1)) +
  ggtitle(label = 'Log-rank test')

le_results_MC <- le_results %>%
  ggplot(aes(x = effect_time, y = maxcombo_power, group = p_cens)) +
  geom_point(aes(colour = p_cens), size = 3) +
  geom_smooth(aes(group = p_cens, colour = p_cens),
              method = "lm",
              linewidth = 0.5) +
  geom_hline(yintercept = 0.8) +
  xlab('Effect Time') +
  ylab('Test Power') + scale_y_continuous(limits = c(0.4, 1.1)) +
  ggtitle(label = 'Maxcombo test')

# 2D plots of survival functions
base <-
  ggplot() +
  xlim(0, 10)

cc_survival <- base + lapply(shape_list, function(shape) {
  fun_name <- paste0("fun.", shape)
  geom_function(
    fun = pweibull,
    aes(colour = shape),
    args = list(
      shape = shape,
      scale = scale_2_cc,
      lower.tail = FALSE
    )
  )
}) + geom_function(
  fun = pweibull,
  colour = "red",
  args = list(
    shape = 1,
    scale = scale_1,
    lower.tail = FALSE
  )
) + xlab('Time') +
  ylab('Survival Probability') +
  ggtitle(label = 'Survival Functions')

cc_hazard <- base + lapply(shape_list, function(shape) {
  fun_name <- paste0("fun.", shape)
  geom_function(
    fun = hweibull,
    aes(colour = shape),
    args = list(
      shape = shape,
      scale = scale_2_cc
    )
  )
}) + geom_function(
  fun = hweibull,
  colour = "red",
  args = list(
    shape = 1,
    scale = scale_1
  )
) + xlab('Time') +
  ylab('Hazard') +
  ggtitle(label = 'Hazard Functions')

# 2D plots of survival functions for late effect
pexp_le <- function(q, rate_1, rate_2, effect_time) {
  return(ifelse(
    q <= effect_time,
    pexp(q, rate = rate_1, lower.tail = FALSE),
    (1 - pexp(
      effect_time, rate = rate_1, lower.tail = TRUE
    )) *
      pexp(q - effect_time, rate = rate_2, lower.tail = FALSE)
  ))
}

base <-
  ggplot() +
  xlim(0, 10)

le_survival <-
  base + lapply(effect_time_list, function(effect_time) {
    fun_name <- paste0("fun.", effect_time)
    geom_function(
      fun = pexp_le,
      aes(colour = effect_time),
      args = list(
        rate_1 = rate_1_le,
        rate_2 = rate_2_le,
        effect_time = effect_time
      )
    )
  }) + geom_function(
    fun = pexp,
    colour = "red",
    args = list(rate = rate_1_le,
                lower.tail = FALSE)
  ) + xlab('Time') +
  ylab('Survival Probability') +
  ggtitle(label = 'Survival Functions')

le_hazard <- ggplot() +
  xlim(0, 2) + lapply(effect_time_list, function(effect_time) {
    fun_name <- paste0("fun.", effect_time)
    geom_function(
      fun = function(x, rate_1, rate_2, effect_time) {
        return(ifelse(x < effect_time, rate_1, rate_2))
      },
      aes(colour = effect_time),
      args = list(
        rate_1 = rate_1_le,
        rate_2 = rate_2_le,
        effect_time = effect_time
      )
    )
  }) + geom_function(
    fun = function(x, rate_1) {
      return(rate_1)
    },
    colour = "red",
    args = list(rate_1 = rate_1_le)
  ) + xlab('Time') +
  ylab('Hazard') +
  ggtitle(label = 'Hazard Functions')

grid.arrange(cc_results_LR,
             cc_results_MC,
             cc_survival,
             cc_hazard,
             ncol = 2,
             top = textGrob("Crossing Curves",
                             gp=gpar(fontsize=17)))

grid.arrange(le_results_LR,
             le_results_MC,
             le_survival,
             le_hazard,
             ncol = 2,
             top = textGrob("Late Effect",
                            gp=gpar(fontsize=17)))

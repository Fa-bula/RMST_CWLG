# Number of observations in each treatment groups
n_list <- c(50, 100, 200)
# List of censoring probabilities
p_cens_list <- seq(0.2,0.4,0.05)
# Trial Duration
t_duration_list <- c(1, 2, 3)
# Level of significance 
alpha <- 0.05
# Number of experiments
N <- 100

# Crossing Curves Data
# Scale parameter of first (control) group
scale_1_cc <- 1
# Scale parameter of the second (treatment) group
scale_2_cc <- 1.4
# List of shape parameters of Weibull distribution
shape_list <- seq(1,4,0.1)

# Late Effect Data
# Rate parameter of the first (control) group
rate_1_le <- 1
# Rate parameter of the second (treatment) group after effect_time
rate_2_le <- 0.5
# List of effect times
effect_time_list_le <- seq(0,0.6,0.05)

# Early Effect Data
# Rate parameter of the first (control) group
rate_1_ee <- 1
# Rate parameter of the second (treatment) group before effect_time
rate_2_ee <- 0.4
# List of effect times
effect_time_list_ee <- seq(0.3,1,0.05)

library("survival")
library("nph")
library("eha")
library("survRM2")

source("calculate_power_cc.R")
source("calculate_power_le.R")
source("parameters.R")
source("run_tests.R")

cc_results <- data.frame()

for (t_duration in t_duration_list) {
  for (n in n_list) {
    for (shape in shape_list) {
      for (p_cens in p_cens_list) {
        # Parameters of generated data
        params <- list()
        params[[1]] <- c(1, scale_1_cc, p_cens)
        params[[2]] <- c(shape, scale_2_cc, p_cens)
        cc_powers <- calculate_power_cc(params = params)
        logrank_power <- cc_powers$logrank_power
        maxcombo_power <- cc_powers$maxcombo_power
        rmst_power <- cc_powers$rmst_power
        cc_results <-
          rbind(
            cc_results,
            data.frame(
              n,
              t_duration,
              p_cens,
              shape,
              logrank_power,
              maxcombo_power,
              rmst_power
            )
          )
      }
    }
  }
}

le_results <- data.frame()
for (t_duration in t_duration_list) {
  for (n in n_list) {
    for (effect_time in effect_time_list_le) {
      for (p_cens in p_cens_list) {
        # Parameters of generated data
        params <- list()
        params[[1]] <- c(rate_1_le, rate_1_le, 0, p_cens)
        params[[2]] <- c(rate_1_le, rate_2_le, effect_time, p_cens)
        le_powers <- calculate_power_le(params = params)
        logrank_power <- le_powers$logrank_power
        maxcombo_power <- le_powers$maxcombo_power
        rmst_power <- le_powers$rmst_power
        le_results <-
          rbind(
            le_results,
            data.frame(
              n,
              t_duration,
              p_cens,
              effect_time,
              logrank_power,
              maxcombo_power,
              rmst_power
            )
          )
      }
    }
  }
}

ee_results <- data.frame()
for (t_duration in t_duration_list) {
  for (n in n_list) {
    for (effect_time in effect_time_list_ee) {
      for (p_cens in p_cens_list) {
        # Parameters of generated data
        params <- list()
        params[[1]] <- c(rate_1_ee, rate_1_ee, 0, p_cens)
        params[[2]] <- c(rate_2_ee, rate_1_ee, effect_time, p_cens)
        ee_powers <- calculate_power_le(params = params)
        logrank_power <- ee_powers$logrank_power
        maxcombo_power <- ee_powers$maxcombo_power
        rmst_power <- ee_powers$rmst_power
        ee_results <-
          rbind(
            ee_results,
            data.frame(
              n,
              t_duration,
              p_cens,
              effect_time,
              logrank_power,
              maxcombo_power,
              rmst_power
            )
          )
      }
    }
  }
}

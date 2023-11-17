library(gt)
library(dplyr)


results <- lapply(list(
  list(
    data = "cc_results" ,
    scenario_name = "(Crossing Curves)",
    var = "shape",
    filter_statement = "(shape * 10) %% 5 == 0 & (p_cens * 100) %% 10 == 0"
  ),
  list(
    data = "le_results" ,
    scenario_name = "(Late Effect)",
    var = "effect_time",
    filter_statement = "(effect_time * 500) %% 100 == 0 & (p_cens * 100) %% 10 == 0"
  ),
  list(
    data = "ee_results" ,
    scenario_name = "(Early Effect)",
    var = "effect_time",
    filter_statement = "(effect_time * 1000) %% 100 == 0 & (p_cens * 100) %% 10 == 0"
  )
),
function (description) {
  filter_statement <- description[["filter_statement"]]
  results_list <- list()
  for (n_ in n_list) {
    temp <- get(description[["data"]]) |> filter(n == n_) |>
      select(
        c(
          "t_duration",
          "p_cens",
          description[["var"]],
          "logrank_power",
          "maxcombo_power",
          "rmst_power"
        )
      ) |>
      rename_with(~ paste0(.x, "_", n_, recycle0 = TRUE), ends_with("power"))
    results_list[[length(results_list) + 1]] <- temp
  }
  results_combined <- results_list[[1]]
  for (res in results_list) {
    results_combined <- merge(results_combined, res)
  }
  # Sort by t_duration, p_cens and shape
  results_combined <-
    results_combined[with(results_combined, order(t_duration, p_cens)), ]
  
  results_combined$t_duration_formatted <-
    paste0("Trial Duration: ", results_combined$t_duration)
  
  # Filter only some values of shape and p_cens to keep table size small
  table <- results_combined |>
    filter(eval(parse(text=filter_statement))) |>
    group_by(t_duration_formatted) |> gt() |>
    tab_header(title = paste0("Test Power ", description[["scenario_name"]])) |>
    tab_spanner(label = "N = 50",
                columns = ends_with("_50")) |>
    tab_spanner(label = "N = 100",
                columns = ends_with("_100")) |>
    tab_spanner(label = "N = 200",
                columns = ends_with("_200")) |>
    cols_label(
      starts_with("logrank_power") ~ "Logrank",
      starts_with("maxcombo_power") ~ "MaxCombo",
      starts_with("rmst_power") ~ "RMST",
      p_cens = "{{P_censoring}}"
    ) |>
    cols_width(
      starts_with("logrank_power") ~ px(100),
      starts_with("maxcombo_power") ~ px(100),
      starts_with("rmst_power") ~ px(100)
    ) |>
    cols_align(align = "center",
               columns = contains("power")) |>
    cols_align(align = "left",
               columns = c(description[["var"]], "p_cens")) |>
    cols_hide(t_duration)
  
  return(table)
})


results[[1]] |> cols_label(shape = "{{Shape}}") |>
  gtsave("Crossing_curves.html")

results[[2]] |> cols_label(effect_time = "{{t_effect}}") |>
  gtsave("Late_effect.html")

results[[3]] |> cols_label(effect_time = "{{t_effect}}") |>
  gtsave("Early_effect.html")

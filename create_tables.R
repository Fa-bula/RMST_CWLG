library(gt)
library(dplyr)

cc_results_list <- list()
for (n_ in n_list) {
  temp <- cc_results |> filter(n == n_) |>
    select(c(
      "p_cens",
      "shape",
      "logrank_power",
      "maxcombo_power",
      "rmst_power"
    )) |>
    rename_with( ~ paste0(.x, "_", n_, recycle0 = TRUE), ends_with("power"))
  cc_results_list[[length(cc_results_list) + 1]] <- temp
}

cc_results_combined <- cc_results_list[[1]]
for (res in cc_results_list) {
  cc_results_combined <- merge(cc_results_combined, res)
}

# Sort by p_cens and shape
cc_results_combined <-
  cc_results_combined[with(cc_results_combined, order(p_cens, shape)),]

# Filter only some values of shape and p_cens to keep table size small
cc_results_combined |> filter((shape * 10) %% 5 == 0 &
                                (p_cens * 100) %% 10 == 0) |>
  gt() |> tab_header(title = "Test Power (Crossing Curves)") |>
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
    p_cens = "{{P_censoring}}",
    shape = "Shape"
  ) |>
  cols_width(
    shape ~ px(50),
    starts_with("logrank_power") ~ px(100),
    starts_with("maxcombo_power") ~ px(100),
    starts_with("rmst_power") ~ px(100)
  ) |>
  cols_align(align = "center",
             columns = contains("power")) |>
  cols_align(align = "left",
             columns = c("shape", "p_cens"))

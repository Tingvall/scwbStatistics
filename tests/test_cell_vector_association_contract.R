test_dispatch_and_output_schema <- function() {
  f <- statistics_fixture(); on.exit(wb_close_bundle(f$bundle), add = TRUE)
  categorical <- statistics_execute(f, "cell_vector_association",
    list(a = f$groups, b = f$groups_b), list(min_n_per_group = 3L, method = "auto"))
  grouped <- statistics_execute(f, "cell_vector_association",
    list(a = f$continuous, b = f$groups), list(min_n_per_group = 3L, method = "auto"))
  correlated <- statistics_execute(f, "cell_vector_association",
    list(a = f$continuous, b = f$reverse), list(min_n_per_group = 3L, method = "auto"))
  stopifnot(
    setequal(names(categorical$outputs), c("summary", "detail")),
    categorical$outputs$summary$table$association_type == "categorical_x_categorical",
    grouped$outputs$summary$table$association_type == "continuous_x_categorical",
    correlated$outputs$summary$table$association_type == "continuous_x_continuous",
    categorical$outputs$summary$table$effect_size_name == "cramers_v",
    "min_expected" %in% names(categorical$outputs$summary$table),
    "spearman" %in% names(correlated$outputs$summary$table))
  TRUE
}

test_six_operation_effective_outputs <- function() {
  f <- statistics_fixture(); on.exit(wb_close_bundle(f$bundle), add = TRUE)
  results <- list(
    de = statistics_execute(f, "de_wilcox", list(matrix = f$matrix), list(
      group_a = as.list(f$cells[1:6]), group_b = as.list(f$cells[7:12]),
      min_pct = 0, p_adjust_method = "BH")),
    markers = statistics_execute(f, "markers_topn",
      list(matrix = f$matrix, clusters = f$groups),
      list(top_n = 2L, min_pct = 0, p_adjust_method = "BH")),
    assoc = statistics_execute(f, "cell_vector_assoc",
      list(vector_cont = f$continuous, vector_group = f$groups),
      list(min_n_per_group = 3L)),
    association = statistics_execute(f, "cell_vector_association",
      list(a = f$groups, b = f$groups_b),
      list(min_n_per_group = 3L, method = "auto")),
    compare = statistics_execute(f, "cell_vector_compare",
      list(a = f$continuous, b = f$reverse),
      list(compare_space = "rank", ties_method = "average",
        signal_a = "forward", signal_b = "reverse")),
    trend = statistics_execute(f, "feature_trend_spearman",
      list(matrix = f$matrix, pseudotime = f$continuous),
      list(min_pct_nonzero = 0, min_sd = 0, p_adjust_method = "BH", chunk_size = 2L))
  )
  expected <- list(de = "de_table", markers = "markers",
    assoc = c("assoc_summary", "group_stats"), association = c("summary", "detail"),
    compare = c("summary", "comparison"), trend = "trend_table")
  stopifnot(all(vapply(results, function(x) identical(x$state, "reopened"), logical(1))))
  for (name in names(results)) stopifnot(setequal(names(results[[name]]$outputs), expected[[name]]))
  stopifnot(
    results$de$outputs$de_table$table_type == "de_wilcox",
    results$markers$outputs$markers$table_type == "markers_topn",
    results$assoc$outputs$assoc_summary$table_type == "cell_vector_assoc",
    results$association$outputs$summary$table$association_type == "categorical_x_categorical",
    nrow(results$compare$outputs$comparison$table) == length(f$cells),
    identical(results$trend$reader_observation$observed_batches,
      list(f$features[1:2], f$features[3:4], f$features[5])))
  TRUE
}

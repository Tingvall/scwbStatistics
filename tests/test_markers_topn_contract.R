test_feature_subset_top_n_and_completion <- function() {
  f <- statistics_fixture(); on.exit(wb_close_bundle(f$bundle), add = TRUE)
  result <- statistics_execute(f, "markers_topn", list(matrix = f$matrix, clusters = f$groups),
    list(features = as.list(f$features[1:2]), top_n = 1L, min_pct = 0,
      p_adjust_method = "BH"))
  table <- result$outputs$markers$table
  stopifnot(result$state == "reopened", all(table$feature_id %in% f$features[1:2]),
    nrow(table) <= 2L, identical(result$outputs$markers$table_type, "markers_topn"))
  TRUE
}

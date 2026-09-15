library(scworkbench)

activated <- wb_operation_pack_load("scwbStatistics")
stopifnot(length(activated$operations) == 6L)
again <- wb_operation_pack_load("scwbStatistics")
stopifnot(identical(activated$identity$pack_sha256, again$identity$pack_sha256))

bundle <- wb_init_bundle(tempfile("scwbStatistics-contract-"))
execute <- getFromNamespace(".wb_operation_pack_v2_execute", "scworkbench")
cells <- paste0("cell_", sprintf("%02d", 1:12))
features <- paste0("gene_", 1:5)
values <- matrix(c(
  0:11, 11:0, rep(c(0, 1), 6), rep(c(4, 2, 0), 4),
  c(0, 0, 1, 0, 2, 1, 3, 1, 5, 2, 8, 3)
), nrow = 5L, byrow = TRUE, dimnames = list(features, cells))
matrix_input <- structure(list(kind = "matrix", matrix_id = "fixture_matrix",
  values = values, dimensions = as.integer(dim(values)), cell_id = cells,
  feature_id = features, storage_profile = "matrix_ordinary_v1"),
  class = "wb_primitive")
continuous <- wb_prim_cell_vector(stats::setNames(seq_along(cells), cells), "continuous")
continuous_b <- wb_prim_cell_vector(stats::setNames(rev(seq_along(cells)), cells), "continuous")
groups <- wb_prim_cell_vector(stats::setNames(rep(c("A", "B"), each = 6L), cells), "categorical")
groups_b <- wb_prim_cell_vector(stats::setNames(rep(c("X", "Y"), 6L), cells), "categorical")

run <- function(name, inputs, params = list()) execute(bundle,
  paste0("scwbstatistics::", name), inputs, params)
expect_code <- function(expr, code) {
  error <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(error, "wb_operation_pack_error"),
    identical(as.character(error$code), code))
  invisible(error)
}

de <- run("de_wilcox", list(matrix = matrix_input), list(
  group_a = as.list(cells[1:6]), group_b = as.list(cells[7:12]),
  min_pct = 0, p_adjust_method = "BH"))
stopifnot(nrow(de$outputs$de_table$table) == 5L)

markers <- run("markers_topn", list(matrix = matrix_input, clusters = groups),
  list(top_n = 2L, min_pct = 0, p_adjust_method = "BH"))
stopifnot(identical(markers$outputs$markers$table_type, "markers_topn"))

assoc <- run("cell_vector_assoc", list(vector_cont = continuous,
  vector_group = groups), list(min_n_per_group = 3L))
stopifnot(setequal(names(assoc$outputs), c("assoc_summary", "group_stats")))

categorical <- run("cell_vector_association", list(a = groups, b = groups_b),
  list(min_n_per_group = 3L, method = "auto"))
grouped <- run("cell_vector_association", list(a = continuous, b = groups),
  list(min_n_per_group = 3L, method = "auto"))
correlated <- run("cell_vector_association", list(a = continuous, b = continuous_b),
  list(min_n_per_group = 3L, method = "auto"))
stopifnot(categorical$outputs$summary$table$association_type == "categorical_x_categorical",
  grouped$outputs$summary$table$association_type == "continuous_x_categorical",
  correlated$outputs$summary$table$association_type == "continuous_x_continuous")
chisq <- run("cell_vector_association", list(a = groups, b = groups_b),
  list(min_n_per_group = 3L, method = "chisq"))
kruskal <- run("cell_vector_association", list(a = continuous, b = groups),
  list(min_n_per_group = 3L, method = "kruskal_anova"))
correlation <- run("cell_vector_association", list(a = continuous, b = continuous_b),
  list(min_n_per_group = 3L, method = "correlation"))
stopifnot(chisq$outputs$summary$table$method == "chi_square",
  kruskal$outputs$summary$table$method == "kruskal_anova",
  correlation$outputs$summary$table$method == "correlation")
expect_code(run("cell_vector_association", list(a = continuous, b = groups),
  list(min_n_per_group = 3L, method = "correlation")), "PYBENCH_PROCESS_FAILED")
expect_code(run("cell_vector_association", list(a = continuous, b = continuous_b),
  list(min_n_per_group = 3L, method = "fisher")), "PYBENCH_PROCESS_FAILED")

comparison <- run("cell_vector_compare", list(a = continuous, b = continuous_b),
  list(compare_space = "rank", ties_method = "average", signal_a = "a", signal_b = "b"))
stopifnot(nrow(comparison$outputs$comparison$table) == length(cells))
reordered <- cells[c(8, 2, 11, 4, 1, 7, 5, 10, 3, 9, 6, 12)]
comparison_reordered <- run("cell_vector_compare", list(
  a = wb_prim_cell_vector(stats::setNames(seq_along(reordered), reordered), "continuous"),
  b = wb_prim_cell_vector(stats::setNames(rev(seq_along(reordered)), reordered), "continuous")),
  list(compare_space = "raw", ties_method = "average", signal_a = "a", signal_b = "b"))
stopifnot(identical(comparison_reordered$outputs$comparison$table$cell_id, reordered))

tied_missing_a <- wb_prim_cell_vector(stats::setNames(
  c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, NA, 6), cells), "continuous")
tied_missing_b <- wb_prim_cell_vector(stats::setNames(
  c(6, 6, 5, 5, 4, 4, 3, 3, 2, 2, 1, NA), cells), "continuous")
tied_missing <- run("cell_vector_compare", list(
  a = tied_missing_a, b = tied_missing_b), list(compare_space = "rank",
  ties_method = "average", signal_a = "tied_a", signal_b = "tied_b"))
stopifnot(
  nrow(tied_missing$outputs$comparison$table) == 10L,
  anyDuplicated(tied_missing$outputs$comparison$table$a_cmp) > 0L,
  !anyNA(tied_missing$outputs$comparison$table)
)

missing_group_values <- rep(c("A", "B"), each = 6L)
missing_group_values[c(2L, 11L)] <- NA_character_
missing_grouped <- run("cell_vector_assoc", list(
  vector_cont = continuous,
  vector_group = wb_prim_cell_vector(stats::setNames(
    missing_group_values, cells), "categorical")),
  list(min_n_per_group = 3L))
stopifnot(missing_grouped$outputs$assoc_summary$table$n_cells == 10L)

trend_a <- run("feature_trend_spearman", list(matrix = matrix_input,
  pseudotime = continuous), list(min_pct_nonzero = 0, min_sd = 0,
  p_adjust_method = "BH", chunk_size = 2L))
trend_b <- run("feature_trend_spearman", list(matrix = matrix_input,
  pseudotime = continuous), list(min_pct_nonzero = 0, min_sd = 0,
  p_adjust_method = "BH", chunk_size = 128L))
stopifnot(identical(trend_a$outputs$trend_table$table,
  trend_b$outputs$trend_table$table))
stopifnot(identical(trend_a$reader_observation$observed_batches,
  list(features[1:2], features[3:4], features[5])))
stopifnot(identical(as.integer(trend_a$reader_observation$observed_batch_sizes),
  c(2L, 2L, 1L)))
stopifnot(identical(as.integer(trend_b$reader_observation$observed_call_count), 1L),
  identical(trend_b$reader_observation$observed_batches, list(features)))
trend_order <- match(features, trend_a$outputs$trend_table$table$feature_id)
raw_p <- trend_a$outputs$trend_table$table$p_val[trend_order]
incorrect_chunk_adjustment <- unlist(lapply(split(raw_p,
  ceiling(seq_along(raw_p) / 2L)), stats::p.adjust, method = "BH"), use.names = FALSE)
stopifnot(any(abs(trend_a$outputs$trend_table$table$p_val_adj[trend_order] -
  incorrect_chunk_adjustment) > 1e-15))

empty_matrix <- matrix_input
empty_matrix$values[] <- 0
empty_de <- run("de_wilcox", list(matrix = empty_matrix), list(
  group_a = as.list(cells[1:6]), group_b = as.list(cells[7:12]),
  min_pct = 1, p_adjust_method = "BH"))
stopifnot(nrow(empty_de$outputs$de_table$table) == 0L)
expect_code(run("de_wilcox", list(matrix = matrix_input), list(
  group_a = as.list(cells[1]), group_b = as.list(cells[7:12]),
  min_pct = 0, p_adjust_method = "BH")), "PYBENCH_INPUT_IDENTITY_MISMATCH")
expect_code(run("markers_topn", list(matrix = matrix_input,
  clusters = wb_prim_cell_vector(stats::setNames(rep("one", 12L), cells))),
  list(top_n = 2L, min_pct = 0, p_adjust_method = "BH")),
  "PYBENCH_PROCESS_FAILED")
expect_code(run("feature_trend_spearman", list(matrix = matrix_input,
  pseudotime = wb_prim_cell_vector(stats::setNames(rep(1, 12L), cells), "continuous")),
  list(min_pct_nonzero = 0, min_sd = 0, p_adjust_method = "BH", chunk_size = 2L)),
  "PYBENCH_PROCESS_FAILED")

records <- vapply(list(de, markers, assoc, categorical, grouped, correlated,
  chisq, kruskal, correlation, comparison, comparison_reordered, trend_a,
  trend_b, empty_de, tied_missing, missing_grouped),
  function(value) value$state, character(1))
stopifnot(all(records == "reopened"))
wb_close_bundle(bundle)
cat("SCWBSTATISTICS_PACK_CONTRACTS_OK operations=6 association_paths=6 edge_cases=6\n")

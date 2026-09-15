statistics_activate_pack <- function() {
  core_source <- Sys.getenv("SCWORKBENCH_TEST_CORE_SOURCE", unset = "")
  if (nzchar(core_source)) {
    if (!requireNamespace("devtools", quietly = TRUE)) stop("devtools is required for source tests")
    devtools::load_all(core_source, quiet = TRUE)
  } else {
    library(scworkbench)
  }
  source <- Sys.getenv("SCWBSTATISTICS_TEST_SOURCE", unset = "")
  if (nzchar(source)) {
    if (!requireNamespace("devtools", quietly = TRUE)) stop("devtools is required for source tests")
    devtools::load_all(source, quiet = TRUE)
  } else if (!requireNamespace("scwbStatistics", quietly = TRUE)) {
    stop("scwbStatistics is not installed")
  }
  if (nzchar(source)) {
    wb_operation_pack_activate(file.path(source, "inst", "scworkbench",
      "operation-pack.json"))
  } else {
    wb_operation_pack_load("scwbStatistics")
  }
  invisible(TRUE)
}

statistics_fixture <- function() {
  statistics_activate_pack()
  bundle <- wb_init_bundle(tempfile("scwbStatistics-test-"))
  cells <- paste0("cell_", sprintf("%02d", 1:12))
  features <- paste0("gene_", 1:5)
  values <- matrix(c(0:11, 11:0, rep(c(0, 1), 6), rep(c(4, 2, 0), 4),
    c(0, 0, 1, 0, 2, 1, 3, 1, 5, 2, 8, 3)), nrow = 5L, byrow = TRUE,
    dimnames = list(features, cells))
  matrix_input <- structure(list(kind = "matrix", matrix_id = "fixture_matrix",
    values = values, dimensions = as.integer(dim(values)), cell_id = cells,
    feature_id = features, storage_profile = "matrix_ordinary_v1"),
    class = "wb_primitive")
  list(bundle = bundle, cells = cells, features = features, matrix = matrix_input,
    continuous = wb_prim_cell_vector(stats::setNames(seq_along(cells), cells), "continuous"),
    reverse = wb_prim_cell_vector(stats::setNames(rev(seq_along(cells)), cells), "continuous"),
    groups = wb_prim_cell_vector(stats::setNames(rep(c("A", "B"), each = 6L), cells), "categorical"),
    groups_b = wb_prim_cell_vector(stats::setNames(rep(c("X", "Y"), 6L), cells), "categorical"))
}

statistics_execute <- function(fixture, name, inputs, params = list()) {
  getFromNamespace(".wb_operation_pack_v2_execute", "scworkbench")(
    fixture$bundle, paste0("scwbstatistics::", name), inputs, params)
}

statistics_expect_code <- function(expr, code) {
  error <- tryCatch(force(expr), error = identity)
  stopifnot(inherits(error, "wb_operation_pack_error"),
    identical(as.character(error$code), code))
  invisible(error)
}

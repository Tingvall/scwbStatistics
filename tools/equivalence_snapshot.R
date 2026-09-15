args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("usage: equivalence_snapshot.R <core|pack> <output.rds>", call. = FALSE)
}

mode <- args[[1]]
output <- args[[2]]
if (!mode %in% c("core", "pack")) stop("invalid comparator mode", call. = FALSE)

suppressPackageStartupMessages(library(scworkbench))
if (identical(mode, "pack")) wb_operation_pack_load("scwbStatistics")

short_names <- c(
  "de_wilcox", "markers_topn", "cell_vector_assoc",
  "cell_vector_association", "cell_vector_compare",
  "feature_trend_spearman"
)

normalize_interface <- function(value) {
  value$op_name <- sub("^scwbstatistics::", "", value$op_name)
  value$source_pack <- NULL
  value
}

bundle <- wb_init_bundle(tempfile("scwbStatistics-equivalence-"))
on.exit(wb_close_bundle(bundle), add = TRUE)
cells <- paste0("cell_", sprintf("%02d", seq_len(12L)))
features <- paste0("gene_", seq_len(5L))
values <- matrix(c(
  0:11, 11:0, rep(c(0, 1), 6L), rep(c(2, 2, 3), 4L), rep(1, 12L)
), nrow = 5L, byrow = TRUE, dimnames = list(features, cells))
wb_define_cells(bundle, cells, metadata = data.frame(
  cell_id = cells, forward = seq_along(cells), reverse = rev(seq_along(cells)),
  group = rep(c("A", "B"), each = 6L), stringsAsFactors = FALSE
))
wb_attach_matrix(bundle, "RNA_counts", values, feature_type = "gene")

continuous <- wb_prim_cell_vector(
  stats::setNames(seq_along(cells), cells), value_type = "continuous"
)
reverse <- wb_prim_cell_vector(
  stats::setNames(rev(seq_along(cells)), cells), value_type = "continuous"
)
groups <- wb_prim_cell_vector(
  stats::setNames(rep(c("A", "B"), each = 6L), cells),
  value_type = "categorical"
)
groups_b <- wb_prim_cell_vector(
  stats::setNames(rep(c("X", "Y"), 6L), cells), value_type = "categorical"
)

run_case <- function(name, inputs) {
  result <- wb_session_run_op(wb_session_new(), bundle, name,
    inputs = utils::modifyList(list(cell_set = "all", min_cells = 1L), inputs))
  run <- result$run
  run$run_id <- NULL
  run$created_at <- NULL
  run$op_name <- sub("^scwbstatistics::", "", run$op_name)
  outputs <- run$outputs[sort(names(run$outputs), method = "radix")]
  identities <- lapply(outputs, function(primitive) {
    unname(scworkbench:::.wb_prov_payload_identity(bundle, primitive)$content_sha256)
  })
  list(
    inputs = run$inputs,
    effective_params = run$params,
    outputs = outputs,
    output_identities = identities
  )
}

cases <- list(
  de_wilcox = run_case("de_wilcox", list(
    matrix = "RNA_counts", group_a = cells[1:6], group_b = cells[7:12],
    min_pct = 0, p_adjust_method = "BH"
  )),
  markers_topn = run_case("markers_topn", list(
    matrix = "RNA_counts", clusters = groups, top_n = 2L, min_pct = 0,
    p_adjust_method = "BH"
  )),
  cell_vector_assoc = run_case("cell_vector_assoc", list(
    vector_cont = continuous, vector_group = groups, min_n_per_group = 3L
  )),
  cell_vector_association_categorical = run_case("cell_vector_association", list(
    a = groups, b = groups_b, min_n_per_group = 3L, method = "auto"
  )),
  cell_vector_association_grouped = run_case("cell_vector_association", list(
    a = continuous, b = groups, min_n_per_group = 3L, method = "auto"
  )),
  cell_vector_association_continuous = run_case("cell_vector_association", list(
    a = continuous, b = reverse, min_n_per_group = 3L, method = "auto"
  )),
  cell_vector_compare = run_case("cell_vector_compare", list(
    a = continuous, b = reverse, compare_space = "rank",
    ties_method = "average", signal_a = "forward", signal_b = "reverse"
  )),
  feature_trend_chunked = run_case("feature_trend_spearman", list(
    matrix = "RNA_counts", pseudotime = continuous, min_pct_nonzero = 0,
    min_sd = 0, p_adjust_method = "BH", chunk_size = 2L
  )),
  feature_trend_unchunked = run_case("feature_trend_spearman", list(
    matrix = "RNA_counts", pseudotime = continuous, min_pct_nonzero = 0,
    min_sd = 0, p_adjust_method = "BH", chunk_size = 128L
  ))
)

interfaces <- stats::setNames(lapply(short_names, function(name) {
  normalize_interface(wb_op_interface(name))
}), short_names)

pack <- NULL
if (identical(mode, "pack")) {
  inspected <- wb_operation_pack_inspect("scwbstatistics")
  pack <- list(
    pack_identity = inspected$identity$pack_identity,
    manifest_source_identity = inspected$identity$manifest_source_identity,
    manifest_identity = inspected$identity$manifest_identity,
    distribution_identity = inspected$identity$distribution_identity,
    entry_point_identity = inspected$identity$entry_point_identity,
    operation_definition_identities = inspected$identity$operation_definition_identities
  )
}

saveRDS(list(
  mode = mode,
  fixture_identity = digest::digest(list(
    cells = cells, features = features, values = unname(values),
    dimnames = dimnames(values)
  ), algo = "sha256", serialize = TRUE),
  interfaces = interfaces,
  cases = cases,
  pack = pack
), output, version = 3L)

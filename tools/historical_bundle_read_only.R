args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("usage: historical_bundle_read_only.R <output.json> <bundle>...", call. = FALSE)
}

output <- args[[1]]
bundles <- args[-1]
suppressPackageStartupMessages(library(scworkbench))

tree_identity <- function(root) {
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE,
    no.. = TRUE, include.dirs = FALSE)
  relative <- substring(files, nchar(normalizePath(root)) + 2L)
  rows <- Map(function(path, rel) list(
    path = rel,
    size = unname(file.info(path)$size),
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE)
  ), files, relative)
  digest::digest(rows[order(relative, method = "radix")], algo = "sha256",
    serialize = TRUE)
}

inspect_one <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  before <- tree_identity(path)
  manifest_readable <- !inherits(tryCatch(
    jsonlite::read_json(file.path(path, "manifest.json"), simplifyVector = FALSE),
    error = identity), "error")
  bundle <- wb_open_bundle(path, validate = FALSE, read_only = TRUE)
  runs <- DBI::dbGetQuery(bundle$con, paste(
    "SELECT snapshot_id, run_id, op_name, contract_hash",
    "FROM prov_runs WHERE op_name IN ('de_wilcox', 'cell_vector_association')",
    "ORDER BY snapshot_id, run_id"
  ))
  outputs <- if (nrow(runs)) DBI::dbGetQuery(bundle$con, paste0(
    "SELECT snapshot_id, run_id, output_name, primitive_kind, artifact_id ",
    "FROM prov_run_outputs WHERE run_id IN (",
    paste(rep("?", length(unique(runs$run_id))), collapse = ","),
    ") ORDER BY snapshot_id, run_id, output_name"
  ), params = as.list(unique(runs$run_id))) else data.frame()
  artifact_ids <- unique(as.character(if ("artifact_id" %in% names(outputs)) {
    outputs$artifact_id
  } else character()))
  artifact_ids <- artifact_ids[!is.na(artifact_ids) & nzchar(artifact_ids)]
  loaded <- stats::setNames(lapply(artifact_ids, function(id) {
    primitive <- wb_art_load(bundle, id)
    list(kind = as.character(primitive$kind),
      content_sha256 = scworkbench:::.wb_prov_payload_identity(
        bundle, primitive)$content_sha256)
  }), artifact_ids)
  wb_close_bundle(bundle)
  after <- tree_identity(path)
  list(
    path = path,
    manifest_readable = manifest_readable,
    opened_read_only = isTRUE(bundle$read_only),
    fingerprint_before = before,
    fingerprint_after = after,
    unchanged = identical(before, after),
    matching_run_count = nrow(runs),
    output_count = nrow(outputs),
    runs = runs,
    outputs = outputs,
    loaded_artifacts = loaded
  )
}

results <- lapply(bundles, inspect_one)
passed <- all(vapply(results, function(x) {
  isTRUE(x$manifest_readable) && isTRUE(x$opened_read_only) &&
    isTRUE(x$unchanged) && x$matching_run_count > 0L
}, logical(1)))
evidence <- list(
  schema_version = "scwbStatistics.phase3a.historical_read_only.v1",
  disposition = if (passed) "passed" else "failed",
  packs_active = 0L,
  bundles = results
)
jsonlite::write_json(evidence, output, auto_unbox = TRUE, pretty = TRUE,
  null = "null", na = "null")
cat(sprintf("PHASE3A_HISTORICAL_READ_ONLY %s bundles=%d runs=%d outputs=%d\n",
  evidence$disposition, length(results),
  sum(vapply(results, `[[`, integer(1), "matching_run_count")),
  sum(vapply(results, `[[`, integer(1), "output_count"))))
if (!passed) quit(status = 1L)

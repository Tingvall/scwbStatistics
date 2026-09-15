args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) {
  stop(paste(
    "usage: run_equivalence_comparator.R <base-core> <candidate-core>",
    "<pack-source> <candidate-core-commit> <pack-commit>"
  ), call. = FALSE)
}

base_core <- normalizePath(args[[1]], mustWork = TRUE)
candidate_core <- normalizePath(args[[2]], mustWork = TRUE)
pack_source <- normalizePath(args[[3]], mustWork = TRUE)
candidate_commit <- args[[4]]
pack_commit <- args[[5]]
script <- normalizePath(file.path(pack_source, "tools", "equivalence_snapshot.R"),
  mustWork = TRUE)

install_source <- function(source, library, log) {
  status <- system2(file.path(R.home("bin"), "R"), c(
    "CMD", "INSTALL", "--no-byte-compile", paste0("--library=", library), source
  ), stdout = log, stderr = log)
  if (!identical(status, 0L)) {
    stop(paste(readLines(log, warn = FALSE), collapse = "\n"), call. = FALSE)
  }
}

run_snapshot <- function(mode, library, output) {
  env <- paste0("R_LIBS=", library)
  log <- paste0(output, ".log")
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", script, mode, output), env = env, stdout = log, stderr = log)
  if (!identical(status, 0L)) {
    stop(paste(readLines(log, warn = FALSE), collapse = "\n"), call. = FALSE)
  }
}

root <- tempfile("scwbStatistics-comparator-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
old_lib <- file.path(root, "old-lib"); dir.create(old_lib)
new_lib <- file.path(root, "new-lib"); dir.create(new_lib)
install_source(base_core, old_lib, file.path(root, "install-old.log"))
install_source(candidate_core, new_lib, file.path(root, "install-new.log"))
install_source(pack_source, new_lib, file.path(root, "install-pack.log"))
old_path <- file.path(root, "old.rds")
new_path <- file.path(root, "new.rds")
run_snapshot("core", old_lib, old_path)
run_snapshot("pack", new_lib, new_path)
old <- readRDS(old_path)
new <- readRDS(new_path)

compare_component <- function(name, left, right) {
  list(name = name, passed = identical(left, right))
}

interface_checks <- Map(function(name, left, right) {
  compare_component(name, left, right)
}, names(old$interfaces), old$interfaces, new$interfaces)
case_checks <- lapply(names(old$cases), function(name) {
  fields <- c("inputs", "effective_params", "outputs", "output_identities")
  checks <- lapply(fields, function(field) {
    compare_component(field, old$cases[[name]][[field]], new$cases[[name]][[field]])
  })
  list(name = name, passed = all(vapply(checks, `[[`, logical(1), "passed")),
    fields = checks, output_identities = new$cases[[name]]$output_identities)
})

trend_policy <- identical(
  new$cases$feature_trend_chunked$output_identities,
  new$cases$feature_trend_unchunked$output_identities
)
passed <- identical(old$fixture_identity, new$fixture_identity) &&
  all(vapply(interface_checks, `[[`, logical(1), "passed")) &&
  all(vapply(case_checks, `[[`, logical(1), "passed")) && trend_policy

evidence <- list(
  schema_version = "scwbStatistics.phase3a.equivalence.v1",
  disposition = if (passed) "passed" else "failed",
  source = list(
    base_scworkbench_commit = "42d0c09a8c4abae2174bbbf12741c7303fde32e0",
    candidate_scworkbench_commit = candidate_commit,
    candidate_scwbStatistics_commit = pack_commit
  ),
  fixture_identity = old$fixture_identity,
  intentional_exclusions = c(
    "ephemeral run identifier", "execution timestamp", "qualified operation name",
    "pack and operation-definition identities", "runtime and installation paths"
  ),
  identity_transition = list(
    historical_unqualified_definition_preserved = TRUE,
    prospective_namespace = "scwbstatistics",
    pack = new$pack
  ),
  interface_checks = interface_checks,
  execution_checks = case_checks,
  terminal_policy_checks = list(
    feature_trend_chunked_equals_unchunked = trend_policy,
    policy = "exact_canonical_value"
  )
)

output <- file.path(pack_source, "notes", "phase3a_statistics_equivalence.json")
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
json <- jsonlite::toJSON(evidence, auto_unbox = TRUE, pretty = TRUE,
  null = "null", digits = NA)
writeLines(json, output, useBytes = TRUE)
cat(sprintf("PHASE3A_EQUIVALENCE %s interfaces=%d cases=%d output=%s\n",
  evidence$disposition, length(interface_checks), length(case_checks), output))
if (!passed) quit(status = 1L)

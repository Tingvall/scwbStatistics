args <- commandArgs(trailingOnly = TRUE)
cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd, value = TRUE)
script <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
test_dir <- if (nzchar(script)) {
  normalizePath(dirname(script), mustWork = TRUE)
} else if (file.exists("run_contract_tests.R") && file.exists("helper_statistics_fixture.R")) {
  normalizePath(".", mustWork = TRUE)
} else {
  normalizePath("tests", mustWork = TRUE)
}
repo <- normalizePath(file.path(test_dir, ".."), mustWork = FALSE)
checking <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = ""))
if (!checking && file.exists(file.path(repo, "DESCRIPTION"))) {
  Sys.setenv(SCWBSTATISTICS_TEST_SOURCE = repo)
} else {
  Sys.unsetenv("SCWBSTATISTICS_TEST_SOURCE")
}
selected <- if (length(args) == 2L && identical(args[[1]], "--test-file")) args[[2]] else NULL
files <- sort(list.files(test_dir, pattern = "^test_.*\\.R$", full.names = TRUE))
if (!is.null(selected)) {
  requested <- normalizePath(if (file.exists(selected)) selected else file.path(test_dir, basename(selected)),
    mustWork = FALSE)
  files <- files[normalizePath(files, mustWork = TRUE) == requested]
  if (length(files) != 1L) stop("unknown focused test file: ", selected)
}
source(file.path(test_dir, "helper_statistics_fixture.R"), local = globalenv())
records <- list()
for (file in files) {
  env <- new.env(parent = globalenv())
  sys.source(file, envir = env)
  tests <- sort(ls(env, pattern = "^test_"))
  if (!length(tests)) stop("no tests declared in ", file)
  for (name in tests) {
    cat("RUN ", name, " [", basename(file), "]\n", sep = "")
    started <- proc.time()[["elapsed"]]
    value <- tryCatch(get(name, env)(), error = identity)
    elapsed <- proc.time()[["elapsed"]] - started
    if (inherits(value, "error") || !isTRUE(value)) {
      message <- if (inherits(value, "error")) conditionMessage(value) else "did not return TRUE"
      cat("FAIL ", name, " -- ", message, "\n", sep = "")
      records[[length(records) + 1L]] <- FALSE
    } else {
      cat("PASS ", name, sprintf(" (%.3fs)\n", elapsed), sep = "")
      records[[length(records) + 1L]] <- TRUE
    }
  }
}
passed <- sum(unlist(records)); failed <- length(records) - passed
cat("SCWBSTATISTICS TEST SUMMARY files=", length(files), " discovered=", length(records),
  " executed=", length(records), " passed=", passed, " skipped=0 failed=", failed, "\n", sep = "")
if (failed) quit(status = 1L)

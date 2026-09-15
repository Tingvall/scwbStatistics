test_statistics_session_parameter_documentation <- function() {
  statistics_activate_pack()
  expected <- list(de_wilcox = c("group_a", "group_b"),
    cell_vector_association = "cell_set")
  for (name in names(expected)) {
    params <- vapply(wb_op_interface(name)$params, `[[`, character(1), "name")
    stopifnot(all(expected[[name]] %in% params))
  }
  TRUE
}

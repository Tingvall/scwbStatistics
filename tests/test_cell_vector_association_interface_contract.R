test_effective_cell_coverage_declaration <- function() {
  statistics_activate_pack()
  iface <- wb_op_interface("cell_vector_association")
  stopifnot(identical(iface$inputs$a$effective_cell_coverage, "cell_indexed"),
    identical(iface$inputs$b$effective_cell_coverage, "cell_indexed"))
  TRUE
}

test_declared_output_kinds <- function() {
  statistics_activate_pack()
  iface <- wb_op_interface("cell_vector_compare")
  stopifnot(identical(iface$produces$summary$kind, "obs_table"),
    identical(iface$produces$comparison$kind, "obs_table"))
  TRUE
}

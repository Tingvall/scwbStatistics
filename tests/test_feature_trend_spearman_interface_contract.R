test_feature_axis_chunk_declaration <- function() {
  statistics_activate_pack()
  iface <- wb_op_interface("feature_trend_spearman")
  stopifnot(isTRUE(iface$chunking$chunkable),
    identical(as.character(iface$chunking$axis), "features"),
    identical(as.character(iface$chunking$status), "implemented_generic"),
    identical(as.character(iface$chunking$chunk_size_param), "chunk_size"))
  TRUE
}

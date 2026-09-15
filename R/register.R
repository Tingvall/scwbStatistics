#' Register the scwbStatistics operation pack
#'
#' Installation is inert. This zero-argument entry point is invoked only by
#' scworkbench's explicit operation-pack activation boundary.
#'
#' @return A validated scworkbench operation-pack registration.
#' @export
register_operation_pack <- function() {
  scworkbench::wb_operation_pack_register(
    system.file("scworkbench", "operation-pack.json", package = "scwbStatistics"),
    list(
      `scwbstatistics::de_wilcox` = .run_de_wilcox,
      `scwbstatistics::markers_topn` = .run_markers_topn,
      `scwbstatistics::cell_vector_assoc` = .run_cell_vector_assoc,
      `scwbstatistics::cell_vector_association` = .run_cell_vector_association,
      `scwbstatistics::cell_vector_compare` = .run_cell_vector_compare,
      `scwbstatistics::feature_trend_spearman` = .run_feature_trend_spearman
    )
  )
}


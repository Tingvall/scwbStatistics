\name{scwbStatistics-operations}
\alias{scwbstatistics::de_wilcox}
\alias{scwbstatistics::markers_topn}
\alias{scwbstatistics::cell_vector_assoc}
\alias{scwbstatistics::cell_vector_association}
\alias{scwbstatistics::cell_vector_compare}
\alias{scwbstatistics::feature_trend_spearman}
\title{scwbStatistics operation definitions}
\description{Six ABI v2 definitions activated through \code{scworkbench::wb_operation_pack_load("scwbStatistics")}.}
\details{
\code{de_wilcox} performs two-group Wilcoxon testing. \code{markers_topn}
performs one-versus-rest marker ranking. \code{cell_vector_assoc} is the
narrow continuous-by-categorical predecessor returning \code{assoc_summary}
and \code{group_stats}; \code{cell_vector_association} is a distinct typed
dispatcher returning \code{summary} and method-specific \code{detail}.
\code{cell_vector_compare} compares shared numeric cell vectors in raw or rank
space. \code{feature_trend_spearman} tests feature values against a continuous
cell vector and produces identical canonical output for chunked and unchunked
execution. These are operation names, not exported R functions; inspect live
interfaces with \code{scworkbench::wb_op_interface()} after activation.
}
\examples{
\dontrun{
scworkbench::wb_operation_pack_load("scwbStatistics")
scworkbench::wb_op_interface("scwbstatistics::de_wilcox")
}
}

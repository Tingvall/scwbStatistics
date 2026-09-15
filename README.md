# scwbStatistics

`scwbStatistics` is the ABI v2 statistical operation pack for scworkbench.
Installation is inert. Activate it explicitly:

```r
library(scworkbench)
wb_operation_pack_load("scwbStatistics")
wb_list_ops()
```

It contributes `scwbstatistics::de_wilcox`,
`scwbstatistics::markers_topn`, `scwbstatistics::cell_vector_assoc`,
`scwbstatistics::cell_vector_association`,
`scwbstatistics::cell_vector_compare`, and
`scwbstatistics::feature_trend_spearman`. Unqualified calls resolve only when
one active operation has that short name; provenance records the qualified
identity.

Core resolves typed inputs and owns identifiers, private staging, promotion,
receipts, provenance, and bundle publication. The pack receives only typed
readers and sanctioned output writers. Finalized outputs remain inspectable
without this package, while execution and strict replay require the exact
compatible pack identity.

`cell_vector_assoc` is the narrower continuous-by-categorical predecessor and
returns `assoc_summary` plus `group_stats`. `cell_vector_association` is a
semantically distinct typed dispatcher supporting categorical/categorical,
continuous/categorical, and continuous/continuous inputs and returns `summary`
plus method-specific `detail`. Neither operation is an alias.

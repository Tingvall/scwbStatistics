# Phase 3A Statistics Amendment 01 — scwbStatistics report

## Revision boundary

- Rejected parent: `c55a8277252df398b41062238cb120e1f0316d5f`
- Corrected implementation descendant: `2e3da682198e7e993fd66024cfb106ff05135f06`
- Companion scworkbench implementation: `a94f3ac2bb07b18e2645bde2960eb63613ae99eb`

The implementation changes `DESCRIPTION`, `R/statistics.R`, `README.md`,
`inst/scworkbench/operation-pack.json`, `tests/pack_contracts.R`, and
`tools/run_equivalence_comparator.R`. The closure commit adds this report and
the regenerated `notes/phase3a_statistics_equivalence.json`.

## Bounded execution and manifest ownership

`scwbstatistics::feature_trend_spearman` resolves its feature sequence once,
partitions it into deterministic contiguous batches, invokes the core typed
matrix reader once per nonempty batch, merges rows in resolved feature order,
and applies the requested P-value adjustment once over the merged universe.
It never materializes the complete selected matrix before partitioning.

Instrumented traces are:

- ordinary matrix, size 2: `[gene_1,gene_2]`, `[gene_3,gene_4]`, `[gene_5]`
- ordinary matrix, unchunked: `[gene_1,gene_2,gene_3,gene_4,gene_5]`
- backed matrix, size 2: `[g1,g2]`, `[g3,g4]`, `[g5]`
- backed matrix, unchunked: `[g1,g2,g3,g4,g5]`

The fixture proves that one global adjustment differs from per-batch
adjustment. Core-observed batches drive retained chunk evidence; a declaration
or observation mismatch fails before receipt insertion or publication with
zero residue.

The authenticated manifest now supplies all six public interfaces and the
generic binding, dimension, chunk, and retained-parameter declarations used by
core. Package provenance is Hendrik A. de Weerd
`<dirkdeweerd.work@gmail.com>` with `aut` and `cre` roles.

## Identities

- Manifest source: `14b4610595e255f199c58748e387981b294825a145a3ffffa2e0c5e309c175ad`
- Manifest: `96f4fbe8f12d1842e7d57da142fb4942dd5b023cce02b7f1f5f9bbc90e26d0cb`
- Installed distribution: `d5f05fdd0ae14192d504834ffefc9788c9d43a1bdb96d01603bab70644ee3337`
- Entry point: `fc66ea2a37b421f4e04836745be756a3c4d77785e73c72d44d68b537c1cfa06c`
- Installed pack: `715a7c07d12459e0f0c8246aecbcd5b9a318c7dee3d7ead256c2cbbb8cf9b30c`
- `de_wilcox`: `dca55697427cede0ef77a2d29399b5db6ddd163111c6b886d158ec356a5583d6`
- `markers_topn`: `c2c82d51918368a6a1da8a7fda2c95720265a89e8161745e5e1c95b93e881d9b`
- `cell_vector_assoc`: `8c29eda950bffd0d9bad3a5f0d12bd2562fad5e9c51d8bec6a740a5902a37978`
- `cell_vector_association`: `3325465e1afcb766abb03810d35c166920db98bcf94a2f593b54e8d25b9f44df`
- `cell_vector_compare`: `0d6e62442271c553ac803d89296040878c2c3bd143b1eb7b91a4a0711289f6a7`
- `feature_trend_spearman`: `e6c6ebc993c3e119c407bf88f3b76ab43bbf280a27de9ef65d9136f0f43112d4`

Comparator-source identities in
`notes/phase3a_statistics_equivalence.json` are separately labeled because
they describe the source-overlay comparator boundary rather than the clean
installed-package boundary above.

## Qualification

- Pack suite: 6 operations, 6 association paths, 6 edge-case groups; passed.
- Old-core/new-pack comparator: 6 field-level interfaces and 9 execution cases;
  passed, including exact observed chunk batches.
- Pack `R CMD check --no-manual`: `Status: OK`.
- Clean offline install with core: 53 core and 59 activated catalog entries;
  lifecycle smoke and pack contracts passed.
- Two source builds had identical membership and extracted content after
  removal of only generated `Packaged:` metadata.
- Syntax, JSON/CSV, links, package content, privacy/path, and `git diff --check`
  gates passed.

Because the canonical shared ABI v2 schema changed from
`8343f8ea40ce3cc57027b0810c4d05b4df00196edd544361d15dc1fd59fe96aa`
to `d6756c057704820f2b6f1069035546a85fa2cbf504885f519bfe4535550f727c`,
this candidate is ready for Quill protocol synchronization, not composite
acceptance.

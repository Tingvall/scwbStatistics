.assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  invisible(TRUE)
}

.param <- function(params, name, default = NULL) {
  value <- params[[name]]
  if (is.null(value)) default else value
}

.chars <- function(value) {
  if (is.list(value)) unname(vapply(value, as.character, character(1))) else as.character(value)
}

.matrix_view <- function(reader, features = NULL, cells = NULL) {
  value <- reader(features = features, cells = cells)
  .assert(inherits(value, "wb_primitive") && identical(value$kind, "matrix"),
    "read_matrix did not return a matrix primitive.")
  .assert(!is.null(value$values) && length(dim(value$values)) == 2L,
    "read_matrix did not return a two-dimensional typed value view.")
  value
}

.wilcox_rows <- function(values, features, cells_a, cells_b) {
  ia <- match(cells_a, colnames(values)); ib <- match(cells_b, colnames(values))
  .assert(!anyNA(ia) && !anyNA(ib), "Requested comparison cells are absent from the matrix reader view.")
  rows <- lapply(seq_along(features), function(index) {
    va <- as.numeric(values[index, ia, drop = TRUE])
    vb <- as.numeric(values[index, ib, drop = TRUE])
    p <- tryCatch(suppressWarnings(stats::wilcox.test(
      va, vb, alternative = "two.sided", exact = FALSE)$p.value), error = function(error) 1)
    data.frame(
      feature_id = features[[index]], pct_a = mean(va > 0), pct_b = mean(vb > 0),
      avg_expr_a = mean(va), avg_expr_b = mean(vb),
      avg_log1p_fc = log1p(mean(va)) - log1p(mean(vb)), p_val = as.numeric(p),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.run_de_wilcox <- function(readers, writers, params, context) {
  cells_a <- unique(.chars(.param(params, "group_a")))
  cells_b <- unique(.chars(.param(params, "group_b")))
  .assert(length(cells_a) >= 2L && length(cells_b) >= 2L,
    "de_wilcox requires at least two cells per group.")
  .assert(!length(intersect(cells_a, cells_b)), "de_wilcox groups must be disjoint.")
  features <- .param(params, "features")
  if (!is.null(features)) features <- .chars(features)
  view <- .matrix_view(readers$matrix, features = features,
    cells = unique(c(cells_a, cells_b)))
  feature_ids <- as.character(view$feature_id %||% rownames(view$values))
  de <- .wilcox_rows(view$values, feature_ids, cells_a, cells_b)
  min_pct <- as.numeric(.param(params, "min_pct", 0))
  if (is.finite(min_pct) && min_pct > 0) de <- de[pmax(de$pct_a, de$pct_b) >= min_pct, , drop = FALSE]
  method <- as.character(.param(params, "p_adjust_method", "BH"))[[1]]
  de$p_val_adj <- if (nrow(de)) stats::p.adjust(de$p_val, method = method) else numeric()
  de <- de[order(de$p_val_adj, -de$avg_log1p_fc), , drop = FALSE]; rownames(de) <- NULL
  output <- scworkbench::wb_prim_feature_table(de, "de_wilcox", meta = list(
    matrix_id = view$matrix_id, n_cells_a = length(cells_a), n_cells_b = length(cells_b)))
  writers$write_terminal_primitive("de_table", output)
  invisible(list(status = "ok"))
}

.run_markers_topn <- function(readers, writers, params, context) {
  clusters <- readers$clusters()
  cells <- as.character(context$effective_cells %||% clusters$cell_id)
  labels <- as.character(clusters$value[match(cells, clusters$cell_id)])
  keep <- !is.na(labels); cells <- cells[keep]; labels <- labels[keep]
  .assert(length(cells) >= 10L && length(unique(labels)) >= 2L,
    "markers_topn requires at least ten labelled cells in two clusters.")
  features <- .param(params, "features"); if (!is.null(features)) features <- .chars(features)
  view <- .matrix_view(readers$matrix, features = features, cells = cells)
  features <- as.character(view$feature_id %||% rownames(view$values))
  top_n <- as.integer(.param(params, "top_n", 20L))
  min_pct <- as.numeric(.param(params, "min_pct", 0.1))
  adjust <- as.character(.param(params, "p_adjust_method", "BH"))[[1]]
  rows <- lapply(sort(unique(labels)), function(label) {
    a <- cells[labels == label]; b <- cells[labels != label]
    if (length(a) < 2L || length(b) < 2L) return(NULL)
    de <- .wilcox_rows(view$values, features, a, b)
    de <- de[pmax(de$pct_a, de$pct_b) >= min_pct, , drop = FALSE]
    if (!nrow(de)) return(NULL)
    de$p_val_adj <- stats::p.adjust(de$p_val, method = adjust); de$cluster <- label
    de <- de[de$avg_log1p_fc > 0, , drop = FALSE]
    utils::head(de[order(de$p_val_adj, -de$avg_log1p_fc), , drop = FALSE], top_n)
  })
  rows <- Filter(Negate(is.null), rows)
  markers <- if (length(rows)) do.call(rbind, rows) else data.frame(
    feature_id=character(), pct_a=numeric(), pct_b=numeric(), avg_expr_a=numeric(),
    avg_expr_b=numeric(), avg_log1p_fc=numeric(), p_val=numeric(), p_val_adj=numeric(),
    cluster=character(), stringsAsFactors=FALSE)
  rownames(markers) <- NULL
  output <- scworkbench::wb_prim_feature_table(markers, "markers_topn", meta = list(
    matrix_id = view$matrix_id, n_clusters = length(unique(labels)), top_n = top_n))
  writers$write_terminal_primitive("markers", output)
  invisible(list(status = "ok"))
}

.association_grouped <- function(vector_cont, vector_group, min_n_per_group) {
  ids <- intersect(names(vector_cont), names(vector_group))
  frame <- data.frame(cell_id=ids, value=suppressWarnings(as.numeric(vector_cont[ids])),
    group=as.character(vector_group[ids]), stringsAsFactors=FALSE)
  frame <- frame[is.finite(frame$value) & !is.na(frame$group) & nzchar(frame$group), , drop=FALSE]
  counts <- table(frame$group)
  frame <- frame[frame$group %in% names(counts[counts >= min_n_per_group]), , drop=FALSE]
  .assert(length(unique(frame$group)) >= 2L, "Need at least two groups after min_n_per_group filtering.")
  stats_rows <- do.call(rbind, lapply(split(frame$value, frame$group), function(value) data.frame(
    n=length(value), mean=mean(value), median=stats::median(value), sd=stats::sd(value))))
  stats_rows$group <- rownames(stats_rows); rownames(stats_rows) <- NULL
  stats_rows <- stats_rows[, c("group", "n", "mean", "median", "sd"), drop=FALSE]
  kw <- stats::kruskal.test(value ~ group, data=frame); fit <- stats::lm(value ~ group, data=frame)
  analysis <- stats::anova(fit); total <- sum((frame$value - mean(frame$value))^2)
  group_ss <- if ("group" %in% rownames(analysis)) as.numeric(analysis["group", "Sum Sq"]) else NA_real_
  eta2 <- if (is.finite(group_ss) && is.finite(total) && total > 0) group_ss / total else NA_real_
  summary <- data.frame(feature_id="value_by_group", n_cells=nrow(frame),
    n_groups=length(unique(frame$group)), kruskal_p=as.numeric(kw$p.value),
    anova_p=as.numeric(stats::pf(analysis["group", "F value"], analysis["group", "Df"],
      analysis["Residuals", "Df"], lower.tail=FALSE)), eta2=eta2, stringsAsFactors=FALSE)
  list(summary=summary, detail=stats_rows, n_cells=nrow(frame), n_groups=length(unique(frame$group)))
}

.run_cell_vector_assoc <- function(readers, writers, params, context) {
  continuous <- readers$vector_cont(); grouped <- readers$vector_group()
  x <- suppressWarnings(as.numeric(continuous$value)); names(x) <- continuous$cell_id
  g <- as.character(grouped$value); names(g) <- grouped$cell_id
  cells <- as.character(context$effective_cells %||% intersect(names(x), names(g)))
  x <- x[cells]; g <- g[cells]
  minimum <- as.integer(.param(params, "min_n_per_group", 3L))
  result <- .association_grouped(x, g, minimum)
  writers$write_terminal_primitive("assoc_summary", scworkbench::wb_prim_feature_table(
    result$summary, "cell_vector_assoc", meta=list(test="kruskal+anova", min_n_per_group=minimum)))
  writers$write_terminal_primitive("group_stats", scworkbench::wb_prim_obs_table(
    result$detail, "cell_vector_assoc_group_stats", "group"))
  invisible(list(status="ok"))
}

.run_cell_vector_association <- function(readers, writers, params, context) {
  a <- readers$a(); b <- readers$b(); ids <- intersect(a$cell_id, b$cell_id)
  minimum <- as.integer(.param(params, "min_n_per_group", 3L))
  method <- as.character(.param(params, "method", "auto"))[[1]]
  av <- a$value[match(ids, a$cell_id)]; bv <- b$value[match(ids, b$cell_id)]
  type_a <- as.character(a$value_type %||% "categorical")[[1]]
  type_b <- as.character(b$value_type %||% "categorical")[[1]]
  .assert(type_a %in% c("categorical", "continuous") &&
    type_b %in% c("categorical", "continuous"),
    "cell_vector_association supports categorical and continuous vectors only.")
  if (type_a == "categorical" && type_b == "categorical") {
    .assert(method %in% c("auto", "chisq", "fisher"),
      "categorical x categorical association supports method auto, chisq, or fisher.")
    frame <- data.frame(cell_id=ids, a=as.character(av), b=as.character(bv), stringsAsFactors=FALSE)
    frame <- frame[!is.na(frame$a) & nzchar(frame$a) & !is.na(frame$b) & nzchar(frame$b), , drop=FALSE]
    .assert(nrow(frame)>=2L && length(unique(frame$a))>=2L && length(unique(frame$b))>=2L,
      "categorical association requires two non-missing levels in each vector.")
    tab <- table(frame$a, frame$b); chi <- suppressWarnings(stats::chisq.test(tab, correct=FALSE))
    use_fisher <- method == "fisher" || (method == "auto" && any(chi$expected < 5))
    test <- if (use_fisher) stats::fisher.test(tab) else chi; n <- sum(tab)
    denom <- min(nrow(tab)-1L, ncol(tab)-1L)
    effect <- if (denom > 0L) sqrt(as.numeric(chi$statistic)/n/denom) else NA_real_
    detail <- as.data.frame(as.table(tab), stringsAsFactors=FALSE); names(detail) <- c("a_level","b_level","observed")
    detail$expected <- as.numeric(as.data.frame(as.table(chi$expected))$Freq)
    detail$pearson_residual <- (detail$observed-detail$expected)/sqrt(detail$expected)
    summary <- data.frame(association_type="categorical_x_categorical",
      method=if(use_fisher) "fisher_exact" else "chi_square", n_cells=n,
      n_a_levels=nrow(tab), n_b_levels=ncol(tab), statistic=as.numeric(chi$statistic),
      p_value=as.numeric(test$p.value), effect_size=effect, effect_size_name="cramers_v",
      min_expected=min(chi$expected), n_expected_lt_5=sum(chi$expected<5),
      expected_count_ok=all(chi$expected>=5), stringsAsFactors=FALSE)
    detail_type <- "cell_vector_association_contingency"; entity <- "contingency_cell"
  } else if (xor(type_a == "continuous", type_b == "continuous")) {
    .assert(method %in% c("auto", "kruskal_anova"),
      "continuous x categorical association supports method auto or kruskal_anova.")
    cont <- if (type_a == "continuous") a else b; group <- if (type_a == "categorical") a else b
    x <- suppressWarnings(as.numeric(cont$value)); names(x) <- cont$cell_id
    g <- as.character(group$value); names(g) <- group$cell_id
    grouped <- .association_grouped(x[ids], g[ids], minimum)
    summary <- grouped$summary; summary$association_type <- "continuous_x_categorical"
    summary$method <- "kruskal_anova"; summary$effect_size_name <- "eta2"
    detail <- grouped$detail; detail_type <- "cell_vector_assoc_group_stats"; entity <- "group"
  } else {
    .assert(method %in% c("auto", "correlation"),
      "continuous x continuous association supports method auto or correlation.")
    xa <- suppressWarnings(as.numeric(av)); xb <- suppressWarnings(as.numeric(bv))
    keep <- is.finite(xa) & is.finite(xb); xa <- xa[keep]; xb <- xb[keep]; ids <- ids[keep]
    .assert(length(ids)>=3L && stats::sd(xa)>0 && stats::sd(xb)>0,
      "continuous association requires three non-constant shared finite values.")
    pearson <- stats::cor.test(xa, xb, method="pearson")
    spearman <- suppressWarnings(stats::cor.test(xa, xb, method="spearman", exact=FALSE))
    summary <- data.frame(association_type="continuous_x_continuous", method="correlation",
      n_cells=length(ids), pearson=as.numeric(pearson$estimate), pearson_p=pearson$p.value,
      spearman=as.numeric(spearman$estimate), spearman_p=spearman$p.value,
      effect_size=as.numeric(pearson$estimate), effect_size_name="pearson_r", stringsAsFactors=FALSE)
    detail <- data.frame(cell_id=ids, a=xa, b=xb, stringsAsFactors=FALSE)
    detail_type <- "cell_vector_association_cells"; entity <- "cell"
  }
  writers$write_terminal_primitive("summary", scworkbench::wb_prim_obs_table(
    summary, "cell_vector_association_summary", "vector_pair"))
  writers$write_terminal_primitive("detail", scworkbench::wb_prim_obs_table(
    detail, detail_type, entity))
  invisible(list(status="ok"))
}

.safe_cor <- function(x, y, method) {
  keep <- is.finite(x) & is.finite(y); x <- x[keep]; y <- y[keep]
  if (length(x)<3L || stats::sd(x)==0 || stats::sd(y)==0) return(NA_real_)
  suppressWarnings(as.numeric(stats::cor(x, y, method=method)))
}

.run_cell_vector_compare <- function(readers, writers, params, context) {
  a <- readers$a(); b <- readers$b(); ids <- intersect(a$cell_id, b$cell_id)
  xa <- suppressWarnings(as.numeric(a$value)); names(xa) <- a$cell_id
  xb <- suppressWarnings(as.numeric(b$value)); names(xb) <- b$cell_id
  frame <- data.frame(cell_id=ids, a=as.numeric(xa[ids]), b=as.numeric(xb[ids]),
    stringsAsFactors=FALSE)
  frame <- frame[is.finite(frame$a) & is.finite(frame$b), , drop=FALSE]
  .assert(nrow(frame)>=3L, "cell_vector_compare requires at least three shared finite values.")
  space <- as.character(.param(params,"compare_space","rank"))[[1]]
  ties <- as.character(.param(params,"ties_method","average"))[[1]]
  if (space=="rank") { frame$a_cmp <- rank(frame$a,ties.method=ties); frame$b_cmp <- rank(frame$b,ties.method=ties) }
  else { frame$a_cmp <- frame$a; frame$b_cmp <- frame$b }
  frame$delta <- frame$a_cmp-frame$b_cmp; frame$abs_delta <- abs(frame$delta)
  summary <- data.frame(signal_a=as.character(.param(params,"signal_a","a"))[[1]],
    signal_b=as.character(.param(params,"signal_b","b"))[[1]], compare_space=space,
    n_shared_cells=nrow(frame), pearson=.safe_cor(frame$a_cmp,frame$b_cmp,"pearson"),
    spearman=.safe_cor(frame$a_cmp,frame$b_cmp,"spearman"), kendall=.safe_cor(frame$a_cmp,frame$b_cmp,"kendall"),
    mean_delta=mean(frame$delta), mean_abs_delta=mean(frame$abs_delta),
    median_abs_delta=stats::median(frame$abs_delta), rmse=sqrt(mean(frame$delta^2)),
    max_abs_delta=max(frame$abs_delta), stringsAsFactors=FALSE)
  meta <- list(compare_space=space,ties_method=ties)
  writers$write_terminal_primitive("summary", scworkbench::wb_prim_obs_table(summary,
    "cell_vector_compare_summary", "signal_pair", meta=meta))
  writers$write_terminal_primitive("comparison", scworkbench::wb_prim_obs_table(
    frame[,c("cell_id","a","b","a_cmp","b_cmp","delta","abs_delta")],
    "cell_vector_compare", "cell", meta=meta))
  invisible(list(status="ok"))
}

.run_feature_trend_spearman <- function(readers, writers, params, context) {
  pseudotime <- readers$pseudotime()
  requested_cells <- as.character(context$effective_cells %||% pseudotime$cell_id)
  pt <- suppressWarnings(as.numeric(pseudotime$value)); names(pt) <- pseudotime$cell_id
  pt <- pt[requested_cells]
  keep <- is.finite(pt); pt <- pt[keep]
  .assert(length(pt)>=10L && stats::sd(pt)>0, "feature_trend_spearman requires ten finite non-constant pseudotime values.")
  features <- .param(params,"features"); if(!is.null(features)) features <- .chars(features)
  view <- .matrix_view(readers$matrix, features=features, cells=names(pt)); matrix <- as.matrix(view$values)
  minimum_pct <- as.numeric(.param(params,"min_pct_nonzero",0)); minimum_sd <- as.numeric(.param(params,"min_sd",0))
  filtered_cells <- 0L; filtered_pct <- 0L; filtered_sd <- 0L
  rows <- lapply(seq_len(nrow(matrix)), function(index) {
    y <- as.numeric(matrix[index,]); ok <- is.finite(y) & is.finite(pt)
    if(sum(ok)<10L) { filtered_cells <<- filtered_cells + 1L; return(NULL) }
    y <- y[ok]; x <- pt[ok]; pct <- mean(y>0); deviation <- stats::sd(y)
    if(pct<minimum_pct) { filtered_pct <<- filtered_pct + 1L; return(NULL) }
    if(!is.finite(deviation) || deviation<minimum_sd) { filtered_sd <<- filtered_sd + 1L; return(NULL) }
    rho <- suppressWarnings(stats::cor(y,x,method="spearman")); if(!is.finite(rho)) return(NULL)
    p <- if(abs(rho)>=1) 0 else 2*stats::pt(abs(rho*sqrt((length(y)-2)/pmax(1-rho^2,.Machine$double.eps))),df=length(y)-2,lower.tail=FALSE)
    data.frame(feature_id=rownames(matrix)[[index]],rho=rho,abs_rho=abs(rho),
      direction=if(rho>0) "up" else if(rho<0) "down" else "flat",
      slope=tryCatch(as.numeric(stats::coef(stats::lm(y~x))[[2]]),error=function(error) NA_real_),
      p_val=p,pct_nonzero=pct,sd_expr=deviation,mean_expr=mean(y),n_used=length(y),
      n_dropped_na=sum(!ok),stringsAsFactors=FALSE)
  })
  rows <- Filter(Negate(is.null),rows)
  if(length(rows)) { table <- do.call(rbind,rows); table$p_val_adj <- stats::p.adjust(table$p_val,method=as.character(.param(params,"p_adjust_method","BH"))[[1]]); table <- table[order(table$p_val_adj,-table$abs_rho),]; rownames(table)<-NULL }
  else table <- data.frame(feature_id=character(),rho=numeric(),abs_rho=numeric(),direction=character(),slope=numeric(),p_val=numeric(),p_val_adj=numeric(),pct_nonzero=numeric(),sd_expr=numeric(),mean_expr=numeric(),n_used=integer(),n_dropped_na=integer(),stringsAsFactors=FALSE)
  output <- scworkbench::wb_prim_feature_table(table,"feature_trend_spearman",meta=list(
    matrix_id=view$matrix_id,n_cells_input=length(pseudotime$cell_id),n_cells_used=length(pt),
    n_features_input=nrow(matrix),n_features_tested=nrow(table),min_pct_nonzero=minimum_pct,
    min_sd=minimum_sd,p_adjust_method=as.character(.param(params,"p_adjust_method","BH"))[[1]],
    n_filtered_min_cells=filtered_cells,n_filtered_min_pct_nonzero=filtered_pct,
    n_filtered_min_sd=filtered_sd))
  writers$write_terminal_primitive("trend_table",output)
  invisible(list(status="ok"))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

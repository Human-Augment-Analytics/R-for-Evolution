
# internal utility: get coefficient-level p-value column name from summary()
.p_col_from_summary <- function(coef_mat) {
  pcols <- intersect(colnames(coef_mat), c("Pr(>|t|)", "Pr(>|z|)"))
  if (length(pcols)) pcols[1] else NA_character_
}

# internal utility: safely extract ANOVA p-value (car::Anova)
.ano_p <- function(anova_obj, term) {
  if (is.null(anova_obj)) return(NA_real_)
  rn <- rownames(anova_obj)
  if (is.null(rn) || !term %in% rn) return(NA_real_)
  pcols <- intersect(colnames(anova_obj), c("Pr(>F)", "Pr(>Chisq)"))
  if (!length(pcols)) return(NA_real_)
  as.numeric(anova_obj[term, pcols[1]])
}

# extract Linear (Directional) Coefficients
extract_linear_coefficients <- function(trait_cols, results) {
  sm <- results$summary$coefficients
  pcol <- .p_col_from_summary(sm)
  keep <- intersect(rownames(sm), trait_cols)

  rows <- lapply(keep, function(t) {
    p_val <- if (!is.na(pcol)) sm[t, pcol] else .ano_p(results$anova, t)
    data.frame(
      Term = t, Type = "Linear",
      Beta_Coefficient = sm[t, "Estimate"],
      Standard_Error   = sm[t, "Std. Error"],
      P_Value = as.numeric(p_val),
      check.names = FALSE
    )
  })

  if (!length(rows)) {
    return(data.frame(Term=character(), Type=character(),
                      Beta_Coefficient=numeric(), Standard_Error=numeric(),
                      P_Value=numeric(), check.names = FALSE))
  }
  do.call(rbind, rows)
}

# extract Quadratic Coefficients
extract_quadratic_coefficients <- function(trait_cols, results) {
  sm <- results$summary$coefficients
  pcol <- .p_col_from_summary(sm)

  rows <- lapply(trait_cols, function(t) {
    term <- paste0("I(", t, "^2)")
    if (!term %in% rownames(sm)) return(NULL)

    # Multiply estimate and SE by 2 for gamma_ii
    est <- 2 * sm[term, "Estimate"]
    se  <- 2 * sm[term, "Std. Error"]
    p_val <- if (!is.na(pcol)) sm[term, pcol] else .ano_p(results$anova, term)

    data.frame(
      Term = paste0(t, "²"),
      Type = "Quadratic",
      Beta_Coefficient = as.numeric(est),
      Standard_Error   = as.numeric(se),
      P_Value          = as.numeric(p_val),
      check.names = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(Term=character(), Type=character(),
                      Beta_Coefficient=numeric(), Standard_Error=numeric(),
                      P_Value=numeric(), check.names = FALSE))
  }
  do.call(rbind, rows)
}

# extract Correlational Coefficients
extract_interaction_coefficients <- function(trait_cols, results) {
  if (length(trait_cols) < 2) {
    return(data.frame(Term=character(), Type=character(),
                      Beta_Coefficient=numeric(), Standard_Error=numeric(),
                      P_Value=numeric(), check.names = FALSE))
  }

  sm <- results$summary$coefficients
  pcol <- .p_col_from_summary(sm)
  pairs <- utils::combn(trait_cols, 2, simplify = FALSE)

  rows <- lapply(pairs, function(p) {
    raw_term <- paste0(p[1], ":", p[2])  # term name in model matrix / summary
    if (!raw_term %in% rownames(sm)) return(NULL)

    p_val <- if (!is.na(pcol)) sm[raw_term, pcol] else .ano_p(results$anova, raw_term)

    data.frame(
      Term = paste(p[1], "×", p[2]),
      Type = "Correlational",
      Beta_Coefficient = sm[raw_term, "Estimate"],   # gamma_ij = b_ij
      Standard_Error   = sm[raw_term, "Std. Error"],
      P_Value          = as.numeric(p_val),
      check.names = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(Term=character(), Type=character(),
                      Beta_Coefficient=numeric(), Standard_Error=numeric(),
                      P_Value=numeric(), check.names = FALSE))
  }
  do.call(rbind, rows)
}


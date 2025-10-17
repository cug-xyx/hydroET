#' Elasticity Coefficient Method (ECM)
#'
#' @param df a data.frame with input data
#' @param y_name y column name
#' @param x_name x column name(s)
#' @param method method used to calculate delta value
#'
#' @return a data.frame
#' 
#' @references Zhang et al., 2023, Nature Water
#' 
#' @export
ecm <- function(df, y_name, x_name, method = "lu2025") {
  y <- df[[y_name]]
  x <- as.matrix(df[x_name])
  nx <- ncol(x)

  if (length(y[!is.na(y)]) < 5) return(
    data.frame(
      var = colnames(x), elasticity = NA_real_, p_elast = NA_real_,
      R2_elast = NA_real_, fp_elast = NA_real_, cont_lu2025 = NA_real_,
      cont_perc_lu2025 = NA_real_, cont_perc_li2024 = NA_real_
    )
  )

  y_mean <- mean(y, na.rm = TRUE)
  x_mean <- apply(x, 2, mean, na.rm = TRUE)

  y_norm <- (y - y_mean) / y
  x_norm <- apply(x, 2, \(z) (z - mean(z, na.rm = TRUE)) / z)

  lm_res <- lm(y_norm ~ x_norm) |> summary()

  elasticity <- lm_res$coefficients[2:(nx + 1), "Estimate"]
  p_elast <- lm_res$coefficients[2:(nx + 1), "Pr(>|t|)"] |> round(4)
  r2_elast <- lm_res$adj.r.squared
  fp_elast <- pf(
    lm_res$fstatistic[1], lm_res$fstatistic[2],
    lm_res$fstatistic[3], lower.tail = FALSE
  )[[1]] |> round(4)

  # Contribution (Lu et al., 2025, Earth Future)
  x_delta <- apply(x, 2, cal_delta, method = method)
  cont_lu2025 <- elasticity * x_delta / x_mean * y_mean

  df_res <- data.frame(
    var = colnames(x), elasticity = elasticity, p_elast = p_elast,
    R2_elast = r2_elast, fp_elast = fp_elast, cont_lu2025 = cont_lu2025
  )
  rownames(df_res) <- NULL
  df_res$cont_perc_lu2025 <- (
    df_res$cont_lu2025 / sum(abs(df_res$cont_lu2025), na.rm = TRUE) * 100
  ) |> round(3)

  # Contribution (Li et al., 2024, AFM)
  # Relative contribution of contributing factors to the variation  y (Δy)
  df_res$cont_perc_li2024 <- (
    df_res$elasticity / sum(abs(df_res$elasticity), na.rm = TRUE) * 100
  ) |> round(3)

  df_res
}


#' cal delta value used in ecm
#'
#' @param x a vector
#' @param method method
cal_delta <- function(x, method = "lu2025") {
  n <- length(x)
  mid <- floor(n / 2)
  inds_past <- 1:mid
  inds_present <- (mid + 1):n

  if (method == "lu2025") {
    x_mean <- mean(x, na.rm = TRUE)
  } else if (method == "feng2020") {
    x_mean <- mean(x[inds_past], na.rm = TRUE)
  }

  x_present <- mean(x[inds_present], na.rm = TRUE)

  x_present - x_mean
}
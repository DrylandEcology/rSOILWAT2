context("Imputation")

Nrows <- 30
Nmiss <- 5

df <- data.frame(
  linear = seq_len(Nrows),
  all_missing = NA,
  all_same = 1,
  cyclic = cos(2 * pi * seq_len(Nrows) / Nrows)
)

vars <- colnames(df)
vars_with_values <- vars[!apply(df, 2, anyNA)]

types <- c("mean", "locf")
cyclicity <- c(FALSE, TRUE)

#---TESTS
test_that("Impute missing values", {
  #-------
  # Check 1: If every value is missing, then no numeric values are imputed.
  # Check 2: If missing values remain, then a warning is issued.
  # Check 3: If no value is missing, then no value is modified.
  df1 <- df

  for (type in types) {
    res1 <- expect_warning(impute_df(
      x = df1,
      imputation_type = type
    ))
    expect_identical(df1, res1)
  }

  #------- Last values are missing
  df2 <- df[, vars_with_values]
  idsNA <- (Nrows - Nmiss):Nrows
  df2[idsNA, ] <- NA

  for (type in types) {
    res2 <- expect_silent(impute_df(
      x = df2,
      imputation_type = type,
      cyclic = FALSE
    ))

    for (v in vars_with_values) {
      expect_true(!anyNA(res2[, v]))
      if (type == "locf") {
        expect_true(all(res2[idsNA, v] == df2[idsNA[1] - 1, v]))
      }
    }
  }

  #------- First values are missing
  df3 <- df[, vars_with_values]
  idsNA <- 1:Nmiss
  df3[idsNA, ] <- NA

  for (type in types) for (cyclic in cyclicity) {
    if (type == "locf" && !cyclic) {
      res3 <- expect_warning(impute_df(
        x = df3,
        imputation_type = type,
        cyclic = cyclic
      ))
    } else {
      res3 <- expect_silent(impute_df(
        x = df3,
        imputation_type = type,
        cyclic = cyclic
      ))
    }

    for (v in vars_with_values) {
      if (type == "locf") {
        if (cyclic) {
          # last value is imputed
          expect_true(all(res3[idsNA, v] == df3[Nrows, v]))
        } else {
          # no value to can be imputed --> NAs
          expect_true(all(is.na(res3[idsNA, v])))
        }

      } else if (type == "mean") {
        expect_true(!anyNA(res3[, v]))
      }
    }
  }

})

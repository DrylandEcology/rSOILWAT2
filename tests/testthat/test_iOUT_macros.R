context("iOUT macros")
# Consider moving these unit tests to the SOILWAT2-repository once we can
# run unit tests that include `SW_Output.h`

# defined by SOILWAT2
NVEGTYPES <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["NVEGTYPES"]]
SW_OUTNPERIODS <- rSW2_glovars[["kSOILWAT2"]][["kINT"]][["SW_OUTNPERIODS"]]
ncol_TimeOUT <- c(2, 2, 2, 1)
SW_OUTNKEYS <- 2 # is actually much larger

# test input and inits
tobase0 <- -1
tobase1 <- 1
n_vars <- 5
n_layers <- 3
ncol_OUT <- c(n_vars, NVEGTYPES * n_layers)
nrow_OUT <- c(31, 6, 5, 1)
irow_OUT <- rep(0, SW_OUTNPERIODS)
vars <- paste0("Out", seq_len(n_vars))

p_OUT <- matrix(list(), nrow = SW_OUTNKEYS, ncol = SW_OUTNPERIODS)


# macros defined by SOILWAT2: i, k, and pd are base0
# here, convert i and k from base1 to base0;
# here, need `irow_OUT` as argument otherwise functions don't see correct values
iOUT <- function(i, pd, irow_OUT) {
  (irow_OUT[(pd)] + nrow_OUT[(pd)] * (ncol_TimeOUT[(pd)] + (i + tobase0)))
}
iOUT2 <- function(i, k, pd, irow_OUT) {
  (irow_OUT[(pd)] + nrow_OUT[(pd)] *
      (ncol_TimeOUT[(pd)] + (i + tobase0) + n_layers * (k + tobase0)))
}

#---TESTS
test_that("Tests of iOUT and iOUT2", {
  for (key in seq_len(SW_OUTNKEYS)) {
    for (pd in seq_len(SW_OUTNPERIODS)) {
      #init
      p_OUT[key, pd][[1]] <- matrix(NA,
        nrow = nrow_OUT[pd], ncol = ncol_OUT[key] + ncol_TimeOUT[pd])

      # fill in time header
      t0 <- 0
      for (k in seq_len(ncol_TimeOUT[pd])) {
        p_OUT[key, pd][[1]][, k] <- t0 + seq_len(nrow_OUT[pd])
        t0 <- t0 + nrow_OUT[pd]
      }

      irow_OUT[pd] <- 0 + tobase1
      for (time in seq_len(nrow_OUT[pd])) {
        if (key == 1) {
          for (i in seq_len(n_vars)) {
            # test `iOUT` for 'key = 1'
            icol <- ncol_TimeOUT[pd] + i
            #print(paste("key =", key, "var =", i, "irow =", irow_OUT[pd], "icol =", icol, "iOUT =", iOUT(i, pd, irow_OUT)))
            p_OUT[key, pd][[1]][irow_OUT[pd], icol] <- iOUT(i, pd, irow_OUT)
          }

        } else if (key == 2) {
          # test `iOUT2` for 'key = 2'
          for (k in seq_len(NVEGTYPES)) {
            for (i in seq_len(n_layers)) {
              icol <- ncol_TimeOUT[pd] + i + n_layers * (k - 1)
              #print(paste("key =", key, "veg =", k, "slyr =", i, "irow =", irow_OUT[pd], "icol =", icol, "iOUT =", iOUT2(i, k, pd, irow_OUT)))
              p_OUT[key, pd][[1]][irow_OUT[pd], icol] <- iOUT2(i, k, pd, irow_OUT)
            }
          }
        }

        irow_OUT[pd] <- irow_OUT[pd] + 1
      }

      expect_equal(as.vector(p_OUT[key, pd][[1]]),
        seq_len(nrow_OUT[pd] * (ncol_OUT[key] + ncol_TimeOUT[pd])),
        info = paste("key =", key, "pd =", pd))
    }
  }
})



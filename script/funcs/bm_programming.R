#' 3,500,000 (default) Fibonacci numbers calculation (vector calc).
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_prog_fib <- function(runs = 3, size = 3.5e6, verbose = T) {

  phi <- 1.6180339887498949 # nolint

  df_timing <-
    create_timing_df(runs = runs, test_name = 'fib', test_group = 'prog')

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          a <- floor(runif(size) * 1000)
          ((phi ^ a) - (-phi ^ (-a))) / sqrt(5)

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> %s Fibonacci numbers calculation (vecter cal) ',
          size
        ),
        df_timing,
        df_timing[i, 'test']
      )

    }

  }

  on.exit(invisible(gc()), add = T)

  return(df_timing)

}

#' Create a 3500 x 3500 (default) Hibert matrix (matrix calc).
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_prog_hilbert <- function(runs = 3, size = 3500, verbose = T) {

  df_timing <-
    create_timing_df(runs = runs, test_name = 'hilbert', test_group = 'prog')

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          a <- matrix(rep(1:size, size), size, size)
          1 / (t(a) + 0:(size - 1))

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> %s x %s Hilbert matrix (matrix cal)',
          size, size
        ),
        df_timing,
        df_timing[i, 'test']
      )

    }

  }

  on.exit(invisible(gc()), add = T)

  return(df_timing)

}

#' Grand common divisors of 1,000,000 (default) pairs (recursion).
#'
#' @param runs
#' @param size
#'
#' @returns
#' @export
#'
#' @examples
bm_prog_gcd <- function(runs = 3, size = 1e6, verbose = T) {

  df_timing <-
    create_timing_df(runs = runs, test_name = 'gcd', test_group = 'prog')

  gcd2 <- function(x, y) {

    if (sum(y > 1e-4) == 0) { x }

    else {

      y[y == 0] = x[y == 0]
      Recall(y, x %% y)

    }

  }

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          a <- ceiling(runif(size) * 1000)
          b <- ceiling(runif(size) * 1000)

          gcd2(a, b)

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Grand common divisors of %s pairs (recursion)',
          size
        ),
        df_timing,
        df_timing[i, 'test']
      )

    }

  }

  on.exit(invisible(gc()), add = T)

  return(df_timing)

}

#' Create a 1600 x 1600 (default) Toeplitz matrix (loop).
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_prog_toeplitz <- function(runs = 3, size = 3000, verbose = T) {

  df_timing <-
    create_timing_df(runs = runs, test_name = 'toeplitz', test_group = 'prog')

  matrix_tp <- matrix(rep(0, size ^ 2), nrow = size, ncol = size)

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          for (j in seq_len(size)) {

            for (k in seq_len(size)) {

              matrix_tp[k, j] = abs(j - k) + 1

            }

          }

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> %s x %s Toeplitz matrix (loop)',
          size, size
        ),
        df_timing,
        df_timing[i, 'test']
      )

    }

  }

  on.exit(invisible(gc()), add = T)

  return(df_timing)

}

#' Escoufier's method on a 60 x 60 (default) matrix (mixed).
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_prog_escoufier <- function(runs = 3, size = 60, verbose = T) {

  df_timing <-
    create_timing_df(runs = runs, test_name = 'escoufier', test_group = 'prog')

  # calculate the trace of a matrix (sum of its diagonal elements)
  tr <- function(y) {

    sum(y[1 + 0:(min(dim(y)) - 1) * (dim(y)[1] + 1)], na.rm = F)

  }

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          x      <- matrix(abs(rnorm(size ^ 2)), size, size)
          p      <- ncol(x)
          vt     <- 1:p
          vr     <- NULL
          rv_cor <- 1:p
          vrt    <- NULL

          for (j in seq_len(size)) {

            r_vmax <- 0

            for (k in seq_len(p - j + 1)) {

              x2   <- cbind(x, x[, vr], x[, vt[k]])
              R    <- cor(x2)
              r_yy <- R[1:p, 1:p]
              r_xx <- R[(p + 1):(p + j), (p + 1):(p + j)]
              r_xy <- R[(p + 1):(p + j), 1:p]
              r_yx <- t(r_xy)

              rvt <-
                tr(r_yx %*% r_xy) /
                sqrt(tr(r_yy %*% r_yy)) *
                tr(r_xx %*% r_xx)

              if (rvt > r_vmax) { r_vmax <- rvt; vrt = vt[k] }

              vr[j]     <- vrt
              rv_cor[j] <- r_vmax
              vt        <- vt[vt != vr[j]]

            }

          }

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          "\t>>> %s x %s matrix with Escoufier's method (mixed)",
          size, size
        ),
        df_timing,
        df_timing[i, 'test']
      )

    }

  }

  on.exit(invisible(gc()), add = T)

  return(df_timing)

}



#' Fast Fourier Transformation of 2500000 (default) random values.
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_func_fft <- function(runs = 3, size = 1e6, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'fft', test_group = 'matrix_func'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          fft(rnorm(size))

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> FFT over %s random values',
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

#' Eigen values of a 640 x 640 (default) random matrix.
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_func_eigen <- function(runs = 3, size = 640, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'eigen', test_group = 'matrix_func'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          eigen(
            matrix(rnorm(size), size, size),
            symmetric = F,
            only.values = T
          )

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Eigenvalues of a %s x %s random matrix',
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

#' Determinant of a 2500 x 2500 (default) random matrix.
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_func_determinant <- function(runs = 3, size = 2500, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'determinant', test_group = 'matrix_func'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          det(matrix(rnorm(size ^ 2), size, size))

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Determinant of a %s x %s random matrix',
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

#' Cholesky decomposition of a 3000 x 3000 (default) random matrix.
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_func_cholesky <- function(runs = 3, size = 3000, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'cholesky', test_group = 'matrix_func'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          a <- crossprod(matrix(rnorm(size ^ 2), ncol = size, nrow = size))

          chol(a)

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Cholesky decomposition of a %s x %s random matrix',
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

#' Inverse of a 1600 x 1600 (default) random matrix
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_func_inverse <- function(runs = 3, size = 1600, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'inverse', test_group = 'matrix_func'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          a <- crossprod(matrix(rnorm(size ^ 2), ncol = size, nrow = size))
          solve(a)

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Inverse of a %s x %s random matrix',
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

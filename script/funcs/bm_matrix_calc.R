#' Transpose a 2500 x 2500 (default) matrix.
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_calc_manip <- function(runs = 3, size = 2500, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'manip', test_group = 'matrix_calc'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          a <- matrix(rnorm(size ^ 2) / 10, ncol = size, nrow = size)
          b <- t(a)
          dim(b) <- c(floor(size / 2), floor(size * 2))
          t(b)

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Transpose a %s x %s normal distributed matrix',
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

#' Exponentiate a 2500 x 2500 (default) matrix by 1000.
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_calc_power <- function(runs = 3, size = 2500, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'power', test_group = 'matrix_calc'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          abs(matrix(rnorm(size ^ 2) / 2, ncol = size, nrow = size)) ^ 1000

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Raise a %s x %s normal distributed matrix by 1000',
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

#' Sort a 2500 x 2500 (default) matrix.
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_calc_sort <- function(runs = 3, size = 2500, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'sort', test_group = 'matrix_calc'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          sort(
            matrix(rnorm(size ^ 2), ncol = size, nrow = size),
            method = 'quick'
          )

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Sort a %s x %s matrix',
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

#' 2500 x 2500 (default) cross-product matrix (a = a' * a).
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_calc_cross_product <- function(runs = 3, size = 2500, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'crossprod', test_group = 'matrix_calc'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          crossprod(matrix(rnorm(size ^ 2), ncol = size, nrow = size))

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          '\t>>> Cross-product a %s x %s matrix',
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

#' Linear regression over a 1000 x 1000 (default) matrix.
#'
#' @param runs
#' @param size
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
bm_matrix_calc_lm <- function(runs = 3, size = 1000, verbose = T) {

  df_timing <-
    create_timing_df(
      runs = runs, test_name = 'lm', test_group = 'matrix_calc'
    )

  for (i in seq_len(nrow(df_timing))) {

    sys_time <-
      system.time(
        {

          invisible(gc())

          a <-
            # new(
            #   'dgeMatrix',
            #   x = rnorm(size ^ 2),
            #   Dim = as.integer(c(size, size))
            # )
            matrix(rnorm(size ^ 2), ncol = size, nrow = size)
          b <- as.double(1:size)

          solve(
            crossprod(a),
            crossprod(a, b)
          )

        }
      )

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      msg_verbose(
        sprintf(
          "\t>>> Linear regression over a %s x %s matrix (c = a \\ b')",
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

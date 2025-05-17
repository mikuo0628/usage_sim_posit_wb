simulate_io <- function(
    run     = 3,
    size,
    tmpdir  = tempdir(),
    verbose = T,
    cores   = 0L
) {



}


# Helpers -----------------------------------------------------------------

bm_write_read <- function(
    runs    = 3,
    size    = 5,
    tmpdir  = tempdir(),
    verbose = T
) {

  require(magrittr)

  n    <- 62.5e3 * size
  df   <- data.frame(matrix(rnorm(n), ncol = 10))
  test <- rep(paste0('read_', size), runs)

  df_timing <-
    create_timing_df(runs, paste(sep = '_', c('write', 'read'), size), 'io')

  padding <- max(nchar(df_timing$test))

  temp_file <- tempfile(fileext = '.csv', tmpdir = tmpdir)

  for (i in seq_len(nrow(df_timing))) {

    # browser()
    invisible(gc())

    if (grepl('write', df_timing[i, 'test'])) {

      sys_time <- system.time(write.csv(df, file = temp_file, row.names = F))

    } else if (grepl('read', df_timing[i, 'test'])) {

      sys_time <- system.time(read.csv(file = temp_file))

    } else { return(NULL) }

    df_timing[i, 1:3] <- sys_time[1:3]

    if (verbose) {

      message(
        sprintf(
          paste0('\t>>> %', padding, 's: csv with %s values (size ~ %s MB)'),
          df_timing[i, 'test'], n, size
        )
      )

    }

  }

  on.exit(unlink(temp_file), add = T)
  on.exit(invisible(gc()),   add = T)

  return(df_timing)

}


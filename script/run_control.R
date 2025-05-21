############################################################################## #
#' Purpose: Control for running benchmarks to simulate usage
#'          - random run order
#'          - random runs
#'          - random size
#' Author:  Michael Kuo
############################################################################## #


# Workspace setup ---------------------------------------------------------

seed <- sample(c(100:1000), 1) # or enter your fave number

size_mult_min <- 5
size_mult_max <- 10

if (inherits(try(find.package('magrittr'), silent = T), 'try-error')) {

  renv::restore(prompt = F)

}

require(magrittr)

list.files('script/funcs', recursive = T, full.names = T) %>%
  sapply(source)



# Get func info -----------------------------------------------------------

df_funcs <-
  ls() %>%
  { .[grepl('^bm_', .)] } %>%
  { data.frame(func = .) }

## Randomize runs and sizes
for (i in seq_along(df_funcs$func)) {

  df_funcs[i, 2:3] <- formals(df_funcs[i, 'func'])[1:2]

  df_funcs[i, 'use_runs'] <- sample(c(3:10), 1)

  df_funcs[i, 'use_size'] <-
    as.integer(
      sample(
        c((df_funcs[i, 'size'] * size_mult_min):
            (df_funcs[i, 'size'] * size_mult_max)),
        1
      )
    )

}

## Randomize exec order
df_funcs <- df_funcs[sample(nrow(df_funcs)), ]



# Execute -----------------------------------------------------------------

for (i in seq_len(nrow(df_funcs))) {

  do.call(
    what = df_funcs[i, 'func'],
    args =
      df_funcs[i, c('use_runs', 'use_size')] %>%
      as.list %>%
      set_names(c('runs', 'size'))
  )

}

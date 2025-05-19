mean_times <- function(df, which_test) {

  subset(df, test == which_test)$elapsed %>%
    { .[which(. != 0)] } %>%
    mean() %>%
    format(nsmall = 3, digit = 3)

}

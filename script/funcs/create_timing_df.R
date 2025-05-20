create_timing_df <- function(runs, test_name, test_group) {

  require(magrittr)

  system.time(T) %>%
    { .[which(!is.na(.))] } %>%
    { .[which(!grepl('child', names(.)))] } %>%
    { set_names(., gsub('\\.self', '', names(.))) } %>%
    as.list() %>%
    as.data.frame() %>%
    { .[rep(1, runs), ] } %>%
    { rownames(.) <- NULL; . } %>%
    cbind(
      data.frame(
        # test             = rep(test_name, rep(runs, length(test_name))),
        test             = rep(test_name, runs),
        test_grp         = test_group,
        stringsAsFactors = F
      )
    )

}

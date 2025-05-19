msg_verbose <- function(main_msg, df, test_name) {

  message(
    paste(
      main_msg,
      sprintf(
        '~ avg %s (sec)',
        mean_times(df, test_name)
      )
    )
  )

}

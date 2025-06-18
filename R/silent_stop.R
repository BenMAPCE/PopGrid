silent_stop <- function(){
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

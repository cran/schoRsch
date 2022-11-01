# -----------------------------------------------
# Function: Clean part of global workspace. 
# -----------------------------------------------
clear_all_but <- function(keep = NULL) {
  clear_vars <- ls(envir = parent.frame())
  if (!is.null(keep)) {
    clear_vars <- clear_vars[!(clear_vars %in% keep)]
  }
  rm(list = clear_vars, envir = parent.frame())
}
#' @export
#' @rdname clean_up
clean_up_tidyverse <- function(
    lifecycles = getOption("now.lifecycles", default = c("superseded", "deprecated", "defunct")),
    force_remove = NULL,
    force_keep = NULL) {
  clean_up(tidyverse::tidyverse_packages(), lifecycles, force_remove, force_keep)
}

#' @export
#' @rdname clean_up
clean_attach <- function(
    pkgs,
    remove = getOption("now.lifecycles", default = c("superseded", "deprecated", "defunct")),
    force_remove = NULL,
    force_keep = NULL)  {
  # clean up first so we don't get unnecessary name conflict warnings
  clean_up(pkgs, remove, force_remove, force_keep)
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }
}

#' @export
#' @rdname clean_up
clean_attach_tidyverse <- function(
    remove = getOption("now.lifecycles", default = c("superseded", "deprecated", "defunct")),
    force_remove = NULL,
    force_keep = NULL)  {
  # clean up first so we don't get unnecessary name conflict warnings
  clean_up_tidyverse(remove, force_remove, force_keep)
  library(tidyverse)
}

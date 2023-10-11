#' clean up namespace and package env
#'
#' `clean_up()` removes functions of given life cycles from
#' the namespace's exports so they're not attached with `library()` and not available
#' or suggested with `::`. It also removes them from the package env (on the search path)
#' if relevant. `clean_up_tidyverse()` is just a wrapper to apply it to the tidyverse
#' packages.
#'
#' @param pkgs package names
#' @param remove lifecycles of functions to remove ("experimental", "superseded",
#' "deprecated", "defunct", "questioning"), can also include "reexports" to prevent
#' packages from reexporting functions from other packages.
#' @param force_remove,force_keep Names of additional functions to remove (resp. exceptions to keep),
#' can also be provided in the "pkg::fun" form to remove ambiguity.
#'
#' @return Returns `NULL` invisibly, called for side effects
#' @export
clean_up <- function(
    pkgs,
    remove = getOption("now.lifecycles", default = c("superseded", "deprecated", "defunct")),
    force_remove = NULL,
    force_keep = NULL)  {
  remove <- match.arg(remove, c("experimental", "superseded", "deprecated", "defunct", "questioning", "reexports"), several.ok = TRUE)
  if (is.null(force_remove)) force_remove <- character()
  if (is.null(force_keep)) force_keep <- character()
  # load namespaces first so they can populate imports properly before we remove those from the namespaces
  for (pkg in pkgs) loadNamespace(pkg)

  for (pkg in pkgs) {
    force_remove_pkg <- force_remove[startsWith(force_remove, paste0(pkg, "::")) | !grepl("::", force_remove)]
    force_remove_pkg <- sub(paste0(pkg, "::"), "", force_remove_pkg )
    force_keep_pkg <- force_keep[startsWith(force_keep, paste0(pkg, "::")) | !grepl("::", force_keep)]
    force_keep_pkg <- sub(paste0(pkg, "::"), "", force_keep_pkg)

    lifecycles_from_doc <- c(
      get_lifecycles_from_doc(pkg),
      get_lifecycles_from_code(pkg)
    )
    to_remove <- unique(names(lifecycles_from_doc[lifecycles_from_doc %in% remove]))

    to_remove <- setdiff(
      c(to_remove, force_remove),
      force_keep
    )

    ns <- asNamespace(pkg)
    if ("reexports" %in% remove) {
      imports <- parent.env(ns)
      reexports <- intersect(names(imports), getNamespaceExports(pkg))
      to_remove <- c(to_remove, reexports)
    }
    # remove relevant exports
    env <- ns$.__NAMESPACE__.$exports
    env_unlock(env)
    rm(list = intersect(to_remove, names(env)), envir = env)
    env_lock(env)

    # remove from pkg env if pkg is already attached
    pkg_env_nm <- sprintf("package:%s", pkg)
    if (pkg_env_nm %in% search()) {
      env <- as.environment(pkg_env_nm)
      env_unlock(env)
      rm(list = intersect(to_remove, names(env)), envir = env)
      env_lock(env)
      invisible(to_remove)
    }
  }
  invisible(NULL)
}

get_lifecycles_from_doc <- function(pkg) {
  # native functions
  pkg_doc <- tools::Rd_db(pkg)
  lifecycles <- unlist(unname(lapply(pkg_doc, function(doc) {
    pattern <- "^lifecycle-(.*)\\.svg$"
    desc <- structure(Filter(\(x) attr(x, "Rd_tag") == "\\description", doc)[[1]], class = "Rd")
    lifecycle_line <- grep(pattern, as.character(desc), value = TRUE)
    if (!length(lifecycle_line)) return(NULL)
    lifecycle <- sub(pattern, "\\1", lifecycle_line)
    aliases <- unlist(Filter(\(x) attr(x, "Rd_tag") == "\\alias", doc))
    setNames(rep_len(lifecycle, length(aliases)), aliases)
  })))
  lifecycles[names(lifecycles) %in% getNamespaceExports(pkg)]
}

# helps spot out a few false negatives
get_lifecycles_from_code <- function(pkg) {
  ns <- asNamespace(pkg)
  imports <- parent.env(ns)
  funs <- c(
    Filter(is.function, as.list(ns)),
    Filter(is.function, as.list(imports))
  )
  funs <- funs[names(funs) %in% getNamespaceExports(pkg)]
  lifecycles <- sapply(funs, function(x) {
    body_ <- body(x)
    if (is.symbol(body_) || rlang::is_primitive(x) || length(body_) %in% 0:1) return("regular")
    if (rlang::is_call(body_, "{")) {
      calls <- as.list(body_[-1])
      #first_call <- body_[[2]]
    } else {
      calls <- list(body_)
      #first_call <- body_
    }
    if (any(sapply(calls, is_call, "deprecate_stop"))) return("defunct")
    if (any(sapply(calls, is_call, "deprecate_warn"))) return("deprecated")
    if (any(sapply(calls, is_call, "lazy_deprec"))) return("deprecated")
    if (any(sapply(calls, is_call, "deprecate_soft"))) return("deprecated")
    ss_lgl <- sapply(calls, is_call, "signal_stage")
    if (any(ss_lgl)) return(calls[ss_lgl][[1]][[2]])
    "regular"
  })
  lifecycles[lifecycles != "regular"]
}

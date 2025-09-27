run_rhub_checks <- function(path = ".", platforms = NULL,
                            include_os = c("linux", "macos", "windows"),
                            email = NULL, confirmation = NULL) {
  if (!requireNamespace("rhub", quietly = TRUE)) {
    stop("The rhub package is required. Install it via install.packages(\"rhub\").")
  }

  available <- rhub::rhub_platforms()

  resolve_alias <- function(x) {
    idx <- match(x, available$name, nomatch = 0)
    if (idx > 0) {
      return(available$name[idx])
    }
    alias_hits <- which(vapply(available$aliases, function(al) x %in% al, logical(1)))
    if (length(alias_hits)) {
      return(available$name[alias_hits[1]])
    }
    NA_character_
  }

  if (is.null(platforms)) {
    selected <- available[available$os_type %in% include_os, , drop = FALSE]
    if (!nrow(selected)) {
      selected <- available
    }
    platforms <- selected$name
  } else if (identical(platforms, "all")) {
    platforms <- available$name
  } else {
    mapped <- vapply(platforms, resolve_alias, character(1))
    if (anyNA(mapped)) {
      stop(sprintf("Unknown platforms requested: %s", paste(platforms[is.na(mapped)], collapse = ", ")))
    }
    platforms <- unique(mapped)
  }

  if (is.null(confirmation) && !interactive()) {
    confirmation <- TRUE
  }

  rhub::rc_submit(
    path = path,
    platforms = platforms,
    email = email,
    confirmation = confirmation
  )
}

summarise_rhub_jobs <- function(submissions) {
  if (is.null(submissions)) {
    return(data.frame(name = character(), id = character(), actions_url = character(),
                      repo_url = character(), stringsAsFactors = FALSE))
  }

  if (!is.list(submissions)) {
    stop("Expected a list returned by run_rhub_checks().")
  }

  if (!is.null(submissions$result) || !is.null(submissions$actions_url)) {
    submissions <- list(submissions)
  }

  extract_field <- function(x, field) {
    value <- x[[field]]
    if (is.null(value)) NA_character_ else as.character(value)
  }

  data.frame(
    name = vapply(submissions, extract_field, character(1), field = "name"),
    id = vapply(submissions, extract_field, character(1), field = "id"),
    actions_url = vapply(submissions, extract_field, character(1), field = "actions_url"),
    repo_url = vapply(submissions, extract_field, character(1), field = "repo_url"),
    stringsAsFactors = FALSE
  )
}

# Private functions from ggplot2 used in our package
# Ideally this file would not exist :-)

check_labeller <- ggplot2:::check_labeller


# From https://github.com/tidyverse/ggplot2/blob/1f6f0cb4462aa9e3b5f51dc7fd594073e78351da/R/facet-grid-.r#L160
# Once ggplot2 3.2.0 is out, instead of defining this function use:
# grid_as_facets_list <- ggplot2:::grid_as_facets_list
# Returns a list of quosures objects. The list has exactly two elements, `rows` and `cols`.
grid_as_facets_list <- function(rows, cols) {
  is_rows_vars <- is.null(rows) || is_quosures(rows)
  if (!is_rows_vars) {
    if (!is.null(cols)) {
      stop("`rows` must be `NULL` or a `vars()` list if `cols` is a `vars()` list", call. = FALSE)
    }
    # For backward-compatibility
    facets_list <- as_facets_list(rows)
    if (length(facets_list) > 2L) {
      stop("A grid facet specification can't have more than two dimensions", call. = FALSE)
    }
    # Fill with empty quosures
    facets <- list(rows = quos(), cols = quos())
    facets[seq_along(facets_list)] <- facets_list
    # Do not compact the legacy specs
    return(facets)
  }

  is_cols_vars <- is.null(cols) || is_quosures(cols)
  if (!is_cols_vars) {
    stop("`cols` must be `NULL` or a `vars()` specification", call. = FALSE)
  }

  list(
    rows = compact_facets(as_facets_list(rows)),
    cols = compact_facets(as_facets_list(cols))
  )
}

# From https://github.com/tidyverse/ggplot2/blob/1f6f0cb4462aa9e3b5f51dc7fd594073e78351da/R/facet-.r
# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
f_as_facets_list <- function(f) {
  lhs <- function(x) if (length(x) == 2) NULL else x[-3]
  rhs <- function(x) if (length(x) == 2) x else x[-2]

  rows <- f_as_facets(lhs(f))
  cols <- f_as_facets(rhs(f))

  list(rows, cols)
}

# From https://github.com/tidyverse/ggplot2/blob/1f6f0cb4462aa9e3b5f51dc7fd594073e78351da/R/facet-.r
# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
f_as_facets <- function(f) {
  if (is.null(f)) {
    return(as_quosures(list()))
  }

  env <- f_env(f) %||% globalenv()

  # as.quoted() handles `+` specifications
  vars <- as.quoted(f)

  # `.` in formulas is ignored
  vars <- discard_dots(vars)

  as_quosures(vars, env, named = TRUE)
}

# From https://github.com/tidyverse/ggplot2/blob/1f6f0cb4462aa9e3b5f51dc7fd594073e78351da/R/facet-.r#L317
# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
# Flatten a list of quosures objects to a quosures object, and compact it
compact_facets <- function(x) {
  x <- flatten_if(x, is_list)
  null <- vapply(x, quo_is_null, logical(1))
  new_quosures(x[!null])
}


# From https://github.com/tidyverse/ggplot2/blob/1f6f0cb4462aa9e3b5f51dc7fd594073e78351da/R/facet-.r#L277
# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
as_facets_list <- function(x) {
  if (inherits(x, "uneval")) {
    stop("Please use `vars()` to supply facet variables", call. = FALSE)
  }
  if (is_quosures(x)) {
    x <- quos_auto_name(x)
    return(list(x))
  }

  # This needs to happen early because we might get a formula.
  # facet_grid() directly converted strings to a formula while
  # facet_wrap() called as.quoted(). Hence this is a little more
  # complicated for backward compatibility.
  if (is_string(x)) {
    x <- parse_expr(x)
  }

  # At this level formulas are coerced to lists of lists for backward
  # compatibility with facet_grid(). The LHS and RHS are treated as
  # distinct facet dimensions and `+` defines multiple facet variables
  # inside each dimension.
  if (is_formula(x)) {
    return(f_as_facets_list(x))
  }

  # For backward-compatibility with facet_wrap()
  if (!is_bare_list(x)) {
    x <- as_quoted(x)
  }

  # If we have a list there are two possibilities. We may already have
  # a proper facet spec structure. Otherwise we coerce each element
  # with as_quoted() for backward compatibility with facet_grid().
  if (is.list(x)) {
    x <- lapply(x, as_facets)
  }

  x
}

# Dependency of as_facets_list
as_facets <- ggplot2:::as_facets

# From https://github.com/tidyverse/ggplot2/blob/92099706e0e009fbd58bfa83f7fc8b41deec8877/R/facet-.r#L325
# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
as_quoted <- function(x) {
  if (is.character(x)) {
    if (length(x) > 1) {
      x <- paste(x, collapse = "; ")
    }
    return(parse_exprs(x))
  }
  if (is.null(x)) {
    return(list())
  }
  if (is_formula(x)) {
    return(simplify(x))
  }
  list(x)
}

# From https://github.com/tidyverse/ggplot2/blob/92099706e0e009fbd58bfa83f7fc8b41deec8877/R/facet-.r#L341
# Added because as_quoted uses it.
# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
simplify <- function(x) {
  if (length(x) == 2 && is_symbol(x[[1]], "~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3) {
    return(list(x))
  }
  op <- x[[1]]; a <- x[[2]]; b <- x[[3]]

  if (is_symbol(op, c("+", "*", "~"))) {
    c(simplify(a), simplify(b))
  } else if (is_symbol(op, "-")) {
    c(simplify(a), expr(-!!simplify(b)))
  } else {
    list(x)
  }
}

# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
# Iterate through a formula and return a quoted version
simplify_formula <- function(x) {
  if (length(x) == 2 && x[[1]] == as.name("~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3)
    return(list(x))
  op <- x[[1]]
  a <- x[[2]]
  b <- x[[3]]
  if (op == as.name("+") || op == as.name("*") || op ==
      as.name("~")) {
    c(simplify(a), simplify(b))
  }
  else if (op == as.name("-")) {
    c(simplify(a), bquote(-.(x), list(x = simplify(b))))
  }
  else {
    list(x)
  }
}

# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
as.quoted <- function(x, env = parent.frame()) {
  x <- if (is.character(x)) {
    lapply(x, function(x) parse(text = x)[[1]])
  } else if (is.formula(x)) {
    simplify_formula(x)
  } else if (is.call(x)) {
    as.list(x)[-1]
  } else {
    stop("Only knows how to quote characters, calls, and formula", call. = FALSE)
  }
  attributes(x) <- list(env = env, class = 'quoted')
  x
}

# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
is.formula <- function(x) inherits(x, "formula")

# This is a dependency of grid_as_facets_list
# When ggplot2 3.2.0 is out, remove it
discard_dots <- function(x) {
  x[!vapply(x, identical, logical(1), as.name("."))]
}

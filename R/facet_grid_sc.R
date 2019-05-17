#' Lay out panels in a grid with different scales
#'
#' `facet_grid_sc` is a variant of `facet_grid`
#' @inheritParams ggplot2::facet_grid
#' @param scales A list of two elements (`x` and `y`). Each element can be either
#' `"fixed"` (scale limits shared across facets), `"free"` (with varying limits per facet), or
#'  a named list, with a different scale for each facet value. Previous scale values
#'  (`"fixed"`, `"free_x"`, `"free_y"`, `"free"` are accepted but soft-deprecated).
#' @export
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' library(scales)
#' # Custom scales per facet:
#'  mydf <- data.frame(
#'    Subject = rep(c("A", "B", "C", "D"), each = 3),
#'    Magnitude = rep(c("SomeValue", "Percent", "Scientific"), times = 4),
#'    Value=c(c(170,0.6,2.7E-4),
#'            c(180, 0.8, 2.5E-4),
#'            c(160, 0.71, 3.2E-4),
#'            c(159, 0.62, 3E-4)))
#'
#'  scales_y <- list(
#'    Percent = scale_y_continuous(labels=percent_format()),
#'    SomeValue = scale_y_continuous(),
#'    Scientific = scale_y_continuous(labels=scientific_format())
#'  )
#'
#'  ggplot(mydf) +
#'    geom_point(aes(x=Subject, y=Value)) +
#'    facet_grid_sc(rows = vars(Magnitude), scales = list(y = scales_y))
#'
facet_grid_sc <- function(rows = NULL, cols = NULL, scales = "fixed",
                          space = "fixed", shrink = TRUE,
                          labeller = "label_value", as.table = TRUE,
                          switch = NULL, drop = TRUE, margins = FALSE,
                          facets = NULL) {
  # `facets` is soft-deprecated and renamed to `rows`
  if (!is.null(facets)) {
    rows <- facets
  }
  # Should become a warning in a future release
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }

  if (is.list(scales)) {
    free <- list(
      x = identical(scales$x, "free") || is.list(scales$x),
      y = identical(scales$y, "free") || is.list(scales$y)
    )
  } else {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
      x = any(scales %in% c("free_x", "free")),
      y = any(scales %in% c("free_y", "free"))
    )
  }

  custom_scales <- list(x = NULL, y = NULL)
  if (is.list(scales)) {
    # A different scale per facet:
    if (is.list(scales$x)) {
      if (is.null(names(scales$x))) {
        stop("Custom facet scales for x should be named according to facet column values", call. = FALSE)
      }
      custom_scales$x <- scales$x
    }
    if (is.list(scales$y)) {
      if (is.null(names(scales$y))) {
        stop("Custom facet scales for y should be named according to facet row values", call. = FALSE)
      }
      custom_scales$y <- scales$y
    }
  }

  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )

  if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }

  facets_list <- ggplot2:::grid_as_facets_list(rows, cols)
  n <- length(facets_list)
  if (n > 2L) {
    stop("A grid facet specification can't have more than two dimensions", call. = FALSE)
  }
  if (n == 1L) {
    rows <- rlang::quos()
    cols <- facets_list[[1]]
  } else {
    rows <- facets_list[[1]]
    cols <- facets_list[[2]]
  }

  # Check for deprecated labellers
  labeller <- ggplot2:::check_labeller(labeller)

  ggplot2::ggproto(NULL, FacetGridScales,
          shrink = shrink,
          params = list(rows = rows, cols = cols, margins = margins,
                        scales = custom_scales,
                        free = free, space_free = space_free, labeller = labeller,
                        as.table = as.table, switch = switch, drop = drop)
  )
}

#' ggproto facet
#'
#' @export
FacetGridScales <- ggplot2::ggproto(
  "FacetGridScales", ggplot2::FacetGrid,
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- list()
    if (!is.null(params$scales$x)) {
      facet_x_names <- as.character(layout[[names(params$cols)]])
      scales$x <- lapply(params$scales$x[facet_x_names], function(x) x$clone())
    } else if (!is.null(x_scale)) {
      scales$x <- lapply(seq_len(max(layout$SCALE_X)), function(i) x_scale$clone())
    }
    if (!is.null(params$scales$y)) {
      facet_y_names <- as.character(layout[[names(params$rows)]])
      scales$y <- lapply(params$scales$y[facet_y_names], function(x) x$clone())
    } else if (!is.null(y_scale)) {
      scales$y <- lapply(seq_len(max(layout$SCALE_Y)), function(i) y_scale$clone())
    }
    scales
  }
)

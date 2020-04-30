#' Lay out panels in a grid with different scales
#'
#' `facet_grid_sc` is a variant of `facet_grid`
#' @inheritParams ggplot2::facet_grid
#' @param scales A list of two elements (`x` and `y`). Each element can be either
#' `"fixed"` (scale limits shared across facets), `"free"` (with varying limits per facet), or
#'  a named list, with a different scale for each facet value. Previous scale values
#'  (`"fixed"`, `"free_x"`, `"free_y"`, `"free"` are accepted but soft-deprecated).
#' @export
#' @import rlang
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

  facets_list <- grid_as_facets_list(rows, cols)

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  ggproto(NULL, FacetGridScales,
          shrink = shrink,
          params = list(rows = facets_list$rows, cols = facets_list$cols, margins = margins,
                        scales = custom_scales,
                        free = free, space_free = space_free, labeller = labeller,
                        as.table = as.table, switch = switch, drop = drop)
  )
}


#' ggproto facet
#'
#' @export
FacetGridScales <- ggproto(
  "FacetGridScales", FacetGrid,
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- list()
    if (!is.null(params$scales$x)) {
      facet_x_names <- unique(as.character(layout[[names(params$cols)]]))
      scales$x <- lapply(params$scales$x[facet_x_names], function(x) {
        new <- x$clone()
        new$oob <- function(x, ...) x
        new
      })
    } else if (!is.null(x_scale)) {
      scales$x <- lapply(seq_len(max(layout$SCALE_X)), function(i) x_scale$clone())
    }
    if (!is.null(params$scales$y)) {
      facet_y_names <- unique(as.character(layout[[names(params$rows)]]))
      scales$y <- lapply(params$scales$y[facet_y_names], function(x){
        new <- x$clone()
        new$oob <- function(x, ...) x
        new
      })
    } else if (!is.null(y_scale)) {
      scales$y <- lapply(seq_len(max(layout$SCALE_Y)), function(i) y_scale$clone())
    }
    scales
  },
  train_scales = function(x_scales, y_scales, layout, data, params, self) {
    # Transform data first
    data <- lapply(data, function(layer_data) {
      self$finish_data(layer_data, layout,
                       x_scales, y_scales, params)
    })

    # Then use parental method for scale training
    ggproto_parent(Facet, self)$train_scales(x_scales, y_scales,
                                             layout, data, params)
  },
  finish_data = function(data, layout, x_scales, y_scales, params) {
    # Divide data by panel
    panels <- split(data, data$PANEL, drop = T)
    panels <- lapply(names(panels), function(i) {
      dat  <- panels[[i]]

      # Match panel to their scales
      panel_id <- match(as.numeric(i), layout$PANEL)
      xidx <- layout[panel_id, "SCALE_X"]
      yidx <- layout[panel_id, "SCALE_Y"]

      # Decide what variables need to be transformed
      y_vars <- intersect(y_scales[[yidx]]$aesthetics, names(dat))
      x_vars <- intersect(x_scales[[xidx]]$aesthetics, names(dat))

      # Get scales
      y_scale <- y_scales[[yidx]]
      x_scale <- x_scales[[xidx]]

      # It doesn't make sense to perform transforms on discrete scales
      if (!is.null(x_scale) && !x_scale$is_discrete()) {
        # Get inverse transforms for class checking
        x_inv <- x_scale$trans$inverse(1)
        for (j in x_vars) {
          if (inherits(dat[[j]], class(x_inv))) {
            # Transform variables by appropriate scale
            dat[, j] <- x_scale$transform(dat[, j])
          }
        }
      }

      if (!is.null(y_scale) && !y_scale$is_discrete()) {
        y_inv <- y_scale$trans$inverse(1)
        for (j in y_vars) {
          if (inherits(dat[[j]], class(y_inv))) {
            dat[, j] <- y_scale$transform(dat[, j])
          }
        }
      }

      dat
    })

    # Recombine the data
    data <- unsplit(panels, data$PANEL)
    data
  }
)

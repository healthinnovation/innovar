#' Vector of custom colors
#'
#'
#' @export
lis_colors <- c(
  `ccvi1`  = "#005378",
  `ccvi2`  = "#006FA0",
  `ccvi3`  = "#5C99B3",
  `ccvi4`  = "#9BCFC8",
  `ccvi5`  = "#F2DDBD",
  `npr1`  = "#03595C",
  `npr2`  = "#58C3B0",
  `npr3`  = "#E7E2BF",
  `npr4`  = "#EFA255",
  `npr5`  = "#DC4326",
  `Blmbrg1`  = "#C397DC",
  `Blmbrg2`  = "#9DFAF0",
  `Blmbrg3`  = "#85D8FC",
  `Blmbrg4`  = "#F0F0F0",
  `ecomst1`  = "#EC251D",
  `ecomst2`  = "#F25941",
  `ecomst3`  = "#F58A6A",
  `ecomst4`  = "#F8B797",
  `ecomst5`  = "#FED1BE",
  `ecomst6`  = "#CFCDC6",
  `ctp1`  = "#1C0B07",
  `ctp2`  = "#632718",
  `ctp3`  = "#D04D39",
  `ctp4`  = "#ED9991",
  `ctp5`  = "#F9DDDC",
  `jama1`  = "#0B4E60",
  `jama2`  = "#186179",
  `jama3`  = "#2E7998",
  `jama4`  = "#2C9BB4",
  `jama5`  = "#39A69F",
  `jama6`  = "#7DC9C1",
  `jama7`  = "#DAE2BC",
  `jama8`  = "#FEBE82",
  `jama9`  = "#F69354",
  `jama10`  = "#8C3A33",
  `mlobo1`  = "#191E34",
  `mlobo2`  = "#094B56",
  `mlobo3`  = "#00787C",
  `mlobo4`  = "#3DA48F",
  `mlobo5`  = "#FEECD5",
  `btran1`  = "#7F1D6E",
  `btran2`  = "#DA2D7A",
  `btran3`  = "#E3506E",
  `btran4`  = "#F68671",
  `btran5`  = "#FEDCA0",
  `nasa1`  = "#070510",
  `nasa2`  = "#240F4F",
  `nasa3`  = "#501278",
  `nasa4`  = "#942A7F",
  `nasa5`  = "#C33C72",
  `nasa6`  = "#F7705E",
  `nasa7`  = "#F5E6B2",
  `nasa8`  = "#B2CFD3",
  `nasa9`  = "#87BEEB",
  `nasa10`  = "#6BB4F8"
  )

#' Function to extract lis colors as hex codes
#'
#' @param ... Character names of lis_colors
#'
#' @export lis_cols

lis_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (lis_colors)

  lis_colors[cols]
}

#' Vector of custom palettes
#'
#'
#' @export

lis_palettes <- list(
  `ccvi`  = lis_cols("ccvi1", "ccvi2", "ccvi3", "ccvi4", "ccvi5"),
  `npr`  = lis_cols("npr1", "npr2", "npr3", "npr4", "npr5"),
  `Blmbrg`  = lis_cols("Blmbrg1", "Blmbrg2", "Blmbrg3", "Blmbrg4"),
  `ecomst`  = lis_cols("ecomst1", "ecomst2", "ecomst3", "ecomst4","ecomst5","ecomst6"),
  `ctp`  = lis_cols("ctp1", "ctp2", "ctp3", "ctp4","ctp5"),
  `jama`  = lis_cols("jama1", "jama2", "jama3", "jama4","jama5","jama6","jama7","jama8","jama9","jama10"),
  `mlobo`  = lis_cols("mlobo1", "mlobo2", "mlobo3", "mlobo4","mlobo5"),
  `btran`  = lis_cols("btran1", "btran2", "btran3", "btran4","btran5"),
  `nasa`  = lis_cols("nasa1", "nasa2", "nasa3", "nasa4","nasa5","nasa6","nasa7","nasa8","nasa9","nasa10")
)

#' Return function to interpolate a lis color palette
#'
#' @param palette Character name of palette in lis_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @importFrom grDevices colorRampPalette
#' @export lis_pal
lis_pal <- function(palette = "ccvi", reverse = FALSE, ...) {
  pal <- lis_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor for lis colors
#'
#' @param palette Character name of palette in lis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @examples
#'\dontrun{
#' library(unikn)
#' library(tidyverse)
#' names(lis_palettes) %>%
#' map(.f = ~lis_pal(., reverse = T)(10)) %>%
#' seecol(pal_names = names(lis_palettes))
#'}
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#' @export scale_color_lis

scale_color_lis <- function(palette = "ccvi", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lis_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("lis_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for lis colors
#'
#' @param palette Character name of palette in lis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @examples
#'\dontrun{
#' library(unikn)
#' library(tidyverse)
#' names(lis_palettes) %>%
#' map(.f = ~lis_pal(., reverse = T)(10)) %>%
#' seecol(pal_names = names(lis_palettes))
#'}
#'
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#' @export scale_fill_lis
scale_fill_lis <- function(palette = "ccvi", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lis_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("lis_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

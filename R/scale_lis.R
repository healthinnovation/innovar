# Palettes as vectors of colors

ccvi <- c(
  `ccvi1`  = "#005378", `ccvi2`  = "#006FA0", `ccvi3`  = "#5C99B3",
  `ccvi4`  = "#9BCFC8", `ccvi5`  = "#F2DDBD"
)

npr <- c(
  `npr1`  = "#03595C", `npr2`  = "#58C3B0", `npr3`  = "#E7E2BF",
  `npr4`  = "#EFA255", `npr5`  = "#DC4326"
)

blmbrg <- c(
  `blmbrg1`  = "#C397DC", `blmbrg2`  = "#9DFAF0", `blmbrg3`  = "#85D8FC",
  `blmbrg4`  = "#F0F0F0"
)

ecomst <- c(
  `ecomst1`  = "#EC251D", `ecomst2`  = "#F25941", `ecomst3`  = "#F58A6A",
  `ecomst4`  = "#F8B797", `ecomst5`  = "#FED1BE", `ecomst6`  = "#CFCDC6"
)

ctp <- c(
  `ctp1`  = "#1C0B07", `ctp2`  = "#632718", `ctp3`  = "#D04D39",
  `ctp4`  = "#ED9991", `ctp5`  = "#F9DDDC"
)

jama <- c(
  `jama1`  = "#0B4E60", `jama2`  = "#186179", `jama3`  = "#2E7998",
  `jama4`  = "#2C9BB4", `jama5`  = "#39A69F", `jama6`  = "#7DC9C1",
  `jama7`  = "#DAE2BC", `jama8`  = "#FEBE82", `jama9`  = "#F69354",
  `jama10`  = "#8C3A33"
)

mlobo <- c(
  `mlobo1`  = "#191E34", `mlobo2`  = "#094B56", `mlobo3`  = "#00787C",
  `mlobo4`  = "#3DA48F", `mlobo5`  = "#FEECD5"
)

btran <- c(
  `btran1`  = "#7F1D6E", `btran2`  = "#DA2D7A", `btran3`  = "#E3506E",
  `btran4`  = "#F68671", `btran5`  = "#FEDCA0"
)

nasa <- c(
  `nasa1`  = "#070510", `nasa2`  = "#240F4F", `nasa3`  = "#501278",
  `nasa4`  = "#942A7F", `nasa5`  = "#C33C72", `nasa6`  = "#F7705E",
  `nasa7`  = "#F5E6B2", `nasa8`  = "#B2CFD3", `nasa9`  = "#87BEEB",
  `nasa10`  = "#6BB4F8"
)

# List of palettes

lis_palettes <- list(
  `ccvi` = ccvi, `npr` = npr, `blmbrg` = blmbrg, `ecomst` = ecomst, `ctp` = ctp,
  `jama` = jama, `mlobo` = mlobo, `btran` = btran, `nasa` = nasa
)

#' Return function to interpolate a lis color palette
#'
#' @param palette Character name of palette in lis_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @examples
#'\dontrun{
#' library(scales)
#' pal <- lis_pal("ccvi")(9)
#' show_col(pal)
#'}
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
#' library(ggplot2)
#'
#' # Default discrete palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 4) +
#' scale_color_lis()
#'
#' # Default continuous palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' geom_point(size = 4, alpha = .6) +
#' scale_color_lis(discrete = FALSE)
#'}
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#' @export scale_color_lis

scale_color_lis <- function(palette = "ccvi", discrete = TRUE, reverse = FALSE,
                            ...) {
  pal <- lis_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("lis_", palette), palette = pal, ...)
  }
  else {
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
#' library(ggplot2)
#'
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#' geom_bar() +
#' theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#' scale_fill_lis()
#'}
#'
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#' @export scale_fill_lis

scale_fill_lis <- function(palette = "ccvi", discrete = TRUE, reverse = FALSE,
                           ...) {
  pal <- lis_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("lis_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' Show a set of palette color Lab
#' @param name Character name of palette in lis_palettes
#' @param rev  Boolean indicating whether the palette should be reversed
#' @param n    Integer, number of colors
#' @param ...  Additional arguments passed to seecol()
#'
#' @details Name of color palette available
#' \itemize{
#' \item \bold{ccvi}
#' \item \bold{npr}
#' \item \bold{blmbrg}
#' \item \bold{ctp}
#' \item \bold{jama}
#' \item \bold{mlobo}
#' \item \bold{btran}
#' \item \bold{nasa}
#' }
#' @examples
#'\dontrun{
#' library(innovar)
#' show_pa(name = "nasa",rev = TRUE, n = 5)
#' show_pal()
#'
#'}
#'@export show_pal

show_pal <- function(name = "all",n = 5,rev = TRUE,...){
  if(name %in% names(lis_palettes)){
    lis_names <- name
    lis_panel <- lis_names %>%
      map(.f = ~lis_pal(.,reverse = rev)(n=n)) %>%
      unikn::seecol(
        pal_names = names(lis_names),
        title = sprintf(
          "%s%s",'Name of the color palette : ',
          toupper(lis_names))
        ,...
      )
  } else if (name == "all"){
    lis_names <- names(lis_palettes)
    lis_panel <- lis_names %>%
      map(.f = ~lis_pal(.,reverse = rev)(n=n)) %>%
      unikn::seecol(
        pal_names = lis_names,
        title = "Name of all innovar colour palettes",
        ...
      )
  } else{
    stop("Color palette is incorrect,please use show_pal() and choose a color")
  }

  }

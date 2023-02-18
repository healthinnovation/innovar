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

politico <- c(
  `strongly` = "#F1BBAC",`supporting` = "#F8D2B0",`mixed` = "#DBDADA",
  `nosupporting` = "#C67399",`opposing` = "#9C4482"
)

mortality <- c(
  `veryhigh` ="#6E0900",`high` = "#FF4D00",`middle` ="#7BDBD5",
  `low` = "#006495",`verylow` = "#4C194A"
  )

# The 08 best colour palettes for 2020

green <- c(
  `veryhigh` ="#bc6c25",`high` = "#dda15e",`middle` ="#fefae0",
  `low` = "#283618",`verylow` = "#606c38"
)

golden <- c(
  `veryhigh` ="#772e25",`high` = "#c44536",`middle` ="#edddd4",
  `low` = "#197278",`verylow` = "#283d3b"
)

dark_green <- c(
  `veryhigh` ="#2f3e46",`high` = "#354f52",`middle` ="#52796f",
  `low` = "#84a98c",`verylow` = "#cad2c5"
  )

blue_fall <- c(
  `veryhigh` ="#eaac8b",`high` = "#e56b6f",`middle` ="#b56576",
  `low` = "#6d597a",`verylow` = "#355070"
)

vermilion <- c(
  `veryhigh` ="#ffba08",`high` = "#f48c06",`middle` ="#dc2f02",
  `low` = "#6a040f",`verylow` = "#03071e"
)

wheat <- c(
  `veryhigh` ="#432534",`high` = "#c44900",`middle` ="#efd6ac",
  `low` = "#183a37",`verylow` = "#04151f"
)

peach <- c(
  `veryhigh` ="#041f1e",`high` = "#1e2d2f",`middle` ="#c57b57",
  `low` = "#f1ab86",`verylow` = "#f7dba7"
)

persian <- c(
  `veryhigh` ="#495867",`high` = "#c18c5d",`middle` ="#ce796b",
  `low` = "#e7ad99",`verylow` = "#ecc8af"
)

# List of palettes

innova_palettes <- list(
  `ccvi` = ccvi, `npr` = npr, `blmbrg` = blmbrg, `ecomst` = ecomst, `ctp` = ctp,
  `jama` = jama, `mlobo` = mlobo, `btran` = btran, `nasa` = nasa,
  `politico` = politico, `mortality` = mortality,
  `green` = green,`golden` = golden,`dark_green` = dark_green,
  `blue_fall` = blue_fall,`vermilion` = vermilion,
  `wheat` = wheat,`peach` = peach,`persian` = persian
)

#' Return function to interpolate a lis color palette
#'
#' @param palette Character name of palette in lis_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @examples
#'\dontrun{
#' library(innovar)
#' library(scales)
#' pal <- innova_pal("ccvi")(9)
#' show_col(pal)
#'}
#'
#' @importFrom grDevices colorRampPalette
#' @export innova_pal

innova_pal <- function(palette = "ccvi", reverse = FALSE, ...) {
  pal <- innova_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal,...)
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
#' library(innovar)
#' # Default discrete palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 4) +
#' scale_color_innova()
#'
#' # Default continuous palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' geom_point(size = 4, alpha = .6) +
#' scale_color_innova(discrete = FALSE)
#'}
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#' @export scale_color_innova

scale_color_innova <- function(palette = "ccvi", discrete = TRUE, reverse = FALSE,
                            ...) {
  pal <- innova_pal(palette = palette, reverse = reverse)
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
#' library(innovar)
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#' geom_bar() +
#' theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#' scale_fill_innova()
#'}
#'
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#' @export scale_fill_innova

scale_fill_innova <- function(palette = "ccvi", discrete = TRUE, reverse = FALSE,
                           ...) {
  pal <- innova_pal(palette = palette, reverse = reverse)

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
#' \item \bold{politico}
#' \item \bold{mortality}
#' }
#' @examples
#' \dontrun{
#' library(innovar)
#' # show_pal(name = "nasa",rev = TRUE, n = 5)
#' show_pal()
#' }
#'@export show_pal

show_pal <- function(name = "all",n = 5,rev = TRUE,...){
  require(unikn)
  if(sum(unique(name %in% names(innova_palettes))) == 1) {
    list_names <- innova_palettes[name]
    range_color <- sapply(X = list_names,FUN = function(x){list(x[1:n])})

    if(rev == 1){
      list_panel <- rev(range_color) %>%
        map(.f = ~.) %>%
        unikn::seecol(
          pal_names = names(list_names),
          title = "Name of specific innovar colour palettes"
          ,...
        )
    }else{
      list_panel <- range_color %>%
        map(.f = ~.) %>%
        unikn::seecol(
          pal_names = names(list_names),
          title = "Name of specific innovar colour palettes"
          ,...
        )
    }

  } else if (name == "all"){
    list_names <- names(innova_palettes)
    list_panel <- list_names %>%
      map(.f = ~innova_pal(.,reverse = rev)(n=n)) %>%
      unikn::seecol(
        pal_names = list_names,
        title = "Name of all innovar colour palettes",
        ...
      )
    } else {
    stop("Color palette is incorrect,please use show_pal() and choose a color")
  }

}


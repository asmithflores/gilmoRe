#' Color palettes
#'
#' @export
gilmore_palettes <- list(
  `lorelai` = c("#B0568B", "#4D505A", "#AA6F6C", "#763C39", "#1C0504"),
  `rory` = c("#353573", "#5478BF", "#D7E7FF", "#FF9D92", "#46201D"),
  `sookie` = c("#FFBFA5", "#F87B75", "#E75782", "#640A02", "#2338AF"),
  `emily` = c("#DAB6C4", "#DBAA70", "#CB9B8D", "#984849", "#0E0508"),
  `lane` = c("#E0F0F6", "#F2B9B6", "#FCE1BB", "#2A5972", "#2F3D43"),
  `luke` = c("#A39998", "#182454", "#32341D", "#22222A", "#13140C"),
  `dean` = c("#0D070A", "#C48667", "#154018", "#0D260C", "#8C5642"),
  `jess` = c("#261D1F", "#594F54", "#110914", "#B6B6B6", "#B1714B"),
  `logan` = c("#DADAF2", "#C5937C", "#D4987E", "#121617", "#674225"),
  `kirk` = c("#72211D", "#2F2724", "#F2C3A7", "#D99E89", "#AA8654")
)


#' Function to interpolate a color palette
#'
#' @param palette Character name of palette in gilmore_palettes
#' @param reverse Boolean true if palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return A vector of colors
#' @export
gilmore_pal <- function(palette = "lorelai", reverse = FALSE, ...){
  pal <- gilmore_palettes[[palette]]

  if(reverse){
    pal <- rev(pal)
  }

  grDevices::colorRampPalette(pal, ...)
}

#' Color scale for Gilmore Girls colors
#'
#' @param palette Character name of palette in gilmore_palettes
#' @param discrete Boolean if color aesthetic is discrete
#' @param reverse Boolean indicating whether palette should be reversed
#' @param ... Additional arguments used to discrete_scale() or scale_fill_gradientn()
#'   to automatically interpolate between colours.
#'
#' @return No return value. Called for side effects
#' @export
#' @examples
#' library(ggplot2)
#' data <- data.frame(c = LETTERS[1:3],x = c(1,5,7),y = c(5,9,13))
#' ggplot(data, aes(x,y,color = c))+geom_point()+scale_color_gilmore()
scale_color_gilmore <- function(palette = "lorelai",
                               discrete = TRUE, reverse=FALSE,...){
  pal <- gilmore_pal(palette = palette, reverse = reverse)

  if(discrete){
    ggplot2::discrete_scale("colour", paste0("gilmore_", palette), palette = pal, ...)
  }else{
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale for Gilmore Girls colors
#'
#' @param palette Character name of palette in gilmore_palettes
#' @param discrete Boolean if color aesthetic is discrete
#' @param reverse Boolean if palette should be reversed
#' @param ... Additional arguments used to discrete_scale() or scale_fill_gradientn()
#'   to automatically interpolate between colours.
#'
#' @return No return value. Called for side effects
#' @export
#' @examples
#' library(ggplot2)
#' data <- data.frame(c = LETTERS[1:3],x = c(1,5,7),y = c(5,9,13))
#' ggplot(data, aes(x,fill=c))+geom_bar()+scale_fill_gilmore()
scale_fill_gilmore <- function(palette = "lorelai",
                              discrete = TRUE, reverse = FALSE, ...){
  pal <- gilmore_pal(palette = palette, reverse = reverse)

  if(discrete){
    ggplot2::discrete_scale("fill", paste0("gilmore_", palette), palette = pal, ...)
  }else{
    ggplot2::scale_fill_gradientn(colours = pal(256),...)
  }
}

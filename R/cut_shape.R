
#' Cuts shapes
#'
#' @param image
#' This image you want to cut the shape from.
#' can be either an image(e.g. URL)
#' @param shape
#' The image that contains the shape you want to cut
#' @param color
#'  The color that identifies the shape, as a hex code eg .00xx
#' @param fuzz
#'How well to match the color of the pixels.
#' @return
#' retuns a cut shape
#' @export
#'
#' @examples
#' cut_shape("https://cdn.pixabay.com/photo/2019/09/08/19/01/pumpkin-4461665_1280.jpg", "https://imageog.flaticon.com/icons/png/512/30/30209.png?size=1200x630f&pad=10,10,10,10&ext=png&bg=FFFFFFFF")
#'
#'
cut_shape <- function(image, shape, color = "#000000", fuzz = 10){
  # read the images if they are not already
  if(!typeof(image) == "externalptr"){
    image %>% magick::image_read() -> image
  }

  if(!typeof(shape) == "externalptr"){
    shape %>% magick::image_read() -> shape
  }

  # make the specified color transparent
  shape %>%
    magick::image_transparent(color = color, fuzz = fuzz) -> shape
  shape_width <- shape %>% magick::image_info() %>% dplyr::pull(width)
  shape_height <- shape %>% magick::image_info() %>% dplyr::pull(height)

  # resize the image to be cut from to match the shape
  image_width <- image %>% magick::image_info() %>% dplyr::pull(width)
  image_height <- image %>% magick::image_info() %>% dplyr::pull(height)
  scale_factor <- max(shape_width/image_width, shape_height/image_height)
  # for now, take the crop from the middle of the image
  image %>% magick::image_scale(scale_factor * image_width) %>%
    magick::image_crop(
      paste0(shape_width,"x",shape_height,
             "+",
             (magick::image_info(.)$width - shape_width)/2,
             "+",
             (magick::image_info(.)$height - shape_height)/2)) -> image

  # make the cut!
  magick::image_flatten(c(image, shape)) %>%
    magick::image_trim()
}

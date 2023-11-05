# Dependencies
library(magick)
library(purrr)

#' Hex wall
#' 
#' Create a wall of hex stickers
#' @param path The path to a folder of hexagon stickers
#' @param sticker_row_size The number of stickers in the longest row
#' @param sticker_width The width of each sticker in pixels
#' @param remove_small Should hexagons smaller than the sticker_width 
#' be removed?
#' @param coords A data.frame of coordinates defining the placement of hexagons
#' @param scale_coords Should the coordinates be scaled to the hexagon size?
#' @param remove_size Should hexagons of an abnormal size be removed?
#' @param sort_mode How should the files be sorted?
#' @param background_color The colour of the background canvas
#' @param n_stickers The number of hexagons to produce. Recycled in file order.
hexwall <- function(sticker_files, 
                    sticker_row_size = 16, 
                    sticker_width = 500, 
                    remove_small = TRUE,
                    total_stickers = NULL, 
                    remove_size = FALSE,
                    coords = NULL, 
                    scale_coords = TRUE,
                    sort_mode = c("filename", "random", "color", "colour"), 
                    background_color = "transparent", 
                    n_stickers = NULL){
  # devoptera::args2vars(hexwall)
  
  sort_mode <- match.arg(sort_mode)
  sort_mode <- sort_mode[1]
  # Load stickers
  # sticker_files <- list.files(path)
  if(is.null(n_stickers)) n_stickers <- length(sticker_files)
  sticker_files <- rep_len(sticker_files, n_stickers)
  stickers <- sticker_files %>% 
    purrr::map(function(path){
      switch(tools::file_ext(path),
             svg = magick::image_read_svg(path),
             pdf = magick::image_read_pdf(path),
             magick::image_read(path))
    }) %>%
    purrr::map(magick::image_transparent, "white") %>%
    purrr::map(magick::image_trim) %>%
    purrr::set_names(sticker_files)
  
  # Low resolution stickers
  low_res <- stickers %>%
    purrr::map_lgl(~ remove_small && 
                     magick::image_info(.x)$width < (sticker_width-1)/2 && 
                     magick::image_info(.x)$format != "svg")
  # which(low_res)
  stickers <- stickers %>%
    purrr::map(magick::image_scale, sticker_width)
  # Incorrectly sized stickers
  bad_size <- stickers %>%
    purrr::map_lgl(~ remove_size && 
                     with(magick::image_info(.x), 
                          height < (median(height)-2) | 
                            height > (median(height) + 2)))
  # which(bad_size)
  
  # Remove bad stickers
  sticker_rm <- low_res | bad_size
  stickers <- stickers[!sticker_rm]
  
  if(any(sticker_rm)){
    message(sprintf("Automatically removed %i incompatible stickers: %s",
                    sum(sticker_rm), paste0(names(sticker_rm[sticker_rm]), collapse = ", ")))
  }

  if(is.null(total_stickers)){
    if(!is.null(coords)){
      total_stickers <- NROW(coords)      
    }
    else{
      total_stickers <- length(stickers)
    }
  }
  
  # Coerce sticker sizes
  sticker_height <- stickers %>%
    purrr::map(magick::image_info) %>%
    purrr::map_dbl("height") %>% ## <- important
    median
  stickers <- purrr::map(stickers,
                         magick::image_resize, 
                         paste0(sticker_width, "x", sticker_height, "!"))
  
  # Repeat stickers sorted by file name
  stickers <- rep_len(stickers, total_stickers)
  
  if(sort_mode == "random"){
    # Randomly arrange stickers
    stickers <- sample(c(stickers, sample(stickers, total_stickers - length(stickers), replace = TRUE))) 
  } else if(sort_mode %in% c("color", "colour")){
    # Sort stickers by colour
    sticker_col <- stickers %>% 
      purrr::map(magick::image_resize, "1x1!") %>%
      purrr::map(magick::image_data) %>%
      purrr::map(~paste0("#", paste0(.[,,1], collapse=""))) %>%
      purrr::map(colorspace::hex2RGB) %>%
      purrr::map(as, "HSV") %>%
      purrr::map_dbl(~.@coords[,1]) %>%
      sort(index.return = TRUE) %>%
      .$ix
    stickers <- stickers[sticker_col]
  }
  
  if(is.null(coords)){
    # Arrange rows of stickers into images
    sticker_col_size <- ceiling(length(stickers)/(sticker_row_size-0.5))
    row_lens <- rep(c(sticker_row_size,sticker_row_size-1), length.out=sticker_col_size)
    row_lens[length(row_lens)] <- row_lens[length(row_lens)]  - (length(stickers) - sum(row_lens))
    sticker_rows <- purrr::map2(row_lens, cumsum(row_lens),
                         ~ seq(.y-.x+1, by = 1, length.out = .x)) %>%
      purrr::map(~ stickers[.x] %>%
                   purrr::invoke(c, .) %>%
            image_append)
    
    # Add stickers to canvas
    canvas <- magick::image_blank(
      sticker_row_size*sticker_width, 
      sticker_height + (sticker_col_size-1)*sticker_height/1.33526, "white")
    purrr::reduce2(sticker_rows, seq_along(sticker_rows), 
            ~ magick::image_composite(
              ..1, ..2,
              offset = paste0("+", ((..3-1)%%2)*sticker_width/
                                2, "+", round((..3-1)*sticker_height/1.33526))
            ),
            .init = canvas)
  }
  else{
    sticker_pos <- coords
    if(scale_coords){
      sticker_pos <- sticker_pos %>% 
        as_tibble %>%
        dplyr::mutate_all(function(x){
          x <- x-min(x)
          dx <- diff(sort(abs(x)))
          x / min(dx[dx!=0])
        }) %>%
        dplyr::mutate(y = y / min(diff(y)[diff(y)!=0])) %>%
        dplyr::mutate(x = x*sticker_width/2,
               y = abs(y-max(y))*sticker_height/1.33526)
    }
    
    # Add stickers to canvas
    canvas <- magick::image_blank(max(sticker_pos$x) + sticker_width, 
                          max(sticker_pos$y) + sticker_height, background_color)
    purrr::reduce2(stickers, sticker_pos%>%split(1:NROW(.)), 
            ~ magick::image_composite(
              ..1, ..2,
              offset = paste0("+", ..3$x, "+", ..3$y)
            ),
            .init = canvas)
  }
}

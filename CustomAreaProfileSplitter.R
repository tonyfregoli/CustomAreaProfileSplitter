### Script Writer: Tony Fregoli
### Script date: 03/02/2023
### Script Purpose: Tile Composer

library(pacman)
p_load(tidyverse, magick, vroom, stringr)


# Parameters
  USERPROFILE         <- gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
  WORKING_PATH        <- paste("", sep="")
  IMAGES_INPUT_PATH   <- paste(WORKING_PATH, "Input/", sep="")
  PATTERNS_INPUT_PATH <- paste(WORKING_PATH, "Patterns/", sep="")
  OUTPUT_PATH         <- paste(WORKING_PATH, "Output/", sep="")

  PADDING             <- 20
  TILE_WIDTHS         <- vroom(paste(WORKING_PATH, "TileWidths.csv", sep=''))
  TILE_HEIGHTS        <- vroom(paste(WORKING_PATH, "TileHeights.csv", sep=''))
  TILE_NAMES          <- vroom(paste(WORKING_PATH, "TileNames.csv", sep=''))
  
# Functions
getImages <- function(path) {
  # gets images and returns object list(name, image)
  # p - path
  image <- list()
  name <- list()
  
  files <- list.files(path=path, pattern = "\\.png$")
  image <- lapply(files, function(x) image_read(paste(path,x,sep='')))
  name  <- lapply(files, function(x) gsub('.png','', x))

  output <- list(name, image)
  names(output) <- c("name", "image")
  
  return(output)
}

getPatterns <- function(path) {
  # gets patterns and returns object list(name, pattern)
  # p - path
  pattern <- list()
  name <- list()
  
  files <- list.files(path=path, pattern = "^pattern_.+\\.csv")
  pattern <- lapply(files, function(x) vroom(paste(path,x,sep=''), col_names = FALSE, delim =","))
  name <- lapply(files, function(x) str_match(x, regex("_(.+)\\."))[2])

  output <- list(name, pattern)
  names(output) <- c("name", "pattern")
  
  return(output)
}

splitToTiles <- function(image, tnames, twidths, theights) {
  # splits input image into its tiles returns list(name, tile)
  # image
  # tnames
  # twidths
  # theights
  
  tile <- list()
  name <- list()
  
  for (r in 1:nrow(tnames)) {
    for (c in 1:ncol(tnames)) {
      
      i <- (r-1)*ncol(tnames) + c
      
      height <- theights$height[r]
      width  <- twidths$width[c]
      top    <- theights$top[r]
      left   <- twidths$left[c]
      
      tile_parm  <- paste(width, "x", height, "+", left, "+", top, sep='')
      tile_name  <- tnames[[r,c]]
      tile_image <- image_crop(image, tile_parm)
      
      if(tile_name == 'blank') {
        tile_image <- image_fill(tile_image, "#F5F5F6", point = "+10+10")
      }
      
      tile[[i]] <- tile_image
      name[[i]] <- tile_name
      
    }
  }

  output <- list(name, tile)
  names(output) <- c("name", "tile")

  return(output)
}

composeTiles <- function(image_tiles, pattern, padding = 0, bg = "#F5F5F6") {
  # compose tiles according to layout pattern 
  # image_tiles - object as returned from splitToTiles()
  # pattern - custom layout pattern (df)
  # padding - padding
  # bg - background filler colour 

  pattern <- pattern %>%
    map(function(x) match(x, image_tiles$name)) %>%
    as_tibble()

  for(r in 1:nrow(pattern)) {
    
    for(c in 1:ncol(pattern)) {

      i <- pattern[[r,c]]
      tile <- image_tiles$tile[[i]] %>%
        image_background(bg)
      
      if(c == 1) {
        rowTilesVector <- c(tile)
      } else {
        rowTilesVector <- append(rowTilesVector, tile)
      }
      
      if(padding > 0) {
        paddingHeight <- sapply(rowTilesVector, function(x) image_info(x)[['height']]) %>%
          max()
        #print(paddingHeight)
      }
      
    }

    if(padding > 0) {
      paddingTile = image_blank(padding, paddingHeight, "white") %>%
        image_background(bg)
      rowTilesVectorPadded <- c(paddingTile)
      
      for(i in 1:length(rowTilesVector)) {
        rowTilesVectorPadded <- rowTilesVectorPadded %>%
          append(rowTilesVector[i]) %>%
          append(paddingTile)
      }
      
      rowTilesVector <- rowTilesVectorPadded
    }
    
    rowTilesComposed <- image_append(rowTilesVector)

    if(r == 1) {
      imageComposedVector <- c(rowTilesComposed)
    } else {
      imageComposedVector <- append(imageComposedVector, rowTilesComposed)
    }
    
    if(padding > 0) {
      paddingWidth <- sapply(imageComposedVector, function(x) image_info(x)[['width']]) %>%
        max()
      #imageComposedVector <- append(imageComposedVector, paddingTile)
    }
    
  }

  if(padding > 0) {
    paddingImage = image_blank(paddingWidth, padding, "white") %>%
      image_background(bg)
    imageComposedVectorPadded <- c(paddingImage)
    
    for(i in 1:length(imageComposedVector)) {
      imageComposedVectorPadded <- append(imageComposedVectorPadded, imageComposedVector[i])
      imageComposedVectorPadded <- append(imageComposedVectorPadded, paddingImage)
    }
    
    imageComposedVector <- imageComposedVectorPadded
  }    
  
  output <- image_append(imageComposedVector, stack = TRUE)
  
  return(output)
}

saveTiles <- function(tile_name_prefix, image_tiles, path) {
  # 
  for(i in 1:length(image_tiles$name)) {
    image_tile_filename <- paste(path, tile_name_prefix, "_", image_tiles$name[[i]], sep ='') %>%
      substr(1,252) %>%
      paste(".png", sep='')

    image_write(image_tiles$tile[[i]], image_tile_filename)
  }

}


# Main
input_images <- list.files(path=IMAGES_INPUT_PATH, pattern = "\\.png$", full.names = TRUE) 
output_patterns <- getPatterns(PATTERNS_INPUT_PATH)

for (input_image_filename in input_images) { 
    
  input_image_name  <- gsub('.png','', basename(input_image_filename))
  input_image       <- image_read(input_image_filename)
  input_image_tiles <- splitToTiles(input_image, TILE_NAMES, TILE_WIDTHS, TILE_HEIGHTS)

  output_directory <- paste(OUTPUT_PATH, input_image_name, "/", sep="")
  output_tiles_subdirectory <- paste(output_directory, input_image_name, "_tiles/", sep="")
  
  dir.create(output_directory)
  dir.create(output_tiles_subdirectory)
  
  saveTiles(input_image_name, input_image_tiles, output_tiles_subdirectory)
  
  for(p in 1:length(output_patterns$name)) {

    output_pattern_name <- output_patterns$name[[p]]
    output_pattern      <- output_patterns$pattern[[p]]
    output_image        <- composeTiles(input_image_tiles, output_pattern, padding = PADDING)
    
    output_image_filename <- paste(output_directory, input_image_name, "_", output_pattern_name,".png",sep="")
    image_write(output_image, output_image_filename)
  }

  message(paste("Slides and individual tiles saved for", input_image_name, sep=" "))
}

message("Job done")


#input_images    <- getImages(IMAGES_INPUT_PATH)
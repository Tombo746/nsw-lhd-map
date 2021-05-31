lhd_map <- function(include = c(),
                   exclude = c()) {
  
  #df <- utils::read.csv("nswlhd.csv",
  #                      colClasses = c("numeric", "numeric", "integer", "logical", "integer", "numeric", "integer", "character", "character", "character", "character"),
  #                      stringsAsFactors = FALSE,
  #                      )
  
  df <- utils::read.csv(system.file("nswlhd", "nswlhd.csv", package = "nsw-lhd-map"),
                        colClasses = c("numeric", "numeric", "integer", "logical", "integer", "numeric", "integer", "character", "character", "character", "character"),
                        stringsAsFactors = FALSE,
                        )
  
  if (length(include) > 0) {
    df <- df[df$lhd_code %in% include |
               df$lhd_short %in% include |
               df$lhd_name %in% include |
               df$metropolitan %in% include, ]
  }
  
  if (length(exclude) > 0) {
    df <- df[!(df$lhd_code %in% include |
                 df$lhd_short %in% include |
                 df$lhd_name %in% include |
                 df$metropolitan %in% include), ]
  }
  
  df
}
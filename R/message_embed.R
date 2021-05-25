print.message_embed <- function(x) {
        if (any("message_embed" == class(x))) {
                cat("Message Embed\n")
                print(x$get_body())
        }
}
#' Creates a Message Embed
#'
#' @title Message embed
#' @author Dos Banaag
#' @export
message_embed <- function() {
        me <- new.env()
        
        color <- NULL
        timestamp <- NULL
        fields <- c()  
        
        me$created_at <- Sys.time()
        me$set_timestamp <- function(isoc8601_time = NULL) {
                timezone <- format(Sys.time(), format = "%Z")
                if (is.null(isoc8601_time)) {
                        initial_ts <- as.POSIXct(Sys.time())
                        initial_ts <- strsplit(as.character(initial_ts), " ")[[1]]
                        final_isoc8601 <- paste(initial_ts[1], "T", initial_ts[2], timezone, ":00", sep = "")
                        
                        timestamp <<- final_isoc8601
                } else {
                        if (typeof(isoc8601_time) != "character") {
                                stop("first argument of set_timestamp must be character string with ISOC 8601")
                        } else {
                                initial_ts <- as.POSIXct(isoc8601_time)
                                initial_ts <- strsplit(as.character(initial_ts), " ")[[1]]
                                final_isoc8601 <- paste(initial_ts[1], "T", initial_ts[2], timezone, ":00", sep = "")
                                
                                timestamp <<- final_isoc8601
                        }
                }
        }
        me$get_body <- function() {
                body <- list()
                body$payload_json <- list()
                
                body$payload_json$embed <- list()
                body$payload_json$embed$color <- jsonlite::unbox(strtoi(me$color))
                body$payload_json$embed$description <- jsonlite::unbox(me$description)
                body$payload_json$embed$timestamp <- jsonlite::unbox(timestamp)
                body$payload_json$embed$title <- jsonlite::unbox(me$title)
                body$payload_json$embed$type <- jsonlite::unbox(me$type)
                body$payload_json$embed$url <- jsonlite::unbox(me$url)
                
                if (!is.null(me$image)) {
                        if (class(me$image) == "form_file") {
                                image_parts <- strsplit(me$image$path, "\\\\")[[1]]
                                body$payload_json$embed$image <- list()
                                body$payload_json$embed$image$url <- jsonlite::unbox(paste("attachment://", image_parts[length(image_parts)], sep = ''))
                                body$file1 <- me$image
                        } else {
                                body$payload_json$embed$image <- list()
                                body$payload_json$embed$image$url <- jsonlite::unbox(me$image)
                        }
                }
                
                if (!is.null(me$thumbnail)) {
                        if (class(me$thumbnail) == "form_file") {
                                thumbnail_parts <- strsplit(me$thumbnail$path, "\\\\")[[1]]
                                body$payload_json$embed$thumbnail <- list()
                                body$payload_json$embed$thumbnail$url <- jsonlite::unbox(paste("attachment://", thumbnail_parts[length(thumbnail_parts)], sep = ''))
                                body$file2 <- me$thumbnail
                        } else {
                                body$payload_json$embed$thumbnail <- list()
                                body$payload_json$embed$thumbnail$url <- jsonlite::unbox(me$image)
                        }
                }
                
                body$payload_json <- jsonlite::toJSON(body$payload_json)
                body
        }
        
        class(me) <- append(class(me), "message_embed")
        me
}
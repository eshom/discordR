print.disc_api_endpoint <- function(x) {
        if (any("endpoint" == class(x))) {
                cat(paste("Endpoint", x$link))
        }
}

#' Endpoints contain the Discord API endpoint and the necessary path parameters.
#' @title Endpoint
#' @param endpoint_path Discord API endpoint
#' @param blanks path parameters contained in a vector
#' @author Dos Banaag
#' @examples
#' disc_api_endpoint("/channels/{}/messages", c("123456789"))
#' @export
disc_api_endpoint <- function(endpoint_path, blanks) {
        if (typeof(endpoint_path) != "character") {
                stop("First variable vector must be character type")
        }
        
        if (!is.vector(blanks)) {
                stop("Second variable of Endpoint must be a vector of character type values")
        } else if (typeof(blanks) != "character") {
                stop("Second variable vector must consist of character type")
        }
        
        endpoint_parts <- strsplit(endpoint_path, "\\{\\}")[[1]]
        
        link <- ""
        for (i in 1:length(blanks)) {
                link <- paste(link, endpoint_parts[i], sep = '')
                link <- paste(link, blanks[i], sep = '')
        }
        if (length(endpoint_parts) == length(blanks) + 1) {
                link <- paste(link, endpoint_parts[length(endpoint_parts)], sep = '')
        }
        
        endpoint <- list(
                endpoint = endpoint_path,
                link = endpoint_url_get(link)
        )
        class(endpoint) <- append(class(endpoint), "Endpoint")
        endpoint
}
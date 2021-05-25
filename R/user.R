print.user <- function(x) {
        if (any("user" == class(x))) {
                cat("User\n")
                cat(paste("    id: ",  x$id,  '\n', sep = ''))
                cat(paste("    tag: ", x$tag, '\n', sep = ''))
        }
}
#' Creates a user
#'
#' @title User
#' @param client Discord client
#' @param data initial data collected about the user
#' @author Dos Banaag
#' @export
user <- function(client, data) {
        usr <- new.env()
        usr$bot <- data$bot
        usr$client <- client
        usr$discriminator <- data$discriminator
        usr$id <- data$id
        usr$username <- data$username
        usr$verified <- data$verified
        if ((!is.null(data[["username"]])) && (!is.null(data[["discriminator"]]))) {
                usr$tag <- paste(data$username, '#', data$discriminator, sep = '')
        }
        
        class(usr) <- append(class(usr), "user")
        usr
}
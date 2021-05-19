#' Sets the bot token to be used in API calls that require
#' authorization.
#'
#' The token can also be set by defining the `DISCORD_BOT_TOKEN` environment
#' variable. This environment variable is read when the package is first loaded.
#' @title Set a bot token
#' @param token Character vector. The bot token to set.
#' @author Erez Shomron
#' @export
set_token <- function(token) {
        options(discordR_bot_token = token)
}

#' Gets the bot token to be used in API calls that require authorization.
#' @title Get a bot token
#' @return Character vector. The bot token that has been set by [set_token()]
#' @author Erez Shomron
get_token <- function() {
        getOption("discordR_bot_token")
}

get_base_url <- function() {
        paste0(getOption("discordR_base_url"), "/v",
               getOption("discordR_api_version"))
}

get_endpoint_url <- function(end_path, base_url = get_base_url()) {
        paste(base_url, end_path, sep = "/")
}

get_bot_authorization_str <- function() {
        paste("Bot", getOption("discordR_bot_token"))
}

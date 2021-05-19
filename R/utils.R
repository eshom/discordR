base_url_get <- function() {
        paste0(getOption("discordR_base_url"), "/v",
               getOption("discordR_api_version"))
}

endpoint_url_get <- function(end_path, base_url = base_url_get()) {
        paste(base_url, end_path, sep = "/")
}

bot_authorization_str_get <- function() {
        paste("Bot", getOption("discordR_bot_token"))
}

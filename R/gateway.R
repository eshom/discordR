gateway_bot_get <- function(token = get_token()) {
        url <- get_endpoint_url("gateway/bot")
        res <- httr::GET(url,
                         httr::add_headers(Authorization = get_bot_authorization_str()))
        c_type <- res$header[["content-type"]]

        if (c_type != "application/json") {
                stop("Expecting mime-type 'application/json', received ",
                     c_type, "instead")
        }

        jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
}

gateway_bot_url_cache <- function(gateway_url = gateway_bot_get()$url) {
        g_cache[["gateway_bot"]] <- gateway_url
}

gateway_websocket_get <- function(gateway_url = g_cache$gateway_bot) {
        query <- paste0("?", "v=", getOption("discordR_api_version"), "&",
                        "encoding=json")
        url <- paste(gateway_url, query, sep = "/")
        websocket::WebSocket$new(url, autoConnect = FALSE)
}

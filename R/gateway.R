gateway_bot_get <- function(token = token_get()) {
        url <- endpoint_url_get("gateway/bot")
        res <- httr::GET(url,
                         httr::add_headers(Authorization = bot_authorization_str_get()))
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
        websocket::WebSocket$new(gateway_url, autoConnect = FALSE)
}

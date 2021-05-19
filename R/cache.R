## global environment to cache responses
g_cache <- new.env(parent = emptyenv())

g_cache[["gateway_bot"]] <- NULL # url returned from `gateway_bot_url_cache()`

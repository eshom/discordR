# dispatch <- function(ws_client, packet) { # client might need to be taken as a parameter in the future
dispatch <- function(disc_client, packet) {
        if (packet$t == "READY") {
                disc_client$user <- packet$d$user
                disc_client$user$tag <- paste(packet$d$user$username, '#', packet$d$user$discriminator, sep = "")

                if (!is.null(g_cache$dispatch_functions[["ready"]])) {
                        g_cache$dispatch_functions$ready()
                }
        }
}

hello <- function(ws_client, packet) {
        hb_interval <- floor(packet$d$heartbeat_interval / 1000)
        ws_client$send('{ "op": 1, "d": null }')
        ws_client$send(paste('{ "op": 2, "d": { "token": "', token_get(), '", "properties": { "$os": "', Sys.info()[["sysname"]], '", "$browser": "discordR", "$device": "discordR" } } }', sep = ''))
        sustain_hb <- function() {
                ws_client$send('{ "op": 1, "d": null }')
                later::later(sustain_hb, delay = hb_interval - 1)
        }
        
        sustain_hb()
}

heartbeat <- function(ws_client, packet) {
        hb_interval <- floor(packet$d$heartbeat_interval / 1000)
        ws_client$send('{ "op": 1, "d": null }')
        ws_client$send(paste('{ "op": 2, "d": { "token": "', token_get(), '", "properties": { "$os": "', Sys.info()[["sysname"]], '", "$browser": "discordR", "$device": "discordR" } } }', sep = ''))
        sustain_hb <- function() {
                ws_client$send('{ "op": 1, "d": null }')
                later::later(sustain_hb, delay = hb_interval - 1)
        }
        
        sustain_hb()
}

#' Creates a bot
#'
#' @title Create a bot
#' @author Dos Banaag
#' @export
discord_client <- function() {
        ws_client <- NULL
        disc_client <- new.env()
        
        # Turning on/off the bot
        disc_client$login <- function(token) {
                token_set(token)
                gateway_bot_url_cache()
                ws_client <<- gateway_websocket_get()
                ws_client$onMessage(function(event) {
                        packet <- jsonlite::fromJSON(event$data)
                        opcode <- packet$op
                        if (opcode == get_op("Hello", "Gateway")) {
                                hello(ws_client, packet)
                        } else {
                                dispatch(disc_client, packet)
                        }
                })
                ws_client$connect()
        }
        disc_client$destroy <- function() {
                if (!is.null(ws_client)) {
                        ws_client$close()
                        token_set(NULL)
                }
        }

        # store event functions
        disc_client$on <- function(event, action) {
                event_with_0_params <- c("ready")
                event_with_1_params <- c("guild_create", "raw", "message")

                if (is.element(event, event_with_0_params)) {
                        if (count_args(action)$needed == 0) {
                                g_cache$dispatch_functions[[event]] <- action
                        } else {
                                stop('second argument must have 0 formal mandatory arguments')
                        }
                } else if (is.element(event, event_with_1_params)) {
                        if (count_args(action)$needed == 1) {
                                g_cache$dispatch_functions[[event]] <- action
                        } else {
                                stop('second argument must have 1 formal mandatory arguments')
                        }
                } else {
                        stop("invalid event")
                }
        }
        
        disc_client
}
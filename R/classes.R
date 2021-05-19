library(R6)
library(jsonlite)
library(websocket)
library(later)
library(httr)

count_args <- function(input_func) {
        if (typeof(input_func) != "closure") {
                stop('function must be inputted')
        }

        mandatory_count <- 0
        optional_count <- 0
        if (length(formals(input_func)) > 0) {
                func_args <- toString(deparse(input_func)[1])
                func_args <- strsplit(func_args, "function ")[[1]][2]
                func_args <- strsplit(func_args, ", ")
                for (argument in func_args) {
                        if (grepl("=", argument, fixed=TRUE)) {
                                optional_count <- optional_count + 1
                        } else {
                                mandatory_count <- mandatory_count + 1
                        }
                }
        }
        list(needed = mandatory_count, optional = optional_count, total = mandatory_count + optional_count)
}

index_of_ep <- function(endpoint, df) {
        if (nrow(df) == 0) {
                return(NULL)
        } else {
                return_index <- NULL
                for (i in 1:nrow(df)) {
                        if (df[i, 1] == endpoint$endpoint) {
                                return_index <- i
                        }
                }
                return(return_index)
        }
}

since_epoch <- function() { as.numeric(as.POSIXct(Sys.time())) }

message_types = c("DEFAULT", "RECIPIENT_ADD", "RECIPIENT_REMOVE", 
                  "CALL", "CHANNEL_NAME_CHANGE", "CHANNEL_ICON_CHANGE", 
                  "CHANNEL_PINNED_MESSAGE", "GUILD_MEMBER_JOIN", 
                  "USER_PREMIUM_GUILD_SUBSCRIPTION", 
                  "USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_1", 
                  "USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_2", 
                  "USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_3", 
                  "CHANNEL_FOLLOW_ADD", "GUILD_DISCOVERY_DISQUALIFIED", 
                  "GUILD_DISCOVERY_REQUALIFIED", 
                  "GUILD_DISCOVERY_GRACE_PERIOD_INITIAL_WARNING", 
                  "GUILD_DISCOVERY_GRACE_PERIOD_FINAL_WARNING", "
                  THREAD_CREATED", "REPLY", "APPLICATION_COMMAND", 
                  "THREAD_STARTER_MESSAGE", "GUILD_INVITE_REMINDER")
channel_types = c("text", "dm", "voice", "GROUP_DM", "category", "news", 
                  "store", "GUILD_NEWS_THREAD", "GUILD_PUBLIC_THREAD", 
                  "GUILD_PRIVATE_THREAD", "GUILD_STAGE_VOICE")

#' Colelction
#' 
#' A collection is a vector, but with useful methods that are used throughout the library.
#' @export
discord.collection <- R6Class("discord.collection",
        public = list(
                clear = function() {
                        private$content <- c()
                },
                delete = function(condition) {
                        un_condition <- function(x) { if (condition(x)) return(FALSE) else return(TRUE) }
                        remaining <- Vectorize(un_condition)
                        private$content <- private$content[remaining(private$content)]
                },
                each = function(process) {
                        if (typeof(process) == "closure") {
                                stop("first argument must be a function")
                        } else {
                                for (i in private$content) {
                                        process(i)
                                }
                        }
                },
                find = function(condition) {
                        input_filter <- Vectorize(condition)
                        if (length(private$content[input_filter(private$content)])) {
                                return(private$content[input_filter(private$content)][1][[1]])
                        } else {
                                return(NULL)
                        }
                },
                first = function() {
                        private$content[1][[1]]
                },
                push = function(value) {
                        private$content <- c(private$content, value)
                }
        ),
        private = list(
                content = c()
        )
)

#' Endpoint
#' 
#' The endpoint class has the Discord API endpoint and the necessary path parameters.
#' @param endpoint Discord API endpoint
#' @param blanks path parameters contained in a vector
#' @examples
#' discord.endpoint$new("/channels/{}/messages", '123456789')
#' @export
discord.endpoint <- R6Class('discord.endpoint',
        public = list(
                initialize = function(endpoint, blanks) {
                        if (typeof(endpoint) != "character") {
                                stop("First variable vector must be character type")
                        }

                        if (!is.vector(endpoint)) {
                                stop("Second variable of discord.endpoint must be a vector of character type values")
                        } else if (typeof(endpoint) != "character") {
                                stop("Second variable vector must consist of character type")
                        }

                        endpoint.parts <- strsplit(endpoint, "\\{\\}")[[1]]
                        
                        self$endpoint <- endpoint
                        self$link <- ""
                        for (i in 1:length(blanks)) {
                                self$link <- paste(self$link, endpoint.parts[i], sep = '')
                                self$link <- paste(self$link, blanks[i], sep = '')
                        }
                        self$link <- paste("https://discord.com/api/v8", self$link, sep = '')
                        
                        if (length(endpoint.parts) == length(blanks) + 1) {
                                self$link <- paste(self$link, endpoint.parts[length(endpoint.parts)], sep = '')
                        }
                },
                link = NULL,
                endpoint = NULL
        )
)

discord.endpoint_df <- R6Class("discord.endpoint_df",
        public = list(
                df = data.frame(
                        endpoint     = c(),
                        calls_limit  = c(),
                        calls_left   = c(),
                        refresh_time = c()
                ),
                set_df = function(new_df) {
                        self$df <- new_df
                },
                get_df = function() {
                        self$df
                }
        )
)

#' Channel
#' 
#' This refers to a channel in Discord.
#' @param client The client that initiated the channel
#' @param data Data about the channel
#' @export
discord.channel <- R6Class("discord.channel",
        public = list(
                initialize = function(client, data) {
                        self$client <- client
                               
                        if (!is.null(data[["id"]])) {
                                self$id <- data$id
                        }
                        if (!is.null(data[["timestamp"]])) {
                                self$created_timestamp <- data$timestamp
                        }
                },
                client = NULL,
                deleted = NULL,
                id = NULL,
                type = NULL,
                delete = function() {},
                fetch = function() {}
        ),
        private = list()
)

#' Guild Channel
#' 
#' This refers to a channel in a server on Discord.
#' @param guild The server of the channel
#' @param data Data about the channel
#' @export
discord.guild_channel <- R6Class("discord.guild_channel",
        inherit = discord.channel,
        public = list(
                initialize = function(guild, data) {
                        self$guild <- guild
                        
                        if (!is.null(data[["channel_id"]])) {
                                self$id <- data$channel_id
                        } else if (!is.null(data[["id"]])) {
                                self$id <- data$id
                        }
                },
                guild = NULL,
                type = NULL
        )
)

discord.dm_channel <- R6Class("discord.dm_channel",
        inherit = discord.channel,
        public = list(
                initialize = function(client, data) {
                        self$client <- client

                        if (!is.null(data[["id"]])) {
                                self$id <- data$id
                        }
                        if (!is.null(data[["last_message_id"]])) {
                                self$last_message_id <- data$last_message_id
                        }
                        if (!is.null(data[["timestamp"]])) {
                                self$created_timestamp <- data$timestamp
                        }
                },
                last_message = NULL,
                last_message_id = ''
        )
)

#' Guild Channel
#' 
#' This refers to a text channel in a server on Discord.
#' @param guild The server of the channel
#' @param data Data about the channel
#' @export
discord.text_channel <- R6Class("discord.text_channel",
        inherit = discord.guild_channel,
        public = list(
                initialize = function(guild, data) {
                        self$guild <- guild
                        self$type <- channel_types[1]

                        if (!is.null(data[["channel_id"]])) {
                                self$id <- data$channel_id
                        } else if (!is.null(data[["id"]])) {
                                self$id <- data$id
                        }
                },
                send = function(content, options = NULL) {
                        use_json_payload <- FALSE
                        msg_body <- list()

                        if (typeof(content) == "character") {
                                msg_body$content <- content
                        } else if (typeof(content) == "environment") {
                                use_json_payload <- TRUE
                                msg_body$embed <- content$get_list()
                        } else if (typeof(content) == "list") {
                                if (!is.null(content$path) && !is.null(content$type)) {
                                        msg_body$file <- content
                                }
                        }
                        
                        if (!is.null(options)) {
                                if (!is.null(options$tts)) {
                                        if (typeof(content) == "character") {
                                                if (typeof(options$tts) == "logical") {
                                                        if (options$tts) {
                                                                msg_body$tts <- "True"
                                                        } else {
                                                                msg_body$tts <- "False"
                                                        }
                                                } else {
                                                        stop("tts must have logical value")
                                                }
                                        } else {
                                                stop("tts only applies to messages with character values")
                                        }
                                }
                        }
                        
                        if (use_json_payload) {
                                embed_img_set <- FALSE
                                if (!is.null(msg_body$embed$image_file)) {
                                        embed_img_set <- TRUE
                                }
                                
                                if (embed_img_set) {
                                        embed_img_copy <- msg_body$embed$image_file
                                        msg_body$embed <- msg_body$embed[names(msg_body$embed) != "image_file"]

                                        if (typeof(embed_img_copy) == "character") {
                                                msg_body$embed$image$url <- embed_img_copy
                                                msg_body <- list(
                                                        payload_json = RJSONIO::toJSON(msg_body, pretty=TRUE)
                                                )
                                        } else {
                                                msg_body$embed$image <- msg_body$embed$image[names(msg_body$embed$image) == "url"]
                                                msg_body <- list(
                                                        file = embed_img_copy,
                                                        payload_json = RJSONIO::toJSON(msg_body, pretty=TRUE)
                                                )
                                        }
                                } else {
                                        msg_body <- list(
                                                payload_json = RJSONIO::toJSON(msg_body, pretty=TRUE)
                                        )
                                }
                        }
                        
                        channel_endpoint <- discord.endpoint$new("/channels/{}/messages", self$id)
                        self$guild$client$client_POST(channel_endpoint, msg_body)
                }
        )
)

discord.guild_member_manager <- R6Class("discord.guild_member_manager",
        public = list(
                initialize = function() {
                },
                cache = NULL,
                client = NULL,
                guild = NULL
        )
)

#' Guild
#' 
#' This refers to a server on Discord.
#' @param client The client that initiated the guild
#' @param data Data about the guild
#' @export
discord.guild <- R6Class("discord.guild", 
        public = list(
                initialize = function(client, data) {
                        self$client <- client

                        if (!is.null(data[["guild_id"]])) {
                                self$guild_id <- data$guild_id
                        } else if (!is.null(data[["id"]])) {
                                self$id <- data$id
                        }
                        if (!is.null(data[["description"]])) {
                                self$description <- data$description
                        }
                        if (!is.null(data[["name"]])) {
                                self$name <- data$name
                        }
                        if (!is.null(data[["joined_at"]])) {
                                self$joined_at <- unclass(as.POSIXct(data$joined_at))
                                self$joined_timestamp <- data$joined_at
                        }
                },
                client = NULL,
                description = NULL,
                members = c(),
                name = '',
                id = NULL,
                joined_at = '',
                joined_timestamp = ''
        ),
        private = list(
        )
)

#' Guild Member Manager
#' 
#' Guild Member Managers help store information about members in a server.
#' @param guild guild of the guild memer manager
#' @export
discord.guild_manager <- R6Class("discord.guild_manager",
        public = list(
                add = function(guild) {
                        self$cache$push(guild)
                },
                cache = discord.collection$new(),
                client = NULL,
                set_client = function(input_client) {
                        self$client <- input_client
                }
        )
)

#' Message Embed
#' 
#' This is an embed message.
#' @export
discord.message_embed <- R6Class("discord.message_embed",
        public = list(
                author = NULL,
                color = NULL,
                created_at = unclass(Sys.time()),
                description = NULL,
                fields = c(),
                # files = NULL,
                footer = NULL,
                image = NULL,
                # length = NULL,
                # provider = NULL,
                thumbnail = NULL,
                isoc_timestamp = "",
                timestamp = FALSE,
                title = NULL,
                # type = NULL,
                url = NULL,
                # video = NULL,
                add_field = function(name, value, inline = FALSE) {
                        new_field <- list(name = name, value = value, inline = inline)
                        append(self$fields, new_field)
                },
                set_color = function(color) {
                        self$color <- strtoi(color)
                },
                set_description = function(new_description) {
                        self$description <- new_description
                },
                set_image = function(image) {
                        self$image <- image
                        private$file1 <- image
                },
                set_thumbnail = function(thumbnail) {
                        self$thumbnail <- thumbnail
                        private$file2 <- thumbnail
                },
                set_timestamp = function(isoc8601_time = NULL) {
                        self$timestamp <<- TRUE
                        timezone <- format(Sys.time(), format = "%Z")
                        if (is.null(isoc8601_time)) {
                                initial_ts <- as.POSIXct(Sys.time())
                                initial_ts <- strsplit(as.character(initial_ts), " ")[[1]]
                                final_isoc8601 <- paste(initial_ts[1], "T", initial_ts[2], timezone, ":00", sep = "")

                                self$timestamp <- final_isoc8601
                        } else {
                                if (typeof(isoc8601_time) != "character") {
                                        stop("first argument of set_timestamp must be character string with ISOC 8601")
                                } else {
                                        initial_ts <- as.POSIXct(isoc8601_time)
                                        initial_ts <- strsplit(as.character(initial_ts), " ")[[1]]
                                        final_isoc8601 <- paste(initial_ts[1], "T", initial_ts[2], timezone, ":00", sep = "")

                                        self$timestamp <- final_isoc8601
                                }
                        }
                },
                set_title = function(title) {
                        if (typeof(title) == "character") {
                                self$title <- title
                        } else {
                                stop("title must be a character string")
                        }
                },
                set_url = function(url) {
                        if (typeof(url) == "character") {
                                self$url <- url
                        } else {
                                stop("title must be a character string")
                        }
                },
                get_list = function() {
                        embed_list <- list()

                        if (!is.null(self$description)) {
                                embed_list$description <- self$description
                        }
                        if (!is.null(self$color)) {
                                embed_list$color <- self$color
                        }
                        if (!is.null(private$file1)) {
                                if (typeof(private$file1) == "list") {
                                        embed_list$image_file <- private$file1
                                        path_parts <- strsplit(private$file1$path, "\\\\")[[1]]
                                        embed_list$image$url <- paste("attachment://", path_parts[length(path_parts)], sep = '')
                                } else {
                                        embed_list$image$url <- private$file1
                                }
                        }
                        if (!is.null(private$file2)) {
                                if (typeof(private$file2) == "list") {
                                        embed_list$image_file <- private$file2
                                        path_parts <- strsplit(private$file2$path, "\\\\")[[1]]
                                        embed_list$thumbnail$url <- paste("attachment://", path_parts[length(path_parts)], sep = '')
                                } else {
                                        embed_list$thumbnail <- private$file2
                                }
                        }
                        if (self$timestamp) {
                                embed_list$timestamp <- self$timestamp
                        }
                        if (!is.null(self$title)) {
                                embed_list$title <- self$title
                        }
                        if (!is.null(self$url)) {
                                embed_list$url <- self$url
                        }
                        
                        embed_list
                }
        ),
        private = list(
                file0 = NULL,
                file1 = NULL,
                file2 = NULL
        )
)

#' Message
#' 
#' This is a message.
#' @export
discord.message <- R6Class("discord.message",
        public = list(
                initialize = function(client, data, channel) {
                        self$client <- client
                        self$channel <- channel

                        if (!is.null(channel[["guild"]])) {
                                self$guild <- channel$guild
                        }

                        if (!is.null(data[["author"]])) {
                                self$author <- data$author
                        }
                        if (!is.null(data[["channel"]])) {
                                self$channel <- data$channel
                        }
                        if (!is.null(data[["content"]])) {
                                self$content <- data$content
                        }
                        if (!is.null(data[["id"]])) {
                                self$id <- data$id
                        }
                        if (!is.null(data[["timestamp"]])) {
                                self$created_timestamp <- data$timestamp
                        }
                        if (!is.null(data[["type"]])) {
                            self$type <- message_types[data$type + 1]
                        }
                },
                author = NULL,
                channel = NULL,
                client = NULL,
                content = '',
                created_at = unclass(Sys.time()),
                created_timestamp = NULL,
                deleted = NULL,
                guild = NULL,
                id = NULL,
                type = NULL,
                delete = function() {}
        )
)

#' Client
#' 
#' This is a client that interacts with the Discord API.
#' @export
discord.client <- R6Class("discord.client",
        public = list(
                user = NULL,
                login = function(token) {
                        self$guilds$set_client(self)
                        private$token <- token
                        open_gateway <- GET(
                                "https://discordapp.com/api/v8/gateway/bot",
                                add_headers(Authorization = paste("Bot ", token, sep = ''))
                        )
                    
                        private$ws <- WebSocket$new(content(open_gateway, as="parsed")$url, autoConnect = FALSE)
                        private$ws$onMessage(function(event) {
                                packet <- RJSONIO::fromJSON(event$data)
                            
                                if (packet$op == 10) {
                                        hb_interval <- floor(packet$d$heartbeat_interval / 1000)
                                        private$ws$send('{ "op": 1, "d": null }')
                                        private$ws$send(paste('{ "op": 2, "d": { "token": "', private$token, '", "properties": { "$os": "linux", "$browser": "my_library", "$device": "my_library" } } }', sep = ""))
                                        sustain_hb <- function() {
                                                private$ws$send('{ "op": 1, "d": null }')
                                                later(sustain_hb, delay = hb_interval - 1)
                                        }
                                    
                                        sustain_hb()
                                } else {
                                        if (is.null(packet$t)) {
                                        
                                        } else if (packet$t == "GUILD_CREATE") {
                                                guild <- discord.guild$new(self, packet$d)
                                                self$guilds$cache$push(guild)
                                        
                                                if (!is.null(private$functions[["guild_create"]])) {
                                                        if (typeof(private$functions$guild_create) == "closure") {
                                                                private$functions$guild_create(guild)
                                                        }
                                                }
                                        } else if (packet$t == "MESSAGE_CREATE") {
                                                filter_guild_id <- function(guild_x) {
                                                        if (guild_x$id == packet$d$guild_id) {
                                                                return(TRUE)
                                                        } else {
                                                                return(FALSE)
                                                        }
                                                }
                                        
                                                guild <- NULL
                                                if (!is.null(self$guilds$cache$find(filter_guild_id))) {
                                                        guild <- self$guilds$cache$find(filter_guild_id)
                                                } else {
                                                        guild <- discord.guild$new(self, packet$d)
                                                }

                                                channel <- discord.text_channel$new(guild, packet$d)
                                                message <- discord.message$new(self, packet$d, channel)
                                                if (!is.null(private$functions[["message"]])) {
                                                        if (typeof(private$functions$message) == "closure") {
                                                                private$functions$message(message)
                                                        }
                                                }
                                        } else if (packet$t == "READY") {
                                                self$user <- packet$d$user
                                                self$user$tag <- paste(packet$d$user$username, '#', packet$d$user$discriminator, sep = "")

                                                if (!is.null(private$functions[["ready"]])) {
                                                        if (typeof(private$functions$ready) == "closure") {
                                                                private$functions$ready()
                                                        }
                                                }
                                        }
                                }

                                if (packet$op != 10) {
                                        if (!is.null(private$functions[["raw"]])) {
                                                if (typeof(private$functions$raw) == "closure") {
                                                        private$functions$raw(packet)
                                                }
                                        }
                                }
                        })
                        private$ws$connect()
                },
                destroy = function() {
                        if (typeof(private$ws) == "environment") {
                                private$ws$close()
                        }
                },
                on = function(event, action) {
                        if (event == "guild_create") {
                                if (count_args(action)$needed == 1) {
                                        private$functions$guild_create <- action
                                } else {
                                        stop('second argument must have 1 formal mandatory arguments')
                                }
                        } else if (event == "raw") {
                                if (count_args(action)$needed == 1) {
                                        private$functions$raw <- action
                                } else {
                                        stop('second argument must have 1 formal mandatory arguments')
                                }
                        } else if (event == "ready") {
                                if (count_args(action)$needed == 0) {
                                        private$functions$ready <- action
                                } else {
                                        stop('second argument must have 0 formal mandatory arguments')
                                }
                        } else if (event == "message") {
                                if (count_args(action)$needed == 1) {
                                        private$functions$message <- action
                                } else {
                                        stop('second argument must have 1 formal mandatory argument')
                                }
                        }
                },
                client_POST = function(endpoint, body) {
                        ep_index <- private$client_WAIT(endpoint)
                        sent_post <- POST(
                                endpoint$link,
                                add_headers(
                                        .headers = c(
                                                `Authorization` = paste("Bot ", private$token, sep = ''),
                                                `Content-Type` = "multipart/form-data"
                                        )
                                ),
                                verbose(),
                                body = body,
                                encode = "multipart"
                        )

                        response_headers <- headers(sent_post)

                        call_df_copy <- private$endpoints$get_df()
                        call_df_copy[ep_index, 2] <- strtoi(response_headers$`x-ratelimit-limit`)
                        call_df_copy[ep_index, 3] <- strtoi(response_headers$`x-ratelimit-remaining`)
                        call_df_copy[ep_index, 4] <- as.double(response_headers$`x-ratelimit-reset`)

                        private$endpoints$set_df(call_df_copy)
                },
                guilds = discord.guild_manager$new()
        ),
        private = list(
                token = '',
                functions = list(),
                ws = NULL,
                endpoints = discord.endpoint_df$new(),
                client_WAIT = function(endpoint) {
                        found_index <- index_of_ep(endpoint, private$endpoints$get_df())
                        if (is.null(found_index)) {
                                new_endpoint <- data.frame(endpoint$endpoint, 0, 0, 0)
                                names(new_endpoint) <- c("endpoint", "calls_limit", "calls_left", "refresh_time")
                                new_calls_copy <- rbind(private$endpoints$get_df(), new_endpoint)
                    
                                private$endpoints$set_df(new_calls_copy)
                                found_index <- index_of_ep(endpoint, private$endpoints$get_df())
                        } else {
                                if ((private$endpoints$get_df()[found_index, 3] == 0) && (private$endpoints$get_df()[found_index, 4] > since_epoch())) {
                                        while (private$endpoints$get_df()[found_index, 4] > since_epoch()) {}
                                }
                        }

                        return(found_index)
                }
        )
)
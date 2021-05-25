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

print.message <- function(msg) {
        cat("Message\n")
        cat(paste("    id: ",      msg$id,         '\n', sep = ''))
        cat(paste("    author: ",  msg$author$tag, '\n', sep = ''))
        cat(paste("    content: ", msg$content,    '\n', sep = ''))
}
#' This is a message.
#' 
#' @title Message
#' @param client The client that initiated the guild
#' @param data Data about the guild
#' @param channel The channel of the message
#' @author Dos Banaag
#' @export
message <- function(client, data, channel) {
        msg <- new.env()
        msg$author <- user(client, data$author)
        msg$channel <- channel
        msg$client <- client
        msg$content <- data$content
        if (!is.null(channel[["guild"]])) {
                msg$guild <- channel$guild
        }
        msg$id <- data$id
        msg$created_timestamp <- data$timestamp
        if (!is.null(data[["type"]])) {
                msg$type <- message_types[data$type + 1]
        }
        
        class(msg) <- append(class(msg), "message")
        msg
}
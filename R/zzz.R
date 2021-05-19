.onLoad <- function(libname, pkgname) {
        options(discordR_base_url = "https://discord.com/api",
                discordR_api_version = "9",
                discordR_bot_token = Sys.getenv("DISCORD_BOT_TOKEN"))
}

.onUnload <- function(libpath) {
        options(discordR_base_url = NULL,
                discordR_api_version = NULL,
                discordR_bot_token = NULL)
}

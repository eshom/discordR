get_base_url <- function() {
        paste(getOption("discordR_base_url"), getOption("discordR_api_version"),
              sep = "/")
}

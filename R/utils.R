count_args <- function(input_func) { # counts the arguments needed to run a function
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
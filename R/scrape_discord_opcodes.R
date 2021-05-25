library(rvest)
library(devtools)

url <- "https://github.com/discord/discord-api-docs/blob/master/docs/topics/Opcodes_and_Status_Codes.md"

site <- read_html(url)
selected_tables <- html_nodes(site, "table")
tables <- html_table(selected_tables)
tables <- setNames(tables, c("gateway_opcodes", "gateway_close_event_codes",
                             "voice_opcodes", "voice_close_event_codes",
                             "http_response_codes", "json_error_codes",
                             "rpc_error_codes", "rpc_close_event_codes"))
tables <- lapply(tables, as.data.frame)
tables <- tables
## Lower case column names and replace spaces with underscore
tables <- lapply(tables, function(x) {
        colnames(x) <- tolower(colnames(x))
        colnames(x) <- gsub("\\s", "_", colnames(x))
        x
})

## The section the table was taken from. Keep track of it for merging purposes.
section <- c("Gateway", "Gateway", "Voice", "Voice",
             "HTTP", "JSON", "RPC", "RPC")

tables <- Map(function(x, sec) {
        within(x, {section <- sec})
}, tables, section)


opcodes <- with(tables, merge(gateway_opcodes, voice_opcodes,
                                all = TRUE, sort = FALSE))

close_event_codes <- with(tables, {
        tmp <- merge(gateway_close_event_codes, voice_close_event_codes,
                     all = TRUE, sort = FALSE)
        merge(tmp, rpc_close_event_codes, all = TRUE, sort = FALSE)
})

http_response_codes <- tables$http_response_codes

error_codes <- with(tables, {
        merge(json_error_codes, rpc_error_codes, all = TRUE, sort = FALSE)
})

use_data(opcodes, close_event_codes, http_response_codes, error_codes,
         internal = TRUE, overwrite = TRUE)

get_op <- function(op_name, op_section) {
        with(opcodes, opcodes$code[name == op_name & section == op_section])
}
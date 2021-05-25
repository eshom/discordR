DISC_get <- function() {}

DISC_post <- function(endpoint, body, ct = "multipart/form-data", e = c("multipart", "form", "json", "raw")) {
        sent_post <- httr::POST(
                endpoint$link,
                httr::add_headers(
                        `Authorization` = bot_authorization_str_get(),
                        `Content-Type` = ct
                ),
                httr::verbose(),
                body = body,
                encode = e
        )

        httr::content(sent_post)
}
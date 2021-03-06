% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/token.R
\name{token_set}
\alias{token_set}
\title{Set a bot token}
\usage{
token_set(token)
}
\arguments{
\item{token}{Character vector. The bot token to set.}
}
\description{
Sets the bot token to be used in API calls that require
authorization.
}
\details{
The token can also be set by defining the \code{DISCORD_BOT_TOKEN} environment
variable. This environment variable is read when the package is first loaded.
}
\author{
Erez Shomron
}

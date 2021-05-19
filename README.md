## About

discordR is an [R](https://cran.r-project.org/bin/windows/base/) module that makes it easier to use the [Discord API](https://discord.com/developers/docs/intro).

- Object-oriented

## Example usage

```R
library(discordR)
client <- discord.client$new()

client$on("ready", function() {
    print(paste("Logged in as ", client$user$tag, "!", sep = ''))
})

client$on("message", function(message) {
    if (message$content == 'ping') {
        message$channel$send('pong')
    }
})

client$login('token')
```
## About

discordR is an [R](https://nodejs.org) wrapper for the [Discord API](https://discord.com/developers/docs/intro).

- Object-oriented

## Example usage

```R
library(discordR)
client <- discord_client()

client$on("ready", function() {
    print(paste0("Logged in as ", client$user$tag, "!"))
})

client$login('token')
```

## Links

- [GitHub](https://github.com/eshom/discordR)

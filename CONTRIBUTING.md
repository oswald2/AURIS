# Contributing

Contributions are most welcome. Please follow the following guidelines if you wish to contribute. As the topic is broad, diverse and 
very specific, there is a Discord server for questions and discussions: [https://discord.gg/jEdJxfVd](Discord).

# Guidelines

 * Format Haskell files with brittany, an example brittany config file is located in the root dir as `brittany_config.yaml`. You can copy the file to `~/.config/brittany` for usage. The reason why I use brittany is that ormulo removes empty lines. I use that sometimes like a paragraph for logically separating code and find it very annoying if it is back to spaghetti with ormulo.
 * Apply suggestions from hlint (where sensible). There are some suggestions I disabled, there is a `.hlint.yaml` file in the root directory which specifies them.
 * Record fields should be prefixed with an abbrevation of either the data type or the constructor name (potentially in camelCase). This is to prevent the module namespace becoming polluted with record field getter functions. Also, currently the project uses the `lens` library with template haskell creation of lenses, therefore if lenses should be used, the field names require an underline in front.
 * There are plenty of language extensions already enabled in the cabal files in the `default-extensions` stanza. Module specific ones (like e.g. TemplateHaskell) should then be specified in the module itself.
 * All current libraries directly created for AURIS use the `rio` library as a prelude and most IO related things come from `unliftio`. This was a natural choice and should be kept for new code.
 * There are no hard restrictions on how to import modules (qualified, explicit imports). Data types are normally imported unqualified (if possible), explicit imports currently in the code are mainly from the haskell-language-server functionality to automatically propose them.
 * As the system is quite heavily under development and APIs are changing, it is a good idea to export record fields and also newtype accessors with the (natural) exception for the need of encapsulation and smart constructors.
 

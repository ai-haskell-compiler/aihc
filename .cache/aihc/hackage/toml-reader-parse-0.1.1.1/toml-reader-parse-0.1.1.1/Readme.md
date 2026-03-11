# Synopsis

Parser combinators for directly parsing TOML values produced by
`toml-reader`. Parsers track where values originate in the source
document and intelligently combine errors between alternative parsing
branches to report only the most relevant error to the user.


#### All CardinalIO classes ####
## -----------------------------

setClass("ImzMeta", contains = "SimpleList")

setClass("ImzML", contains = "SimpleList")

setClass("Analyze75", contains = "SimpleList")

setClassUnion("character_OR_NULL", c("character", "NULL"))

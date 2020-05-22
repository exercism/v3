module HigherOrderFunctions

let prefix (prefixWith: string): string -> string = fun str -> prefixWith + str

let cutoff (cutoffStr: string): string -> string =
    fun str -> str.Substring(0, str.IndexOf(cutoffStr) + 1)

let replace (source: string) (target: string): string -> string = fun str -> str.Replace(source, target)

let combiner (first: string -> string) (second: string -> string): string -> string = first >> second

let headline str =
    let prefixer = prefix "[BREAKING] "
    let cutoffer = cutoff "."
    let replacer = replace "." "!"
    let first = combiner prefixer cutoffer
    let second = combiner first replacer
    second str

module HigherOrderFunctions

let prefix (prefixWith: string): string -> string = fun str -> prefixWith + str

let cutoff (cutoffAt: string): string -> string =
    fun str -> str.Substring(0, str.LastIndexOf(cutoffAt) + cutoffAt.Length)

let replace (source: string) (target: string): string -> string = fun str -> str.Replace(source, target)

let combiner (first: string -> string) (second: string -> string): string -> string = first >> second

let headline str =
    let prefixer = prefix "[BREAKING] "
    let replacer = replace "." "!"
    let cutoffer = cutoff "!"
    let first = combiner prefixer replacer
    let second = combiner first cutoffer
    second str

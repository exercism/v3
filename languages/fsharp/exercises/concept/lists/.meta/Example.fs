module Lists

let newList: string list = []

let existingList: string list = [ "F#"; "Clojure"; "Haskell" ]

let addLanguage (language: string) (languages: string list): string list = language :: languages

let lastAddedLanguage (languages: string list): string = List.head languages

let countLanguages (languages: string list): int = List.length languages

let reverseList (languages: string list): string list = List.rev languages

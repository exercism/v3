module ListsTests

open FsUnit.Xunit
open Xunit

open Lists

[<Fact>]
let ``New list``() =
    let expected: string list = []
    newList |> should equal expected

[<Fact(Skip = "Remove to run test")>]
let ``Existing list``() = existingList |> should equal [ "F#"; "Clojure"; "Haskell" ]

[<Fact(Skip = "Remove to run test")>]
let ``Add language to new list``() = addLanguage "Scala" newList |> should equal [ "Scala" ]

[<Fact(Skip = "Remove to run test")>]
let ``Add language to existing list``() =
    addLanguage "Common Lisp" existingList |> should equal [ "Common Lisp"; "F#"; "Clojure"; "Haskell" ]

[<Fact(Skip = "Remove to run test")>]
let ``Add language to custom list``() = addLanguage "Racket" [ "Scheme" ] |> should equal [ "Racket"; "Scheme" ]

[<Fact(Skip = "Remove to run test")>]
let ``Last added language of existing list``() = lastAddedLanguage existingList |> should equal "F#"

[<Fact(Skip = "Remove to run test")>]
let ``Last added language of custom list``() = lastAddedLanguage [ "Crystal"; "Nim" ] |> should equal "Crystal"

[<Fact(Skip = "Remove to run test")>]
let ``Count languages on new list``() = countLanguages newList |> should equal 0

[<Fact(Skip = "Remove to run test")>]
let ``Count languages on existing list``() = countLanguages existingList |> should equal 3

[<Fact(Skip = "Remove to run test")>]
let ``Count languages on custom list``() = countLanguages [ "Python"; "JavaScript" ] |> should equal 2

[<Fact(Skip = "Remove to run test")>]
let ``Reverse order of new list``() =
    let expected: string list = []
    reverseList newList |> should equal expected

[<Fact(Skip = "Remove to run test")>]
let ``Reverse order of existing list``() = reverseList existingList |> should equal [ "Haskell"; "Clojure"; "F#" ]

[<Fact(Skip = "Remove to run test")>]
let ``Reverse order of custom list``() =
    reverseList [ "Kotlin"; "Java"; "Scala"; "Clojure" ] |> should equal [ "Clojure"; "Scala"; "Java"; "Kotlin" ]

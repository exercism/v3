module TracksOnTracksOnTracksTests

open FsUnit.Xunit
open Xunit

open TracksOnTracksOnTracks

[<Fact>]
let ``New list``() =
    let expected: string list = []
    newList |> should equal expected

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Existing list``() = existingList |> should equal [ "F#"; "Clojure"; "Haskell" ]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Add language to new list``() = addLanguage "Scala" newList |> should equal [ "Scala" ]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Add language to existing list``() =
    addLanguage "Common Lisp" existingList |> should equal [ "Common Lisp"; "F#"; "Clojure"; "Haskell" ]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Add language to custom list``() = addLanguage "Racket" [ "Scheme" ] |> should equal [ "Racket"; "Scheme" ]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Count languages on new list``() = countLanguages newList |> should equal 0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Count languages on existing list``() = countLanguages existingList |> should equal 3

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Count languages on custom list``() = countLanguages [ "Python"; "JavaScript" ] |> should equal 2

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Reverse order of new list``() =
    let expected: string list = []
    reverseList newList |> should equal expected

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Reverse order of existing list``() = reverseList existingList |> should equal [ "Haskell"; "Clojure"; "F#" ]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Reverse order of custom list``() =
    reverseList [ "Kotlin"; "Java"; "Scala"; "Clojure" ] |> should equal [ "Clojure"; "Scala"; "Java"; "Kotlin" ]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Empty list is not exciting``() = excitingList [] |> should equal false

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Singleton list with fsharp is exciting``() = excitingList [ "F#" ] |> should equal true

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Singleton list without fsharp is not exciting``() = excitingList [ "C#" ] |> should equal false

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Two-item list with fsharp as first item is exciting``() = excitingList [ "F#"; "Clojure" ] |> should equal true

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Two-item list with fsharp as second item is exciting``() = excitingList [ "Nim"; "F#" ] |> should equal true

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Two-item list without fsharp is not exciting``() = excitingList [ "Python"; "Go" ] |> should equal false

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Three-item list with fsharp as first item is exciting``() =
    excitingList [ "F#"; "Lisp"; "Clojure" ] |> should equal true

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Three-item list with fsharp as second item is exciting``() =
    excitingList [ "Java"; "F#"; "C#" ] |> should equal true

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Three-item list with fsharp as third item is not exciting``() =
    excitingList [ "Julia"; "Assembly"; "F#" ] |> should equal false

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Four-item list with fsharp as first item is exciting``() =
    excitingList [ "F#"; "C"; "C++"; "C#" ] |> should equal true

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Four-item list with fsharp as second item is not exciting``() =
    excitingList [ "Elm"; "Erlang"; "C#"; "Scheme" ] |> should equal false

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Four-item list with fsharp as third item is not exciting``() =
    excitingList [ "Delphi"; "D"; "F#"; "Prolog" ] |> should equal false

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Four-item list with fsharp as fourth item is not exciting``() =
    excitingList [ "Julia"; "Assembly"; "Crystal"; "F#" ] |> should equal false

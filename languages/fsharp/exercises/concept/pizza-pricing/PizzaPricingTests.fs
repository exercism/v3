module RecursionTests

open FsUnit.Xunit
open Xunit

open Recursion

[<Fact>]
let ``Price for pizza margherita``() = pizzaPrice Margherita |> should equal 7

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Price for pizza formaggio``() = pizzaPrice Formaggio |> should equal 10

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Price for pizza caprese``() = pizzaPrice Caprese |> should equal 9

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Price for pizza margherita with extra sauce``() = pizzaPrice (ExtraSauce Margherita) |> should equal 8

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Price for pizza caprese with extra toppings``() = pizzaPrice (ExtraToppings Caprese) |> should equal 11

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Price for pizza formaggio with extra sauce and toppings``() =
    pizzaPrice (ExtraSauce(ExtraToppings Caprese)) |> should equal 12

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Price for pizza caprese with extra sauce and toppings``() =
    pizzaPrice (ExtraToppings(ExtraSauce Formaggio)) |> should equal 13

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Order price for no pizzas``() = orderPrice [] |> should equal 0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Order price for single pizza caprese``() = orderPrice [ Caprese ] |> should equal 12

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Order price for single pizza formaggio with extra sauce``() =
    orderPrice [ ExtraSauce Formaggio ] |> should equal 14

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Order price for one pizza margherita and one pizza caprese with extra toppings``() =
    orderPrice
        [ Margherita
          ExtraToppings Caprese ]
    |> should equal 20

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Order price for very large order``() =
    orderPrice
        [ Margherita
          ExtraSauce Margherita
          Caprese
          ExtraToppings Caprese
          Formaggio
          ExtraSauce Formaggio
          ExtraToppings(ExtraSauce Formaggio)
          ExtraToppings(ExtraSauce Formaggio) ]
    |> should equal 82

[<Fact(Skip = "Remove to run test")>]
let ``Order price for gigantic order``() = orderPrice (List.replicate 100_000 Margherita) |> should equal 700_000

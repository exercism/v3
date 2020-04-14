module FloatingPointNumbersTests

open FsUnit.Xunit
open Xunit

open FloatingPointNumbers

[<Fact>]
let ``Minimal first interest rate``() = interestRate 0m |> should equal 0.5f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Tiny first interest rate``() = interestRate 0.000001m |> should equal 0.5f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Maximum first interest rate``() = interestRate 999.9999m |> should equal 0.5f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Minimal second interest rate``() = interestRate 1_000.0m |> should equal 1.621f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Tiny second interest rate``() = interestRate 1_000.0001m |> should equal 1.621f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Maximum second interest rate``() = interestRate 4_999.9990m |> should equal 1.621f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Minimal third interest rate``() = interestRate 5_000.0000m |> should equal 2.475f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Tiny third interest rate``() = interestRate 5_000.0001m |> should equal 2.475f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Large third interest rate``() = interestRate 5_639_998.742909m |> should equal 2.475f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Minimal negative interest rate``() = interestRate -0.000001M |> should equal -3.213f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Small negative interest rate``() = interestRate -0.123M |> should equal -3.213f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Regular negative interest rate``() = interestRate -300.0M |> should equal -3.213f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Large negative interest rate``() = interestRate -152964.231M |> should equal -3.213f

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Annual balance update for empty start balance``() = annualBalanceUpdate 0.0m |> should equal 0.0000m

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Annual balance update for small positive start balance``() =
    annualBalanceUpdate 0.000001m |> should equal 0.000001005m

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Annual balance update for average positive start balance``() =
    annualBalanceUpdate 1_000.0m |> should equal 1016.210000m

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Annual balance update for large positive start balance``() =
    annualBalanceUpdate 1_000.0001m |> should equal 1016.210101621m

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Annual balance update for huge positive start balance``() =
    annualBalanceUpdate 898124017.826243404425m |> should equal 920352587.26744292868451875m

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Annual balance update for small negative start balance``() =
    annualBalanceUpdate -0.123M |> should equal -0.11904801M

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Annual balance update for large negative start balance``() =
    annualBalanceUpdate -152964.231M |> should equal -148049.49025797M

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Amount to donate for empty start balance``() = amountToDonate 0.0m 2.0 |> should equal 0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Amount to donate for small positive start balance``() = amountToDonate 0.000001m 2.1 |> should equal 0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Amount to donate for average positive start balance``() = amountToDonate 1_000.0m 2.0 |> should equal 40

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Amount to donate for large positive start balance``() = amountToDonate 1_000.0001m 0.99 |> should equal 19

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Amount to donate for huge positive start balance``() =
    amountToDonate 898124017.826243404425m 2.65 |> should equal 47600572

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Amount to donate for small negative start balance``() = amountToDonate -0.123M 3.33 |> should equal 0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Amount to donate for large negative start balance``() = amountToDonate -152964.231M 5.4 |> should equal 0

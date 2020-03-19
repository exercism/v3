module Recursion

type Pizza =
    | Margherita
    | Formaggio
    | Caprese
    | ExtraSauce of Pizza
    | ExtraToppings of Pizza

let pizzaPrice pizza =
    let rec loop acc current =
        match current with
        | Margherita -> acc + 7
        | Formaggio -> acc + 10
        | Caprese -> acc + 9
        | ExtraSauce saucedPizza -> loop (acc + 1) saucedPizza
        | ExtraToppings toppedPizza -> loop (acc + 2) toppedPizza

    loop 0 pizza

let orderPrice pizzas =
    let rec loop price current =
        match current with
        | [] -> price
        | pizza :: otherPizzas -> loop (price + pizzaPrice pizza) otherPizzas

    match pizzas with
    | [] -> 0
    | [ pizza ] -> 3 + pizzaPrice pizza
    | [ firstPizza; secondPizza ] -> 2 + pizzaPrice firstPizza + pizzaPrice secondPizza
    | pizzas -> loop 0 pizzas

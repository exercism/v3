defmodule BoutiqueSuggestions do

  @tops [
    %{
      item_name: "Long Sleeve T-shirt",
      price: 19.95,
      quantity: 15,
      color: "Deep Red",
      base_color: "red"
    },
    %{
      item_name: "Brushwood Shirt",
      price: 19.10,
      quantity: 10,
      color: "Camel-Sandstone Woodland Plaid",
      base_color: "brown"
    },
    %{
      item_name: "Sano Long Sleeve Shirt",
      price: 45.47,
      quantity: 3,
      color: "Linen Chambray",
      base_color: "yellow"
    }
  ]

  @bottoms [
    %{
      item_name: "Wonderwall Pants",
      price: 48.97,
      quantity: 3,
      color: "French Navy",
      base_color: "blue"
    },
    %{
      item_name: "Terrena Stretch Pants",
      price: 79.95,
      quantity: 7,
      color: "Cast Iron",
      base_color: "grey"
    },
    %{
      item_name: "Happy Hike Studio Pants",
      price: 99.00,
      quantity: 8,
      color: "Ochre Red",
      base_color: "red"
    }
  ]

  def tops(), do: @tops
  def bottoms(), do: @bottoms

  def get_combinations(tops, bottoms) do
    maximum_price = 125.00
    for top <- tops,
        %{base_color: top_base_color, price: top_price} = top,
        bottom <- bottoms,
        %{base_color: bottom_base_color, price: bottom_price} = bottom,
        (top_price + bottom_price) <= maximum_price,
        top_base_color != bottom_base_color
    do
      %{top: top, bottom: bottom}
    end
  end
end

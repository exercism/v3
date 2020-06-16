# Shopping cart

## Story

We need to model a shopping cart for a corner shop. The cart should keep track of the list of items added, and correctly calculate the price.

Items are added to the basket using stock-keeping unit (SKU) codes. The items in the shop are:

| Name      | Price | SKU    |
| --------- | ----- | ------ |
| Potatoes  | 10.00 | STAPOT |
| Rice      | 30.00 | STARIC |
| Coffee    | 14.99 | STACOF |
| Newspaper | 2.99  | MEDNEW |

## Tasks

The shopping cart should allow these operations to be performed:

- Add items to the cart: Reads an SKU and adds it to the cart.
- Check the total amount: The total value of the items in the cart as a number with two decimal places (e.g. 3.99).
- Check the items contained in the cart: A list of the names of the items in the cart, ordered alphabetically, with no duplicates, separated by a comma and a space.

## Terminology

- Item: Article that can be added to the shopping cart.
- SKU: The SKU is an identifier comprised of the first three uppercase letters of the product type + the first three uppercase letters of the product name.
- Price: Item's value as a number with two decimal places.

## Implementations

- [Ruby 1-a (research)][implementation-ruby-research-1-a]

## Alternative version

An alternative version or extension of the story uses discount coupons that can be added to the shopping cart.

### Story

In order to keep customers happy, the corner shop has added discount coupons. This means that, in order to support these discounts, our shopping cart needs to be able to read and apply discount coupons to the current items. The supported coupons are:

| Type                 | SKU         | Rule                                             |
| -------------------- | ----------- | ------------------------------------------------ |
| Fixed price discount | FIX_TEN     | Discounts 10.00 off the total amount of the cart |
| Product discount     | HALF_STAPOT | Reduces all STAPOT items to half price           |

### Tasks

The shopping cart should have a new `scan coupon` operation which takes a SKU and adds it to the cart. Please bear in mind that the `total_amount` must not be a negative number, the minimum value for the `total_amount` is 0.

### Terminology

- Coupon: Discount that can be applied to the cart. Coupons are unique and can be applied only once. If the coupon type is a fixed price discount, the second part of the SKU is the discounted amount in uppercase letters.

### Implementations

- [Ruby 2-a (research)][implementation-ruby-research-2-a]

[implementation-ruby-research-1-a]: https://github.com/exercism/research_experiment_1/tree/master/exercises/ruby-1-a
[implementation-ruby-research-2-a]: https://github.com/exercism/research_experiment_1/tree/master/exercises/ruby-2-a

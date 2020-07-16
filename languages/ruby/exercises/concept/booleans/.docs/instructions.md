Working with an amusement park, you've been handed a specification to design a system to help make sure that a person meets the criteria to use the rides in the park. In this park, the minimum safe height is 106cm (~42"). If a person's height is greater than or equal to the minimum height, a random number between 1-1000 (inclusive) is issued to the rider. If the person's height is less than the minimum, no ride pass is issued. A ride pass may be revoked by the park for dangerous behaviours, a `false` value is issued to the rider.

## 1. Set the height

Implement the `MINIMUM_HEIGHT` constant of the RidePass class.

```ruby
RidePass::MINIMUM_HEIGHT
# => 106
```

## 2. Issue the ride pass

Implement the `#issue` method of the RidePass class. It should take the height as a positional argument.

```ruby
issuer = RidePass.new
issuer.issue(120)
# => 534
```

## 3. Check if the ride pass is valid

Implement the `#valid?` method of the RidePass class. It should take the ride pass value as a positional argument. If the ride pass value is not false or nil, it is valid.

```ruby
issuer = RidePass.new
issuer.valid?(627)
# => true
```

## 4. Revoke the pass

Implement the `#revoke` method of the RidePass class. If the ride pass value exists, return false to indicate that it has been revoked.

```ruby
issuer = RidePass.new
issuer.revoke(393)
# => false
```

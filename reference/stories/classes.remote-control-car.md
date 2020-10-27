# Remote control car

## Story

In this exercise you'll be playing around with a remote controlled car, which you've finally saved enough money for to buy.

Cars start with full (100%) batteries. Each time you drive the car using the remote control, it covers 20 meters and drains one percent of the battery.

The remote controlled car has a fancy LED display that shows two bits of information:

- The total distance it has driven, displayed as: `"Driven <METERS> meters"`.
- The remaining battery charge, displayed as: `"Battery at <PERCENTAGE>%"`.

If the battery is at 0%, you can't drive the car anymore and the battery display will show `"Battery empty"`.

## Tasks

These are example tasks that fit the remote control car exercise:

- Buy a brand-new remote controlled car
- Display the distance driven
- Display the battery percentage
- Update the number of meters driven when driving
- Update the battery percentage when driving
- Prevent driving when the battery is drained

## Implementations

- [C#: classes][implementation-csharp] (reference implementation)
- [Elixir: structs][implementation-elixir] (adapted for Elixir structs)

## Related

- [`concepts/object`][concepts-objects]
- [`types/class`][types-class]

[concepts-objects]: ../concepts/objects.md
[types-class]: ../types/class.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/classes/.docs/instructions.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/remote-control-car/.docs/instructions.md

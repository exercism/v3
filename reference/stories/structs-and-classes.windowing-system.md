# Windowing system

## Story

In this exercise, you will be simulating a windowing based computer system. You will create some windows that can be moved and resized and display their contents. The following image is representative of the values you will be working with below.

```
                  <--------------------- screenSize.width --------------------->

       ^          ╔════════════════════════════════════════════════════════════╗
       |          ║                                                            ║
       |          ║          position.x,_                                      ║
       |          ║          position.y  \                                     ║
       |          ║                       \<----- size.width ----->            ║
       |          ║                 ^      *──────────────────────┐            ║
       |          ║                 |      │        title         │            ║
       |          ║                 |      ├──────────────────────┤            ║
screenSize.height ║                 |      │                      │            ║
       |          ║            size.height │                      │            ║
       |          ║                 |      │       contents       │            ║
       |          ║                 |      │                      │            ║
       |          ║                 |      │                      │            ║
       |          ║                 v      └──────────────────────┘            ║
       |          ║                                                            ║
       |          ║                                                            ║
       v          ╚════════════════════════════════════════════════════════════╝
```

## Tasks

These are example tasks that fit the story of implementing a simplified windowing system for an operating system:

- Define a Size struct
- Define a Position struct
- Define a Window class
- Add a method to resize windows
- Add a method to move windows
- Add methods to update the window text and display window information
- Create a new Window

## Terminology

These are recommendations, not rules, for recurring terminology in the instructions (including stub commentary)

- `screenSize` refers to the size of the full display
- `size` refers to the size of the windows that appear on the display
- `position` refers to the position of the upper left corner of each window relative to the upper left corner of the display.

## Implementations

- [Swift: structs-and-classes][implementation-swift] (reference implementation)

## Reference

- [`types/struct`][types-struct]
- [`types/class`][types-class]

[types-struct]: ../types/struct.md
[types-class]: ../types/class.md
[implementation-swift]: ../../languages/swift/exercises/concept/windowing-system/.docs/instructions.md

You are an internationally renowned organizing guru; your books and lectures have helped people around the world to organize and declutter their lives. To do this you have developed a number of tools to help organize all sorts of things. You have a tool to organize `Int`s a tool to organize `String`s a tool to organize `Double`s and on and on. There's only one problem… you have so many tools, you can't keep them organized.

What you need to do in order to declutter your life is write a single tool that can organize _any_ type of thing. You have two tasks to help you do just that.

##1. Swap two elements in any array

For this task, you will need to write a function, `swapAt(_:_:_:)` where the first parameter is an inout array of elements of any type and the second and third parameters are `Int` indices of two elements in the array. If the two indices are valid indices, the function will swap the values at the two indices, returning the modified array via the inout parameter. If either index is invalid, the array should remain unchanged.

##2. Bubble sort any array

For this task, you will need to write a function `bubbleSort(_:swapIf:)`. The first parameter of `bubbleSort` is an inout array of elements of any type. The second parameter is a function that takes two values of the same type as the elements of the array and returns `true` if the first value should appear _after_ the second element in the array.

The `bubbleSort` will perform a [Bubble Sort][bubble-sort] using the `swapIf` parameter to determine if two elements need to be swapped and `swapAt` from you first task to swap the elements when needed.

[bubble-sort]: https://en.wikipedia.org/wiki/Bubble_sort

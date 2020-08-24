In this exercise, you will be simulating a windowing based computer system. You will create some windows that can be moved and resized and display their contents.

## 1. Define a Size struct

Define a struct named `Size` with two `Int` properties, `height` and `width` that store the window's current height and width, respectively. The initial height and width should be 80 and 60, respectively. Include a method `resize(newHeight:newWidth:)` that takes new height and width parameters and changes the properties to reflect the new size.

## 2. Define a Position struct

Define a struct named `Position` with two `Int` properties, `x` and `y` that store the current horizontal and vertical position, respectively, of the window's upper left corner. The initial values of x and y should each be 0. The position (0, 0) is the upper left corner of the screen with `x` values getting larger as you move right and `y` values getting larger as you move down.

Include a method `moveTo(newX:newY:)` that takes new x and y parameters and changes the properties to reflect the new position.

## 3. Define a Window class

Define a window class with the following properties and methods:

- `title` : `String`, Initial value is "New Window"
- `screenSize` : `Size`, constant value with `width` = 800 and `height` = 600
- `size` : `Size`, initial value is the default value of the `Size` struct
- `position` : `Position`, initial value is the default value of the `Position` struct
- `contents` : `String?`, initial value is `nil`
- `resize(to:)` : `(Size) -> ()` - This method takes a `Size` struct as input and attempts to resize the window to the specified size. However, the new size cannot exceed certain bounds. - The minimum allowed height or width is 1. Requested heights or widths less than 1 will be clipped to 1. - The maximum height and width depends on the current position of the window, the edges of the window cannot move past the edges of the screen. Values larger than these bounds will be clipped to the largest size they can take. E.g. if the window's position is at `x` = 400, `y` = 300 and a resize to `height` = 400, `width` = 300 is requested, then the window would be resized to `height` = 300, `width` = 300 as the screen is not large enough in the `y` direction to fully accommodate the request.
- `resize(deltaW:deltaH)` : `(Int, Int) -> ()` - This method takes two ints as input and attempts to resize the window by changing its size by the amounts passed in as parameters. For example, if the method call `resize(deltaW: -10, deltaH: 20)` is made, the instance will try to resize itself so that it is 10 units narrower in the x direction and 20 units taller in the y direction. However, as with `resize(to:)` the new size cannot exceed certain bounds. - The minimum allowed height or width is 1. Requested heights or widths less than 1 will be clipped to 1. - The maximum height and width depends on the current size and position of the window, the edges of the window cannot move past the edges of the screen. Values larger than these bounds will be clipped to the largest size they can take. E.g. if the window's position is at `x` = 400, `y` = 300, with `width` = 80 and `height` = 60 and a resize with `deltaW` = 400, `deltaH` = 100 is requested, then the window would be resized to `height` = 160, `width` = 400 as the screen is not large enough in the `x` direction to fully accommodate the request.
- `move(to:)` : `(Position) -> ()` - This is similar to `resize(to:)`, however, this method adjusts the _position_ of the window to the requested value, rather than the size. As with `resize` the new position cannot exceed certain limits. - The smallest position is 0 for both `x` and `y`. - The maximum position in either direction depends on the current size of the window; the edges cannot move past the edges of the screen. Values larger than these bounds will be clipped to the largest size they can take. E.g. if the window's size is at `x` = 250, `y` = 100 and a move to `x` = 600, `y` = 200 is requested, then the window would be moved to `x` = 550, `y` = 200 as the screen is not large enough in the `x` direction to fully accommodate the request.
- `move(deltaX:deltaY)` : `(Int, Int) -> ()` - This is similar to `resize(deltaW:deltaH)`, however, this method adjusts the _position_ of the window to the requested value, rather than the size. As with `resize` the new position cannot exceed certain limits. - The smallest position is 0 for both `x` and `y`. - The maximum position in either direction depends on the current size and position of the window; the edges cannot move past the edges of the screen. Values larger than these bounds will be clipped to the largest size they can take. E.g. if the window's current size is `width` = 150, height = 150 and its current position is at `x` = 500, `y` = 450, and a move with `deltaX` = -600, `y` = 200 is requested, then the window would be moved to `x` = 0, `y` = 450.
- `update(title:)` : `(String) -> ()` - This method sets the `title` property to the value of the string that was passed in.
- `update(text:)` : `(String?) -> ()` - This method sets the `contents` property to the value of the optional string that was passed in.
- `display()` : `() -> String` - This method returns a string describing the current state of the window. For example, if the window has the `title` "My First Window" with position: x = 10, y = 100; size: width = 200, height = 150; and contents: "I 😍 my window", it should return the string: `"My First Window\nPosition (10, 100), Size: (200 x 150)\nI 😍 my window\n"` - If `contents` is nil, the last line should read "[This window intentionally left blank]"

## 4. Create some Windows

Create two instances of windows and modify them via their methods as follows:

- The first window should be given the title "Main Window", with a width of 400, a height of 300 and positioned at x = 100, y = 100. Its contents should be "This is the main window". Assign this instance to the name `mainWindow`.
- The second window should be given the title of "Help Dialog", with a width and height of 100 each, positioned at x = 690 and y = 10. Its contents should be "Somebody called for help?". Assign this instance to the name `helpWindow`.

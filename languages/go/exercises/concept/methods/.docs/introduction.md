# methods

A [method][methods] is a function with a special _receiver_ argument. The receiver appears in its own argument list between `func` keyword and the name of the method.

```go
func (receiver type) MethodName(parameters) (returnTypes){

}
```

You can only define a method with a receiver whose type is defined in the same package as the method. The receiver can be either a struct type or a non-struct type as in the example below.

```go
package main

import "fmt"

type Name string

func (s Name) Greetings() string {
	return fmt.Sprintf("Welcome %s !", s)
}

func main() {
	s := Name("Bronson")
	fmt.Println(s.Greetings())
}
// Output: Welcome Bronson !
```

Methods with a value receiver operates on a copy of the value passed to it, meaning that any modification done to the receiver inside the method is not visible to the caller.

You can declare methods with pointer receivers in order to modify the value to which the receiver points. Such modifications are visible to the caller or the method as well.

```go
package main

import "fmt"

type rect struct {
	width, height int
}

func (r *rect) area() int {
	return r.width * r.height
}

func main() {
	r := rect{width: 10, height: 20}
	fmt.Println("Area is: ", r.area())
}
// Output: Area is 200
```

You can find several examples [here][pointers_receivers]. Also checkout this short tutorial about [methods][methods_tutorial].

Remember: a method is just a function with a receiver argument.

[methods]: https://tour.golang.org/methods/1
[pointers_receivers]: https://tour.golang.org/methods/4
[methods_tutorial]: https://www.callicoder.com/golang-methods-tutorial/

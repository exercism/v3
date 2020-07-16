## General

## 1. Set the height

Use a class [constant][constants] to store the height value as an [integer][integers].

## 2. Issue the ride pass

- Create a [method][methods] to [return][returns] the ride pass value.
- The [`Kernel`][kernel-class] is included for every class, it contains a method for obtaining a random number.
- Determine if a ride pass should be issued using a [control expression][control-expressions] and the [Integer][integer-class] class' included [Comparable][comparible] methods.

## 3. Check if the ride pass is valid

- Remember, all objects and values in ruby have a _truthy_ and _falsey_ value.
- [BasicObject][basicobject-class] provides a method to convert an object to a boolean value.

## 4. Revoke the pass

- If the ride pass value is _nil_, then the pass does not exist, and no operation should occur.

[basicobject-class]: https://docs.ruby-lang.org/en/master/BasicObject.html
[comparable-class]: https://docs.ruby-lang.org/en/master/Comparable.html
[constants]: https://www.rubyguides.com/2017/07/ruby-constants/
[control-expressions]: https://docs.ruby-lang.org/en/master/syntax/control_expressions_rdoc.html
[integer-class]: https://docs.ruby-lang.org/en/master/Integer.html
[kernel-class]: https://docs.ruby-lang.org/en/master/Kernel.html
[methods]: https://launchschool.com/books/ruby/read/methods
[returns]: https://www.freecodecamp.org/news/idiomatic-ruby-writing-beautiful-code-6845c830c664/

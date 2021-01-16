In Common Lisp a function's argument list is known as a ('lambda list')[lambda-list]. A lambda list can can have arguments of different types. These different types are designated with the use of ('lambda list keywords')[lambda-list-keyword] which all begin with `&`. The most commonly used types are optional, keyword and rest arguments types. Every parameter in the lambda list after a particular lambda list keyword is will be of that type. A lambda list keyword can only be used once in a lambda list.

Lambda lists are also used in other constructs which will be discussed later such as destructuring and macros.

--
[lambda-list]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list
[lambda-list-keyword]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list_keyword

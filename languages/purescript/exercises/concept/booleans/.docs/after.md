PureScript uses `true` and `false` to represent the two truth values of logic.

In PureScript, for each of the three logical operations (AND, OR and NOT) there is a corresponding operator: `&&`, `||` and `not`. In general, there are rules regarding the order of the operations, and in this case `not` (negation) is applied first, and then `&&` (conjunction) and then `||` (disjunction).

You can always 'escape' these rules by using an operator with an even higher precedence, namely, `( )` named the 'Grouping operator' or simply called 'parentheses'.

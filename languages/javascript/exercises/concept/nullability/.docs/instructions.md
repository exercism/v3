In this exercise, you'll be determining the text that needs to be printed on a badge.

A badge requires the `id` of the employee, the `name` of the employee, as well as the department in which he is working.

## 1. Text to print on the badge

Implement a function that returns the text to print on the badge.

Beware that the `id` should be between square brackets (i.e. `[20]`), and that only the id and department can be null.

A new owner should have no `id`, and no `department`

```javascript
printBadge(20, 'Bob', 'Marketing')
// => '[20] Bob - MARKETING'
```

In this exercise, you'll be implementing rules for calculating the total salary of a empyoyee in a month. The International Siderurgy Company (ISC) needs help to calculate the salary for the employees, given that different factors can alter the final wage value for each employee.

You have the following tasks

## 1. Decide a penalty for days skipped needs to be applied

Implement the `multiplierPerDaysSkipped` method that returns the penalty multiplier based on the days the employee skipped the job. A 15% penalty is applied if more than five days wore skipped.

```java
multiplierPerDaysSkipped(3);
// => 1

multiplierPerDaysSkipped(7);
// =>0.85
```

## 2. Calculate the bonus for products sold

Implement the `multiplierPerProductsSold` and `bonusProductSold` methods. The ISC pays ten monetary units for each product sold, but if the employee sold more than twenty products, the multiplier is improved to thirteen. `multiplierPerProductsSold` should decide which multiplier is applied and `bonusProductSold` should return the total bonus in monetary units.

```java
multiplierPerProductsSold(25);
// => 13

bonusProductSold(5);
// => 50
```

## 3. Calculate the final salary for the employee

Implement the `finalSalary` method. It should be able to multiply the base salary by the penalty and sum the bonus and return the result, but keep in mind that salaries should be capped at the maximum value

```java
finalSalary(0,0);
// => 1000

finalSalary(0,100);
// => 2000
```

### Attention

You should not use _if-else_ statements in this exercise, only the _ternary operator_ is allowed

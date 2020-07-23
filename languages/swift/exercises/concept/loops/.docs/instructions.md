You are an elf working in Santa Claus' logistics division and you have been given a pair of tasks from the boss to help determine the gifts that Santa will deliver.

It is believed that Santa sorts the various children of the world into naughty and nice to determine who gets presents, but that's not the case. What Santa really does is rank the children on a scale from 0 on up, where the higher the rank, the better the present the child gets. This ranking is based on an arcane formula that only Santa fully understands. But when it comes to the implementation of the system you know this much.

- Each year, Santa distills the ranking into a "magic" hash function.
- Every child in the world is represented in Santa's privacy-preserving database as an integer ID number in the range [1, 4,000,000,000).
- The ranking level of each child can be encoded by using the magic hash to transform the ID into a hashed ID.
- The ranking level can then be extracted from the hashed ID.

## Tasks

## 1. Hash the IDs from the database

Your first task is to implement the function `hashIDs(_:) which, given an array of ID numbers, takes each ID, transforms it with Santa's magic hash, and constructs a tuple of the original ID and the hashed ID, returning an array of these tuples.

Santa's magic hash is implemented by multiplying the ID by the magic constant _p_ and taking the remainder modulo another magic constant, _m_. This year, you are given the following magic constants:

- m = `3_999_999_979`
- p = `2_305_843_009`

```swift
let ids = [980035519, 1402277754, 1821223877]
hashIDs(ids)
// => [(980035519, 1921828986), (1402277754, 712641739), (1821223877, 2651471804)]
```

## 2. Compute the digital sum of a number

In order to extract Santa's ranking from a hashed ID, you will need to first write a helper function that computes the digital sum of a number. The digital sum of a number is just what it sounds like, the sum of the digits of a number. E.g. the digital sum of 10437 is 1 + 0 + 4 + 3 + 7 = 15.

For this task you will need to implement the function `digitalSum(_:)` that takes an `Int` as input and returns an `Int`. If the input number is less than 0, the function should return 0, otherwise the digital sum of the input as an `Int`.

```swift
digitalSum(10437)
// => 15
digitalSum(992)
// => 20
digitalSum(-111)
// => 0
```

## 3. Compute the ranking level for a hashed ID

For this task, you will need to implement the function, `rankingLevel(_:)` that takes a hashed ID as input and computes the ranking level for the given input in terms of what is known as a Harshad number.

A Harshad number is simply a number that is evenly divisible by the sum of its digits. So 1475 is not a Harshad number because 1 + 4 + 7 + 5 = 17, and 1475 is not divisible by 17. However, 8118 is a Harshad number because 8 + 1 + 1 + 8 = 18 and 8118 is divisible by 18.

Given this definition of a Harshad number, the ranking level of a number is the length of the sequence of _distinct_ Harshad numbers where each element of the sequence is equal to the previous element divided by the sum of its digits.

For example, consider the numbers 5217, 8118, and 7560:

5217 has a digital sum of 5 + 2 + 1 + 7 = 15 but 5217 is not a multiple of 15, so the sequence is empty and the ranking level of 5217 is 0.

8118 has a digital sum of 8 + 1 + 1 + 8 = 18, and 8118 / 18 = 451.  
451 has a digital sum of 4 + 5 + 1 = 10, but 451 is not a multiple of 10, so we stop the process.

Here, our sequence is just [8118]. Its length is 1 and so the ranking level is 1.

7560 has a digital sum = 7 + 5 + 6 + 0 = 18, and 7560 / 18 => 420.  
420 has a digital sum of 4 + 2 + 0 = 6 and 420 / 6 => 70.  
70 has a digital sum of 7 + 0 = 7 and 70 / 7 => 10.  
10 has a digital sum of 1 + 0 = 1 and 10 / 1 => 10, but this is not a distinct number, it is already in our sequence, so we stop the process.

Here our sequence is [7560, 420, 70, 10]. It's length is 4 and thus our ranking level is 4.

```swift
let level5217 = rankingLevel(5217)
// => 0

let level8118 = rankingLevel(8118)
// => 1

let level7560 = rankingLevel(7560)
// => 4
```

## 4. Compute the ranking for each ID

For this task you will implement the function, `rankIDs(hashedIDs:)` that takes as input an array of tuples of two integers representing a ID number and its hash, and outputs an array of tuples of two integers where the first is the original ID and the second is the ranking level extracted from the hashed ID.

```swift
let hashes = [(580076017, 3510227371), (1426046281, 817553565), (3757259083, 3021461315)]
let rankedIDs = rankIDs(hashedIDs: hashes)
// => [(580076017, 2), (1426046281, 1), (3757259083, 0)]
```

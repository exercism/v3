# Instructions

You are a freelance developer living in a town that hosts an annual carnival. One fan favorite, "Lucky Leo's Guessing Game", runs on custom software. Your firm has been tasked with building it.
The carnival picks a number between 1 and 7 each day. Participants try to guess that number. If they guess correctly, they win whatever is contained within the day's slot.

You must design and implement the storage mechanism for the 7 slots. The slot can contain any kind of prize. Past years have awarded local currency, extra carnival tickets, or chicken eggs.

### 1. Create a function to return the storage slots

- Your function should return a tuple with 7 slots.
- The slots should contain various prizes

```rust
let possible_prizes = prizes(); // (100, "🥚", None, None, None, None);
```

### 1. Create a function to compare the guess and return a prize

- Your function should accept a `usize` for the day of the week
- It should compare the guess with the "right answer" selected by the carnival
- If the guess is correct, it should return the value for that slot
- If the guess is incorrect, it should return `None`

```rust
let win_or_lose = guess(1); // Some(T) or None
```

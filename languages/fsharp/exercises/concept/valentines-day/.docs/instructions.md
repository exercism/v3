In this exercise, it's Valentine's day and you and your partner are planning on doing something nice together. Your partner has lots of ideas, and is now asking you to rate the ideas, in order to find the activity to engage in.

The following ideas are proposed by your partner:

- Playing a board game
- Chill out
- Watch a movie
- Go to a restaurant
- Take a walk

You have six tasks to help choose your Valentine's day activity.

## 1. Define the approval

For each idea your partner proposes, you respond with one of three options: yes, no or maybe.

Define the `Approval` discriminated union to represent these options as the following three cases: `Yes`, `No` and `Maybe`.

## 2. Define the cuisines

Your partner has selected two possible restaurants: one based on the Korean cuisine and the other based on the Turkish cuisine.

Define the `Cuisine` discriminated union to represent these cuisines as the following two cases: `Korean` and `Turkish`.

## 3. Define the movie genres

There are tons of movies to choose from, so to narrow things down, your partner also lists their genre.

Define the `Genre` discriminated union to represent the following genres as cases: `Crime`, `Horror`, `Romance` and `Thriller`.

## 4. Define the activity

As said, your partner has come up with five different activities: playing a board game, chill out, watch a movie, go to a restaurant and taking a walk.

Define the `Activity` discriminated union to represent these activity types:

- `BoardGame`: no associated data.
- `Chill`: no associated data.
- `Movie`: has its `Genre` as associated data.
- `Restaurant`: has its `Cuisine` as associated data.
- `Walk`: has an `int` representing the number of kilometers to walk as associated data.

## 5. Rate the activity

Finally, you're ready to rate your partner's ideas. This is how you feel about your partner's idea:

- Playing a board game: no.
- Chill out: no.
- Watch a movie: yes if is is a romantic movie; otherwise, no.
- Go to a restaurant: yes if the cuisine is Korean, maybe if it is Turkish.
- Take a walk: yes if the walk is less then three kilometers; maybe if it is less than five kilometers; otherwise, no.

Implement a function named `rateActivity` that takes an `Activity` value and returns the `Approval` based on the above sentiments:

```fsharp
rateActivity (Restaurant Turkish)
// => Maybe
```

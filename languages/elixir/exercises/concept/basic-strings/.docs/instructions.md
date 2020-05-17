In this exercise, you are going to help high school sweethearts profess their love on social media by generating an ASCII heart with their initials:

```
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     J. K.  +  M. B.     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *
```

### 1. Get the name's first letter

Implement the `HighSchoolSweetheart.first_letter/1` function. It should take a name and return its first letter. It should clean up any unnecessary whitespace from the name.

```elixir
iex> HighSchoolSweetheart.first_letter("Jane")
"J"
```

### 2. Format the first letter as an initial

Implement the `HighSchoolSweetheart.initial/1` function. It should take a name and return its first letter, uppercase, followed by a dot. Make sure to reuse `HighSchoolSweetheart.first_letter/1` that you defined in the previous step.

```elixir
iex> HighSchoolSweetheart.initial("Robert")
"R."
```

### 3. Split the full name into the first name and the last name

Implement the `HighSchoolSweetheart.initials/1` function. It should take a full name, consisting of a first name and a last name separated by a space, and return the initials. Make sure to reuse `HighSchoolSweetheart.initial/1` that you defined in the previous step.

```elixir
iex> HighSchoolSweetheart.initials("Lance Green")
"L. G."
```

### 4. Put the initials inside of the heart

Implement the `HighSchoolSweetheart.pair/2` function. It should take two full names and return the initials. Make sure to reuse `HighSchoolSweetheart.initials/1` that you defined in the previous step.

```elixir
iex> HighSchoolSweetheart.pair("Barbara Miller", "William Lewis") |> IO.puts()
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     B. M.  +  W. L.     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *

:ok
```

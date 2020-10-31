Something something dot matrix display.

<!-- TODO: This needs to be phrased better. -->

## 1. Define the Exercism logo matrix `E`

```julia
[
    0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0;
    0 1 0 0 1 0 1 0 0 0 0 1 0 1 0 0 1 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1;
    0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0;
    0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0;
    0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0;
]
```

If you "render" <!-- TODO print/render/whatever fits the theme --> it, it looks like this:

```
  XX          XX
 X              X
 X   X      X   X
 X  X X    X X  X
 X              X
X                X
 X    X    X    X
 X     X  X     X
 X      XX      X
 X              X
  XX          XX
```

## 2. Define a function that makes the logo frown

Define `frown!` and `frown` functions that take the Exercism logo matrix and change the smiling mouth to a frowning mouth:

```
  XX          XX
 X              X
 X   X      X   X
 X  X X    X X  X
 X              X
X                X
 X      XX      X
 X     X  X     X
 X    X    X    X
 X              X
  XX          XX
```

## 3. Rotate the Exercism logo matrix by 90° to the left / 270° to the right / $-\frac{π}{2}$

Define a method `rot270`.

In this particular case we don't care about the type of the matrix, so we can transpose it.

## 4. Rotate the Exercism logo matrix by 90° to the right / $+\frac{π}{2}$

## 5. Put together a stickerwall

Define `stickerwall`.

```
  XX          XX   X   XX          XX
 X              X  X  X              X
 X   X      X   X  X  X   X      X   X
 X  X X    X X  X  X  X  X X    X X  X
 X              X  X  X              X
X                X X X                X
 X    X    X    X  X  X      XX      X
 X     X  X     X  X  X     X  X     X
 X      XX      X  X  X    X    X    X
 X              X  X  X              X
  XX          XX   X   XX          XX
                   X
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                   X
  XX          XX   X   XX          XX
 X              X  X  X              X
 X   X      X   X  X  X   X      X   X
 X  X X    X X  X  X  X  X X    X X  X
 X              X  X  X              X
X                X X X                X
 X      XX      X  X  X    X    X    X
 X     X  X     X  X  X     X  X     X
 X    X    X    X  X  X      XX      X
 X              X  X  X              X
  XX          XX   X   XX          XX
```

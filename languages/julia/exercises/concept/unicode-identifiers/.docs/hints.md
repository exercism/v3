## 3. Define emoji constants to represent instances of `Clock`

You can convert emoji to strings, and thus Symbols, programmatically by `using REPL.REPLCompletions: emoji_symbols`:

```julia
julia> emoji_symbols["\\:clock12:"]
"🕛"
```

```julia
julia> Symbol(emoji_symbols["\\:clock12:"])
:🕛
```

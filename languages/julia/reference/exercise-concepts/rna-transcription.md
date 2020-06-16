# RNA transcription

## Example implementations

- [Using regex for sanity checks, a dictionary to store RNA transcriptions and `map` to apply the transcription to the string](https://github.com/exercism/julia/blob/master/exercises/rna-transcription/example.jl)
- [Using a function to store the RNA transcriptions](https://exercism.io/tracks/julia/exercises/rna-transcription/solutions/cfbd7e9776a54d72873c1c877d13d1a3)
- [Using exception handling to detect invalid strands](https://exercism.io/tracks/julia/exercises/rna-transcription/solutions/57f124dd4e744f61b9d721e1f08eab63)
- [Using a loop to apply the transcription dictionary to the input](https://exercism.io/tracks/julia/exercises/rna-transcription/solutions/abe6edd3fd67402f901043eddda5fed3)
- [Using the pipe operator to process everything in one line](https://exercism.io/tracks/julia/exercises/rna-transcription/solutions/f56c87e20f1d4765a73f34ab9df6be45)

## Required Concepts

- strings: the input that needs to be processed is a string
- chars: characters have to be processed to other characters
- pairs: used to construct the transcriptions dictionary
- dictionaries: store RNA transcriptions
- methods: one needs to implement a method
- errors: throw an error when the RNA strand is invalid
- conditionals: used to check if the input is valid

## Implementation-specific concepts

- map: many solutions use `map` to apply the transcription dictionary to the entire input
- enumeration: some solutions manually iterate the input string
- short-circuiting: some solutions use short-circuiting to throw an error if the input is invalid
- piping-operator: one implementation used `|>` to process everything in one line

## Concepts that were commonly used but probably shouldn't be

- exception-handling: ["It is better to avoid errors than to rely on catching them."](https://docs.julialang.org/en/v1/manual/style-guide/#Don't-overuse-try-catch-1)

# Continuous Integration

- [Markdown formatting](#markdown-formatting)
- [Concept CI](#concept-ci)

  - [`concepts.csv`](#conceptscsv)
  - [CI checks](#ci-checks)

## Markdown and JSON formatting

The formatting of all Markdown and JSON files is automatically checked against the Markdown and JSON formatting used by [prettier][prettier].

## Concept CI

The following tooling can be used to keep concept names consistent within a track.

### `concepts.csv`

This machine-readable file defines all concept slugs used in the track.
The first column contains the concept.
The second column **optionally** contains the category of the concept, e.g. some tracks differentiate between functional and object-oriented concepts.
The first row contains the headers, i.e. `concept,category`.

Concepts may contain annotations separated with a dot, `.`, from the concept name, e.g. `numbers.basic` and `numbers.advanced`.
These annotations can be used when a single concept has multiple exercises.
As a rule of thumb, annotations should only be used when multiple exercises refer to the same concept document, e.g. `string-formatting` might have its own document, while `strings.basic` and `strings.advanced` both refer to `strings.md`.

#### Example

```csv
concept,category
encapsulation,object-oriented
classes,object-oriented
inheritance,object-oriented
anonymous-functions,functions
higher-order-functions,functions
local-functions,functions
numbers.basic,
numbers.advanced,
```

For a longer example, see the Julia track's [`concepts.csv`][julia-concepts-csv].

### CI checks

_If any issues with the CI script come up, please ping @SaschaMann in the PR/issue._

The following checks are currently available:

- Check if all concepts used by exercises in `config.json` are defined in `concepts.csv`
  - **sensitive to annotations** (e.g. `strings.basic` in concepts.csv will be treated as `strings.basic`. if `concepts.csv` only contains `strings`, it will error)
- Check if concept extraction docs only contains valid concepts, under the following assumptions:
  - The first list is ignored, as it is assumed to contain the example implementations
  - In the rest of the doc, all lists are assumed to have the following format: `- <concept>: <explanation>`. Entries that aren't lists are ignored
  - **sensitive to annotations**
- Check if all exercise directory names are valid concepts.
  - **sensitive to annotations**

#### Workflow file

You can install the Concept CI checks for your track by adding your track to the workflow triggers in [`.github/workflows/concept-ci.yml`][concept-ci-yml].
Add the following below the existing paths in the `on: push: paths:` and `on: pull_request: paths` keys:

<!-- prettier-ignore -->
```yaml
    - 'languages/<language>/config.json'
    - 'languages/<language>/reference/concepts.csv'
    - 'languages/<language>/reference/exercise-concepts/**'
    - 'languages/<language>/exercises/concept/**'
```

where `<language>` needs to be replaced with the track's language slug.

##### Selectively disabling checks

If you would only like to use some of the checks provided by this workflow, then you can selectively disable checks by adding your language slug and options to the `case` statement in the `Detect Track` step of `jobs: steps:`:

```bash
case "$TRACK" in
  common-lisp) OPTS="--no-extraction-check --no-directory-check" ;;
   <language>) OPTS="<options>"
esac
```

where `<language>` needs to be replaced with the track's language slug and `<options>` can be one or more of the following (separated by spaces):

| Flag                    | Result                            |
| ----------------------- | --------------------------------- |
| `--no-config-check`     | Disable config.json checks        |
| `--no-directory-check`  | Disable exercise directory checks |
| `--no-extraction-check` | Disable concept extraction checks |

#### Testing locally

If you want to run the checks locally, you need to [install Julia v1.3][install-julia] and run the following commands:

```
$ cd languages/<language>/
$ julia --color=yes --project=../../.github/bin/concept-checks -e "using Pkg; Pkg.instantiate()"
$ julia --color=yes --project=../../.github/bin/concept-checks ../../.github/bin/concept-checks/concept-checks.jl -t <language>
```

The second line will install required dependencies and recreate the exact environment the script was developed and tested in, the third will run the checks.

If you don't want to change the working directory, you have to specify the `--root`/`-r` argument pointing to the root of the v3 repository and update the paths to `.github/bin/concept-checks` accordingly.

For convenience, you may want to create symlinks to the script in your track directory:

```
$ cd languages/<language>/
$ mkdir bin
$ cd bin/
$ ln -s ../../../.github/bin/concept-checks/concept-checks.jl ./concept-checks.jl
$ ln -s ../../../.github/bin/concept-checks/Project.toml ./Project.toml
$ ln -s ../../../.github/bin/concept-checks/Manifest.toml ./Manifest.toml
```

The checks can then be run with

```
$ julia --project=bin bin/concept-checks.jl -t <language>
```

from your track directory.

_Note that the script takes a fairly long time before actually running the checks due to compiliation times of the dependencies. This is unfortunately normal when running it as a script. If you're familiar with Julia, you can also `include` it from the REPL to avoid this. Alternatively, you can use [PackageCompiler.jl][package-compiler] to compile a custom sysimage. The necessary commands can be found in [`.github/workflows/concept-ci.yml`][concept-ci-yml]._

[concept-ci-yml]: ../../.github/workflows/concept-ci.yml
[julia-concepts-csv]: ../../languages/julia/reference/concepts.csv
[install-julia]: https://julialang.org/downloads/
[package-compiler]: https://github.com/JuliaLang/PackageCompiler.jl
[prettier]: https://prettier.io/

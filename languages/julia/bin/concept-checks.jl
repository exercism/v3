#!/usr/bin/env julia
using ArgParse
using CSV
using JSON
using Markdown

# define the command line interface
function parse_commandline()
    s = ArgParseSettings()
    @add_arg_table s begin
        "--concepts"
            help = "path to the concepts.csv file"
            arg_type = String
            default = "concepts.csv"
        "--config"
            help = "path to the config.json file"
            arg_type = String
            default = "config.json"
        "--root"
            help = "root of the v3 repository"
            arg_type = String
            default = joinpath("..", "..")
        "--track", "-t"
            help = "track slug"
            arg_type = String
            default = "julia"
    end

    return parse_args(ARGS, s)
end

"""
Check if all concepts used by exercises in config.json are defined in concepts.csv

Returns a dictionary of exercises containing undefined concepts: exercise slug ↦ undefined concepts
"""
function check_config_json(concepts, config)
    undefined_concepts = Dict{String,Vector{String}}() # exercise slug ↦ undefined concepts

    # union of concept and practice exercises
    exercises = config["exercises"]["concept"] ∪ config["exercises"]["practice"]

    for exercise in exercises
        # union of concepts and prerequisites
        ex_concepts = exercise["concepts"] ∪ exercise["prerequisites"]

        # vector to store all undefined concepts
        undefined = String[]
        for c in ex_concepts
            # ignore annotations
            c = split(c, ".")[1]

            # if the concept is not in concepts.csv, push it to undefined
            if c ∉ concepts[!, :concept]
                push!(undefined, c)
            end
        end

        # if undefined is empty, all concepts are defined
        # if not, add it to the returned dictionary
        if !isempty(undefined)
            undefined_concepts[exercise["slug"]] = undefined
        end
    end

    return undefined_concepts
end

"""
Check if a concept extraction doc only contains valid concepts.

**Assumptions**:
- The first list is ignored, as it is assumed to contain the example implementations
- In the rest of the doc, all lists are assumed to have the following format: `- <concept>: <explanation>`. Entries that aren't lists are ignored

Returns a dictionary of concepts that don't exist in concepts mapping to the reason for why it's used.
"""
function check_concept_extraction_doc(file, concepts, rootpath, track)
    # ignore meta information and the first two blocks
    md_content = Markdown.parse_file(file, flavor = :github).content

    undefined = Dict{String,String}()

    skippedfirst = false

    for md_block in md_content
        # if it's not a list, ignore it
        typeof(md_block) == Markdown.List || continue

        # if it's the very first list, it's the list of example implementations
        skippedfirst || (skippedfirst = true; continue)
        
        for entry in md_block.items
            entry_str = strip(Markdown.plain(entry))
            concept, explanation = split(entry_str, ": ")

            # if the concept is not in concepts.csv, push it to undefined
            if concept ∉ concepts[!, :concept]
                undefined[concept] = explanation
            end
        end
    end

    undefined
end

function check_concept_extraction_docs(concepts, rootpath, track)
    base_path = joinpath(rootpath, "languages", track, "reference", "exercise-concepts")

    undefined = Dict{String,Dict{String,String}}()

    for ex in readdir(base_path)
        # ignore README.md and _sidebar.md
        ex ∉ ("README.md", "_sidebar.md") || continue

        d = check_concept_extraction_doc(joinpath(base_path, ex), concepts, rootpath, track)
        if !isempty(d)
            undefined[ex] = d
        end
    end

    undefined
end

"""
Check if all exercise directory names are valid concepts.

Returns an array of exercises named after undefined concepts.
"""
function check_exercise_directories(concepts, rootpath, track)
    # vector to store all undefined exercise directory names
    undefined = String[]

    base_path = joinpath(rootpath, "languages", track, "exercises", "concept")

    for ex in readdir(base_path)
        # ignore annotations
        ex = split(ex, ".")[1]

        # ignore README.md, _sidebar.md and other files
        isdir(joinpath(base_path, ex)) || continue

        # if the concept is not in concepts.csv, push it to undefined
        if ex ∉ concepts[!, :concept]
            push!(undefined, ex)
        end
    end

    undefined
end

function main()
    anyerror = false

    args = parse_commandline()

    @info "Reading concepts from $(args["concepts"])..."
    concepts = CSV.read(args["concepts"])

    @info "Reading config from $(args["config"])..."
    config = JSON.parsefile(args["config"])

    @info "Checking $(args["config"])..."
    undefined_concepts = check_config_json(concepts, config)
    if !isempty(undefined_concepts)
        for (exercise, concepts) in undefined_concepts
            @error "Found undefined concepts in $exercise: $(join(concepts, ", "))"
        end
        anyerror = true
    end

    @info "Checking exercise directory names..."
    wrong_dirs = check_exercise_directories(concepts, args["root"], args["track"])
    if !isempty(wrong_dirs)
        @error "Found exercises named after undefined concepts: $(join(wrong_dirs, ", "))"
        anyerror = true
    end

    @info "Checking extracted concepts from v2..."
    undefined_concepts = check_concept_extraction_docs(concepts, args["root"], args["track"])
    if !isempty(undefined_concepts)
        for (ex, errors) in undefined_concepts
            pretty_errors = join(collect("- $(rpad("$k:", 20)) $v" for (k, v) in errors), '\n')
            @error "Found undefined concepts in $ex:\n$pretty_errors"
        end
        anyerror = true
    end
    anyerror
end

if abspath(PROGRAM_FILE) == @__FILE__
    main() ? exit(1) : exit(0)
end

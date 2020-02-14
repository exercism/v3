#!/usr/bin/env julia
using ArgParse
using CSV
using JSON

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

    exercises = config["exercises"]["concept"] ∪ config["exercises"]["practice"]
    for exercise in exercises

        ex_concepts = exercise["concepts"] ∪ exercise["prerequisites"]
        undefined = String[]
        for c in ex_concepts
            if c ∉ concepts[!, :concept]
                push!(undefined, c)
            end
        end

        if !isempty(undefined)
            undefined_concepts[exercise["slug"]] = undefined
        end
    end

    return undefined_concepts
end

"""
Check if all exercise directory names are valid concepts.

Returns an array of exercises named after undefined concepts.
"""
function check_exercise_directories(concepts, rootpath, track)
    undefined = String[]

    base_path = joinpath(rootpath, "languages", track, "exercises", "concept")

    for ex in readdir(base_path)
        # ignore annotations
        ex = split(ex, ".")[1]

        isdir(joinpath(base_path, ex)) || continue
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

    anyerror
end

if abspath(PROGRAM_FILE) == @__FILE__
    main() ? exit(1) : exit(0)
end

using Glob
using JSON

any_errors = false

Threads.@threads for cfg_path in glob("languages/*/exercises/concept/*/.meta/config.json")
    split_path = splitpath(cfg_path)
    exercise_path = joinpath(split_path[1:end - 2]...)
    exercise = joinpath(split_path[2], split_path[5]) # e.g. julia/lasagna

    local cfg
    try
        cfg = JSON.parsefile(cfg_path, use_mmap=false) # use_mmap=false makes it work on Windows
    catch e
        @warn "$cfg_path is not a valid JSON file" e
        continue
    end

    if !haskey(cfg, "editor")
        @warn "$exercise has no editor settings"# cfg
        continue
    end

    missing_files = [f for f in union(cfg["editor"]["test_files"], cfg["editor"]["solution_files"]) if !isfile(joinpath(exercise_path, f))]
    if length(missing_files) > 0
        @error "$exercise contains references to files that don't exist" cfg["editor"] missing_files
        global any_errors = true
    end
end

any_errors && exit(1)

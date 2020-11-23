using JSON

for exercise in readdir(joinpath("exercises", "concept"))
    exercise_path = joinpath("exercises", "concept", exercise)
    isdir(exercise_path) || continue
    cfg_path = joinpath(exercise_path, ".meta", "config.json")

    cfg = JSON.parsefile(cfg_path, use_mmap=false) # use_mmap=false makes it work on Windows
    if haskey(cfg, "editor")
        @info "⏩ $exercise already contains editor config settings"
        continue
    end

    cfg["editor"] = Dict(
        "test_files" => ["runtests.jl"],
        "solution_files" => ["$exercise.jl"],
    )
    
    open(cfg_path, "w") do io
        JSON.print(io, cfg, 4)
    end
    @info "✔️ Added editor settings for $exercise" cfg["editor"]
end

using Pkg
using Test
using JSON

# Setup GHA logger in CI
using Logging: global_logger
using GitHubActions: GitHubActionsLogger
get(ENV, "GITHUB_ACTIONS", "false") == "true" && global_logger(GitHubActionsLogger())

# TODO Change this to test practice exercises when adding the first practice exercise
for exercise in readdir(joinpath("exercises", "concept"))
    # Allow only testing specified exercises
    if !isempty(ARGS) && !(exercise in ARGS)
        continue
    end

    exercise_path = joinpath(joinpath("exercises", "concept"), exercise)
    isdir(exercise_path) || continue

    # Skip exercise tests if the Julia version doesn't meet the required version as specified in .meta/config.json
    cfg = JSON.parsefile(joinpath(exercise_path, ".meta", "config.json"))
    required_version_spec = Pkg.Types.semver_spec(get(cfg, "language_versions", "1.0"))
    if VERSION ∉ required_version_spec
        @warn "$exercise requires Julia $required_version_spec, skipping tests."
        println()
        continue
    end

    # Create an anonymous module so that exercises are tested in separate scopes
    m = Module()

    # When testing the example solution, all tests must pass, even those that are marked as skipped or broken.
    # The student will not be affected by this.
    # Overwrite @test_skip and @test_broken with @test
    Core.eval(m, :(using Test))
    @eval m $(Symbol("@test_skip")) = $(Symbol("@test"))
    @eval m $(Symbol("@test_broken")) = $(Symbol("@test"))

    # runtests.jl includes the solution by calling `include("slug.jl")`
    # Our anonymous module doesn't have `include(s::String)` defined,
    # so we define our own. We manually include the example solution in our
    # anonymous module, so we can define `m.include(s::String)` to do nothing.
    Core.eval(m, :(include(s) = nothing))
    Base.include(m, joinpath(exercise_path, joinpath(".meta", "example.jl")))
    @info "Testing $exercise"
    Base.include(m, joinpath(exercise_path, "runtests.jl"))
    
    println() # to make the output more readable
end

using JSON

const cfg = JSON.parsefile("config.json")
const concept_exercises = cfg["exercises"]["concept"]

function create_symlinks(exercise)
    exercise_path = joinpath("exercises", "concept", exercise["slug"])

    for concept in exercise["concepts"]
        meta_concepts_path = joinpath(exercise_path, ".meta", "concepts")
        link_path = joinpath(meta_concepts_path, concept)
        target_path = joinpath("concepts", concept)

        # If there's no concept dir, display an error
        if !isdir(target_path)
            @error "concepts/$concept, taught by exercise $(exercise["slug"]), does not exist"

            continue
        end

        # If the symlink already exists, don't create one
        islink(link_path) && continue
        isdir(meta_concepts_path) || mkpath(meta_concepts_path)

        symlink(target_path, link_path)
        @info " 🔗 $concept"
    end
end

for exercise in concept_exercises
    @info "Creating symlinks for $(exercise["slug"])..."
    create_symlinks(exercise)
end

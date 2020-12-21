# Writing out the tests explicitely avoids giving away the solution in the testsuite.
# It also leads to a minimal number of prerequisites.
# This file can be used to generate them.

const tab = "    "

include("example.jl")

struct Poem
    title::String
    body::String
    author::String
    acrostic::Bool
    telestich::Bool
end

const poems = (
    Poem("Shire",
    
        """
        Stands so high
        Huge hooves too
        Impatiently waits for
        Reins and harness
        Eager to leave""",

        "Michael Lockwood",
        true,
        true
    ),

    Poem("Summer",
        
        """
        Sunshine warming my toes,
        Underwater fun with my friends.
        Making homemade ice cream on the porch,
        Many long nights catching fireflies.
        Early morning walks to the creek,
        Reveling in the freedom of lazy days.""",

        "John Albert Caballero",
        true,
        false
    ),

    Poem("Sophia",

        """
        Serene, calming quality
        Organized, you always have it together
        Picturesque, strikingly beautiful
        Honest, so genuine
        Imaginative, a creative mind
        Alluring, so attractive""",

        "John Albert Caballero",
        true,
        false
    ),

    Poem("Code",

        """
        Compilers intensily bestow
        On commencing without ego
        Different processes ajar
        Exit with zero quick""",

        "Derk-Jan Karrenbeld",
        true,
        true
    ),

    Poem("",
    
        """
        Ιησούς
        Χριστός
        Θεού
        Υἱός
        Σωτήρ""",

        "",
        true,
        false
    ),

    Poem("Колокол",
    
        """
        Произнося чудесный чистый звук,
        Вишу на колокольне. Высоко!
        Неоднократно сам звенеть хотел,
        Разлиться песней сердца далеко,
        Но мой язык во власти чьих-то рук.
        Вздохнул бы я свободно и легко,
        Когда бы сам, не по заказу, пел.""",

        "Иван Чудасов",
        false,
        true
    )
)

credit(poem) = isempty(poem.title) ? "Unknown" : "'$(poem.title)' by $(poem.author)"

# """
#     generate_poems_def(poems)

# Build a string containing the definition of the `poems` constant.
# """
# function generate_poems_def(poems)
#     s = "const poems = (\n"

#     innerpoems = String[]
#     for p in poems
#         inner_s = "# '$(p.title)' by $(p.author)\n\"\"\"\n"
#         inner_s *= p.body
#         inner_s *= "\"\"\",\n"
#         push!(innerpoems, inner_s)
#     end

#     all_inner = join(tab .* split(join(innerpoems, "\n"), "\n")[begin:end-1], "\n")
#     s *= all_inner
#     s *= "\n)\n"

#     s
# end

"""
    generate_door_responses(door, poems)

Build a string containing the '{Front|Back} door responses' testset.
"""
function generate_door_responses(door, poems)
    s = "@testset \"$(titlecase(door)) door responses\" begin\n"
    respf = Dict(
        "front" => front_response,
        "back" => back_response,
    )

    innertestsets = String[]

    for p in poems
        # Not all test poems are acrostic or telestich
        if (door == "back" && !(p.telestich)) || (door == "front" && !(p.acrostic))
            continue
        end

        inner_s = ""
        inner_s *= "$(tab)@testset \"$(credit(p))\" begin\n"

        for l in string.(split(p.body, "\n", keepempty=false))
            inner_s *= "$(tab^2)@test $(door)_response(\"$l\") == '$(respf[door](l))'\n"
        end

        inner_s *= "$(tab)end\n"

        push!(innertestsets, inner_s)
    end

    s *= join(innertestsets, "\n")
    s *= "end\n"

    s
end

"""
    generate_door_passwords(door, poems)

Build a string containing the '{Front|Back} door passwords' testset.
"""
function generate_door_passwords(door, poems)
    s = "@testset \"$(titlecase(door)) door passwords\" begin\n"

    pwdf = Dict(
        "front" => front_password,
        "back" => back_password,
    )

    inner_s = String[]
    for p in poems
        # Not all test poems are acrostic or telestich
        if door == "back" && !(p.telestich) || (door == "front" && !(p.acrostic))
            continue
        end

        comment = "$(tab) # $(credit(p))"

        input = join(tab^2 .* split(p.body, "\n"), "\n")
        test = "$(tab)@test $(door)_password(\"\"\"\n$input\"\"\"\n$tab) == \"$(pwdf[door](p.body))\"\n"
        push!(inner_s, join((comment, test), "\n"))
    end

    s *= join(inner_s, "\n")
    s *= "end\n"

    s
end

# """
#     generate_bar_passwords(poems)

# Build a string containing the 'Bar passwords' testset.
# """
# function generate_bar_passwords(poems)
#     s = "@testset \"Bar passwords\" begin\n"

#     inner_s = String[]
#     for p in poems
#         comment = "$(tab) # $(credit(p))"

#         input = join(tab^2 .* split(p.body, "\n"), "\n")
#         test = "$(tab)@test bar_password(\"\"\"\n$input\"\"\"\n$tab) == \"$(bar_password(p.body))\"\n"
#         push!(inner_s, join((comment, test), "\n"))
#     end

#     s *= join(inner_s, "\n")
#     s *= "end\n"

#     s
# end


"""
    main(poems)

Stitch all test strings together to the runtests.jl file
"""
function main(poems=poems)
    # Header
    s = "using Test\n\ninclude(\"poetry-club.jl\")\n\n"

    # Testsets
    s *= join((
        generate_door_responses("front", poems),
        generate_door_passwords("front", poems),
        generate_door_responses("back", poems),
        generate_door_passwords("back", poems),
        # generate_bar_passwords(poems)
    ), "\n")

    s
end

if abspath(PROGRAM_FILE) == @__FILE__
    print(main())
end

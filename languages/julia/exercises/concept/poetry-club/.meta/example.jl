front_response(line) = line[begin]
front_password(poem) = titlecase(join(front_response.(split(poem, "\n"))))

function remove_punctuation(s)
    for punct in ".,!"
        s = replace(s, punct => "")
    end
    s
end

back_response(line) = remove_punctuation(line)[end]
back_password(poem) = titlecase(join(back_response.(split(poem, "\n")))) * ", please."

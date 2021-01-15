front_response(line) = line[begin]
front_password(poem) = titlecase(join(front_response.(split(poem, "\n"))))

remove_punctuation(s) = filter(!ispunct, s)

back_response(line) = remove_punctuation(line)[end]
back_password(poem) = titlecase(join(back_response.(split(poem, "\n")))) * ", please."

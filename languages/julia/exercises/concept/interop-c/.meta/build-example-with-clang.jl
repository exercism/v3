using Clang

# LIBCLANG_HEADERS are those headers to be wrapped.
const LIBLINKEDLIST_INCLUDE = joinpath("liblinkedlist", "src") |> normpath
const LIBLINKEDLIST_HEADERS = [joinpath(LIBLINKEDLIST_INCLUDE, header) for header in readdir(LIBLINKEDLIST_INCLUDE) if endswith(header, ".h")]

wc = init(; headers = LIBLINKEDLIST_HEADERS,
            output_file = joinpath(@__DIR__, "liblinkedlist_api.jl"),
            common_file = joinpath(@__DIR__, "liblinkedlist_common.jl"),
            clang_includes = vcat(LIBLINKEDLIST_INCLUDE, CLANG_INCLUDE),
            clang_args = ["-I", joinpath(LIBLINKEDLIST_INCLUDE, "..")],
            header_wrapped = (root, current)->root == current,
            header_library = x->"liblinkedlist",
            clang_diagnostics = true,
            )

run(wc)

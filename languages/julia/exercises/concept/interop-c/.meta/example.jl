module LibLinkedlist

# import Libdl

# # Load in `deps.jl`, complaining if it does not exist
# const depsjl_path = joinpath(@__DIR__, "..", "deps", "deps.jl")
# if !isfile(depsjl_path)
#     error("LibXXX was not build properly. Please run Pkg.build(\"LibXXX\").")
# end
# include(depsjl_path)
# # Module initialization function
# function __init__()
#     check_deps()
# end

# using CEnum

### ctypes.jl ###
## TODO: pending https://github.com/JuliaLang/julia/issues/29420
# this one is suggested in the issue, but it looks like time_t and tm are two different things?
# const Ctime_t = Base.Libc.TmStruct

const Ctm = Base.Libc.TmStruct
const Ctime_t = UInt
const Cclock_t = UInt

export Ctm, Ctime_t, Cclock_t

### liblinkedlist_common.jl
# Automatically generated using Clang.jl

const ll_data_t = Cint
const list = Cvoid

### liblinkedlist_api.jl ###
# Julia wrapper for header: linked_list.h
# Automatically generated using Clang.jl

function list_create()
    ccall((:list_create, liblinkedlist), Ptr{list}, ())
end

function list_is_empty()
    ccall((:list_is_empty, liblinkedlist), Cint, ())
end

function list_push()
    ccall((:list_push, liblinkedlist), Cint, ())
end

function list_pop(list)
    ccall((:list_pop, liblinkedlist), ll_data_t, (Ptr{list},), list)
end

function list_unshift()
    ccall((:list_unshift, liblinkedlist), Cint, ())
end

function list_shift(list)
    ccall((:list_shift, liblinkedlist), ll_data_t, (Ptr{list},), list)
end

function list_destroy(list)
    ccall((:list_destroy, liblinkedlist), Cvoid, (Ptr{list},), list)
end

# export everything
#foreach(names(@__MODULE__, all=true)) do s
#    if startswith(string(s), "SOME_PREFIX")
#        @eval export $s
#    end
#end

end # module

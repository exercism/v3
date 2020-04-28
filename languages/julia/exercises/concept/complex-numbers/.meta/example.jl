using Plots

f(z, c) = z^2 + c

function n(z, c)
    n = 0
    while abs2(z) < 4
        z = f(z, c)
        n += 1
    end
    n
end

function julia_set(c)
    lims = -1.5:0.0005:1.5
    ns = Matrix{Int}(undef, length(lims), length(lims))
    for (i, x) in enumerate(lims), (j, y) in enumerate(lims)
        ns[j, i] = n(complex(x, y), c)
    end
    ns
end

# heatmap(lims, lims, ns, aspect_ratio = 1, size = (1200, 1200))
# p = heatmap(lims, lims, ns, aspect_ratio = 1, size=(1200,1200), legend=:none, xlabel="Re(z)", ylabel="Im(z)", fillcolor=:dark_grad)

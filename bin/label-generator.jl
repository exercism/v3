function main(outfile)
    out = """documentation:
      - docs/**/*
    
    """

    for slug in readdir("languages")
        isdir(joinpath("languages", slug)) || continue
        out *= """track/$slug:
            - languages/$slug/**/*

        """
    end

    write(outfile, out)
end


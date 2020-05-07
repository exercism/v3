static class Badge
{
    public static string Print(int? id, string name, string? department)
    {
        var worksAt = department?.ToUpper() ?? "OWNER";

        if (id == null)
        {
            return $"{name} - {worksAt}";
        }

        return $"[{id}] {name} - {worksAt}";
    }
}
WIP scaffold nullability exercise

- Implement tests
- Implement example
- Implement stub
- Add to config.json

# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
#
# On branch javascript/concept/nullability
# Changes to be committed:
#	new file:   .docs/after.md
#	new file:   .docs/hints.md
#	new file:   .docs/instructions.md
#	new file:   .docs/introduction.md
#	new file:   .eslintrc
#	new file:   .gitignore
#	new file:   .meta/config.json
#	new file:   babel.config.js
#	new file:   example.js
#	new file:   nullability.js
#	new file:   nullability.spec.js
#	new file:   package.json
#

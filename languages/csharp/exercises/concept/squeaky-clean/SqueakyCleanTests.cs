using Xunit;

public class CharsTest
{
    [Fact]
    public void Clean_single_letter()
    {
        Assert.Equal("A", Identifier.Clean("A"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Clean_clean_string()
    {
        Assert.Equal("àḃç", Identifier.Clean("àḃç"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Clean_string_with_spaces()
    {
        Assert.Equal("my___Id", Identifier.Clean("my   Id"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Clean_string_with_control_char()
    {
        Assert.Equal("myCTRLId", Identifier.Clean("my\0Id"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Clean_string_with_no_letters()
    {
        Assert.Equal(string.Empty, Identifier.Clean("😀😀😀"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Clean_empty_string()
    {
        Assert.Equal(string.Empty, Identifier.Clean(string.Empty));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Convert_kebab_to_camel_case()
    {
        Assert.Equal("àḂç", Identifier.Clean("à-ḃç"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Omit_lower_case_greek_letters()
    {
        Assert.Equal("MyΟFinder", Identifier.Clean("MyΟβιεγτFinder"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Combine_conversions()
    {
        Assert.Equal("_AbcĐCTRL", Identifier.Clean("9 -abcĐ😀ω\0"));
    }
}

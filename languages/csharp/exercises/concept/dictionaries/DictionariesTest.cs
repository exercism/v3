using Xunit;

public class DictionariesTest
{
    [Fact]
    public void Empty_dictionary()
    {
        Assert.Empty(Dictionaries.GetEmptyDiectionary());
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Existing_dictionary()
    {
        var idcd = Dictionaries.GetExistingDictionary();
        Assert.Equal(3, idcd.Count);
        Assert.Equal("United States of America", idcd[1]);
        Assert.Equal("Brazil", idcd[55]);
        Assert.Equal("India", idcd[91]);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Add_country_to_empty_dictionary()
    {
        var idcd = Dictionaries.AddCountryToEmptyDictionary(44, "United Kingdom");
        Assert.Equal(1, idcd.Count);
        Assert.Equal("United Kingdom", idcd[44]);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Add_country_to_existing_dictionary()
    {
        var idcd = Dictionaries.AddCountryToExistingDictionary(
            Dictionaries.GetExistingDictionary(), 44, "United Kingdom");
        Assert.Equal(4, idcd.Count);
        Assert.Equal("United States of America", idcd[1]);
        Assert.Equal("United Kingdom", idcd[44]);
        Assert.Equal("Brazil", idcd[55]);
        Assert.Equal("India", idcd[91]);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Get_country_name_from_dictionary()
    {
        var countryName = Dictionaries.GetCountryNameFromDictionary(
            Dictionaries.GetExistingDictionary(), 55);
        Assert.Equal("Brazil", countryName);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Check_country_exists()
    {
        var exists = Dictionaries.CheckCodeExists(
            Dictionaries.GetExistingDictionary(), 55);
        Assert.True(exists);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Try_to_get_non_existent_country_name_from_dictionary()
    {
        var countryName = Dictionaries.GetCountryNameFromDictionary(
            Dictionaries.GetExistingDictionary(), 999);
        Assert.Equal(string.Empty, countryName);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Update_country_name_in_dictionary()
    {
        var idcd = Dictionaries.UpdateDictionary(
            Dictionaries.GetExistingDictionary(), 1, "Les États-Unis");
        Assert.Equal(3, idcd.Count);
        Assert.Equal("Les États-Unis", idcd[1]);
        Assert.Equal("Brazil", idcd[55]);
        Assert.Equal("India", idcd[91]);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Try_to_update_country_name_in_dictionary_for_non_existent_country()
    {
        var idcd = Dictionaries.UpdateDictionary(
            Dictionaries.GetExistingDictionary(), 999, "Newlands");
        Assert.Equal(3, idcd.Count);
        Assert.Equal("United States of America", idcd[1]);
        Assert.Equal("Brazil", idcd[55]);
        Assert.Equal("India", idcd[91]);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Remove_country_from_dictionary()
    {
        var idcd = Dictionaries.RemoveCountryFromDictionary(
            Dictionaries.GetExistingDictionary(), 91);
        Assert.Equal(2, idcd.Count);
        Assert.Equal("United States of America", idcd[1]);
        Assert.Equal("Brazil", idcd[55]);
    }
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Longest_name()
    {
        var idcd = Dictionaries.FindLongestCountryName(
            Dictionaries.GetExistingDictionary());
        Assert.Equal("United States of America", idcd);
    }
}

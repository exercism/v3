using System;
using System.Collections.Generic;

public static class Dictionaries
{
    public static Dictionary<int, string> GetEmptyDiectionary()
    {
        return new Dictionary<int, string>();
    }

    public static Dictionary<int, string> GetExistingDictionary()
    {
        return new Dictionary<int, string>
        {
            {1, "United States of America"},
            {55, "Brazil"},
            {91, "India"}
        };
    }

    public static Dictionary<int, string> AddCountryToEmptyDictionary(int CountryCode, string CountryName)
    {
        return new Dictionary<int, string>() { { CountryCode, CountryName } };
    }

    public static Dictionary<int, string> AddCountryToExistingDictionary(
        Dictionary<int, string> existingDictiopnary, int countryCode, string CountryName)
    {
        existingDictiopnary[countryCode] = CountryName;
        return existingDictiopnary;
    }

    public static string GetCountryNameFromDictionary(
        Dictionary<int, string> existingDictionary, int countryCode)
    {
        if (existingDictionary.ContainsKey(countryCode))
        {
            return existingDictionary[countryCode];
        }
        else
        {
            return string.Empty;
        }
    }
    public static Dictionary<int, string>  UpdateDictionary(
        Dictionary<int, string> existingDictionary, int countryCode, string countryName)
    {
        if (existingDictionary.ContainsKey(countryCode))
        {
            existingDictionary[countryCode] = countryName;
        }

        return existingDictionary;
    }

    public static Dictionary<int, string> RemoveCountryFromDictionary(
        Dictionary<int, string> existingDictionary, int countryCode)
    {
        existingDictionary.Remove(countryCode);
        return existingDictionary;
    }
}

using System;
using System.Collections.Generic;
using System.Reflection;

public class Dictionaries
{
    public static Dictionary<int, string> GetEmptyDiectionary()
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }

    public static Dictionary<int, string> GetExistingDictionary()
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }

    public static Dictionary<int, string> AddCountryToEmptyDictionary(int CountryCode, string CountryName)
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }

    public static Dictionary<int, string> AddCountryToExistingDictionary(
        Dictionary<int, string> existingDictiopnary, int countryCode, string CountryName)
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }

    public static string GetCountryNameFromDictionary(
        Dictionary<int, string> existingDictionary, int countryCode)
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }
    public static Dictionary<int, string> UpdateDictionary(
        Dictionary<int, string> existingDictionary, int countryCode, string countryName)
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }

    public static Dictionary<int, string> RemoveCountryFromDictionary(
        Dictionary<int, string> existingDictionary, int countryCode)
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }
    public static bool CheckCodeExists(Dictionary<int, string> existingDictionary, int countryCode)
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }

    public static string FindLongestCountryName(Dictionary<int, string> existingDictionary)
    {
        throw new NotImplementedException($"Please implement the (static) {MethodBase.GetCurrentMethod().Name} method");
    }
}

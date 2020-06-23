using System;

public static class PhoneNumber
{
    public static (bool IsNewYork, bool IsFake, string LocalNumber) Analyze(string phoneNumber)
    {
        throw new NotImplementedException($"Please implement the (static) PhoneNumber.Analyze() method");
    }

    public static bool IsFake((bool IsNewYork, bool IsFake, string LocalNumber) phoneNumberInfo)
    {
        throw new NotImplementedException($"Please implement the (static) PhoneNumber.IsFake() method");
    }

    public static bool AreDuplicate((bool IsNewYork, bool IsFake, string LocalNumber) phoneNumberInfo,
        (bool IsNewYork, bool IsFake, string LocalNumber) storedPhoneNumberInfo)
    {
        throw new NotImplementedException($"Please implement the (static) PhoneNumber.AreDuplicate() method");
    }
}

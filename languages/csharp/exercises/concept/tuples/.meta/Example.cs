using System;
using System.Runtime.Serialization;

public class PhoneNumbers
{
    public static (bool IsNewYork, bool IsFake, string LocalNumber) Analyze(string phoneNumber)
    {
        return (phoneNumber.Substring(0, 3) == "212"
            , phoneNumber.Substring(4, 3) == "555"
            , phoneNumber.Substring(8, 4));
    }

    public static bool IsFake((bool IsnewYork, bool IsFake, string LocalNumber) phoneNumberInfo)
    {
        return phoneNumberInfo.IsFake;
    }

}

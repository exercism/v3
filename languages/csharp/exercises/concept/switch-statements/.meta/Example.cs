
using System;

// **** please do not modify the Manager class ****
public class Manager
{
    public string NickName { get; }
    public string Activity { get; }

    public Manager(string nickName, string activiy)
    {
        this.NickName = nickName;
        this.Activity = activiy;
    }
}

// **** please do not modify the Incident enum ****
public enum Incident
{
    RedCard,
    YellowCard,
    Fowl,
    Injury
}

public static class PlayAnalyzer
{
    public static string AnalyzeOnField(int shirtNum)
    {
        string playerDescription = string.Empty;
        switch (shirtNum)
        {
            case 1:
                playerDescription = "goalie";
                break;
            case 2:
                playerDescription = "left back";
                break;
            case 5:
                playerDescription = "right back";
                break;
            case 3:
            case 4:
                playerDescription = "part of the back four";
                break;
            case 6:
            case 7:
            case 8:
                playerDescription = "mid fielder";
                break;
            case 9:
                playerDescription = "left wing";
                break;
            case 11:
                playerDescription = "right wing";
                break;
            case 10:
                playerDescription = "striker";
                break;
            default:
                throw new ArgumentException();
        }

        return playerDescription;
    }

    public static string AnalyzeOffField(object report)
    {
        string description = string.Empty;
        switch (report)
        {
            case int shirtNum:
                description = AnalyzeOnField(shirtNum);
                break;
            case string freeFromText:
                description = freeFromText;
                break;
            case Incident incident:
                description = incident.ToString();
                break;
            case Manager manager when !string.IsNullOrWhiteSpace(manager.NickName):
                description = manager.NickName;
                break;
            case Manager manager:
                description = "the manager";
                break;
            default:
                throw new ArgumentException();
        }

        return description;
    }
}

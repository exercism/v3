using System;
using System.Collections.Generic;

public class FacialFeatures
{
    public string EyeColor { get; set;  }
    public decimal PhiltrumWidth { get; set; }

    public FacialFeatures()
    {
    }
}

public class Identity
{
    public string Email { get; set; }
    public FacialFeatures FacialFeatures { get; set; }
    public IList<string> NameAndAddress { get; set; }

    public Identity()
    {
    }
}

public class Authenticator
{
    private Identity admin = new Identity{
        Email = "bert@exerc.ism",
        FacialFeatures = new FacialFeatures
        {
            EyeColor = "green",
            PhiltrumWidth = 0.9m
        },
        NameAndAddress = {"Chanakya Niti Kautilya Arthashastra", "Plausible Address", "Mombai"}
    };

    private IDictionary<string, Identity> developers
        = new Dictionary<string, Identity>
        {
            ["bert"] = new Identity{
                Email = "bert@exerc.ism",
                FacialFeatures = new FacialFeatures
                {
                    EyeColor = "green",
                    PhiltrumWidth = 0.9m
                },
                NameAndAddress = {"Bertrand Meyer", "Avenue des Champs-Élysées", "Paris"}
                },

            ["anders"] = new Identity{
                Email = "anders@exerc.ism",
                FacialFeatures = new FacialFeatures
                {
                    EyeColor = "green",
                    PhiltrumWidth = 0.9m
                },
                NameAndAddress = {"Anders Hejlsberg", "Plausible Address", "Redmond"}
            }
        };

    public Identity GetAdmin()
    {
        return admin;
    }

    public IDictionary<string, Identity> GetDevelopers()
    {
        return developers;
    }
}

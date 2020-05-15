using System;

abstract class Character
{
    protected Character(string characterType)
    {
        throw new NotImplementedException("Please implement the Character() constructor");
    }

    public abstract int DamagePoints(Character target);

    public virtual bool Vulnerable()
    {
        throw new NotImplementedException("Please implement the Character.Vulnerable() method");
    }

    public override string ToString()
    {
        throw new NotImplementedException("Please implement the Character.ToString() method");
    }
}

class Warrior : Character
{
    // TODO: define the constructor for the 'Warrior' class

    public override int DamagePoints(Character target)
    {
        throw new NotImplementedException("Please implement the Warrior.DamagePoints() method");
    }
}

class Wizard : Character
{
    // TODO: define the constructor for the 'Wizard' class

    public override int DamagePoints(Character target)
    {
        throw new NotImplementedException("Please implement the Wizard.DamagePoints() method");
    }

    public void PrepareSpell()
    {
        throw new NotImplementedException("Please implement the Wizard.PrepareSpell() method");
    }
}

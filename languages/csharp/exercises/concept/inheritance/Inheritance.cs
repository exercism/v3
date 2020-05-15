using System;

abstract class Character
{
    public abstract int DamagePoints(Character target);

    public virtual bool Vulnerable()
    {
        throw new NotImplementedException("Please implement the Character.Vulnerable() method");
    }
}

class Warrior : Character
{
    public override int DamagePoints(Character target)
    {
        throw new NotImplementedException("Please implement the Warrior.DamagePoints() method");
    }
}

class Wizard : Character
{
    public override int DamagePoints(Character target)
    {
        throw new NotImplementedException("Please implement the Wizard.DamagePoints() method");
    }

    public void PrepareSpell()
    {
        throw new NotImplementedException("Please implement the Wizard.PrepareSpell() method");
    }
}

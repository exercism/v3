using System;

abstract class Character
{
    public void Attack(Character target)
    {
        throw new NotImplementedException("Please implement the Character.Attack() method");
    }

    public bool Stunned()
    {
        throw new NotImplementedException("Please implement the Character.Stunned() method");
    }

    protected abstract int Damage();
}

class Wizard : Character
{
    protected override int Damage()
    {
        throw new NotImplementedException("Please implement the Wizard.Damage() method");
    }

    public void PrepareSpell()
    {
        throw new NotImplementedException("Please implement the Wizard.PrepareSpell() method");
    }
}

class Warrior : Character
{
    protected override int Damage()
    {
        throw new NotImplementedException("Please implement the Warrior.Damage() method");
    }

    public void DrinkPotion()
    {
        throw new NotImplementedException("Please implement the Warrior.DrinkPotion() method");
    }
}

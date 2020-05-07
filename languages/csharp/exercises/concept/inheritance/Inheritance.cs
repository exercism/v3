using System;

abstract class Player
{
    // TODO: define the constructor for the 'Player' class
    
    public void Attack(Player target)
    {
        throw new NotImplementedException("Please implement the Player.Attack() method");
    }

    public bool Stunned()
    {
        throw new NotImplementedException("Please implement the Player.Stunned() method");
    }

    protected abstract int Damage();
}

class Wizard : Player
{
    // TODO: define the constructor for the 'Wizard' class

    protected override int Damage()
    {
        throw new NotImplementedException("Please implement the Wizard.Damage() method");
    }
    
    public void PrepareSpell()
    {
        throw new NotImplementedException("Please implement the Wizard.PrepareSpell() method");
    }
}

class Warrior : Player
{
    // TODO: define the constructor for the 'Warrior' class

    protected override int Damage()
    {
        throw new NotImplementedException("Please implement the Warrior.Damage() method");
    }

    public void DrinkPotion()
    {
        throw new NotImplementedException("Please implement the Warrior.DrinkPotion() method");
    }
}

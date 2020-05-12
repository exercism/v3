abstract class Character
{
    private int _hitPoints;

    protected Character(int hitPoints)
    {
        _hitPoints = hitPoints;
    }

    protected abstract int Damage();

    public void Attack(Character target)
    {
        if (Stunned())
        {
            return;
        }

        target._hitPoints -= Damage();
    }

    public bool Stunned()
    {
        return _hitPoints <= 0;
    }

    public override string ToString()
    {
        return $"HP: {_hitPoints}";
    }
}

class Wizard : Character
{
    private bool spellPrepared;

    public Wizard() : base(20)
    {
    }

    public void PrepareSpell()
    {
        spellPrepared = true;
    }

    protected override int Damage()
    {
        if (spellPrepared)
        {
            spellPrepared = false;
            return 12;
        }

        return 3;
    }
}

class Warrior : Character
{
    private bool potionDrunk;

    public Warrior() : base(30)
    {
    }

    public void DrinkPotion()
    {
        potionDrunk = true;
    }

    protected override int Damage()
    {
        if (potionDrunk)
        {
            potionDrunk = false;
            return 10;
        }

        return 6;
    }
}

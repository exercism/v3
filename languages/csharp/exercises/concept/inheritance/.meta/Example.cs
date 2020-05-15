abstract class Character
{
    private string className;

    protected Character(string className)
    {
        this.className = className;
    }

    public override string ToString()
    {
        return $"Character is a {className}";
    }

    public abstract int DamagePoints(Character target);

    public virtual bool Vulnerable()
    {
        return false;
    }
}

class Warrior : Character
{
    public Warrior() : base("Warrior")
    {
    }

    public override int DamagePoints(Character target)
    {
        if (target.Vulnerable())
        {
            return 10;
        }

        return 6;
    }
}

class Wizard : Character
{
    private bool spellPrepared;

    public Wizard() : base("Wizard")
    {
    }

    public void PrepareSpell()
    {
        spellPrepared = true;
    }

    public override int DamagePoints(Character target)
    {
        return spellPrepared ? 12 : 3;
    }

    public override bool Vulnerable()
    {
        return !spellPrepared;
    }
}

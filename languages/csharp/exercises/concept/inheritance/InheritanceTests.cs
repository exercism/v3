using Xunit;

public class RolePlayingGameTests
{
    [Fact]
    public void Create_new_wizard()
    {
        var wizard = new Wizard();
        Assert.NotNull(wizard);
    }

    [Fact]
    public void Create_new_warrior()
    {
        var warrior = new Warrior();
        Assert.NotNull(warrior);
    }

    [Fact]
    public void Display_hit_points_of_new_wizard()
    {
        var wizard = new Wizard();
        Assert.Equal("HP: 20", wizard.ToString());
    }

    [Fact]
    public void Display_hit_points_of_new_warrior()
    {
        var warrior = new Warrior();
        Assert.Equal("HP: 30", warrior.ToString());
    }

    [Fact]
    public void New_warrior_attacks_new_wizard()
    {
        var warrior = new Warrior();
        var wizard = new Wizard();
        warrior.Attack(wizard);
        Assert.Equal("HP: 14", wizard.ToString());
    }

    [Fact]
    public void New_wizard_attacks_new_warrior()
    {
        var wizard = new Wizard();
        var warrior = new Warrior();
        wizard.Attack(warrior);
        Assert.Equal("HP: 27", warrior.ToString());
    }

    [Fact]
    public void New_warrior_attacks_with_charge()
    {
        var warrior = new Warrior();
        warrior.DrinkPotion();

        var targetWarrior = new Warrior();
        warrior.Attack(targetWarrior);

        Assert.Equal("HP: 20", targetWarrior.ToString());
    }

    [Fact]
    public void New_wizard_attacks_with_prepared_spell()
    {
        var wizard = new Wizard();
        wizard.PrepareSpell();

        var warrior = new Warrior();
        wizard.Attack(warrior);

        Assert.Equal("HP: 18", warrior.ToString());
    }

    [Fact]
    public void New_warrior_attacks_with_potion_drunk()
    {
        var warrior = new Warrior();
        warrior.DrinkPotion();

        var wizard = new Wizard();
        warrior.Attack(wizard);

        Assert.Equal("HP: 10", wizard.ToString());
    }

    [Fact]
    public void Wizard_prepared_spell_only_affects_one_attack()
    {
        var wizard = new Wizard();
        wizard.PrepareSpell();

        var warrior = new Warrior();
        wizard.Attack(warrior);
        wizard.Attack(warrior);

        Assert.Equal("HP: 15", warrior.ToString());
    }

    [Fact]
    public void Warrior_potion_drunk_only_affects_one_attack()
    {
        var warrior = new Warrior();
        warrior.DrinkPotion();

        var wizard = new Wizard();
        warrior.Attack(wizard);
        warrior.Attack(wizard);

        Assert.Equal("HP: 4", wizard.ToString());
    }

    [Fact]
    public void Massive_battle()
    {
        var warrior = new Warrior();
        var wizard = new Wizard();

        warrior.DrinkPotion();
        warrior.Attack(wizard);

        wizard.Attack(warrior);
        warrior.Attack(wizard);

        wizard.PrepareSpell();
        wizard.Attack(warrior);

        Assert.Equal("HP: 15", warrior.ToString());
        Assert.Equal("HP: 4", wizard.ToString());
    }

    [Fact]
    public void New_warrior_has_not_been_stunned()
    {
        var warrior = new Warrior();
        Assert.False(warrior.Stunned());
    }

    [Fact]
    public void New_wizard_has_not_been_stunned()
    {
        var wizard = new Wizard();
        Assert.False(wizard.Stunned());
    }

    [Fact]
    public void Warrior_that_has_been_stunned()
    {
        var warrior = new Warrior();

        var otherWarrior = new Warrior();
        otherWarrior.Attack(warrior);
        otherWarrior.Attack(warrior);
        otherWarrior.Attack(warrior);
        otherWarrior.Attack(warrior);
        otherWarrior.Attack(warrior);

        Assert.True(warrior.Stunned());
    }

    [Fact]
    public void Wizard_that_has_been_stunned()
    {
        var wizard = new Wizard();

        var warrior = new Warrior();
        warrior.Attack(wizard);
        warrior.Attack(wizard);
        warrior.Attack(wizard);
        warrior.Attack(wizard);

        Assert.True(wizard.Stunned());
    }

    [Fact]
    public void Warrior_that_has_been_stunned_does_not_do_damage()
    {
        var warrior = new Warrior();

        var otherWarrior = new Warrior();
        otherWarrior.Attack(warrior);
        otherWarrior.Attack(warrior);
        otherWarrior.Attack(warrior);
        otherWarrior.Attack(warrior);
        otherWarrior.Attack(warrior);

        warrior.Attack(otherWarrior);

        Assert.Equal("HP: 30", otherWarrior.ToString());
    }

    [Fact]
    public void Wizard_that_has_been_stunned_does_not_do_damage()
    {
        var wizard = new Wizard();

        var otherWizard = new Wizard();
        otherWizard.PrepareSpell();
        otherWizard.Attack(wizard);
        otherWizard.PrepareSpell();
        otherWizard.Attack(wizard);

        wizard.Attack(otherWizard);

        Assert.Equal("HP: 20", otherWizard.ToString());
    }
}

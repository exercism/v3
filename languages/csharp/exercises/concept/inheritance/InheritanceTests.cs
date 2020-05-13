using Xunit;

public class RolePlayingGameTests
{
    [Fact]
    public void CreateNewWizard()
    {
        var wizard = new Wizard();
        Assert.NotNull(wizard);
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void CreateNewWarrior()
    {
        var warrior = new Warrior();
        Assert.NotNull(warrior);
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void DisplayHitPointsOfNewWizard()
    {
        var wizard = new Wizard();
        Assert.Equal("HP: 20", wizard.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void DisplayHitPointsOfNewWarrior()
    {
        var warrior = new Warrior();
        Assert.Equal("HP: 30", warrior.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void NewWarriorAttacksNewWizard()
    {
        var warrior = new Warrior();
        var wizard = new Wizard();
        warrior.Attack(wizard);
        Assert.Equal("HP: 14", wizard.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void NewWizardAttacksNewWarrior()
    {
        var wizard = new Wizard();
        var warrior = new Warrior();
        wizard.Attack(warrior);
        Assert.Equal("HP: 27", warrior.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void NewWarriorAttacksWithCharge()
    {
        var warrior = new Warrior();
        warrior.DrinkPotion();

        var targetWarrior = new Warrior();
        warrior.Attack(targetWarrior);

        Assert.Equal("HP: 20", targetWarrior.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void NewWizardAttacksWithPreparedSpell()
    {
        var wizard = new Wizard();
        wizard.PrepareSpell();

        var warrior = new Warrior();
        wizard.Attack(warrior);

        Assert.Equal("HP: 18", warrior.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void NewWarriorAttacksWithPotionDrunk()
    {
        var warrior = new Warrior();
        warrior.DrinkPotion();

        var wizard = new Wizard();
        warrior.Attack(wizard);

        Assert.Equal("HP: 10", wizard.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void WizardPreparedSpellOnlyAffectsOneAttack()
    {
        var wizard = new Wizard();
        wizard.PrepareSpell();

        var warrior = new Warrior();
        wizard.Attack(warrior);
        wizard.Attack(warrior);

        Assert.Equal("HP: 15", warrior.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void WarriorPotionDrunkOnlyAffectsOneAttack()
    {
        var warrior = new Warrior();
        warrior.DrinkPotion();

        var wizard = new Wizard();
        warrior.Attack(wizard);
        warrior.Attack(wizard);

        Assert.Equal("HP: 4", wizard.ToString());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void MassiveBattle()
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

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void NewWarriorHasNotBeenStunned()
    {
        var warrior = new Warrior();
        Assert.False(warrior.Stunned());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void NewWizardHasNotBeenStunned()
    {
        var wizard = new Wizard();
        Assert.False(wizard.Stunned());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void WarriorThatHasBeenStunned()
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

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void WizardThatHasBeenStunned()
    {
        var wizard = new Wizard();

        var warrior = new Warrior();
        warrior.Attack(wizard);
        warrior.Attack(wizard);
        warrior.Attack(wizard);
        warrior.Attack(wizard);

        Assert.True(wizard.Stunned());
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void WarriorThatHasBeenStunnedDoesNotDoDamage()
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

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void WizardThatHasBeenStunnedDoesNotDoDamage()
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

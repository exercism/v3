import junit.framework.Assert;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

public class QuestLogicTest
{
    @Test
    public void cannot_execute_fast_attack_if_knight_is_awake()
    {
        var knightIsAwake = true;
        assertFalse(QuestLogic.canFastAttack(knightIsAwake));
    }

    @Test
    @Ignore
    public void can_execute_fast_attack_if_knight_is_sleeping()
    {
        var knightIsAwake = false;
        assertTrue(QuestLogic.canFastAttack(knightIsAwake));
    }

    @Test
    @Ignore
    public void cannot_spy_if_everyone_is_sleeping()
    {
        var knightIsAwake = false;
        var archerIsAwake = false;
        var prisonerIsAwake = false;
        assertFalse(QuestLogic.canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void can_spy_if_everyone_but_knight_is_sleeping()
    {
        var knightIsAwake = true;
        var archerIsAwake = false;
        var prisonerIsAwake = false;
        assertTrue(QuestLogic.canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void can_spy_if_everyone_but_archer_is_sleeping()
    {
        var knightIsAwake = false;
        var archerIsAwake = true;
        var prisonerIsAwake = false;
        assertTrue(QuestLogic.canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void can_spy_if_everyone_but_prisoner_is_sleeping()
    {
        var knightIsAwake = false;
        var archerIsAwake = false;


        var prisonerIsAwake = true;
        assertTrue(QuestLogic.canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void can_spy_if_only_knight_is_sleeping()
    {
        var knightIsAwake = false;
        var archerIsAwake = true;
        var prisonerIsAwake = true;
        assertTrue(QuestLogic.canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void can_spy_if_only_archer_is_sleeping()
    {
        var knightIsAwake = true;
        var archerIsAwake = false;
        var prisonerIsAwake = true;
        assertTrue(QuestLogic.canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void can_spy_if_only_prisoner_is_sleeping()
    {
        var knightIsAwake = true;
        var archerIsAwake = true;
        var prisonerIsAwake = false;
        assertTrue(QuestLogic.canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void can_spy_if_everyone_is_awake()
    {
        var knightIsAwake = true;
        var archerIsAwake = true;
        var prisonerIsAwake = true;
        assertTrue(QuestLogic.canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void can_signal_prisoner_ifarcher_is_sleeping_and_prisoner_is_awake()
    {
        var archerIsAwake = false;
        var prisonerIsAwake = true;
        assertTrue(QuestLogic.canSignalPrisoner(archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void cannot_signal_prisoner_ifarcher_is_awake_and_prisoner_is_sleeping()
    {
        var archerIsAwake = true;
        var prisonerIsAwake = false;
        assertFalse(QuestLogic.canSignalPrisoner(archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void cannot_signal_prisoner_ifarcher_and_prisoner_are_both_sleeping()
    {
        var archerIsAwake = false;
        var prisonerIsAwake = false;
        assertFalse(QuestLogic.canSignalPrisoner(archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void cannot_signal_prisoner_ifarcher_and_prisoner_are_both_awake()
    {
        var archerIsAwake = true;
        var prisonerIsAwake = true;
        assertFalse(QuestLogic.canSignalPrisoner(archerIsAwake, prisonerIsAwake));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_everyone_is_awake_and_pet_dog_is_present()
    {
        var knightIsAwake = true;
        var archerIsAwake = true;
        var prisonerIsAwake = true;
        var petDogIsPresent = true;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_everyone_is_awake_and_pet_dog_is_absent()
    {
        var knightIsAwake = true;
        var archerIsAwake = true;
        var prisonerIsAwake = true;
        var petDogIsPresent = false;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void can_release_prisoner_if_everyone_is_asleep_and_pet_dog_is_present()
    {
        var knightIsAwake = false;
        var archerIsAwake = false;
        var prisonerIsAwake = false;
        var petDogIsPresent = true;
        assertTrue(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_everyone_is_asleep_and_pet_dog_is_absent()
    {
        var knightIsAwake = false;
        var archerIsAwake = false;
        var prisonerIsAwake = false;
        var petDogIsPresent = false;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void can_release_prisoner_if_only_prisoner_is_awake_and_pet_dog_is_present()
    {
        var knightIsAwake = false;
        var archerIsAwake = false;
        var prisonerIsAwake = true;
        var petDogIsPresent = true;
        assertTrue(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void can_release_prisoner_if_only_prisoner_is_awake_and_pet_dog_is_absent()
    {
        var knightIsAwake = false;
        var archerIsAwake = false;
        var prisonerIsAwake = true;
        var petDogIsPresent = false;
        assertTrue(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_only_archer_is_awake_and_pet_dog_is_present()
    {
        var knightIsAwake = false;
        var archerIsAwake = true;
        var prisonerIsAwake = false;
        var petDogIsPresent = true;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_only_archer_is_awake_and_pet_dog_is_absent()
    {
        var knightIsAwake = false;
        var archerIsAwake = true;
        var prisonerIsAwake = false;
        var petDogIsPresent = false;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void can_release_prisoner_if_only_knight_is_awake_and_pet_dog_is_present()
    {
        var knightIsAwake = true;
        var archerIsAwake = false;
        var prisonerIsAwake = false;
        var petDogIsPresent = true;
        assertTrue(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_only_knight_is_awake_and_pet_dog_is_absent()
    {
        var knightIsAwake = true;
        var archerIsAwake = false;
        var prisonerIsAwake = false;
        var petDogIsPresent = false;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_only_knight_is_asleep_and_pet_dog_is_present()
    {
        var knightIsAwake = false;
        var archerIsAwake = true;
        var prisonerIsAwake = true;
        var petDogIsPresent = true;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_only_knight_is_asleep_and_pet_dog_is_absent()
    {
        var knightIsAwake = false;
        var archerIsAwake = true;
        var prisonerIsAwake = true;
        var petDogIsPresent = false;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void can_release_prisoner_if_only_archer_is_asleep_and_pet_dog_is_present()
    {
        var knightIsAwake = true;
        var archerIsAwake = false;
        var prisonerIsAwake = true;
        var petDogIsPresent = true;
        assertTrue(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_only_archer_is_asleep_and_pet_dog_is_absent()
    {
        var knightIsAwake = true;
        var archerIsAwake = false;
        var prisonerIsAwake = true;
        var petDogIsPresent = false;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test@Ignore
    public void cannot_release_prisoner_if_only_prisoner_is_asleep_and_pet_dog_is_present()
    {
        var knightIsAwake = true;
        var archerIsAwake = true;
        var prisonerIsAwake = false;
        var petDogIsPresent = true;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }

    @Test
    @Ignore
    public void cannot_release_prisoner_if_only_prisoner_is_asleep_and_pet_dog_is_absent()
    {
        var knightIsAwake = true;
        var archerIsAwake = true;
        var prisonerIsAwake = false;
        var petDogIsPresent = false;
        assertFalse(QuestLogic.canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent));
    }
}

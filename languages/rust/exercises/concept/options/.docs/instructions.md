You're working on implementing an RPG game. The player's character is represented by the following:

```rust
pub struct Player {
    health: u32,
    mana: Option<u32>,
}
```

Players in this game must reach level 10 before they unlock a mana pool so that they can start casting spells. Before that point, the Player's mana is `None`.

You're working on two pieces of functionality in this game, the revive mechanic and the spell casting mechanic. 

The `revive` method should check to ensure that the Player is indeed dead (their health has reached 0), and if they are, the method should return a new Player instance with 100 health. If the Player's level is 10 or above, they should also be revived with 100 mana. If they Player's level is below 10, their mana should be `None`. The `revive` method should preserve the Player's level. 
pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        match self.health {
            // Player is dead!
            0 => {
                if self.level >= 10 {
                    Some(Player { health: 100, mana: Some(100), level: self.level })
                } else {
                    Some(Player { health: 100, mana: None, level: self.level })
                }
            },
            // Player is alive!
            _ => None,
        }
    }

    pub fn cast_spell(&mut self, mana_cost: u32) {
        match self.mana {
            Some(m) => {
                if m >= manacost {
                    self.m -= manacost;
                } else {
                    println!("Not enough mana!");
                }
            }
            None => println!("Only wizards can cast spells!"),
        }
    }
}
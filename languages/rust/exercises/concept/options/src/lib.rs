pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
       unimplemented!() 
    }

    pub fn cast_spell(&mut self, cost: u32) {
        unimplemented!()
    }
}
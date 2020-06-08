pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
       unimplemented!() 
    }

    pub fn cast_spell(&mut self) {
        unimplemented!()
    }
}
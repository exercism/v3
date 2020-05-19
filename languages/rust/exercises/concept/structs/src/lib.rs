#[derive(Clone, Debug)]
pub struct User {
   name: String,
   age: u32,
   weight: f32, 
}

impl User {
    pub fn new(name: String, age: u32, weight: f32) -> Self {
        unimplemented!()
    }

    pub fn get_name(&self) -> &str {
        unimplemented!()
    }

    pub fn get_age(&self) -> u32 {
        unimplemented!()
    }

    pub fn get_weight(&self) -> f32 {
        unimplemented!()
    }

    pub fn update_name(&mut self, new_name: String) {
        unimplemented!()
    }

    pub fn update_age(&mut self, new_age: u32) {
        unimplemented!()
    }

    pub fn update_weight(&mut self, new_weight: f32) {
        unimplemented!()
    }
}
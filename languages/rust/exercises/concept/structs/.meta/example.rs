#[derive(Clone, Debug)]
pub struct User {
   name: String,
   age: u32,
   weight: f32, 
}

impl User {
    pub fn new(name: String, age: u32, weight: f32) -> Self {
        User {
            name,
            age,
            weight,
        }
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn get_age(&self) -> u32 {
        self.age
    }

    pub fn get_weight(&self) -> f32 {
        self.weight
    }

    pub fn update_name(&mut self, new_name: String) {
        self.name = new_name;
    }

    pub fn update_age(&mut self, new_age: u32) {
        self.age = new_age;
    }

    pub fn update_weight(&mut self, new_weight: f32) {
        self.weight = new_weight;
    }
}

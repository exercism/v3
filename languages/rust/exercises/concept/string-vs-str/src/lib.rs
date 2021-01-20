pub struct CSVBuilder {
    content: String
}

impl CSVBuilder {
    // Create a new builder
    pub fn new() -> Self {
        unimplemented!()
    }

    /// Adds an item to the list separated by a space and a comma.
    pub fn add(&mut self, val: &str) {
        unimplemented!()
    }

    /// Start a new line
    pub fn new_line(&mut self) {
        unimplemented!();
    }

    /// Consumes the builder and returns the comma separated list
    pub fn build(self) -> String {
        unimplemented!()
    }
}

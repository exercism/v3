/// vec-macro for creating empty vectors, and vectors of particular size, with vec!

/// create an empty vector
fn populate_empty() -> Vec<u8> {
    vec![]
}

/// create a vector initialized with a size of 3 and each slot being the same value
fn populate_filled(counter: u8) -> Vec<u8> {
    vec![counter; 3]
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_empty() {
        assert_eq!(populate_empty(), Vec::new());
    }
    #[test]
    fn test_filled() {
        assert_eq!(populate_filled(3), vec![3; 3]);
    }
}

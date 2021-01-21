/// vec-macro for creating empty vectors, and vectors of particular size, with vec!

/// Create an empty vector
fn create_empty() -> Vec<u8> {
    vec![]
}

/// Create a buffer of zeroes with `count` bytes capacity.
/// Applications use these for fill and modifying data
fn create_buffer(count: usize) -> Vec<u8> {
    vec![0; count]
}

/// Create a vector containing the first five digits of the fibonacci sequence
fn fibonacci() -> Vec<u8> {
    vec![1, 1, 2, 3, 5]
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_empty() {
        assert_eq!(create_empty(), Vec::new());
    }
    #[test]
    fn test_buffer() {
        let mut zeroized = Vec::new();
        zeroized.extend_from_slice(&[0, 0, 0]);
        assert_eq!(create_buffer(3), zeroized);
    }
    #[test]
    fn test_fibonacci() {
        let mut first_five = Vec::new();
        first_five.extend_from_slice(&[1, 1, 2, 3, 5]);

        assert_eq!(fibonacci(), first_five);
    }
}

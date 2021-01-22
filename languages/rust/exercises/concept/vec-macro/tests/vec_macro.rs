#[cfg(test)]
mod tests {
    use vec_macro::*;
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

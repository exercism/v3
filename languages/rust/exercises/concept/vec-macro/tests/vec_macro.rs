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
        let fibb = fibonacci();
        assert_eq!(fibb.len(), 5);
        for window in fibb.windows(3) {
            assert_eq!(window[0] + window[1], window[2]);
        }
    }
}

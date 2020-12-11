#[cfg(test)]
mod tests {
    use vec::{languages, languages_top};
    #[test]
    #[ignore]
    fn it_has_languages() {
        assert_eq!(languages().len(), 5);
    }
    #[test]
    #[ignore]
    fn has_top_count() {
        assert_eq!(languages_top(3).is_ok(), true);
    }
    #[test]
    #[ignore]
    fn has_error() {
        assert_eq!(languages_top(9).is_err(), true);
    }
    #[test]
    #[ignore]
    fn has_top_count_invalid() {
        assert_eq!(languages_top(3).unwrap().len(), 3);
    }
}

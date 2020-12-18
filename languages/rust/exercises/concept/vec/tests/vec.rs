#[cfg(test)]
mod tests {
    use vec::{grab, humans_and_computers, languages, languages_ranked};
    #[test]
    #[ignore]
    fn it_has_languages() {
        assert_eq!(languages().len(), 5);
    }
    #[test]
    #[ignore]
    fn has_ranked() {
        assert_eq!(&languages_ranked()[0], "1: Rust");
        assert_eq!(&languages_ranked()[1], "2: JavaScript");
        assert_eq!(&languages_ranked()[2], "3: Elixir");
        assert_eq!(&languages_ranked()[3], "4: Lua");
        assert_eq!(&languages_ranked()[4], "5: Scheme");
    }
    #[test]
    #[ignore]
    fn gets_fist() {
        assert_eq!(grab(0), "Rust");
    }
    #[test]
    #[ignore]
    fn gets_last() {
        assert_eq!(grab(4), "Scheme");
    }
    #[test]
    #[ignore]
    fn adds_human_languages() {
        let both = humans_and_computers(["官话".to_string(), "كِسوَهِل".to_string()].to_vec());
        assert_eq!(both[0], "Rust");
        assert_eq!(both[5], "官话");
        assert_eq!(both[6], "كِسوَهِل");
    }
}

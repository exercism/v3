/// return a list of 5 languages to learn
pub fn languages() -> Vec<String> {
    ["Elixir", "JavaScript", "Lua", "Rust", "Scheme"]
        .iter()
        .map(|a_str| a_str.to_string())
        .collect()
}
/// Gather your top languages to learn
pub fn languages_top(count: usize) -> Result<Vec<String>, String> {
    if count > languages().len() {
        Err("Please specify a number less than or equal to 5.".to_string())
    } else {
        Ok(languages()[..count].to_vec())
    }
}

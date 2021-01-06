/// return a list of 5 languages to learn
pub fn languages() -> Vec<String> {
    vec![
        "Rust".to_string(),
        "JavaScript".to_string(),
        "Elixir".to_string(),
        "Lua".to_string(),
        "Scheme".to_string(),
    ]
}
/// Gather your top languages to learn
pub fn grab(which: usize) -> Option<String> {
    if which == languages().len() {
        languages().pop()
    } else if let Some(lang) = languages().get(which) {
        Some(lang.to_string())
    } else {
        None
    }
}
/// Gather your top languages to learn
pub fn humans_and_computers(human: Vec<String>) -> Vec<String> {
    let mut list = languages();
    list.extend(human);
    list
}

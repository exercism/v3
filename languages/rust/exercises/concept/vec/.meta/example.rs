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
/// return a list of languages labeled by their order
pub fn languages_ranked() -> Vec<String> {
    languages()
        .iter()
        .enumerate()
        .map(|(index, lang)| format!("{}: {}", index + 1, lang))
        .collect()
}
/// Gather your top languages to learn
pub fn grab(which: usize) -> String {
    if which == languages().len() {
        languages().pop().unwrap()
    } else {
        languages()[which].to_string()
    }
}
/// Gather your top languages to learn
pub fn humans_and_computers(human: Vec<String>) -> Vec<String> {
    let mut list = languages();
    human.iter().for_each(|lang| list.push(lang.to_string()));
    list
}

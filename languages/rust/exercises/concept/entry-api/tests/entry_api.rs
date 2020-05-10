use entry_api::*;

#[test]
fn test_fn_returns_true_for_good_input() {
    let magazine = &["give", "me", "one", "grand", "today"];
    let note = &["give", "one", "grand", "today"];
    assert!(construct_note(magazine, note));
}

#[test]
#[ignore]
fn test_fn_returns_false_for_bad_input() {
    let magazine = &["I've", "got", "a", "lovely", "bunch", "of", "coconuts"];
    let note = &["I've", "got", "some", "coconuts"];
    assert!(!construct_note(magazine, note));
}

#[test]
#[ignore]
fn test_case_sensitivity() {
    let magazine = &["i've", "got", "some", "lovely", "coconuts"];
    let note = &["I've", "got", "some", "coconuts"];
    assert!(!construct_note(magazine, note));

    let magazine = &["I've", "got", "some", "lovely", "coconuts"];
    let note = &["i've", "got", "some", "coconuts"];
    assert!(!construct_note(magazine, note));
}
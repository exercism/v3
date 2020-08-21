const MESSAGES: (&str, &str, char, &str, &str, &str, &str) = (
    "Bright Sunday to you.",
    "Rainy Monday...",
    '\u{1f600}',
    "Bright Sunday to you.",
    "Bright Sunday to you.",
    "Bright Sunday to you.",
    "Bright Sunday to you.",
);
fn greeting(day: usize) -> String {
    let messages_char = match day {
        2 => Some(MESSAGES.2),
        _ => None,
    };
    let message = match day {
        0 => MESSAGES.0,
        1 => MESSAGES.1,
        _ => "Not today please",
    };

    if let Some(other) = messages_char {
        format!("{}", other)
    } else {
        message.to_string()
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn greeting_sun() {
        assert_eq!("Bright Sunday to you.".to_string(), greeting(0));
    }
    #[test]
    fn greeting_mon() {
        assert_eq!("Rainy Monday...".to_string(), greeting(1));
    }
    #[test]
    fn greeting_tues() {
        assert_eq!("😀".to_string(), greeting(2));
    }
}

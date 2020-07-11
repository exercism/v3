use std::fmt::Display;
const TODAYS_CHOICE: usize = 1;
/// get today's prizes
fn prizes() -> (usize, char, String, i32, bool, f32, ()) {
    (0, '🥚', "🥚🥚".to_string(), 0, false, 1.5, ())
}
/// handle an attendee's guess.
fn win_or_lose(guess: usize) -> Option<impl Display> {
    if guess == TODAYS_CHOICE {
        let winning = prizes();
        if guess == 0 {
            Some(winning.0)
        } else {
            Some(winning.1)
        }
    } else {
        None
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn get_prizes() {
        let list = prizes();
        assert_eq!(list.0, 0);
    }
}

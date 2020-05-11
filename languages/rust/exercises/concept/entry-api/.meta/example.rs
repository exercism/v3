use std::collections::HashMap;

pub fn construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let mut counts = HashMap::with_capacity(magazine.len());

    for m in magazine {
        counts.entry(*m)
            .and_modify(|v| *v += 1)
            .or_insert(1);
    }

    for n in note {
        counts.entry(*n)
            .and_modify(|v| *v -= 1)
            .or_insert(-1);

        if counts[n] < 0 {
            return false;
        }
    }

    true
}

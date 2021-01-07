use vec::{decode, encode};

fn main() {
    let strings_to_encode = [
        "Congrats, you looked deeper 🔍👁️",
        "Don't use this encoding for real:",
        "It's vulnerable to padding attacks in which",
        "an attacker adds arbitrary data after the message end.",
        "Also it's limited by u8 capacity.",
    ];

    let encoded = encode(
        strings_to_encode
            .iter()
            .map(|s| s.as_bytes().to_vec())
            .collect(),
    );
    println!("{:02x?}", encoded);

    let decoded = decode(encoded);
    for field in decoded {
        println!(
            "{}",
            String::from_utf8(field).expect("put valid utf8 in; should get it back out")
        );
    }
}

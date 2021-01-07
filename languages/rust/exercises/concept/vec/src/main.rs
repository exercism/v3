use vec::EncodableData;

fn main() {
    let strings_to_encode = [
        "Congrats, you looked deeper 🔍👁️",
        "Don't use this encoding for real:",
        "It's vulnerable to padding attacks in which",
        "an attacker adds arbitrary data after the message end.",
        "Also it's limited by u8 capacity.",
    ];

    let encodable = EncodableData {
        fields: strings_to_encode
            .iter()
            .map(|s| s.as_bytes().to_vec())
            .collect(),
    };

    let encoded = encodable.encode();
    println!("{:02x?}", encoded.bytes);
}

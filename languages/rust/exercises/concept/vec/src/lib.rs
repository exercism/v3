//! We're going to be working on a codec for a packed data format:
//!
//! Byte 0 contains the number of fields encoded
//! For f in 0..(num_fields):
//!     Byte 1 + (2 * f) contains the field's offset from the data start
//!     Byte 2 + (2 * f) contains the field's length
//! All subsequent bytes are part of the data fields
//!
//! Therefore, the shortest legal encoded data under this scheme is `00`.
//!
//! For example, let's say we have these fields:
//!
//! ```notrust
//! [
//!     [ba 61],
//!     [de ad be ef],
//!     [],
//!     [0e a7 f0 0d],
//! ]
//! ```
//!
//! This will be encoded into this:
//!
//! ```notrust
//! 04 09 02 0b 04 0f 00 0f 04 ba 61 de ad be ef 0e a7 f0 0d
//!  ^  ^  ^  ^     ^           ^     ^
//!  A  B  C  D     E           F     G
//! ```
//!
//! A. Number of fields.
//! B. Header start: offset of first data field. The header size is always
//!    `(fields.len() * 2) + 1`, so that's our initial data offset.
//! C. Length of first field.
//! D. Offset of second field: initial data offset + accumulated data length.
//! E. Offset of third field. Note that even though the length of this field
//!    is 0, we still use the appropriate place in the data section.
//! F. Start of data section. Data section is just all fields' data, concatenated.
//!    This is therefore just the start of the first data field.
//! G. Start of second data field.

pub fn encode(fields: Vec<Vec<u8>>) -> Vec<u8> {
    let mut offset = (fields.len() as u8 * 2) + 1;
    let data_size: usize = fields.iter().map(|field| field.len()).sum();
    let mut data = vec![0; offset as usize + data_size];
    data[0] = fields.len() as u8;
    let mut cursor = 1;

    for field in fields {
        data[cursor] = offset;
        data[cursor + 1] = field.len() as u8;
        data[offset as usize..offset as usize + field.len()].copy_from_slice(&field);
        offset += field.len() as u8;
        cursor += 2;
    }

    data
}

pub fn decode(data: Vec<u8>) -> Vec<Vec<u8>> {
    let n_fields = data[0] as usize;
    let mut fields = Vec::with_capacity(n_fields);

    for chunk in data[1..].chunks(2).take(n_fields) {
        let offset = chunk[0] as usize;
        let length = chunk[1] as usize;
        fields.push(data[offset..offset + length].to_vec());
    }

    fields
}

#[cfg(test)]
mod test {
    use super::*;
    use hex_literal::hex;

    #[test]
    fn encode_example() {
        let fields = vec![
            hex!("ba 61").to_vec(),
            hex!("de ad be ef").to_vec(),
            hex!("").to_vec(),
            hex!("0e a7 f0 0d").to_vec(),
        ];

        let encoded = encode(fields);

        assert_eq!(
            &encoded,
            &hex!("04 09 02 0b 04 0f 00 0f 04 ba 61 de ad be ef 0e a7 f0 0d")
        );
    }

    #[test]
    fn decode_example() {
        let encoded = hex!("04 09 02 0b 04 0f 00 0f 04 ba 61 de ad be ef 0e a7 f0 0d").to_vec();

        let expect = vec![
            hex!("ba 61").to_vec(),
            hex!("de ad be ef").to_vec(),
            hex!("").to_vec(),
            hex!("0e a7 f0 0d").to_vec(),
        ];

        let fields = decode(encoded);

        assert_eq!(fields, expect);
    }
}

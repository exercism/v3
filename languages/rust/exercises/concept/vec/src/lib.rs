/// This data structure contains a list of lists of bytes:
/// they're arbitrary binary blobs. Maybe you're implementing a
/// database, or an asset file for a video game, or something.
///
/// We're going to serialize this into a packed data format:
/// `EncodedData`. This format has two components, the header,
/// and the data.
///
/// The header is a list of pairs of bytes. Within each pair,
/// the first element is the offset within the data structure,
/// and the second element is the length of the field. The header
/// is terminated by a pair of null bytes. Therefore, the shortest
/// legal encoded data under this scheme is `00 00`.
///
/// For example, let's say `EncodableData` has these fields:
///
/// ```notrust
/// [
///     [ba 61],
///     [de ad be ef],
///     [],
///     [0e a7 f0 0d],
/// ]
/// ```
///
/// This will be encoded into this `EncodedData`:
///
/// ```notrust
/// 04 09 02 0b 04 0f 00 0f 04 ba 61 de ad be ef 0e a7 f0 0d
///  ^  ^  ^  ^     ^           ^     ^
///  A  B  C  D     E           F     G
/// ```
///
/// A. Number of fields.
/// B. Header start: offset of first data field. The header size is always
///    `(fields.len() * 2) + 1`, so that's our initial data offset.
/// C. Length of first field.
/// D. Offset of second field: initial data offset + accumulated data length.
/// E. Offset of third field. Note that even though the length of this field
///    is 0, we still use the appropriate place in the data section.
/// F. Start of data section. Data section is just all fields' data, concatenated.
///    This is therefore just the start of the first data field.
/// G. Start of second data field.
pub struct EncodableData {
    pub fields: Vec<Vec<u8>>,
}

impl EncodableData {
    pub fn encode(self) -> EncodedData {
        let fields = self.fields;
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

        EncodedData { bytes: data }
    }
}

/// EncodedData is filled with the contents of a compacted set of binary fields.
///
/// These fields follow a binary protocol as follows:
///
/// byte 0 contains the number of fields encoded
/// for f in 0..(num_fields):
///     byte 1 + (2 * f) contains the field's offset from the data start
///     byte 2 + (2 * f) contains the field's length
/// all subsequent bytes are part of the data fields
///
/// It can be decoded into an `EncodableData`
pub struct EncodedData {
    pub bytes: Vec<u8>,
}

impl EncodedData {
    pub fn decode(self) -> EncodableData {
        let data = self.bytes;
        let n_fields = data[0] as usize;
        let mut fields = Vec::with_capacity(n_fields);

        for chunk in data[1..].chunks(2).take(n_fields) {
            let offset = chunk[0] as usize;
            let length = chunk[1] as usize;
            fields.push(data[offset..offset + length].to_vec());
        }

        EncodableData { fields }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use hex_literal::hex;

    #[test]
    fn encode_example() {
        let encodeable = EncodableData {
            fields: vec![
                hex!("ba 61").to_vec(),
                hex!("de ad be ef").to_vec(),
                hex!("").to_vec(),
                hex!("0e a7 f0 0d").to_vec(),
            ],
        };

        let encoded = encodeable.encode();
        assert_eq!(
            &encoded.bytes,
            &hex!("04 09 02 0b 04 0f 00 0f 04 ba 61 de ad be ef 0e a7 f0 0d")
        );
    }

    #[test]
    fn decode_example() {
        let encoded = EncodedData {
            bytes: hex!("04 09 02 0b 04 0f 00 0f 04 ba 61 de ad be ef 0e a7 f0 0d").to_vec(),
        };

        let expect = vec![
            hex!("ba 61").to_vec(),
            hex!("de ad be ef").to_vec(),
            hex!("").to_vec(),
            hex!("0e a7 f0 0d").to_vec(),
        ];

        let encodeable = encoded.decode();

        assert_eq!(encodeable.fields, expect);
    }
}

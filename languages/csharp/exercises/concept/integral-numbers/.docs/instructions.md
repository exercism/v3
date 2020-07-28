Work continues on the remote control car project. Bandwidth in the telemetry system is at a premium and you have been asked to implement a message protocol for communicating telemetry data.

Data is transmitted in a buffer (byte array). When integers are sent, the size of the buffer is reduced by employing the following protocol.

Each value should be represented in the smallest possible integral type:

| Min Value                  | Max Value                 | Type   |
| -------------------------- | ------------------------- | ------ |
| 4,294,967,296              | 9,223,372,036,854,775,807 | long   |
| -9,223,372,036,854,775,808 | -2,147,483,649            | long   |
| 2,147,483,648              | 4,294,967,295             | uint   |
| 65,536                     | 2,147,483,647             | int    |
| -2,147,483,648             | -32,769                   | int    |
| 0                          | 65,535                    | ushort |
| -32,768                    | -1                        | short  |

The value should be converted to the appropriate number of bytes for its assigned type. The complete buffer comprises a byte indicating the number of additional byte in the buffer (_prefix byte_) followed by the bytes holding the integer (_payload bytes_).

If the payload bytes represent a signed type (short, int or long) then the prefix byte should contain the length as a negative number.

Only the prefix byte and the number of following bytes indicated by the prefix will be sent in the communication. Internally a 9 byte buffer is used (with trailing zeroes, as necessary) both by sending and receiving routines.

## 1. Encode an integral value ready to send

Please implement the static method `TelementryBuffer.ToBuffer()` to encode a buffer taking the parameter passed to the method.

```csharp
TelemetryBuffer.ToBuffer(5)
// => {2, 5, 0, 0, 0, 0, 0, 0, 0 };
TelemetryBuffer.ToBuffer(Int32.MaxValue)
// => {0xfc, 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0 };
```

## 2. Decode a received buffer

Please implement the static method `TelementryBuffer.FromBuffer()` to decode the buffer received and return the value in the form of a `long`.

```csharp
TelemetryBuffer.FromBuffer(new byte[] {0xfc, 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0 })
// => 2147483648
```

If the prefix byte is not shown in the _Type_ column in hte above table then 0 should be returned.

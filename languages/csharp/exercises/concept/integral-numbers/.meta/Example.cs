using System;

public static class TelemetryBuffer
{
    public static sbyte[] ToBuffer(long reading)
    {
        sbyte[] allBytes = new sbyte[9];

        byte[] bytes;
        if (reading > UInt32.MaxValue || reading < Int32.MinValue)
        {
            allBytes[0] = -8;
            bytes = BitConverter.GetBytes(reading);
            bytes.CopyTo(allBytes, 1);
        }
        else if (reading > Int32.MaxValue)
        {
            allBytes[0] = 4;
            bytes = BitConverter.GetBytes((uint)reading);
            bytes.CopyTo(allBytes, 1);
        }
        else if (reading > UInt16.MaxValue || reading < Int16.MinValue)
        {
            allBytes[0] = -4;
            bytes = BitConverter.GetBytes((int) reading);
            bytes.CopyTo(allBytes, 1);
        }
        else if (reading >= 0)
        {
            allBytes[0] = 2;
            bytes = BitConverter.GetBytes((ushort) reading);
            bytes.CopyTo(allBytes, 1);
        }
        else
        {
            allBytes[0] = -2;
            bytes = BitConverter.GetBytes((short) reading);
            bytes.CopyTo(allBytes, 1);
        }

        return allBytes;
    }

    public static long FromBuffer(byte[] buffer)
    {
        switch ((sbyte)buffer[0])
        {
            case -8:
                return BitConverter.ToInt64(buffer);
            case 4:
                return BitConverter.ToUInt32(buffer);
            case -4:
                return BitConverter.ToUInt32(buffer);
            case 2:
                return BitConverter.ToUInt16(buffer);
            case -2:
                return BitConverter.ToInt16(buffer);
            default:
                return 0;
        }
    }

}

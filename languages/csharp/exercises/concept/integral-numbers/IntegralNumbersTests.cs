using System;
using Xunit;

namespace IntegralNumbers
{
    public class TelemetryBufferTests
    {
        [Fact]
        public void ToBuffer_Zero()
        {
            var bytes = TelemetryBuffer.ToBuffer(0);
            Assert.Equal(new byte[] {2, 0, 0, 0, 0, 0, 0, 0, 0}, bytes);
        }

        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_Big()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int64.MaxValue);
            Assert.Equal(new byte[] {unchecked((byte)-8), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f }, bytes);
        }

        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_IntMax()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int32.MaxValue);
            Assert.Equal(new byte[] {unchecked((byte)-4), 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0 }, bytes);
        }

        [Fact /*(Skip = "Remove this Skip property to run this test")*/]
        public void FromBuffer_IntMax()
        {
            Assert.Equal(Int32.MaxValue,
                TelemetryBuffer.FromBuffer(new byte[] {unchecked((byte)-4), 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0 }));
        }

        [Fact /*(Skip = "Remove this Skip property to run this test")*/]
        public void FromBuffer_Invalid()
        {
            Assert.Equal(0,
                TelemetryBuffer.FromBuffer(new byte[] {22, 0xff, 0xff, 0xff, 0x7f, 0, 0, 0, 0 }));
        }

        // etc., etc.
    }
}

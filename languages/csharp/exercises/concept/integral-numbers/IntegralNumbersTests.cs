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
            Assert.Equal(new sbyte[] {0b0000_0010, 0b0000_0000, 0, 0, 0, 0, 0, 0, 0}, bytes);
        }

        [Fact/*(Skip = "Remove this Skip property to run this test")*/]
        public void ToBuffer_Big()
        {
            var bytes = TelemetryBuffer.ToBuffer(Int64.MaxValue);
            Assert.Equal(new sbyte[] {-8, -1, -1, -1, -1, -1, -1, -1, 0x7f }, bytes);
        }

        // etc., etc.
    }
}

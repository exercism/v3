using Xunit;
using System;
using System.Globalization;
using System.Reflection;
using System.Threading;
using Xunit.Sdk;

// TODO: convert Theory-based tests to Fact-based tests.
// This is necessary in order to be able to display the 
// input for which the test failed, which is defined in
// the .meta/config.json file
[UseCulture("en-US")]
public class AppointmentTest
{
    public static TheoryData<string, DateTime> ScheduleData = new TheoryData<string, DateTime>
    {
        { "7/25/2019 13:45:00", new DateTime(2019, 07, 25, 13, 45, 0) },
        { "June 3, 2019 11:30:00", new DateTime(2019, 6, 3, 11, 30, 0) },
        { "Thursday, December 5, 2019 09:00:00", new DateTime(2019, 12, 5, 9, 0, 0) },
        { "December 5, 2019 09:00:00", new DateTime(2019, 12, 5, 9, 0, 0) },
    };

    [Theory]
    [MemberData(nameof(ScheduleData))]
    public void Schedule(string appointmentDateDescription, DateTime expected) =>
        Assert.Equal(expected, Appointment.Schedule(appointmentDateDescription));

    public static TheoryData<DateTime, bool> HasPassedData = new TheoryData<DateTime, bool>
    {
        { DateTime.Now.AddYears(-1).AddHours(2), true },
        { DateTime.Now.AddMonths(-8), true },
        { DateTime.Now.AddMonths(-1), true },
        { DateTime.Now.AddDays(-5), true },
        { DateTime.Now.AddDays(-1), true },
        { DateTime.Now.AddHours(-2), true },
        { DateTime.Now.AddMinutes(-1), true },
        { DateTime.Now.AddMinutes(1), false },
        { DateTime.Now.AddDays(1), false },
        { DateTime.Now.AddDays(7), false },
        { DateTime.Now.AddMonths(3), false },
        { DateTime.Now.AddYears(3).AddDays(-5), false },
    };

    [Theory]
    [MemberData(nameof(HasPassedData))]
    public void HasPassed(DateTime appointmentDate, bool expected) =>
        Assert.Equal(expected, Appointment.HasPassed(appointmentDate));

    public static TheoryData<DateTime, bool> IsAfternoonAppointmentData = new TheoryData<DateTime, bool>
    {
        { new DateTime(2019, 6, 17, 8, 15, 0), false },
        { new DateTime(2019, 1, 11, 9, 0, 0), false },
        { new DateTime(2019, 2, 23, 11, 59, 59), false },
        { new DateTime(2019, 8, 9, 12, 0, 0), true },
        { new DateTime(2019, 8, 9, 12, 0, 1), true },
        { new DateTime(2019, 9, 7, 15, 45, 0), true },
        { new DateTime(2019, 9, 1, 17, 59, 59), true },
        { new DateTime(2019, 9, 1, 18, 0, 0), false },
        { new DateTime(2019, 9, 1, 23, 59, 59), false },
    };

    [Theory]
    [MemberData(nameof(IsAfternoonAppointmentData))]
    public void IsAfternoonAppointment(DateTime appointmentDate, bool expected) =>
        Assert.Equal(expected, Appointment.IsAfternoonAppointment(appointmentDate));

    public static TheoryData<DateTime, string> DescriptionData = new TheoryData<DateTime, string>
    {
        { new DateTime(2019, 03, 29, 15, 0, 0), "You have an appointment on Friday 29 March 2019 at 15:00." },
        { new DateTime(2019, 07, 25, 13, 45, 0), "You have an appointment on Thursday 25 July 2019 at 13:45." },
        { new DateTime(2019, 6, 3, 11, 30, 0), "You have an appointment on Monday 3 June 2019 at 11:30." },
        { new DateTime(2019, 12, 5, 9, 0, 1), "You have an appointment on Thursday 5 December 2019 at 09:00." },
        { new DateTime(2020, 2, 29, 15, 15, 20), "You have an appointment on Saturday 29 February 2020 at 15:15." },
        { new DateTime(2020, 9, 9, 9, 9, 9), "You have an appointment on Wednesday 9 September 2020 at 09:09." },
    };

    [Theory]
    [MemberData(nameof(DescriptionData))]
    public void Description(DateTime appointmentDate, string expected) =>
        Assert.Equal(expected, Appointment.Description(appointmentDate));

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Method)]
    private class UseCultureAttribute : BeforeAfterTestAttribute
    {
        private readonly CultureInfo _culture;
        private readonly CultureInfo _uiCulture;
        private CultureInfo _originalCulture;
        private CultureInfo _originalUiCulture;

        public UseCultureAttribute(string culture)
            : this(culture, culture) { }

        public UseCultureAttribute(string culture, string uiCulture)
        {
            _culture = new CultureInfo(culture, false);
            _uiCulture = new CultureInfo(uiCulture, false);
        }

        public override void Before(MethodInfo methodUnderTest)
        {
            _originalCulture = Thread.CurrentThread.CurrentCulture;
            _originalUiCulture = Thread.CurrentThread.CurrentUICulture;

            Thread.CurrentThread.CurrentCulture = _culture;
            Thread.CurrentThread.CurrentUICulture = _uiCulture;

            CultureInfo.CurrentCulture.ClearCachedData();
            CultureInfo.CurrentUICulture.ClearCachedData();
        }

        public override void After(MethodInfo methodUnderTest)
        {
            Thread.CurrentThread.CurrentCulture = _originalCulture;
            Thread.CurrentThread.CurrentUICulture = _originalUiCulture;

            CultureInfo.CurrentCulture.ClearCachedData();
            CultureInfo.CurrentUICulture.ClearCachedData();
        }
    }
}
module Datetimes

open System

let schedule (appointmentDateDescription: string): DateTime = DateTime.Parse(appointmentDateDescription)

let hasPassed (appointmentDate: DateTime): bool = appointmentDate < DateTime.Now

let isAfternoonAppointment (appointmentDate: DateTime): bool = appointmentDate.Hour >= 12 && appointmentDate.Hour < 18

let description (appointmentDate: DateTime): string = sprintf "You have an appointment on %O." appointmentDate

let anniversaryDate(): DateTime = DateTime(DateTime.Now.Year, 9, 15)

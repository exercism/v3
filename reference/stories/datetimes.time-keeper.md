# The Time-Keeper

This exercise is mostly about time durations but could be used for a general times exercise.

## Story

Sarah is a time-keeper. She's actually the best there is. When asked by kids what a time-keeper does, she loves to show them
a jar full of white sand. See, she says: "I'm keeping time safe!"

Her actual job is not that romantic but still a very interesting and fullfilling job to her. She flies to sport events
around the world with her bag full of timing equipement and measure the times of every athlete.

In this exercise you will write some functions for the new timing software Sarah has asked you to create for her. There
can't be any mistakes, or the next world record might not be valid if a bug is found in the software after! Are you ready
to bear the responsibility?

## Notes

Data coming from time keeping hardware is usually a time stamp (time, datetime or even with time zone). It has a high
precesion of usually up to 1 millisecond sometimes even 100 microseconds. Every hardware device has an internal clock
that must be synchronized to all the other devices.

## Tasks

These are example tasks that fit the time-keeper exercise:

- Take the difference of start and finish time to get the duration.
- One timing device is sitting in another time zone. Calculate a diff of 2 times with different time zones.
- One timing device has no internet connection. For the first 10 runners a helper is reading the recorded times to you on the phone.
  They get entered using the format "10h5m19.234s" which must be parsed.
- Early start was recorded by the backup video camera. Recalculate the new Start Time by subtracting a given duration
  from the incorrect Start time.
- During an event there are lots of stray times detected. To be able to ignore all those "unwanted" times, the time-keeper
  is able allow times from a timing device only in a certain timeframe after the start. E.g. between 10min to 30min after the start.
  Build a filter function with inputs `start time` (timestamp), `from` & `to` (durations), `detection` (timestamp).
- Format the output of a duration on the leader-board in the format "1h5m19.234s +4.843" where `+4.843` is the difference
  to the leader (athlete in first position).

## Implementations

- None yet

## Related

- [`types/datetime`][types-datetime]

[types-datetime]: ../types/datetime.md

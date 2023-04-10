%% Time predicates
:- module(myTime, [
    time/2, 
    earlier/2, 
    later/2, 
    sameTime/2,
    notSame/2,
    duration/3,
    add_minutes_to_time/3,
    hours_to_minutes/2,
    minutes_to_hours/2,
    day/1,
    next_day/2
]).

% Define time predicate with two arguments
% hour, minute
time(Hour, Minute) :-
    integer(Hour),
    integer(Minute),
    Hour >= 0,
    Hour < 24,
    Minute >= 0,
    Minute < 60.

% Check if time1 is earlier than time2
earlier(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 < Hour2;
    Hour1 =:= Hour2, Minute1 < Minute2.

% Check if time1 is later than time2
later(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 > Hour2;
    Hour1 =:= Hour2, Minute1 > Minute2.

sameTime(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 =:= Hour2, Minute1 =:= Minute2.

notSame(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 \= Hour2; Minute1 \= Minute2.

% Define duration predicate with three arguments
% start time, end time, duration whic his in minutes
duration(time(StartHour, StartMinute), time(EndHour, EndMinute), Duration) :-
    StartHour =< EndHour,
    Duration is (EndHour - StartHour) * 60 + (EndMinute - StartMinute).

% ONLY for durations that do not spill into the next day, TimeToAdd is in hours
add_minutes_to_time(time(Hours, Minutes), MinutesToAdd, ResultTime) :-
    TotalMinutes is Hours*60 + Minutes + MinutesToAdd,
    NewHours is TotalMinutes // 60,
    NewMinutes is TotalMinutes mod 60,
    ResultTime = time(NewHours, NewMinutes).

hours_to_minutes(Hours, Minutes) :-
    Minutes is round(Hours * 60).

minutes_to_hours(Minutes, Hours) :-
    Hours is (Minutes / 60).

% Define days of the week
day(monday).
day(tuesday).
day(wednesday).
day(thursday).
day(friday).
day(saturday).
day(sunday).

% Define predicate for next day
next_day(monday, tuesday).
next_day(tuesday, wednesday).
next_day(wednesday, thursday).
next_day(thursday, friday).
next_day(friday, saturday).
next_day(saturday, sunday).
next_day(sunday, monday).
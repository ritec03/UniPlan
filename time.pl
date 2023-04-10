%% Time predicates
:- module(myTime, [
    time/2,         % defines a function that takes two arguments
    earlier/2, 
    later/2, 
    sameTime/2,
    notSame/2,
    duration/3,
    add_duration_to_time/3,
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
% try:
% ?- time(00,00).
% true.
% ?- time(23,59).
% true.
% ?- time(24,00).
% false.
% ?- time(0,59).
% true.
% ?- time(0,60).
% false.

% Check if time1 is earlier than time2
earlier(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 < Hour2;
    Hour1 =:= Hour2, Minute1 < Minute2.
% try:
% ?- earlier(time(7,50), time(8,45)).
% true
% ?- earlier(time(8,30), time(8,45)). 
% true.
% ?- earlier(time(9,00), time(8,45)).
% false.

% Check if time1 is later than time2
later(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 > Hour2;
    Hour1 =:= Hour2, Minute1 > Minute2.

% try:
% ?- later(time(9,00), time(8,45)).
% true
% ?- later(time(8,00), time(8,45)).
% false.
% ?- later(time(13,00), time(13,00)).
% false.

sameTime(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 =:= Hour2, Minute1 =:= Minute2.

% try:
% ?- sameTime(time(00,00), time(00,00)).
% true.
% ?- sameTime(time(05,55), time(05,55)).
% true.
% ?- sameTime(time(4,30), time(5,30)).
% false.
% ?- sameTime(time(4,30), time(4,31)).
% false.
% ?- sameTime(time(3,59), time(4,00)).
% false.


notSame(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    \+ sameTime(time(Hour1, Minute1), time(Hour2, Minute2)).

% try:
% ?- notSame(time(00,00), time(00,00)).
% false.
% ?- notSame(time(05,55), time(05,55)).
% false.
% ?- notSame(time(4,30), time(5,30)).
% true.
% ?- notSame(time(4,30), time(4,31)).
% true.
% ?- notSame(time(3,59), time(4,00)).
% true.


% Define duration predicate with three arguments
% start time, end time, duration
duration(time(StartHour, StartMinute), time(EndHour, EndMinute), Duration) :-
    StartHour =< EndHour,
    Duration is (EndHour - StartHour) * 60 + (EndMinute - StartMinute).

% ONLY for durations that do not spill into the next day, TimeToAdd is in hours
add_duration_to_time(time(Hours, Minutes), TimeToAdd, ResultTime) :-
    hours_to_minutes(TimeToAdd, Duration),
    TotalMinutes is Hours*60 + Minutes + Duration,
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
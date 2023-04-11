:- module(print_schedule, [
    print_schedule/0
]).
:- use_module('activity_scheduler.pl').
:- use_module('time.pl').

% This module provides predicate to print the user's schedule
% stored in the database.

% print schedule
print_schedule :-
    Days = [monday, tuesday, wednesday, thursday, friday, saturday, sunday],
    print_schedule_by_day(Days).

print_schedule_by_day([]).
print_schedule_by_day([Day|Rest]) :-
    write(Day), write(':'), nl,
    print_activities_for_day(Day),
    nl,
    print_schedule_by_day(Rest).

print_activities_for_day(Day) :-
    main:activity(activityType(_, _, Course), Day, StartTime, EndTime),
    format('\t~w from ~w to ~w ~n', [Course, StartTime, EndTime]),
    fail.
print_activities_for_day(_).


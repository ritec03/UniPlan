:- module(print_schedule, [
    print_schedule/0
]).
:- use_module('project2.pl').
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
    duration(StartTime, EndTime, Duration),
    format('\t~w from ~w to ~w (~d minutes)~n', [Course, StartTime, EndTime, Duration]),
    fail.
print_activities_for_day(_).


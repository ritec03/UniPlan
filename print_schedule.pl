:- module(print_schedule, [
    print_schedule/0,
    print_activities_for_day/1
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
    findall(
        activity(ActivityType, Day, StartTime, EndTime),
        activity(ActivityType, Day, StartTime, EndTime), 
        Activities
    ),
    sort_activities(Activities, SortedActivities),
    print_activities(SortedActivities).

print_activities_for_day(_).

sort_activities(Activities, SortedActivities) :-
    map_list_to_pairs(get_start_time, Activities, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, SortedActivities).


get_start_time(activity(_, _, StartTime, _), TotalMinutes) :-
    StartTime = time(Hours, Minutes),
    TotalMinutes is Hours * 60 + Minutes.

print_activities([]).
print_activities([activity(activityType(Type, _, Name), _, time(HH,MM), time(HH1, MM1))|Rest]) :-
    (MM =:= 0 -> MM_Str = "00" ; true),
    (MM1 =:= 0 -> MM1_Str = "00" ; true),
    format('\t ~w:~w to ~w:~w -- Type: ~w, name: ~w~n', [HH, MM_Str, HH1, MM1_Str, Type, Name]),
    print_activities(Rest).


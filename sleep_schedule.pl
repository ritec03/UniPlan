:- module(sleep_schedule, [
    store_sleep_schedule/0
]).

:- use_module('time.pl').
:- use_module('activity_scheduler.pl').

% This module provides the functionality to schedule the user's sleep.
% THe user is prompted to provide the desired hours of sleep and the start
% of sleep and then schedules the sleep according to the user's input.

% add predicate to prohibit establishing sleep schedule the second time
store_sleep_schedule :-
    main:activity(activityType(sleep, _, _), _, _, _),
    write('You cannot add sleeping schedule again, as you already added it.').
% Predicate for storing user's course schedule
store_sleep_schedule :-
    write('Enter how many hours a day would you like to sleep.'), nl,
    read(AllocatedHours),
    write('Enter at what time would you like to go to bed. Example: time(22,30).'), nl,
    read(Bedtime),
    schedule_sleep(AllocatedHours, Bedtime),
    nl,
    write('Sleep schedule added successfully.'), nl.

% Predicate to schedule sleep
% schedule sleep based on allocated hours and bedtime
% Schedule the sleep activity based on allocated hours and bedtime
% This predicates schedules sleep within a single day or two
% consecutive days depending if the sleep crosses the day border

schedule_sleep(AllocatedHours, _) :-
    AllocatedHours > 12 -> 
    format('Maximum allocated time for sleep is 12 h. Aborting').

schedule_sleep(AllocatedHours, Bedtime) :-
    % if the bedtime is after midnight, just schedule sleep
    later(Bedtime, time(0,0)),
    earlier(Bedtime, time(7,0)),
    hours_to_minutes(AllocatedHours, AllocatedMinutes),
    add_minutes_to_time(Bedtime, AllocatedMinutes, Endtime),
    foreach(
        day(Day),
        main:assertz(activity(activityType(sleep, 0, sleep), Day, Bedtime, Endtime))
    ).

schedule_sleep(AllocatedHours, Bedtime) :-
    % if the bedtime is before midnight but the sleep fits, assign it
    (earlier(Bedtime, time(23,59)); sameTime(Bedtime, time(23,59))),
    (later(Bedtime, time(20,00)); sameTime(Bedtime, time(20,00))),
    % calculate amount of sleep before midnight
    duration(Bedtime, time(23,59), Duration),
    % if the duration is less, assign sleep
    hours_to_minutes(AllocatedHours, AllocatedMinutes),
    Duration > AllocatedMinutes,
    add_minutes_to_time(Bedtime, AllocatedMinutes, Endtime),
    foreach(
        day(Day),
        main:assertz(activity(activityType(sleep, 0, sleep), Day, Bedtime, Endtime))
    ).

schedule_sleep(AllocatedHours, Bedtime) :-
    % if the bedtime is before midnight but the sleep does not fit, split it
    (earlier(Bedtime, time(23,59)); sameTime(Bedtime, time(23,59))),
    (later(Bedtime, time(20,00)); sameTime(Bedtime, time(20,00))),
    % calculate amount of sleep before midnight
    duration(Bedtime, time(23,59), Duration),
    % if the duration is less, assign sleep
    Duration =< AllocatedHours * 60,
    hours_to_minutes(AllocatedHours, AllocatedMinutes),
    LeftoverMinutes is (AllocatedMinutes - Duration) - 1,
    add_minutes_to_time(time(0,0), LeftoverMinutes, Endtime),
    foreach(
        (day(Day), next_day(Day, NextDay)),
        (
            main:assertz(activity(activityType(sleep, 0, sleep), Day, Bedtime, time(23,59))),
            main:assertz(activity(activityType(sleep, 0, sleep), NextDay, time(0,0), Endtime))
        )
    ).
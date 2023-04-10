:- module(sleep_schedule, [
    store_sleep_schedule/0
]).

:- use_module('time.pl').
:- use_module('project2.pl').

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
    write('Course schedule added successfully.'), nl.

% Predicate to schedule sleep
% schedule sleep based on allocated hours and bedtime
% Schedule the sleep activity based on allocated hours and bedtime
% TODO request bedtime - should be between 8 and 3 am
% TODO request sleep duraiton - should be between 4 and 10.

schedule_sleep(AllocatedHours, Bedtime) :-
    % if the bedtime is after midnight, just schedule sleep
    later(Bedtime, time(0,0)),
    earlier(Bedtime, time(7,0)),
    add_duration_to_time(Bedtime, AllocatedHours, Endtime),
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
    Duration > AllocatedHours * 60,
    add_duration_to_time(Bedtime, AllocatedHours, Endtime),
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
    Duration =< AllocatedHours * 60,
    hours_to_minutes(AllocatedHours, AllocatedMinutes),
    Leftover is (AllocatedMinutes - Duration),
    minutes_to_hours(Leftover, Leftoverhours),
    add_duration_to_time(time(0,0), Leftoverhours, Endtime),
    foreach(
        (day(Day), next_day(Day, NextDay)),
        (
            main:assertz(activity(activityType(sleep, 0, sleep), Day, Bedtime, time(23,59))),
            main:assertz(activity(activityType(sleep, 0, sleep), NextDay, time(0,0), Endtime))
        )
    ).
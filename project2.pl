:- module(main, [
    overlaps_with_existing_activity/4,
    print_overlapping_activities/4,
    print_schedule/0
]).

:- use_module('time.pl').
:- use_module('course_schedule.pl').
:- use_module('activities.pl').

% Define activity predicate with four arguments
% course, week day, start time, end time
:- dynamic activity/4.
:- dynamic activityHoursTemp/3.
:- dynamic activityFull/2.

%% Schedule predicates

% Here we want to go through all the activities, and schedule the allocated time.
% we only allocate activities that are named (their name is not 0).
% for each activity, one can schedule them or skip
% to schedule each activity, ask what days of the week to allocate them to
% TODO
% then ask the starting time and activity duration.
% then if total weekly scheduled time exceeds the allocated time
% or if overlaps with existing activities are detected, reject
% if success, subtract time from the activity
% in the end, provide the schedule and the leftover times for activities.

% Now, when we allocate an activity, we want add temporary fact of the leftover time for this activity
% if the value of time gets below 0, then give an error of not sufficient allocated time.
% if the schedule_activities finishes, remove the temporary facts and update the allocated hours.

schedule_activities :-
    % go through all the activities
    forall(
        (activityHours(Type, Hours, AName), AName \= 0, \+ activityFull(Type, AName)),
        (
            format('The activity is ~w, of type ~w.~n', [AName, Type]),
            % ask the user whether to schedule them or not
            read_schedule_activity(Schedule),
            (Schedule \= []
            ->
                format('Scheduling activity for days: ~w.~n', [Schedule]),
                forall((member(Day, Schedule), \+ activityFull(Type, AName)),
                    (
                        format('Scheduling ~w for ~w`.~n', [AName, Day]),
                        % Read starting time and duration
                        read_start_time_and_duration(Start, Duration),
                        write('Finished reading duration and start time.'),
                        minutes_to_hours(Duration, DurationHours),
                        add_duration_to_time(Start, DurationHours, EndTime),
                        write('Finished calculating duration and times.'),
                        % Check for overlaps
                        (
                            \+ overlaps_with_existing_activity(AName, Day, Start, EndTime)
                            ->
                                % Add activity to the schedule
                                write('The activity does not overlap with anythin.'),
                                add_activity_to_schedule(Type, AName, Day, Start, EndTime, DurationHours),
                                write('After adding stuff to schedule.')
                            ;
                                format('Overlap detected. Skipping activity on ~w.~n', [Day])
                        )
                    )
                )
            ;
                format('Skipping activity.~n')
            )
        )
    ),
    % now, remove the temporary facts and update allocated hours.
    forall(
        (activityHoursTemp(TypeT, HoursT, ANameT), ANameT \= 0),
        (
            format('Updating fact activityHours(~w, ~w, ~w).~n', [TypeT, HoursT, ANameT]),
            retract(activityHours(TypeT, _, ANameT)),
            HoursT \= 0 -> assertz(activityHours(TypeT, HoursT, ANameT))
        )
    ),
    retract(activityHoursTemp(_,_,_)),
    retract(activityFull(_,_)).

read_schedule_activity(Schedule) :-
    write('Do you want to schedule this activity? (y/n) '),
    read(Answer),
    (Answer = y ->
        (
            write('Enter comma-separated list of days to schedule activity on: '),
            read(DayList),
            % validate the DayList
            (
                (is_list(DayList), foreach(member(Day, DayList), day(Day)))
                -> 
                Schedule = DayList % assign DayList to Schedule if it is a valid list
                ;
                format('Invalid input.~n'),
                Schedule = [] % assign an empty list to Schedule if the answer is 'no'
            )
        )
        ;
        Schedule = []
    ).

% gets Start as time(H,M) and Duration in minutes for the activity
read_start_time_and_duration(Start, Duration) :-
    write('Enter the starting time (HH:MM): '),
    read(Start),
    write('Enter the duration (in minutes): '),
    read(Duration).

add_activity_to_schedule(Type, AName, Day, Start, EndTime, DurationHours) :-
    % add temporary fact - find if those exist, or find activityHours
    write('Starting to add activity to schedule..~n'),
    activityHoursTemp(Type, Hours, AName) ->
        (   
            write('Starting condition.~n'),
            HoursLeft is Hours - DurationHours,
            HoursLeft >= 0 ->
                (
                    format('First condition, THe hours is positive.HoursLeft is ~w, the type is ~w, name is ~w', [HoursLeft, Type, AName]),
                    retract(activityHoursTemp(Type, Hours, AName)),
                    format('after retract'),
                    assertz(activityHoursTemp(Type, HoursLeft, AName)),
                    format('after assert'),
                    (
                        HoursLeft = 0 ->
                        write('Inner condition ~n'),
                        assertz(activityFull(Type, AName))
                        ;
                        true
                    ),
                    format('after inner condition'),
                    assertz(activity(activityType(Type, 0, AName), Day, Start, EndTime)),
                    format('affter assert'), !
                )
            ;
            format('Cannot schedule this activity as there is not enough allocated horus.'),
            % add temporary fact so that activity of this Name does not appear
            assertz(activityFull(Type, AName))
        )
        ;
        (
            activityHours(Type, Hours, AName),
            HoursLeft is Hours - DurationHours,
            HoursLeft >= 0 ->
                (
                    write('Second condition, THe hours is positive..~n'),
                    assertz(activityHoursTemp(Type, HoursLeft, AName)),
                    (
                        HoursLeft = 0 ->
                        write('Inner condition ~n'),
                        assertz(activityFull(Type, AName))
                        ;
                        true
                    ),
                    assertz(activity(activityType(Type, 0, AName), Day, Start, EndTime)), !
                )
            ;
            format('Cannot schedule this activity as there is not enough allocated horus.'),
            assertz(activityFull(Type, AName))
        ).
    

overlaps_with_existing_activity(_, Day, StartTime, EndTime) :-
    activity(_, Day, OtherStartTime, OtherEndTime),
    (
        (later(StartTime, OtherStartTime), earlier(StartTime, OtherEndTime));
        (sameTime(StartTime, OtherStartTime), notSame(StartTime, EndTime));
        (later(OtherStartTime, StartTime), earlier(OtherStartTime, EndTime));
        (notSame(StartTime, EndTime), sameTime(EndTime, OtherEndTime))
    ).

overlapping_activities(_, Day, StartTime, EndTime, OverlappingActivities) :-
    findall(activity(OtherCourse, Day, OtherStartTime, OtherEndTime),
        (
            activity(OtherCourse, Day, OtherStartTime, OtherEndTime),
            (
                (later(StartTime, OtherStartTime), earlier(StartTime, OtherEndTime));
                (sameTime(StartTime, OtherStartTime), notSame(StartTime, EndTime));
                (later(OtherStartTime, StartTime), earlier(OtherStartTime, EndTime));
                (notSame(StartTime, EndTime), sameTime(EndTime, OtherEndTime))
            )
        ),
        OverlappingActivities0),
    list_to_set(OverlappingActivities0, OverlappingActivities).

print_overlapping_activities(Course, Day, StartTime, EndTime) :-
    overlapping_activities(Course, Day, StartTime, EndTime, OverlappingActivities),
    length(OverlappingActivities, NumActivities),
    format('Cannot add activity ~w on ~w from ~w to ~w as it overlaps with ~w other activity:~n', [Course, Day, StartTime, EndTime, NumActivities]),
    print_activities(OverlappingActivities).

print_activities([]).
print_activities([activity(Course, Day, StartTime, EndTime)|Rest]) :-
    duration(StartTime, EndTime, Duration),
    format('\t~w on ~w from ~w to ~w (~w minutes)~n', [Course, Day, StartTime, EndTime, Duration]),
    print_activities(Rest).



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
    activity(activityType(_, _, Course), Day, StartTime, EndTime),
    duration(StartTime, EndTime, Duration),
    format('\t~w from ~w to ~w (~d minutes)~n', [Course, StartTime, EndTime, Duration]),
    fail.
print_activities_for_day(_).

% add predicate to prohibit establishing sleep schedule the second time
store_sleep_schedule :-
    activity(activityType(sleep, _, _), _, _, _),
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
        assertz(activity(activityType(sleep, 0, sleep), Day, Bedtime, Endtime))
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
        assertz(activity(activityType(sleep, 0, sleep), Day, Bedtime, Endtime))
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
            assertz(activity(activityType(sleep, 0, sleep), Day, Bedtime, time(23,59))),
            assertz(activity(activityType(sleep, 0, sleep), NextDay, time(0,0), Endtime))
        )
    ).
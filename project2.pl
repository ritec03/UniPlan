:- use_module('time.pl').
% :- use_module('course_schedule.pl').

% Define activity predicate with four arguments
% course, week day, start time, end time
:- dynamic activity/4.
:- dynamic activityType/3.

% TODO make sure to bring a message if a duplicate is found.
% Predicate for storing user's course schedule
store_course_schedule :-
    write('Enter your course schedule for each day of the week.'), nl,
    write('Example: [[cse101, monday, time(10,00), time(12,00)], [math201, tuesday, time(13,00), time(15,00)]].'), nl,
    read(Schedule),
    assert_course_schedule(Schedule),
    nl,
    write('Course schedule added successfully.'), nl.


assert_course_schedule([]).
assert_course_schedule([[Course, Day, StartTime, EndTime]|Rest]) :-
    time(StartTime),
    time(EndTime),
    member(Day, [monday, tuesday, wednesday, thursday, friday, saturday, sunday]),
    \+ overlaps_with_existing_activity(Course, Day, StartTime, EndTime),
    later(EndTime, StartTime),
    assertz(activity(activityType(course, 0, Course), Day, StartTime, EndTime)),
    assert_course_schedule(Rest).
assert_course_schedule([[Course, Day, StartTime, EndTime]|Rest]) :-
    overlaps_with_existing_activity(Course, Day, StartTime, EndTime),
    format('Cannot add activity ~w on ~w from ~w to ~w as it overlaps with the following activities:~n', [Course, Day, StartTime, EndTime]),
    print_overlapping_activities(Course, Day, StartTime, EndTime),
    assert_course_schedule(Rest).
assert_course_schedule([[_, _, _, _]|Rest]) :-
    assert_course_schedule(Rest).

% activityType(Type, Priority, Name)
activityType(Type, Priority, Name) :-
    member(Type, [course, sleep, homework, fitness, cooking]),
    integer(Priority),
    Priority >= 0,
    Priority =< 5,
    atom(Name),
    assertz(activityType(Type, Priority, Name)).

% activityHours(Type, Hours, Name)
activityHours(Type, HoursPerWeek, Name) :-
    member(Type, [course, sleep, homework, fitness, cooking]),
    float(HoursPerWeek),
    HoursPerWeek >= 0,
    HoursPerWeek =< 20,
    (Name == 0 ; atom(Name)),
    assertz(activityHours(Type, HoursPerWeek, Name)).

:- dynamic activityHours/3.

% Predicate for storing user's activity hours
store_activity_hours :-
    write('Enter your weekly activity hours.'), nl,
    write('Example: [[course, 14.5], [sleep, 56.0], [homework, 7.0], [fitness, 3.0], [cooking, 1.5]].'), nl,
    read(ActivityHours),
    assert_activity_hours(ActivityHours),
    nl,
    write('Activity hours added successfully.'), nl.

% the predicate is going to assign weekly hours to specific activity types
% for example, homework, fitness, cooking.
% if hours are assigned to the same activity type, then the hours are
% overwritten
assert_activity_hours([]).
assert_activity_hours([[Type, Hours]|Rest]) :-
    member(Type, [homework, fitness, cooking]),
    float(Hours),
    Hours >= 0.0,
    Hours =< 20.0,
    % remove previously allocated hours if exist
    (   activityHours(Type, _, _) ->
        retract(activityHours(Type, _, _))
    ;   true
    ),
    % add activity type if does not exist
    (
        (
            \+ activityType(Type, _, 0),
            assertz(activityType(Type, 0, 0))
        );
        true
    ),
    assertz(activityHours(Type, Hours, 0)),
    assert_activity_hours(Rest).
assert_activity_hours([_|Rest]) :-
    format('Invalid activity type or hours provided. Please try again.~n'),
    assert_activity_hours(Rest).

% Prompt user to input named activity hours
% Each activity is a list of [Type, Name, Hours]
% Checks that hours allocated to named activities does not exceed hours allocated to type
store_named_activity_hours :-
    write('Enter named activity hours.'), nl,
    write('Example: [[course, math, 5], [homework, "math hw1", 1.5]]'), nl,
    read(Activities),
    assert_named_activity_hours(Activities),
    nl,
    write('Named activity hours added successfully.'), nl.

assert_named_activity_hours([]).
assert_named_activity_hours([[Type, Name, Hours]|Rest]) :-
    activityType(Type, _, _),
    activityHours(Type, AllocatedHours, 0),
    % check that named activity hours don't exceed hours allocaTted to type
    named_activity_hours(Type, NamedHours),
    TotalHours is NamedHours + Hours,
    TotalHours =< AllocatedHours,
    assertz(activityHours(Type, Hours, Name)),
    assertz(activityType(Type, 0, Name)),
    assert_named_activity_hours(Rest).
assert_named_activity_hours([[Type, Name, Hours]|Rest]) :-
    \+ activityType(Type, _, _),
    format('Invalid activity type: ~w. Please try again.~n', [Type]),
    assert_named_activity_hours(Rest).
assert_named_activity_hours([[Type, Name, Hours]|Rest]) :-
    activityType(Type, _, _),
    activityHours(Type, AllocatedHours, 0),
    named_activity_hours(Type, NamedHours),
    TotalHours is NamedHours + Hours,
    TotalHours > AllocatedHours,
    format('Cannot add named activity ~w to ~w because it would exceed allocated hours.~n', [Name, Type]),
    assert_named_activity_hours(Rest).

% Retrieve the total number of hours for named activities of a specific type
named_activity_hours(Type, TotalHours) :-
    findall(Hours, (activityHours(Type, Hours, Name), Name \= 0), HoursList),
    sum_list(HoursList, TotalHours).
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

schedule_activities :-
    % go through all the activities
    forall(
        (activityHours(Type, Hours, AName), AName \= 0 ),
        (
            format('The activity is ~w, of type ~w.~n', [AName, Type]),
            % ask the user whether to schedule them or not
            read_schedule_activity(Schedule),
            (Schedule \= []
            ->
                format('Scheduling activity for days: ~w.~n', [Schedule]),
                forall(member(Day, Schedule),
                    (
                        % Read starting time and duration
                        read_start_time_and_duration(Start, Duration),
                        minutes_to_hours(Duration, DurationHours),
                        add_duration_to_time(Start, DurationHours, EndTime),
                        % Check for overlaps
                        (
                            \+ overlaps_with_existing_activity(AName, Day, Start, EndTime)
                            ->
                                % Add activity to the schedule
                                assertz(activity(activityType(Type, 0, AName), Day, Start, EndTime))
                            ;
                                format('Overlap detected. Skipping activity on ~w.~n', [Day])
                        )
                    )
                )
            ;
                format('Skipping activity.~n')
            )
        )
    ).

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
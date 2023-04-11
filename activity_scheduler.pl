:- module(main, [
    overlaps_with_existing_activity/4,
    print_overlapping_activities/4,
    activity/4,
    schedule_activities/0
]).

:- use_module('time.pl').
:- use_module('course_schedule.pl').
:- use_module('activities.pl').
:- use_module('sleep_schedule.pl').
:- use_module('print_schedule.pl').

% Define activity predicate with four arguments
% course, week day, start time, end time
:- dynamic activity/4.
:- dynamic activityHoursTemp/3.
:- dynamic activityFull/2.

% This module provides predicates for scheduling the allocated activities
% according to their activity hours.

%% Schedule predicates

% Here we want to go through all the activities, and schedule the allocated time.
% we only allocate activities that are named (their name is not 0).
% for each activity, one can schedule them or skip
% to schedule each activity, we ask what days of the week to allocate them to
% then ask the starting time and activity duration.
% then if total weekly scheduled time exceeds the allocated time
% or if overlaps with existing activities are detected, we reject
% if success, subtract time from the activity in the end,
% add to the schedule and compute the leftover times for activities.

% Now, when we allocate an activity, we want add temporary fact of the leftover time for this activity
% if the value of time gets below 0, then give an error of not sufficient allocated time.
% if the schedule_activities finishes, remove the temporary facts and update the allocated hours.

schedule_activities :-
    % go through all the activities
    forall(
        (activityHours(Type, _, AName), AName \= 0, \+ activityFull(Type, AName)),
        (
            format('The activity is ~w, of type ~w.~n', [AName, Type]),
            % ask the user whether to schedule them or not
            read_schedule_activity(Schedule),
            (Schedule \= []
            ->
                format('Scheduling activity for days: ~w.~n', [Schedule]),
                forall((member(Day, Schedule), \+ activityFull(Type, AName)),
                    (
                        format('~n~nScheduling ~w for ~w`.~n', [AName, Day]),
                        % Read starting time and duration
                        read_start_time_and_duration(Start, Duration),
                        add_minutes_to_time(Start, Duration, EndTime),
                        % TODO add a clause to prohibit crossing day
                        duration(Start, time(23,59), TimeToMidnight),
                        TimeToMidnight >= Duration ->
                        % Check for overlaps
                        (
                            \+ overlaps_with_existing_activity(AName, Day, Start, EndTime)
                            ->
                                % Add activity to the schedule
                                format('The activity does not overlap with anything.~n'),
                                add_activity_to_schedule(Type, AName, Day, Start, EndTime, Duration)
                            ;
                                format('Overlap detected. Skipping activity on ~w.~n', [Day])
                        )
                        ;
                        format('Scheduling activities that cross day boundary is currently not supported. 
                                Skipping activity on ~w. ~n', [Day])
                    )
                )
            ;
                format('Skipping activity.~n')
            )
        )
    ),
    % now, remove the temporary facts and update allocated hours.
    activityHoursTemp(TypeT, HoursT, ANameT) ->
    (
        forall(
            (activityHoursTemp(TypeT, HoursT, ANameT), ANameT \= 0),
            (
                format('Updating activityHours(~w, ~w, ~w).~n', [TypeT, HoursT, ANameT]),
                retract(activityHours(TypeT, _, ANameT)),
                \+ activityFull(TypeT, ANameT) -> assertz(activityHours(TypeT, HoursT, ANameT)) ; true
            )
        ),
        retract(activityHoursTemp(_,_,_)),
        retract(activityFull(_,_))
    )
    ;
    true.

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
    write('Enter the starting time: time(HH, MM).: '),
    read(Start),
    write('Enter the duration (in minutes): '),
    read(Duration).

add_activity_to_schedule(Type, AName, Day, Start, EndTime, DurationMinutes) :-
    % add temporary fact - find if those exist, or find activityHours
    format('Adding the activity to schedule.~n'),
    activityHoursTemp(Type, Hours, AName) ->
        (   
            MinutesLeft is (Hours * 60) - DurationMinutes,
            MinutesLeft >= 0 ->
                (
                    retract(activityHoursTemp(Type, Hours, AName)),
                    minutes_to_hours(MinutesLeft, HoursLeft),
                    assertz(activityHoursTemp(Type, HoursLeft, AName)),
                    (
                        MinutesLeft =< 1 ->
                        assertz(activityFull(Type, AName))
                        ;
                        true
                    ),
                    assertz(activity(activityType(Type, 0, AName), Day, Start, EndTime))
                )
            ;
            format('Cannot schedule this activity as there is not enough allocated hours.~n'),
            % add temporary fact so that activity of this Name does not appear
            assertz(activityFull(Type, AName))
        )
        ;
        (
            activityHours(Type, Hours, AName),
            MinutesLeft is (Hours * 60) - DurationMinutes,
            MinutesLeft >= 0 ->
                (
                    minutes_to_hours(MinutesLeft, HoursLeft),
                    assertz(activityHoursTemp(Type, HoursLeft, AName)),
                    (
                        MinutesLeft =< 1 ->
                        assertz(activityFull(Type, AName))
                        ;
                        true
                    ),
                    assertz(activity(activityType(Type, 0, AName), Day, Start, EndTime)), !
                )
            ;
            format('Cannot schedule this activity as there is not enough allocated hours.~n'),
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

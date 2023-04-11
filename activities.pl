:- module(activities, [
    activityType/3,
    activityHours/3,
    store_activity_hours/0,
    store_named_activity_hours/0
]).

:- use_module('time.pl').
:- use_module('activity_scheduler.pl').

:- dynamic activityType/3.

% activityType(Type, Priority, Name)
% Defines an activity type with a given priority and name, and stores it in the database
% Only allows activity types 'course', 'sleep', 'homework', 'fitness', 'cooking'
% Priority must be an integer between 0 and 5, while name must be an atom
activityType(Type, Priority, Name) :-
    member(Type, [course, sleep, homework, fitness, cooking]),
    integer(Priority),
    Priority >= 0,
    Priority =< 5,
    atom(Name),
    main:assertz(activityType(Type, Priority, Name)).

% activityHours(Type, HoursPerWeek, Name)
% Defines a clause with a given number of hours per week for
% a given activity type with name, and stores it in the database
% Only allows activity types 'course', 'sleep', 'homework', 'fitness', 'cooking'
% HoursPerWeek must be a float between 0 and 20
% Name must be an atom or 0 if hours apply to all activities of this type
activityHours(Type, HoursPerWeek, Name) :-
    member(Type, [course, sleep, homework, fitness, cooking]),
    float(HoursPerWeek),
    HoursPerWeek >= 0,
    HoursPerWeek =< 20,
    (Name == 0 ; atom(Name)),
    main:assertz(activityHours(Type, HoursPerWeek, Name)).

:- dynamic activityHours/3.

% Predicate for storing user's weekly activity hours
% Prompt the user to enter activity hours
% The input must be a list of [activityType, hours]
% Example: [[course, 14.5], [sleep, 56.0], [homework, 7.0], [fitness, 3.0], [cooking, 1.5]]
store_activity_hours :-
    write('Enter your weekly activity hours. Allowed activities are: homework, fitness and cooking.'), nl,
    write('Example: [[homework, 7.0], [fitness, 3.0], [cooking, 1.5]].'), nl,
    read(ActivityHours),
    assert_activity_hours(ActivityHours),
    nl,
    write('Activity hours added successfully.'), nl.

% Prompt the user to enter named activity hours
% The input must be a list of [activityType, name, hours]
% Example: [[course, math, 5], [homework, "math hw1", 1.5]]
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
        main:retract(activityHours(Type, _, _))
    ;   true
    ),
    % add activity type if does not exist
    (
        (
            \+ activityType(Type, _, 0),
            main:assertz(activityType(Type, 0, 0))
        );
        true
    ),
    main:assertz(activityHours(Type, Hours, 0)),
    assert_activity_hours(Rest).
assert_activity_hours([[Type, Hours]|Rest]) :-
    float(Hours),
    (Hours < 0.0; Hours > 20.0),
    \+ member(Type, [homework, fitness, cooking]),
    format('Invalid activity type and hours provided: ~w, ~w. Please try again.~n', [Type, Hours]),
    assert_activity_hours(Rest).
assert_activity_hours([[_, Hours]|Rest]) :-
    float(Hours),
    (Hours < 0.0; Hours > 20.0),
    format('Invalid activity hours provided: ~w. Please try again.~n', [Hours]),
    assert_activity_hours(Rest).
assert_activity_hours([[Type, _]|Rest]) :-
    format('Invalid activity type provided: ~w. Please try again.~n', [Type]),
    assert_activity_hours(Rest).

% Prompt user to input named activity hours - those activities which are not
% types of activities, like fitness, but specific activities within a type,
% like gym, swimming for fitness, or cs homework for homework.
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
    main:assertz(activityHours(Type, Hours, Name)),
    main:assertz(activityType(Type, 0, Name)),
    assert_named_activity_hours(Rest).
assert_named_activity_hours([[Type, _, _]|Rest]) :-
    \+ activityType(Type, _, _),
    format('Invalid activity type: ~w. Skipping the inpu "~w".~n', [Type, Type]),
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
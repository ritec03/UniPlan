:- module(course_schedule, [
    store_course_schedule/0
]).
:- use_module('time.pl').
:- use_module('project2.pl').


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
    main:assertz(activity(activityType(course, 0, Course), Day, StartTime, EndTime)),
    assert_course_schedule(Rest).
assert_course_schedule([[Course, Day, StartTime, EndTime]|Rest]) :-
    overlaps_with_existing_activity(Course, Day, StartTime, EndTime),
    format('Cannot add activity ~w on ~w from ~w to ~w as it overlaps with the following activities:~n', [Course, Day, StartTime, EndTime]),
    print_overlapping_activities(Course, Day, StartTime, EndTime),
    assert_course_schedule(Rest).
assert_course_schedule([[_, _, _, _]|Rest]) :-
    assert_course_schedule(Rest).
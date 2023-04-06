% Define activity predicate with four arguments
% course, week day, start time, end time
:- dynamic activity/4.
:- dynamic activityType/3.

% activityType(Type, Priority, Name)
activityType(Type, Priority, Name) :-
    member(Type, [course, sleep, homework, fitness, cooking]),
    integer(Priority),
    Priority >= 0,
    Priority =< 5,
    atom(Name),
    assertz(activityType(Type, Priority, Name)).


%% Time predicates

% Define time predicate with two arguments
% hour, minute
time(Hour, Minute) :-
    integer(Hour),
    integer(Minute),
    Hour >= 0,
    Hour < 24,
    Minute >= 0,
    Minute < 60.

% Check if time1 is earlier than time2
earlier(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 < Hour2;
    Hour1 =:= Hour2, Minute1 < Minute2.

% Check if time1 is later than time2
later(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 > Hour2;
    Hour1 =:= Hour2, Minute1 > Minute2.

sameTime(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 =:= Hour2, Minute1 =:= Minute2.

notSame(time(Hour1, Minute1), time(Hour2, Minute2)) :-
    Hour1 \= Hour2; Minute1 \= Minute2.

% Define duration predicate with three arguments
% start time, end time, duration
duration(time(StartHour, StartMinute), time(EndHour, EndMinute), Duration) :-
    StartHour =< EndHour,
    Duration is (EndHour - StartHour) * 60 + (EndMinute - StartMinute).

%% Schedule predicates

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

:- use_module('time.pl').
:- use_module('course_schedule.pl').
:- use_module('sleep_schedule.pl').
:- use_module('activities.pl').
:- use_module('activity_scheduler.pl').
:- use_module('print_schedule.pl').

% Define the main predicate
main :-
    format('~n~n~nWelcome to the course, sleep and activity scheduler!'),
    writeln('Please select an option:'),
    writeln('1. Add course schedule'),
    writeln('2. Add sleep schedule'),
    writeln('3. Allocate time for activity types'),
    writeln('4. Allocate time for specific activities'),
    writeln('5. Schedule activities'),
    writeln('6. Print schedule'),
    writeln('0. Quit'),
    read(Input),
    (
        Input =:= 1 -> store_course_schedule, main;
        Input =:= 2 -> store_sleep_schedule, main;
        Input =:= 3 -> store_activity_hours, main;
        Input =:= 4 -> store_named_activity_hours, main;
        Input =:= 5 -> schedule_activities, main;
        Input =:= 6 -> print_schedule, main;
        Input =:= 0 -> format('Goodbye!');
        writeln('Invalid input, please try again.'), main
    ).
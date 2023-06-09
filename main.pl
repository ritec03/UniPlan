:- use_module('time.pl').
:- use_module('course_schedule.pl').
:- use_module('sleep_schedule.pl').
:- use_module('activities.pl').
:- use_module('activity_scheduler.pl').
:- use_module('print_schedule.pl').

% reads user input, converts it to string, then tries to convert the input
% as a number, and if it fails, leaves the input as a string.
read_input(Input) :-
    read_line_to_codes(user_input, Codes),
    string_to_atom(Codes, Atom),
    (
        catch(number_chars(Input, Codes), _, fail) -> true ;
        Atom = Input
    ).


% welcome screen predicate
welcome :-
    nl,nl,nl,
    write('Welcome to the course, sleep and activity scheduler!'),
    nl,
    main.

% Define the main predicate
main :-
    nl,
    writeln('Please select an option (end with a dot \'.\')'),
    nl,
    write(' Cmd |  {command description}')                      ,nl,
    write('-----+------------------------------------------')   ,nl,
    write('  0  |  Quit')                                       ,nl,
    write('  1  |  Add course schedule')                        ,nl,
    write('  2  |  Add sleep schedule')                         ,nl,
    write('  3  |  Allocate time for activity types')           ,nl,
    write('  4  |  Allocate time for specific activities')      ,nl,
    write('  5  |  Schedule activities')                        ,nl,
    write('  6  |  Print schedule')                             ,nl,
    nl,
    read_input(Input),
    (
        Input == 0 -> format('Closing...~n'), halt;
        Input == 1 -> store_course_schedule, main;
        Input == 2 -> store_sleep_schedule, main;
        Input == 3 -> store_activity_hours, main;
        Input == 4 -> store_named_activity_hours, main;
        Input == 5 -> schedule_activities, main;
        Input == 6 ->  (
            get_time(T),
            format_time(atom(Time), '%Y-%m-%d %H:%M:%S', T),
            format('\n\tThis is the current schedule, generated on ~w\n', [Time]),
            print_schedule
        );
        writeln('Invalid input, please try again.')
    ),
    main.
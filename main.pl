:- use_module('project2.pl').
:- use_module('time.pl').
:- use_module('course_schedule.pl').
:- use_module('activities.pl').
:- use_module('sleep_schedule.pl').
:- use_module('print_schedule.pl').

welcome :-
    write('Welcome to the UniPlan application'), 
    nl,
    write('----------------------------------'), 
    nl,
    start_loop.

start_loop :-
    write('Type \'help\' for help'), 
    nl,
    write('Type \'exit\' to close the app.'), 
    nl,
    read_line(User_input),
    process_input(User_input).

start_silent :-
    read_line(User_input),
    process_input(User_input).

help :-
    nl,
    write(' command  |  {command description}')                                   ,nl,
    write('----------+-------------------------------------------------------')   ,nl,
    write(' studies  |  add your academic schedule')                              ,nl,
    write(' sleep    |  schedule your sleep hours')                               ,nl,
    write(' activity |  add the preferred hours for a pre-defined activity')      ,nl,
    write(' custom   |  add the preferred hours for a user-defined activity')     ,nl,
    write(' schedule |  schedule the pre-defined and user-defined activities')    ,nl,
    write(' print    |  print the current schedule we have so far')               ,nl,
    nl.

process_input(User_input) :-
    (
        string(User_input), User_input \= "" ->
            (
                User_input = "exit" -> write('Exiting...\n'), halt;
                User_input = "help" -> help;
                
                User_input = "studies" -> course_schedule:store_course_schedule
                ;
                User_input = "sleep" -> sleep_schedule:store_sleep_schedule
                ;
                User_input = "activity" -> activities:store_activity_hours
                ;
                User_input = "custom" -> activities:store_named_activity_hours
                ;
                User_input = "schedule" -> main:schedule_activities
                ;
                User_input = "print" -> (
                    get_time(T),
                    format_time(atom(Time), '%Y-%m-%d %H:%M:%S', T),
                    format('\n\tThis is the current schedule, generated on ~w\n', [Time]),
                    print_schedule:print_schedule
                );

                write('Unknown command. Please try again.\n')
            ),
            start_loop;
        start_silent
    ).

read_line(Line) :-
    read_line_to_string(user_input, Line).
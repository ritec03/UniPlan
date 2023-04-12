# UniPlan - A Student Scheduler
UniPlan is a command-line tool that helps students plan their weekly schedules by allocating time for different activity types in advance. UniPlan is designed to help students make the most out of their time by allowing them to prioritize their activities and ensure they have enough time for each task. The tool also helps students avoid conflicts by checking for overlaps between activities.

## Why is Scheduling Important for Students?
Students often have busy schedules that can make it challenging to manage their time effectively. Scheduling helps students organize their time, prioritize tasks, and stay on track to achieve their academic and personal goals. With UniPlan, students can plan their schedules in advance, ensuring they have enough time for coursework, extracurricular activities, social events, and personal time.


## Usage
To use UniPlan, follow these steps:

Download and install SWI-Prolog on your computer from https://www.swi-prolog.org/Download.html

Download the UniPlan project from GitHub.

Open a terminal or command prompt and navigate to the UniPlan directory

Start the program by typing 'main.' in the SWI-Prolog interpreter after consulting the 'main.pl' file.

Follow the prompts to enter your weekly activity hours, course schedule, and sleep schedule, then print the schedule in the interactive menu.

View a mock example of how creating a schedule works [here](https://docs.google.com/document/d/1hDUT8BO9lqEi9Iw0MJdScCAYDvkaVpXNfmxn3yjEM1k/edit?usp=sharing). View the google sheets [schedule](https://docs.google.com/spreadsheets/d/1wLUKylAaOnyRwICBiOVqotFNNKcS5MhxPGW6Q-tdP9A/edit?usp=sharing) that is made in the example.

## Functionality of the Program:
UniPlan has four main modules: activity_scheduler.pl, activities.pl, course_schedule.pl, and sleep_schedule.pl. 

The activity_scheduler.pl module provides the scheduling predicates that assign specific activities to time slots in the weekly schedule according to user's input.

The activities.pl module provides predicates for defining activity types and allocated weekly activity hours.

The course_schedule.pl module provides predicates for inputting course schedules.

The sleep_schedule.pl module provides predicates for inputting sleep schedules.

Together, these modules enable the user to input their activity, course, and sleep schedules, and generate a weekly schedule that maximizes productivity and balance.

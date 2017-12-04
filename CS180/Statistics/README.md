# CS 180 - Assignment 5
Build and work with a statistics software package.  Write functions that will compute the max, min, mean, median, and standard deviation of an array of values passed into the function.  These functions should be placed in a package that you name LASTNAME_Stats (replace LASTNAME with your actual last name.)

The specification file for the package is attached.  Change the name as described above and  create the package body file corresponding to the functions and types in the specification.

Then create a test program (called LASTNAME_5 as usual) that reads data from a file and calls each of the functions in your statistics package.

The data file represents grade data from different courses.  Your program should read this data into a gradebook.  The gradebook should use the following type declaration:
``` ada
            type Gradebook_Type is array(1..MAX_COURSES) of LASTNAME_Stats.Value_Type;
```
Declare the variable  `grades : Gradebook_Type`.

You will also need the following two type declarations:
``` ada
            type Name_Type is array(1..MAX_COURSES) of string(1..COURSE_NAME_LENGTH)

            type Num_Values_Type is array(1..MAX_COURSES) of integer;
```
You should declare a variable of each type, one (names) to hold the names of all the courses and the other (numValues) to hold the number of values recorded for each course.

Write a procedure readGrades that will take grades, names, and numValues as parameters and update them by reading the data file.  This is the only subprogram you need to write in the main program file.

The rest of your program should compute the various statistics for the courses and print them to the screen.  Your screen output should be formatted in a nice tabular form with regular column widths and such. For example, something like this (using example numbers)  

`Course nnn N = 12  Min = 24  Max = 99  Median = 84  Mean = 74.2 StDev = 8.4`

  Print the floats with one decimal precision and make your columns evenly spaced.

Stat notes:  The mean is the average and the median is the middle value.  The deviation of a data point is the difference between that point and the mean.  The standard deviation is the square root of the average of the squared deviations.  (You can use Ada’s square root function by including Ada.Numerics.Elementary_Functions and calling Sqrt, or you can write your own (if you’ve had calculus and studied Newton’s method, then writing your own is awesome and definitely the way to go).)

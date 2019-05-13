# Assignment for 3 May 2019

Last Modified: 24 April 2019

This assignment is due to be completed and submitted by start of class on Friday, 3 May.

I have written a branch and bound program that solves the job assignment problem as discussed in class on 24 April. (It uses the same Matrix class as before.) This program expects to read standard input formatted exactly like this:

```
n
x x x x
x x x x
x x x x
x x x x
```
where n is the number of rows (and columns), and all the x entries are positive integers, n rows of them, and n on each row. The rows are interpreted as workers and the columns represent jobs. An entry (w, j) in the matrix represents the cost to the company to have worker w perform job j.

For example, the 4-job, 4-worker input in the slides would appear as:

```
4
9 2 7 8
6 4 3 7
5 8 1 8
7 6 9 4
```    

The first part of the assignment is to run this program, without any changes whatsoever, multiple times with different inputs to observe how it performs, and then to craft the following input sets:

1. find a valid 4 x 4 input that causes the program to generate the minimum possible number of “considering” lines
2. find a valid 4 x 4 input that causes the program to generate the maximum possible number of “discarding” lines
3.  find a valid 4 x 4 input that causes the program to output at least two “new best solution” lines

The second part of the assignment is to modify this program to be more efficient in terms of basic operations. For this part, do not consider output statements per se to be basic operations.

Explain your work in both parts of this assignment in a LaTeX-derived PDF document. For the first part, show your input values (using the verbatim environment), state what results you got with them, and explain why they produce the results they did. For the second part, explain exactly what changes you made to the program and why the changes make the program more efficient. You do not necessarily have to do a formal analysis, but be precise and quantify the improvements you make in terms of basic operations. Explain how you observe the improved efficiency.

By the due date, use the homework submission page to submit the improved .cpp program that contains your changes, the .tex file of your analysis, and the .pdf of your analysis, and the three input .txt files from subparts 1, 2, and 3 of the first part of the assignment, respectively. 
# Assignment for 24 April 2019

Last Modified: 17 April 2019

This assignment is due to be completed and submitted by 2:00 pm Wednesday, 24 April.

Here is a backtracking program that is complete and runs when compiled with the same matrix class we have used previously. Compile this program and run it several times to see that it works and how it works.

Your assignment is to modify this program as follows.

1. In addition to the existing output, at the end of the program, output a line to cerr that reports the total number of nodes of the search tree examined during the program’s execution. Complete this first! Make sure you get the correct number.
2. Now, modify the ordering as we discussed in class so that at level k of the tree there is a fan-out of n − k rather than a fan-out of n. The number of nodes examined should go down! Again, make sure your program is counting the number of nodes correctly.
3. Modify the test function to be as efficient as possible. Update the header comments of the function and explain how your modified function works. You are encouraged to think outside the box. You may modify the arguments to the function if you wish. However, don’t be so “clever” that you slow the program down. You will be graded on the efficiency of your code and on how clean, clear, and understandable it is. Remember to follow the style guide!

Then, write a brief paper analyzing the efficiency of your test function. Analyze my function as it currently exists, analyze your test function after you improve it, and explain the improvement.

Do not discuss your algorithm, even in general terms, with anyone else. If you get any ideas at all from printed or online sources, other than from me or the class slides, you must cite your sources. This does not include simple C++ syntax lookups.

By the due date, use the homework submission page to submit the final .cpp program that contains your changes, the .tex file of your analysis, and the .pdf of your analysis paper. 
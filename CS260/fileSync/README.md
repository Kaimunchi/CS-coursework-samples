# Semester Project: FileSync

## Submission 1
In this project you'd be working on implementing a directory-sync tool. The purpose of this project is to understand Java programming with a problem statement perspective and implementation based what you have learned during the lectures: from beans to brew. :)

This project when completely implemented would be a multi-threaded application that will sync the contents of one of your directories on your local computer to a remote computer. So that if your computer crashes you will still have a back-up.

In Submission-1, you will implement a tool that will scan a directory on your computer, let's call it "home", find all the files and the directories inside home (including files inside sub-directories and so on). Then it would store this information like file-name, file-relative path, file size, file last modified and so on in a separate file stored at a different location than home. You are free (and encouraged) to store this information the way you think is useful. Remember later you might use this information to share with the remote computer.

Make sure to follow everything you have learned in Ch-2 & Ch-3 of your textbook including Javadoc comments and also JUnit test class.

You'd have to present a report (a pdf) that'll include (labeled as such):

1. Analysis of the problem

2. Design

3. Implementation (including screenshots of the sample run)

    a. Source Code

    b. Javadoc file

    c. JUnit Test class

What to submit:

1. The aforementioned report (pdf file)

2. A zip file containing all your source codes, drivers, JUnit test classes.

Deadline: 10/15/2018

Points: 20

## Submission 2
In this project you'd be working on implementing a directory-sync tool. The purpose of this project is to understand Java programming with a problem statement perspective and implementation based what you have learned during the lectures: from beans to brew. :)

This project when completely implemented would be a multi-threaded application that will sync the contents of one of your directories on your local computer to a remote computer. So that if your computer crashes you will still have a back-up.

In phase-1 you implemented a tool that will enumerate the contents of a directory like "home" and store it inside another file like a "file-info.txt". Based on last submission, your understanding of threads and networking now you'd be working on phase-2 (final phase) of this project.

In this final phase, you'd need two computers for demonstration purposes, one as a local computer and other as a remote computer. See below:

+ Using TCP protocol and two computers, one being local computer and other being a remote computer, replicate the "home" directory structure from the local computer onto the remote computer as "home-backup". Once you replicate the directory structure copy all the files from "home" to "home-backup" at their respective places/locations. 

+ The above step is the initial setup which once finished you have a complete backup for one of your directories. Remember that no part of this project would be hard-coded, meaning the remote computer would only learn about the directories and files using your TCP communication.

You'd have to present a report (a pdf) that'll include (labeled as such):

1. Analysis of the problem

2. Design

3. Implementation (including screenshots of the sample run)

    a. Source Code

    b. Javadoc file

    c. JUnit Test class

What to submit:

1. The aforementioned report (pdf file)

2. A zip file containing all your source codes, drivers, JUnit test classes.

Deadline: End of day, December 1 2018.

Points: 80 (Report 60 points + Demo 20 points)

Note: You can test your application using "localhost" but for demo you'd require two computers. You'd have to demonstrate/show me how your program is working on either December 4 or December 6. I will provide the schedule soon. I have merged your Submission-2 and Final Submission into this one report submission. There will not be any other requirement from the project perspective. 

Extra Credit: Extra 50% points of the project grade 

+ Update your tool to be dynamic, meaning for every subsequent times you run your code (after the first time, initial setup), the local computer would send the info about the directories and files at the "home" directory to the remote computer. The remote computer would make decisions which files are modified and would request only those files from the local computer to be uploaded. 

+ Use multithreaded application to upload each file to the remote computer. For example, assume "home" has 4 files as f1, f2, f3 and f4. Your application must use 4 threads to upload the files to the remote computer in parallel, so as to achieve minimum upload (or backup) time.

+ Make sure to include extra credit work in your report submission.

+ No extra time would be given for extra credit.
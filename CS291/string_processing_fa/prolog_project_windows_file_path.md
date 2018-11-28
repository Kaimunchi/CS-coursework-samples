---
puppeteer:
    displayHeaderFooter: true
    headerTemplate: "&nbsp;"
    footerTemplate: '<div style="width: 3.5%;"></div><div style="width: 45%; float:left; font-size: 10px; color: #ccc;">Page <span class="pageNumber"></span> of <span class="totalPages"></span></div><div style="width: 3%;"></div><div style="width: 45%; text-align: right; float:right; font-size: 10px; color:#ccc;">CS 291 Prolog Project: Windows File Path</strong></div><div style="width: 3.5%;"></div>'
---
# CS 291 Prolog Project
## Modeling Real-Life String Validation in Prolog: Windows File Path

### The Problem
Your challenge, should you choose to accept it, is to represent a real-life string validation scenario in Prolog. Your group is tasked with identifying syntacitcally valid absolute Windows File Paths, following the rules below.
+ Paths must begin with a single drive letter, either capital or lowercase.
+ The drive letter must be followed with a single `:`, then either a forward slash `/` or backslash `\`.
+ After this may be one or more directory names, followed by a file name, all of which may contain letters, numbers, spaces, and symbols **other than the following:**
  + `<` (less than)
  + `>` (greater than)
  + `:` (colon)
  + `|` (vertical bar or pipe)
  + `?` (question mark)
  + `*` (asterisk)
+ Paths *may* use a mix of forward- and backslashes.
+ Paths may never have two consecutive slashes of any type.
+ Paths and File Names may **not** end with a single period.

For example,  
+ Valid: `a:/test.exe`  
+ Valid: `b:\apple\hi.txt`
+ Invalid: `new`
+ Invalid: ` ` (empty string)

### Requirements
+ Name your prolog file `cs291_project_group_#.pl`, where # is your group number.
+ At the top of your prolog file, include comments with your group number, group members' names, and the situation you're trying to represent.
+ The entry point to your program should be the predicate `is_accepted/1`
  + This predicate will take a string as a list of individual characters (e.g. `[a,b,a,a,b,a]`)
  + The predicate should resolve to true if the string is accepted, and false otherwise.
+ You may create any other predicates and facts you may need.

### Example Input/Output
```prolog
is_accepted([a,:,/,t,e,s,t,.,e,x,e]). -> true.
is_accepted([b,:,\,a,p,p,l,e,\,h,i,.,t,x,t]). -> true.
is_accepted([]). -> false.
is_accepted([n,e,w]). -> false
```

### Helpful Predicates
The predicate `read_input/2` defined below can be used to print out a `Message` and store a line of input as a list of characters `Chars`. 
```prolog
read_input(Message, Chars) :- write(Message), read_line_to_codes(user_input, Codes), 
        name(Term, Codes), atom_chars(Term, Chars).
```

Disecting that, here is what each predicate does.
+ `write(Message)` prints out to the screen.
+ `read_line_to_codes(A,B)` reads from stream A (in this case standard input) and sets Codes to be a list of ASCII codes representing the next line of input.
+ `name(Term, Codes)` translates between the string Term and the list of ASCII codes Codes.
+ `atom_chars(Term, Chars)` translates between the string Term and the list of its component characters Chars.
  
### Testing
You will have to submit your prolog file for testing. You may submit your program for testing as many times as needed.

#### To Submit for Testing
1. Place the following directive at the beginning of your .pl file, replacing # with your group number. This allows it to be imported into the testing script. `:- module(cs291_project_group_#, [is_accepted/1]).`
2. Fill out and attach your Prolog file (with the module directive) to the form at [link removed]  
3. Please allow up to 24 hours for testing. If you haven't received a response after 24 hours, check your spam folder, then email me.

#### What to Expect in Return
The person who filled out the form will receive an email with a document containing the results of all previous test runs in addition to the latest one. This document is required as part of your final submission. Here is an example test run output:
```prolog
% PL-Unit: example .
ERROR: d:/documents/!trumandocs/2018-19/cs291/prolog/example_aut.plt:9:
        test mix_of_letters: failed

.. done
% 1 test failed
% 3 tests passed
false.
```
Your group will not be allowed to see the actual test code, but may get a hint at what is being tested by the names of the tests used.

### Questions?
Please feel free to email me at [email removed] with any questions or concerns you may have. I'm here to help you, so don't be afraid to ask.
---
puppeteer:
    displayHeaderFooter: true
    headerTemplate: "&nbsp;"
    footerTemplate: '<div style="width: 3.5%;"></div><div style="width: 45%; float:left; font-size: 10px; color: #ccc;">Page <span class="pageNumber"></span> of <span class="totalPages"></span></div><div style="width: 3%;"></div><div style="width: 45%; text-align: right; float:right; font-size: 10px; color:#ccc;">CS 291 Prolog Project: Ada Identifier</strong></div><div style="width: 3.5%;"></div>'
---
# CS 291 Prolog Project
## Modeling Real-Life String Validation in Prolog: Ada Identifier

### The Problem
Your challenge, should you choose to accept it, is to represent a real-life string validation scenario in Prolog. Your group is tasked with identifying valid Ada variable identifiers, following the rules below.
+ must start with a letter
+ can have `_`, but not consecutively
+ may also contain digits
+ no special characters are allowed
+ cannot end in an underscore
+ cannot match a reserved word
  + [abort, abs, abstract, accept, access, aliased, all, and, array, at, begin, body, case, constant, declare, delay, delta, digits, do, else, elsif, end, entry, exception, exit, for, function, generic, goto, if, in, interface, is, limited, loop, mod, new, not, null, of, or, others, out, overriding, package, pragma, private, procedure, protected, raise, range, record, rem, renames, requeue, return, reverse, select, separate, some, subtype, synchronized, tagged, task, terminate, then, type, until, use, when, while, with, xor]

For example,
+ Valid: `apple`
+ Valid: `test_var`
+ Invalid: ` ` (empty string)
+ Invalid: `new` (reserved word)

### Requirements
+ Find a regular expression for Ada identifiers (don't worry about excluding reserved words here).
+ Find a finite automaton representing that regular expression.
+ Represent that automaton (plus the rule of excluding reserved words) in prolog.
  + Name your prolog file `cs291_project_group_#.pl`, where # is your group number.
  + At the top of your prolog file, include comments with your group number, group members' names, and the regular expression you created.
  + The entry point to your program should be the predicate `is_accepted/1`
    + This predicate will take a string as a list of individual characters (e.g. `[a,b,a,a,b,a]`)
    + The predicate should resolve to true if the string is accepted, and false otherwise.
  + You may create any other predicates and facts you may need.

### Example Input/Output
```prolog
is_accepted([a,p,p,l,e]). -> true.
is_accepted([t,e,s,t,_,v,a,r]). -> true.
is_accepted([]). -> false.
is_accepted([n,e,w]). -> false. % This is a reserved word.
```

### Helpful Hints and Predicates
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

When dealing with checking for the underscore, the only method I found to work was to use `name/2` to convert your given atom to an ASCII code and check for that code to be 95 (which is the underscore). Otherwise, prolog appears to treat it as the anonymous variable.
  
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
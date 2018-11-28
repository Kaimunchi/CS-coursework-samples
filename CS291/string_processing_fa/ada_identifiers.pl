% Example Real-World String Processing
% Representing valid Ada Identifiers
% Regex: (letter) ((letter+digit) + (_ (letter+digit)))*
% Brandon Ingli
% 15 November 2018

:- module(ada_identifiers, [is_accepted/1]).

capitalLetters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).
lowercaseLetters(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).
reservedWords([abort, abs, abstract, accept, access, aliased, all, and, array, at, begin, body, case, constant, declare, delay, delta, digits, do, else, elsif, end, entry, exception, exit, for, function, generic, goto, if, in, interface, is, limited, loop, mod, new, not, null, of, or, others, out, overriding, package, pragma, private, procedure, protected, raise, range, record, rem, renames, requeue, return, reverse, select, separate, some, subtype, synchronized, tagged, task, terminate, then, type, until, use, when, while, with, xor]).
is_reserved_word(W) :- reservedWords(RW), member(W, RW), !.
is_capital_letter(L) :- capitalLetters(C), member(L, C), !.
is_lowercase_letter(L) :- lowercaseLetters(C), member(L, C), !.
is_letter(L) :- ( is_capital_letter(L); is_lowercase_letter(L) ), !.
is_number(N) :- member(N, ['0','1','2','3','4','5','6','7','8','9']), !.

is_underscore(C) :- name(C, [A]), A == 95.

is_accepted(S) :- atom_chars(Atom, S), not(is_reserved_word(Atom)), aut(0, S), !.

aut(0, [H|T]) :- is_letter(H), aut(1, T).

aut(1, []) :- !.
aut(1, [H|T]) :- (is_letter(H); is_number(H)), aut(1, T).
aut(1, [H|T]) :- is_underscore(H), aut(2, T).



aut(2, [H|T]) :- (is_letter(H); is_number(H)), aut(1, T).

read_input(Message, Chars) :- write(Message), read_line_to_codes(user_input, Codes), 
        name(Term, Codes), atom_chars(Term, Chars).

main() :- read_input(": ", Chars), is_accepted(Chars).
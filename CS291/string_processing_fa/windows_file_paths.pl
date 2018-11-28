% Example Real-World String Processing
% Representing valid Absolute Windows File Paths
% Regex: (letter) (:) ( \ + / ) ((not invalid char) + (. (not invalid char) ) + ((/+\) nic) + ((/+\) . nic))*
% Brandon Ingli
% 21 November 2018

:- module(windows_file_paths, [is_accepted/1]).

capitalLetters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).
lowercaseLetters(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).
invalidSymbols(['>', '<', ':', '|', '?', '*']).
is_capital_letter(L) :- capitalLetters(C), member(L, C), !.
is_lowercase_letter(L) :- lowercaseLetters(C), member(L, C), !.
is_letter(L) :- ( is_capital_letter(L); is_lowercase_letter(L) ), !.
is_invalid_symbol(S) :- invalidSymbols(I), member(S, I), !.

is_accepted(S) :- aut(0, S), !.

aut(0, [H|T]) :- is_letter(H), aut(1, T).

aut(1, [':'|T]) :- aut(2, T).

aut(2, ['/'|T]) :- aut(3, T).
aut(2, ['\\'|T]) :- aut(3, T).

aut(3, []) :- !.
aut(3, ['.'|T]) :- aut(5, T), !.
aut(3, [H|T]) :- not(H == '/'), not(H == '\\'), not(H == '.'), not(is_invalid_symbol(H)), aut(4, T).

aut(4, []) :- !.
aut(4, ['.'|T]) :- aut(5, T).
aut(4, ['/'|T]) :- aut(6, T).
aut(4, ['\\'|T]) :- aut(6, T).
aut(4, [H|T]) :- not(H == '/'), not(H == '\\'), not(H == '.'), not(is_invalid_symbol(H)), aut(4, T).

aut(5, ['/'|T]) :- aut(6, T).
aut(5, ['\\'|T]) :- aut(6, T).
aut(5, ['.'|T]) :- aut(5, T).
aut(5, [H|T]) :- not(H == '/'), not(H == '\\'), not(H == '.'), not(is_invalid_symbol(H)), aut(4, T).

aut(6, []) :- !.
aut(6, ['.'|T]) :- aut(5, T).
aut(6, [H|T]) :- not(H == '/'), not(H == '\\'), not(H == '.'), not(is_invalid_symbol(H)), aut(4, T).
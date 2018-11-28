% Example Real-World String Processing
% Representing valid Java Identifiers
% Regex: ($+lower+(_ _*($+lower+upper+digit))) ($+lower+upper+digit+(_ _* ($+lower+upper+digit)))*
% Brandon Ingli
% 15 November 2018

:- module(java_identifiers_aut, [is_accepted/1]).

capitalLetters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']).
lowercaseLetters(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).
reservedWords(['abstract', 'assert', 'boolean', 'break', 'byte', 'case', 'catch', 'char', 'class', 'const',
  'continue', 'default', 'double', 'do', 'else', 'enum', 'extends', 'false', 'final', 'finally', 'float',
  'for', 'goto', 'if', 'implements', 'import', 'instanceof', 'int', 'interface', 'long', 'native', 'new',
  'null', 'package', 'private', 'protected', 'public', 'return', 'short', 'static', 'strictfp', 'super',
  'switch', 'synchronized', 'this', 'throw', 'throws', 'transient', 'true', 'try', 'void', 'volatile',
  'while'] ).
is_reserved_word(W) :- reservedWords(RW), member(W, RW), !.
is_capital_letter(L) :- capitalLetters(C), member(L, C), !.
is_lowercase_letter(L) :- lowercaseLetters(C), member(L, C), !.
is_letter(L) :- ( is_capital_letter(L); is_lowercase_letter(L) ), !.
is_number(N) :- member(N, ['0','1','2','3','4','5','6','7','8','9']), !.

is_underscore(C) :- name(C, [A]), A == 95.

is_accepted(S) :- atom_chars(Atom, S), not(is_reserved_word(Atom)), aut(0, S), !.

aut(0, [H|T]) :- is_underscore(H), aut(2, T).
aut(0, [H|T]) :- (is_lowercase_letter(H); H == '$'), aut(1, T).

aut(1, []) :- !.
aut(1, [H|T]) :- (is_letter(H); is_number(H); H == '$'), aut(1, T).
aut(1, [H|T]) :- is_underscore(H), aut(2, T).

aut(2, [H|T]) :- (is_letter(H); is_number(H); H == '$'), aut(1, T).
aut(2, [H|T]) :- is_underscore(H), aut(2, T).

read_input(Message, Chars) :- write(Message), read_line_to_codes(user_input, Codes), 
        name(Term, Codes), atom_chars(Term, Chars).

main() :- read_input(": ", Chars), is_accepted(Chars).
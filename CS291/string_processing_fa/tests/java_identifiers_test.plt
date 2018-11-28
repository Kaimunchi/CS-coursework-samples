% Real-World String Processing PLUnit Test
% Representing valid Java Identifiers
% Brandon Ingli
% 15 November 2018

:- use_module(library(plunit)).
:- use_module("../java_identifiers_aut"). %fill in for each test

:- begin_tests(java_identifiers).

test(reserved_word_1, [fail]) :- atom_chars("new", Chars), is_accepted(Chars).
test(reserved_word_2, [fail]) :- atom_chars("import", Chars), is_accepted(Chars).
test(reserved_word_3, [fail]) :- atom_chars("if", Chars), is_accepted(Chars).
test(reserved_word_4, [fail]) :- atom_chars("throw", Chars), is_accepted(Chars).
test(reserved_word_5, [fail]) :- atom_chars("switch", Chars), is_accepted(Chars).

test(invalid_start_1, [fail]) :- atom_chars("Apple", Chars), is_accepted(Chars).
test(invalid_start_2, [fail]) :- atom_chars("*pple", Chars), is_accepted(Chars).
test(invalid_start_3, [fail]) :- atom_chars(".pple", Chars), is_accepted(Chars).
test(invalid_start_4, [fail]) :- atom_chars(" pple", Chars), is_accepted(Chars).
test(invalid_start_5, [fail]) :- atom_chars(";pple", Chars), is_accepted(Chars).

test(ends_in_underscore, [fail]) :- atom_chars("ends_in_undersCore_", Chars), is_accepted(Chars).

test(consec_underscore) :- atom_chars("two__underscores", Chars), is_accepted(Chars).

test(space, [fail]) :- atom_chars("has a space", Chars), is_accepted(Chars).

test(invalid_char_1, [fail]) :- atom_chars("this;", Chars), is_accepted(Chars).
test(invalid_char_2, [fail]) :- atom_chars("th*is", Chars), is_accepted(Chars).
test(invalid_char_3, [fail]) :- atom_chars("thi-;", Chars), is_accepted(Chars).
test(invalid_char_4, [fail]) :- atom_chars("t^is;", Chars), is_accepted(Chars).
test(invalid_char_5, [fail]) :- atom_chars("thi%s", Chars), is_accepted(Chars).

test(vaild_identifier_1) :- atom_chars("validIdentifierName", Chars), is_accepted(Chars).
test(vaild_identifier_2) :- atom_chars("another_valid_identifier", Chars), is_accepted(Chars).
test(vaild_identifier_3) :- atom_chars("$ThisRemindsMeOfBash", Chars), is_accepted(Chars).
test(vaild_identifier_4) :- atom_chars("iStHiSvaLiDSPoNgEBoB", Chars), is_accepted(Chars).
test(vaild_identifier_5) :- atom_chars("gottaMake$$", Chars), is_accepted(Chars).

:- end_tests(java_identifiers).

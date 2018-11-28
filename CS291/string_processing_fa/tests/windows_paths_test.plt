% Real-World String Processing PLUnit Test
% Representing valid Absolute Windows Paths
% Brandon Ingli
% 21 November 2018

:- use_module(library(plunit)).
:- use_module("../windows_file_paths"). %fill in for each test

:- begin_tests(windows_paths).

test(invalid_start_1, [fail]) :- atom_chars("1pple", Chars), is_accepted(Chars).
test(invalid_start_2, [fail]) :- atom_chars("*pple", Chars), is_accepted(Chars).
test(invalid_start_3, [fail]) :- atom_chars(".pple", Chars), is_accepted(Chars).
test(invalid_start_4, [fail]) :- atom_chars(" pple", Chars), is_accepted(Chars).
test(invalid_start_5, [fail]) :- atom_chars("_pple", Chars), is_accepted(Chars).

test(invalid_leader_1, [fail]) :- atom_chars("b:://", Chars), is_accepted(Chars).
test(invalid_leader_2, [fail]) :- atom_chars("b://test", Chars), is_accepted(Chars).
test(invalid_leader_3, [fail]) :- atom_chars(":/test", Chars), is_accepted(Chars).
test(invalid_leader_4, [fail]) :- atom_chars("/test", Chars), is_accepted(Chars).

test(invalid_char_1, [fail]) :- atom_chars("c:/t<st", Chars), is_accepted(Chars).
test(invalid_char_2, [fail]) :- atom_chars("C:/t>st", Chars), is_accepted(Chars).
test(invalid_char_3, [fail]) :- atom_chars("c:\\t:st", Chars), is_accepted(Chars).
test(invalid_char_4, [fail]) :- atom_chars("c:\\t|st", Chars), is_accepted(Chars).
test(invalid_char_5, [fail]) :- atom_chars("c:/t?st", Chars), is_accepted(Chars).
test(invalid_char_5, [fail]) :- atom_chars("c:/t*st", Chars), is_accepted(Chars).

test(end_in_period, [fail]) :- atom_chars("c:/ends.", Chars), is_accepted(Chars).
test(end_in_dbl_period, [fail]) :- atom_chars("c:/ends..", Chars), is_accepted(Chars).

test(valid_path_1) :- atom_chars("c:/valid\\valid.exe", Chars), is_accepted(Chars).
test(valid_path_2) :- atom_chars("c:/", Chars), is_accepted(Chars).
test(valid_path_3) :- atom_chars("c:\\valid\\$tuff", Chars), is_accepted(Chars).
test(valid_path_4) :- atom_chars("c:/+#!$ i5 v4lid.docx", Chars), is_accepted(Chars).
test(valid_path_5) :- atom_chars("c:/5points4u2.jar", Chars), is_accepted(Chars).
test(valid_path_6) :- atom_chars("c:/  multiple_things_correct.bin", Chars), is_accepted(Chars).
test(valid_path_7) :- atom_chars("c:/  multiple_things_correct.bin/", Chars), is_accepted(Chars).

:- end_tests(windows_paths).

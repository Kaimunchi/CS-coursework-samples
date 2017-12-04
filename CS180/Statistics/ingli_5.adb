with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Text_IO;
with Ingli_Stats;

procedure ingli_5 is
  MAX_COURSES : constant integer := 20;
  COURSE_NAME_LENGTH : constant integer := 6;
  FILE_NAME : constant string(1..10) := "grades.txt";

  type Gradebook_Type is array(1..MAX_COURSES) of Ingli_Stats.Values_Type;
  type Name_Type is array(1..MAX_COURSES) of string(1..COURSE_NAME_LENGTH);
  type Num_Values_Type is array(1..MAX_COURSES) of integer;

  gradebook : Gradebook_Type;
  names : Name_Type;
  numValues : Num_Values_Type;
  numCourses : integer;

  procedure readGrades(gradebook : out Gradebook_Type; names : out Name_Type; numValues : out Num_Values_Type; numCourses : out integer) is

    inFile : Ada.Text_IO.File_Type;
    courseCounter : integer := 1;
    gradeCounter : integer;
    temp : integer;

  begin
    Ada.Text_IO.Open(file=>inFile, mode=>Ada.Text_IO.In_File, name=>FILE_NAME);

    while not Ada.Text_IO.End_Of_File(inFile) loop

      Ada.Text_IO.Get_Line(file=>inFile, item=>names(courseCounter), last=>temp);

      gradeCounter := 0;

      while not Ada.Text_IO.End_Of_Line(inFile) loop
        gradeCounter := gradeCounter + 1;
        Ada.Integer_Text_IO.Get(file=>inFile, item=>gradebook(courseCounter)(gradeCounter));
      end loop;

      Ada.Text_IO.Skip_Line(file=>inFile);

      numValues(courseCounter) := gradeCounter;
      
      courseCounter := courseCounter + 1;

    end loop;

    Ada.Text_IO.Close(file=>inFile);

    numCourses := courseCounter - 1;

  end readGrades;

  procedure printStats(courseNumber : in integer) is
  begin
    Ada.Text_IO.Put(names(courseNumber)(1..COURSE_NAME_LENGTH) & " N = ");
    Ada.Integer_Text_IO.Put(item=>numValues(courseNumber), width=>2);

    Ada.Text_IO.Put("  Min = ");
    Ada.Integer_Text_IO.Put(item=>Ingli_Stats.min(gradebook(courseNumber), numValues(courseNumber)), width=>3);

    Ada.Text_IO.Put("  Max = ");
    Ada.Integer_Text_IO.Put(item=>Ingli_Stats.max(gradebook(courseNumber), numValues(courseNumber)), width=>3);

    Ada.Text_IO.Put("  Median = ");
    Ada.Integer_Text_IO.Put(item=>Ingli_Stats.median(gradebook(courseNumber), numValues(courseNumber)), width=>3);

    Ada.Text_IO.Put("  Mean = ");
    Ada.Float_Text_IO.Put(item=>Ingli_Stats.mean(gradebook(courseNumber), numValues(courseNumber)), fore=>2, aft=>1, exp=>0);

    Ada.Text_IO.Put("  StDev = ");
    Ada.Float_Text_IO.Put(item=>Ingli_Stats.standardDev(gradebook(courseNumber), numValues(courseNumber)), fore=>2, aft=>1, exp=>0);

    Ada.Text_IO.New_Line;

  end printStats;

begin
  readGrades(gradebook, names, numValues, numCourses);

  for i in 1..numCourses loop
    printStats(i);
  end loop;

end ingli_5;

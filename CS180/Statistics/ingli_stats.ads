--Statistics package specification file
--rename by replacing LASTNAME with your actual last name

package Ingli_Stats is

  MAX_VALUES : constant integer := 50;

  type Values_Type is array (1..MAX_VALUES) of integer;

  function min(V : in Values_Type; nValues : in integer) return integer;
  function max(V : in Values_Type; nValues : in integer) return integer;
  function sort(V: in Values_Type; nValues : in integer) return Values_Type;
  function median(V : in Values_Type; nValues : in integer) return integer;
  function mean(V : in Values_Type; nValues : in integer) return float;
  function standardDev(V : in Values_Type; nValues : in integer) return float;

end ingli_Stats;

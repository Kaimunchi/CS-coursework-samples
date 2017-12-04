--Specification file for the Town in an agent-based simulation
--Brandon Ingli, Oct 2017

limited with Ingli_Agents;

package Ingli_Town is

  ROWS : constant integer := 20;
  COLS : constant integer := 40;

  MAP_FILE_NAME : constant string(1..7) := "map.txt";
  WALL_CHAR : constant character := '*';

  type Map_Type is array (1..ROWS, 1..COLS) of character;

  procedure loadMap(M : out Map_Type);
  procedure updateMap(M: in out Map_Type; A: in out Ingli_Agents.Agent_Array);
  procedure printMap(M: in Map_Type);

end Ingli_Town;

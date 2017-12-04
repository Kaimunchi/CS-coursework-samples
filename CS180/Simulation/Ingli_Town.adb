--Definition file for the Town in an agent-based simulation
--Brandon Ingli, Oct 2017

with Ingli_Agents;
with Ada.Text_IO;

package body Ingli_Town is

  procedure loadMap(M: out Map_Type) is
    inFile : Ada.Text_IO.File_Type;

  begin
    Ada.Text_IO.Open(file=>inFile, mode=>Ada.Text_IO.In_File, name=>MAP_FILE_NAME);

    for y in 1..ROWS loop
      for x in 1..COLS loop
        Ada.Text_IO.Get(file=>inFile, item=>M(y,x));
      end loop;
    end loop;

    Ada.Text_IO.Close(file=>inFile);

  end loadMap;

  procedure updateMap(M: in out Map_Type; A: in out Ingli_Agents.Agent_Array) is
  begin
    for agent of A loop
      if not agent.active and agent.symbol /= ' ' then
        agent.symbol := ' ';
        M(agent.lastLocation.y, agent.lastLocation.x) := ' ';
        M(agent.location.y, agent.location.x) := ' ';
      end if;
    end loop;

    for agent of A loop
      if agent.active then
        M(agent.lastLocation.y, agent.lastLocation.x) := ' ';
        M(agent.location.y, agent.location.x) := agent.symbol;
      end if;
    end loop;
  end updateMap;

  procedure printMap(M: in Map_Type) is
  begin
    Ada.Text_IO.New_Line(Spacing=>5);

    for y in 1..ROWS loop
      for x in 1..COLS loop
        Ada.Text_IO.Put(m(y,x));
      end loop;

      Ada.Text_IO.New_Line;
    end loop;

    Ada.Text_IO.New_Line(Spacing=>5);
  end printMap;

end Ingli_Town;

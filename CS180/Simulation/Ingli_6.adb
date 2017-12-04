--Driver program for Agent Based simulation
--Brandon Ingli, Oct 2017

with Ingli_Agents;
with Ingli_Town;

procedure Ingli_6 is
  map : Ingli_Town.Map_Type;
  agents : Ingli_Agents.Agent_Array;
  iteration : integer := 0;
begin
  Ingli_Agents.intitializeLog;
  Ingli_Town.loadMap(map);
  Ingli_Agents.initializeAgents(agents, map);
  Ingli_Agents.updateLog(0, agents);
  Ingli_Town.updateMap(map, agents);

  Ingli_Town.printMap(map);
  delay duration(1);

  while not Ingli_Agents.endSim(agents) loop
    iteration := iteration + 1;

    Ingli_Agents.determineActions(agents);
    Ingli_Agents.processActions(agents, map);

    Ingli_Agents.processRoles(agents);

    Ingli_Agents.updateLog(iteration, agents);

    Ingli_Town.updateMap(map, agents);
    Ingli_Town.printMap(map);

    delay duration(1);
  end loop;

end Ingli_6;

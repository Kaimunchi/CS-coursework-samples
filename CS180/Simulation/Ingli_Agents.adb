--Definition file for the Agents in an agent-based simulation
--Brandon Ingli, Oct 2017

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ingli_Town;

package body Ingli_Agents is

  procedure determineActions(A: in out Agent_Array) is
    package Random_Action is new Ada.Numerics.Discrete_Random(Action_Type);
    actionGenerator : Random_Action.Generator;

  begin

    Random_Action.Reset(actionGenerator);

    for agent of A loop
      if agent.active then
        agent.action := Random_Action.Random(actionGenerator);
      end if;
    end loop;

  end determineActions;

  function canMove(agent: in Agent_Type; M: in Ingli_Town.Map_Type) return boolean is
  begin
    if agent.action = UP and agent.location.y > 2 then
      if M((agent.location.y - 1), agent.location.x) /= Ingli_Town.WALL_CHAR then
        return true;
      end if;

    elsif agent.action = DOWN and agent.location.y < Ingli_Town.ROWS-2 then
      if M((agent.location.y + 1), agent.location.x) /= Ingli_Town.WALL_CHAR then
        return true;
      end if;

    elsif agent.action = LEFT and agent.location.x > 2 then
      if M(agent.location.y, (agent.location.x - 1)) /= Ingli_Town.WALL_CHAR then
        return true;
      end if;

    elsif agent.action = RIGHT and agent.location.x < Ingli_Town.COLS-2 then
      if M(agent.location.y, (agent.location.x + 1)) /= Ingli_Town.WALL_CHAR then
        return true;
      end if;
    end if;

    return false;
  end canMove;

  procedure moveAgent(agent : in out Agent_Type) is
  begin
    case agent.action is
      when UP => agent.location.y := agent.location.y - 1;
      when DOWN => agent.location.y := agent.location.y + 1;
      when LEFT => agent.location.x := agent.location.x - 1;
      when RIGHT => agent.location.x := agent.location.x + 1;
      when others => null;
    end case;
  end moveAgent;

  procedure processActions(A: in out Agent_Array; M: in Ingli_Town.Map_Type) is
    distance : integer;
  begin
    for agent of A loop
      if agent.active then

        case agent.role is
          when KID    => distance := 2;
          when others => distance := 1;
        end case;

        agent.lastLocation := agent.location;

        for i in 1..distance loop
          if canMove(agent, M) then
            moveAgent(agent);
          end if;
        end loop;

      end if;
    end loop;
  end processActions;

  function isNextTo(currAgent : in Agent_Type; otherAgent : in Agent_Type; otherAgentRole : in Role_Type) return boolean is
  begin
    if abs(currAgent.location.x - otherAgent.location.x) <= 1 and currAgent.location.y = otherAgent.location.y and otherAgent.role = otherAgentRole then
      return true;

    elsif abs(currAgent.location.y - otherAgent.location.y) <= 1 and currAgent.location.x = otherAgent.location.x and otherAgent.role = otherAgentRole then
      return true;

    elsif abs(currAgent.location.x - otherAgent.location.x) <= 1 and abs(currAgent.location.x - otherAgent.location.x) <= 1 and otherAgent.role = otherAgentRole then
      return true;

    else
      return false;
    end if;
  end isNextTo;

  procedure processRoles(A: in out Agent_Array) is
    commonerCount : integer;

    package Random_Boolean is new Ada.Numerics.Discrete_Random(boolean);
    booleanGenerator : Random_Boolean.Generator;
    currentWins : boolean;

  begin
    for i in 1..POPULATION loop
      commonerCount := 0;

      for j in 1..POPULATION loop

        if A(i).active and A(j).active and i /= j then

          if A(i).role = COMMONER and isNextTo(A(i), A(j), MERCHANT) and A(i).coins > 0 then
            A(i).coins := A(i).coins - 1;
            A(j).coins := A(j).coins + 1;

          elsif A(i).role = THIEF and isNextTo(A(i), A(j), MERCHANT) then
            A(i).coins := A(i).coins + A(j).coins;
            A(j).coins := 0;

          elsif A(i).role = THIEF and isNextTo(A(i), A(j), THIEF) then

            Random_Boolean.Reset(booleanGenerator);
            currentWins := Random_Boolean.Random(booleanGenerator);

            if currentWins then
              A(i).coins := A(i).coins + A(j).coins;
              A(j).active := false;
            else
              A(j).coins := A(j).coins + A(i).coins;
              A(i).active := false;
            end if;

          elsif A(i).role = THIEF and isNextTo(A(i), A(j), COMMONER) then
            commonerCount := commonerCount + 1;
            if commonerCount >= 2 then
              A(i).active := false;
            end if;

          end if;
        end if;
      end loop;

      if A(i).role = COMMONER and A(i).active then
        A(i).coins := A(i).coins + 1;
      end if;

    end loop;
  end processRoles;

  procedure initializeAgents(A: in out Agent_Array; M: in Ingli_Town.Map_Type) is
    package Random_Role is new Ada.Numerics.Discrete_Random(Role_Type);

    floatGenerator : Ada.Numerics.Float_Random.Generator;
    roleGenerator : Random_Role.Generator;

    isAWall : boolean;

  begin

    Random_Role.Reset(roleGenerator);
    Ada.Numerics.Float_Random.Reset(floatGenerator);

    for agent of A loop

      isAWall := true;
      while isAWall loop
        agent.location.x := integer(Ada.Numerics.Float_Random.Random(floatGenerator) * float(Ingli_Town.COLS-3))+2;
        agent.location.y := integer(Ada.Numerics.Float_Random.Random(floatGenerator) * float(Ingli_Town.ROWS-3))+2;

        if M(agent.location.y, agent.location.x) = Ingli_Town.WALL_CHAR then
          isAWall := true;
        else
          isAWall := false;
        end if;

      end loop;

      agent.role := Random_Role.Random(roleGenerator);

      case agent.role is
        when COMMONER => agent.symbol := '#'; agent.coins := 2;
        when MERCHANT => agent.symbol := '$'; agent.coins := 10;
        when KID      => agent.symbol := 'K'; agent.coins := 0;
        WHEN THIEF    => agent.symbol := '%'; agent.coins := 0;
        when others   => agent.symbol := '?'; agent.coins := 0;
      end case;

      agent.lastLocation := agent.location;
      agent.active := true;
    end loop;

  end initializeAgents;

  function getNumActive(A: in Agent_Array) return Active_Total_Type is
    activeTotals : Active_Total_Type := (others=>0);
    index : integer;
  begin

    for agent of A loop
      index := Role_Type'Pos(agent.role) + 1;
      if agent.active then
        activeTotals(index) := activeTotals(index) + 1;
        activeTotals(NUM_ROLES + 1) := activeTotals(NUM_ROLES + 1) + 1;
      end if;
    end loop;

    return activeTotals;
  end getNumActive;

  function getGoldTotal(A: in Agent_Array) return Gold_Total_Type is
    goldTotals : Gold_Total_Type := (others=>0);
    index : integer;
  begin

    for agent of A loop
      index := Role_Type'Pos(agent.role) + 1;
      if agent.active then
        goldTotals(index) := goldTotals(index) + agent.coins;
        goldTotals(NUM_ROLES + 1) := goldTotals(NUM_ROLES + 1) + agent.coins;
      end if;
    end loop;

    return goldTotals;
  end getGoldTotal;

  procedure intitializeLog is
    logFile : Ada.Text_IO.File_Type;
  begin
    Ada.Text_IO.Create(file=>logFile, mode=>Ada.Text_IO.Out_File, name=>LOG_FILE_NAME);
    Ada.Text_IO.Put_Line(file=>logFile, item=>"Time,Commoners Active,Commoners Gold,Merchants Active,Merchants Gold,Kids Active,Kids Gold,Thieves Active,Thieves Gold,Total Gold");
    Ada.Text_IO.Close(logFile);
  end intitializeLog;

  procedure updateLog(N: in integer; A: in Agent_Array) is
    active : Active_Total_Type;
    gold : Gold_Total_Type;
    logFile : Ada.Text_IO.File_Type;
  begin
    Ada.Text_IO.Open(file=>logFile, mode=>Ada.Text_IO.Append_File, name=>LOG_FILE_NAME);

    Ada.Integer_Text_IO.Put(file=>logFile, item=>N, width=>0);
    Ada.Text_IO.Put(file=>logFile, item=>",");

    active := getNumActive(A);
    gold := getGoldTotal(A);

    for i in 1..NUM_ROLES loop
      Ada.Integer_Text_IO.Put(file=>logFile, item=>active(i), width=>0);
      Ada.Text_IO.Put(file=>logFile, item=>",");
      Ada.Integer_Text_IO.Put(file=>logFile, item=>gold(i), width=>0);
      Ada.Text_IO.Put(file=>logFile, item=>",");
    end loop;

    Ada.Integer_Text_IO.Put(file=>logFile, item=>gold(NUM_ROLES + 1), width=>0);
    Ada.Text_IO.New_Line(file=>logFile);

    Ada.Text_IO.Close(logFile);

  end updateLog;

  function endSim(A: in Agent_Array) return boolean is
    gold : Gold_Total_Type;
    active : Active_Total_Type;
  begin
    gold := getGoldTotal(A);
    active := getNumActive(A);

    for i in 1..NUM_ROLES loop
      if gold(i) >= 100 then
        return true;
      end if;
    end loop;

    if active(Role_Type'Pos(COMMONER) + 1) = 0 then
      return true;
    end if;

    return false;

  end endSim;

  procedure Put(A: in Agent_Array) is
  begin
    for agent of A loop
      Ada.Text_IO.Put("Role: ");
      Role_IO.Put(agent.role);
      Ada.Text_IO.Put("  Location: ");
      Ada.Integer_Text_IO.Put(width=>0, item=>agent.location.x);
      Ada.Text_IO.Put(",");
      Ada.Integer_Text_IO.Put(width=>0, item=>agent.location.y);
      Ada.Text_IO.Put("  Action: ");
      Action_IO.Put(agent.action);
      Ada.Text_IO.New_Line;
    end loop;
  end Put;

end Ingli_Agents;

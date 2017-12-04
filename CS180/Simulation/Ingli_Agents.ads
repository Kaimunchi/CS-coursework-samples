--Specification file for the Agents in an agent-based simulation
--Brandon Ingli, Oct 2017

limited with Ingli_Town;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

package Ingli_Agents is

  POPULATION : constant integer := 25;

  LOG_FILE_NAME : constant string(1..10) := "simlog.csv";

  type Location_Type is record
    x: integer;
    y: integer;
  end record;

  type Role_Type is (COMMONER, MERCHANT, KID, THIEF);
  NUM_ROLES : constant integer := 4;

  type Action_Type is (UP, DOWN, LEFT, RIGHT, WAIT);

  package Role_IO is new Ada.Text_IO.Enumeration_IO(Role_Type);
  package Action_IO is new Ada.Text_IO.Enumeration_IO(Action_Type);

  type Gold_Total_Type is array(1..(NUM_ROLES + 1)) of integer;
  type Active_Total_Type is array(1..(NUM_ROLES + 1)) of integer;

  type Agent_Type is record
    location : Location_Type;
    lastLocation : Location_Type;
    role : Role_Type;
    coins : integer;
    symbol : character;
    action: Action_Type;
    active: boolean;
  end record;

  type Agent_Array is array(1..POPULATION) of Agent_Type;

  procedure determineActions(A: in out Agent_Array);

  function canMove(agent: in Agent_Type; M: in Ingli_Town.Map_Type) return boolean;
  procedure moveAgent(agent : in out Agent_Type);
  procedure processActions(A: in out Agent_Array; M: in Ingli_Town.Map_Type);

  function isNextTo(currAgent : in Agent_Type; otherAgent : in Agent_Type; otherAgentRole : in Role_Type) return boolean;
  procedure processRoles(A: in out Agent_Array);

  procedure initializeAgents(A : in out Agent_Array; M: in Ingli_Town.Map_Type);

  function getNumActive(A: in Agent_Array) return Active_Total_Type;
  function getGoldTotal(A: in Agent_Array) return Gold_Total_Type;

  procedure intitializeLog;
  procedure updateLog(N: in integer; A: in Agent_Array);

  function endSim(A: in Agent_Array) return boolean;
  procedure Put(A: in Agent_Array);

end Ingli_Agents;

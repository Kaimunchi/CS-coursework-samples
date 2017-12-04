# CS 180 - Assignment 6
Design an agent-based simulator based in part on the map technology you developed in
Assignment 4.


Read in and display a map as you did before. This time, though, instead of having a character walk around the map, populate the map with “agents” who (randomly) interact and exchange currency (gold coins.)


You should develop two packages: LASTNAME_Town and LASTNAME_Agents. All the data and subprograms associated with the map itself should go in the Town package while everything dealing with the “people” should go in the Agents package. Part of this assignment is to have you experience making judgments regarding how to organize your code. You’ll need to create both the .ads and .adb files for these packages from scratch.


The town map is represented as a two-dimensional array of characters:

```
type Map_Type is array (1..ROWS, 1..COLS) of character;
```

Define constants ROWS and COLS (20 and 40, respectively) as we did in Assignment 4. The agents are represented as records with the following fields:
```
type Agent_Type is record
  location : Location_Type;
  lastLocation : Location_Type;
  role : Role_Type;
  coin : integer;
  symbol : character;
  action : Action_Type;
  stillActive : boolean;
end record;
```

The Location_Type describes x and y coordinates on the map:

```
type Location_Type is record
  x : integer;
  y : integer;
end record;
```
The Role_Type describes the role of the agent within the simulation:

```
type Role_Type is (COMMONER, MERCHANT, KID, THIEF);
```

The Action_Type describes the movement of the agent:

```
type Action_Type is (UP, DOWN, LEFT, RIGHT, WAIT);
```

Finally, we’re not interested in just one agent but rather in a town full of agents. We store all the
agents together in an array:

```
type Agent_Array is array (1 .. POPULATION) of Agent_Type;
```

Define constant POPULATION to default to 25, but it’s fun to change that to see its effects on the simulation.

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package body Ingli_Stats is

  function min(V: in Values_Type; nValues : in integer) return integer is
    minimum : integer;
  begin
    minimum := V(1);
    for i in 2..nValues loop
      if V(i) < minimum then
        minimum := V(i);
      end if;
    end loop;

    return minimum;
  end min;

  function max(V: in Values_Type; nValues : in integer) return integer is
    maximum : integer;
  begin
    maximum := V(1);
    for i in 2..nValues loop
      if V(i) > maximum then
        maximum := V(i);
      end if;
    end loop;

    return maximum;
  end max;

  function sort(V: in Values_Type; nValues : in integer) return Values_Type is
    temp : integer;
    sorted: boolean;
    toSort : Values_Type;
  begin
    toSort := V;

    sorted := false;
    while not sorted loop
      sorted := true;

      for i in 1..nValues-1 loop
        if(toSort(i) > toSort(i+1)) then
          sorted := false;
          temp := toSort(i);
          toSort(i) := toSort(i+1);
          toSort(i+1) := temp;
        end if;
      end loop;
    end loop;

    return toSort;
  end sort;

  function median(V : in Values_Type; nValues : in integer) return integer is
    sortedVals : Values_Type;
  begin
    sortedVals := sort(V, nValues);
    if nValues mod 2 = 0 then
      return (sortedVals(nValues/2) + sortedVals(nValues/2+1)) / 2;
    else
      return sortedVals(nValues/2+1);
    end if;

  end median;

  function mean(V : in Values_Type; nValues : in integer) return float is
    sum : integer := 0;
  begin
    for i in 1..nValues loop
      sum := sum + V(i);
    end loop;

    return Float(sum) / Float(nValues);

  end mean;

  function standardDev(V : in Values_Type; nValues : in integer) return float is
    sumOfSqDevs : float := 0.0;
    deviation : float;
    avg : float;
  begin
    avg := mean(V, nValues);

    for i in 1..nValues loop
      deviation := abs(avg - Float(V(i)));
      sumOfSqDevs := sumOfSqDevs + (deviation**2);
    end loop;

    return sqrt(sumOfSqDevs/Float(nValues));

  end standardDev;

end ingli_Stats;

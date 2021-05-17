--------------------------------------------------------------------------------
--  UMass , Physics Department
--  Guillermo Loustau de Linares
--  gloustau@cern.ch
--
--  Module: Multi purpose configurable VHDL memory
--  Description: 
--
--------------------------------------------------------------------------------
--  Revisions:
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;



package mpcvmem_pkg is
  
  function init_mem_width(m : integer; x : string) return integer;
  function init_mem_depth(m : integer; x : integer) return integer;
  function init_add_width(m : integer; x : integer) return integer;

end package mpcvmem_pkg;

package body mpcvmem_pkg is
  
  function init_mem_width(m : integer; x : string) return integer is
    variable y : integer;
    begin
    if x = "int" then
      y := m + 1;
    else
      y := m;
    end if;
    return y;
  end function;

  function init_mem_depth(m : integer; x : integer) return integer is
    variable y : integer;
    begin
    if m /= 0 then
      y := m;
    else
      y := 2**x;
    end if;
    return y;
  end function;

  function init_add_width(m : integer; x : integer) return integer is
    variable y : integer;
  begin
    if m /= 0 then
      y := integer(ceil(log2(real(m))));
    else
      y := x;
    end if;
    return y;
  end function;
  
end package body mpcvmem_pkg;
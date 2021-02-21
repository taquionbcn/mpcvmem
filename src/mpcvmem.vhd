--------------------------------------------------------------------------------
--  UMass , Physics Department
--  Guillermo Loustau de Linares
--  gloustau@cern.ch
--
--  Project: ATLAS L0MDT Trigger
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

entity mpcvmem is
  generic(
    g_SIMULATION        : std_logic := '0';
    g_LOGIC_TYPE        : string := "fifo"; -- fifo, pipeline, ram
    g_FIFO_TYPE         : string := "normal"; -- normal , read_ahead
    g_MEMORY_TYPE       : string := "auto"; -- auto, ultra, block, distributed
    --
    g_PIPELINE_IN_REGS  : natural := 0;
    g_PIPELINE_OUT_REGS : natural := 0;

    g_RAM_WIDTH         : natural := 64;
    g_RAM_DEPTH         : integer := 9600     -- maximum depth of the ram, also the maximum delay
  );
  port (
    clk               : in std_logic;
    rst               : in std_logic;
    ena               : in std_logic := '1';
    -- Write port
    i_wr              : in std_logic; -- in pipeline mode behaves as i_wr_data data valid
    i_wr_data         : in std_logic_vector(g_RAM_WIDTH - 1 downto 0);
    -- Read port
    i_rd              : in  std_logic;
    o_rd_data         : out std_logic_vector(g_RAM_WIDTH - 1 downto 0);
    o_rd_dv           : out std_logic;
    -- Flags
    o_empty           : out std_logic;
    o_empty_next      : out std_logic;
    o_full            : out std_logic;
    o_full_next       : out std_logic;
    -- used counter
    o_used            : out integer range g_RAM_DEPTH - 1 downto 0;
    -- The delay can be changed by the offset and resetting the module
    i_delay           : in integer range g_RAM_DEPTH - 1 downto 0 := g_RAM_DEPTH-1  
  );
end entity mpcvmem;

architecture beh of mpcvmem is
  
begin
  
  
  
  
end architecture beh;
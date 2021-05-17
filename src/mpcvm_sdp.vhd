--------------------------------------------------------------------------------
--  UMass , Physics Department
--  Guillermo Loustau de Linares
--  gloustau@cern.ch
--
--  Module: Dual Port memory
--  Description: 
--
--------------------------------------------------------------------------------
--  Revisions: v0.1 SDP
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library mpcvmem_lib;

entity mpcvm_sdp is
  generic(
    g_MEMORY_TYPE         : string := "distributed";
    -- g_MEMORY_STRUCTURE    : string := "SDP";
    -- g_ENABLE_SECOND_PORT  : std_logic := '0';
    g_OUT_PIPELINE        : integer := 2;

    g_RAM_WIDTH           : integer := 0;
    g_ADD_WIDTH           : integer := 0;
    g_RAM_DEPTH           : integer := 0
  );
  port (
    clk : in std_logic;
    rst : in std_logic;
    -- Port A
    ena         : in std_logic;
    i_addr_a    : in std_logic_vector(g_ADD_WIDTH-1 downto 0) := (others => '0');
    i_din_a     : in std_logic_vector(g_RAM_WIDTH-1 downto 0) := (others => '0');
    i_wr_nrd_a  : in  std_logic := '0';
    -- o_dout_a    : out std_logic_vector(g_RAM_WIDTH-1 downto 0);
    -- Port B
    -- ena_b       : in std_logic;
    i_addr_b    : in std_logic_vector(g_ADD_WIDTH-1 downto 0) := (others => '0');
    -- i_din_b     : in std_logic_vector(g_RAM_WIDTH-1 downto 0) := (others => '0');
    -- i_wr_nrd_b  : in  std_logic := '0';
    o_dout_b    : out std_logic_vector(g_RAM_WIDTH-1 downto 0)
  );
end entity mpcvm_sdp;

architecture beh of mpcvm_sdp is
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

  constant RAM_DEPTH : integer := init_mem_depth(g_RAM_DEPTH,g_ADD_WIDTH);

  type mem_ram_t is array (RAM_DEPTH - 1 downto 0) of std_logic_vector(g_RAM_WIDTH-1 downto 0);	
  signal mem : mem_ram_t := (others => (others => '0'));

  signal mem_reg : std_logic_vector(g_RAM_WIDTH-1 downto 0);

  attribute RAM_STYLE : string;
  attribute RAM_STYLE of mem : signal is "ultra";--g_MEMORY_TYPE;

  signal ena_pipes : std_logic_vector(g_OUT_PIPELINE downto 0);
  type data_pl_t is array (g_OUT_PIPELINE-1 downto 0) of std_logic_vector(g_RAM_WIDTH-1 downto 0);
  signal data_pipes : data_pl_t;

begin

  process(clk) begin
    if rising_edge(clk) then
      -- if rst = '1' then
      -- else
        if ena = '1' then
          if i_wr_nrd_a = '1' then
            mem(to_integer(unsigned(i_addr_a))) <= i_din_a;
          end if;
            mem_reg <= mem(to_integer(unsigned(i_addr_b)));
        end if;
      -- end if;
    end if;
  end process;
  -- The enable of the RAM goes through a pipeline to produce a
  -- series of pipelined enable signals required to control the data
  -- pipeline.
  process(clk)
  begin
    if rising_edge(clk) then
      ena_pipes(0) <= ena;
      for i in 0 to g_OUT_PIPELINE-1 loop
        ena_pipes(i+1) <= ena_pipes(i);
      end loop;
    end if;
  end process;

  -- RAM output data goes through a pipeline.
  process(clk)
  begin
    if rising_edge(clk) then
      if(ena_pipes(0) = '1') then
        data_pipes(0) <= mem_reg;
      end if;
      for i in 0 to g_OUT_PIPELINE-2 loop
        if(ena_pipes(i+1) = '1') then
          data_pipes(i+1) <= data_pipes(i);
        end if;
      end loop;
    end if;
  end process;

  o_dout_b <= data_pipes(g_OUT_PIPELINE-1);

end architecture beh;
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

library mpcvmem_lib;
use mpcvmem_lib.mpcvmem_pkg.all;

Library xpm;
use xpm.vcomponents.all;

entity mpcvm_pl_ctrl is
  generic(
    g_SIMULATION          : std_logic := '0'; -- deprecated( only was to force proper simulation in vivado)
    --
    -- g_LOGIC_TYPE          : string := "fifo"; -- fifo, pipeline, ram
    -- g_READ_MODE           : string := "normal"; -- normal , read_ahead
    -- g_MEMORY_TYPE         : string := "auto"; -- auto, ultra, block, distributed
    g_DV_TYPE             : string := "int"; -- int , ext
    -- -- MEMORY PARAMETERS
    -- g_MEMORY_STRUCTURE    : string := "SDP";
    -- g_SECOND_PORT         : string := "none"; -- none, normal, monitor
    -- FIFO 
    -- PIPELINE
    g_PL_DELAY_CYCLES     : integer := 0;
    -- RAM
    -- IN/OUT PARAMETERS
    -- g_PL_LATENCY          : natural := 0;
    -- MEMORY PARAMETERS
    g_ADD_WIDTH           : integer := 0;
    g_MEM_WIDTH           : integer := 0;
    g_MEM_DEPTH           : integer := 0     -- maximum depth of the ram, also the maximum delay
  );
  port (
    clk                   : in std_logic;
    rst                   : in std_logic;
    ena                   : in std_logic := '1';
    --
    i_freeze              : in std_logic := '0';
    i_ext_ctrl            : in std_logic := '0';
    -- Port A
    i_addr_a              : in  std_logic_vector(g_ADD_WIDTH-1 downto 0):= (others => '0');
    o_addr_a              : out std_logic_vector(g_ADD_WIDTH-1 downto 0):= (others => '0');
    -- Port B
    i_addr_b              : in  std_logic_vector(g_ADD_WIDTH-1 downto 0):= (others => '0');
    o_addr_b              : out std_logic_vector(g_ADD_WIDTH-1 downto 0):= (others => '0');
    -- Flags
    o_empty               : out std_logic;
    o_empty_next          : out std_logic;
    o_full                : out std_logic;
    o_full_next           : out std_logic;
    -- used counter
    o_used                : out integer 
  );
end entity mpcvm_pl_ctrl;

architecture beh of mpcvm_pl_ctrl is

  --------------------------------
  -- constants
  --------------------------------
  constant ADD_WIDTH : integer := init_add_width(g_MEM_DEPTH,g_ADD_WIDTH);--integer(ceil(log2(real(g_MEM_DEPTH))));
  constant MEM_DEPTH : integer := init_mem_depth(g_MEM_DEPTH,g_ADD_WIDTH);
  constant MEM_WIDTH : integer := init_mem_width(g_MEM_WIDTH,g_DV_TYPE);--g_MEM_WIDTH + 1;
  --------------------------------
  -- signals
  --------------------------------
  -- signal ena_a, ena_b : std_logic;
  -- signal ena_pipes_a : std_logic_vector(g_OUT_PIPELINE - 1 downto 0);
  -- signal ena_pipes_b : std_logic_vector(g_OUT_PIPELINE - 1 downto 0);
  -- type my_pipes is array (g_OUT_PIPELINE-1 downto 0) of std_logic_vector(MEM_WIDTH-1 downto 0);
  -- signal data_pipes_A : my_pipes;
  -- signal data_pipes_B : my_pipes;

  -- signal ENABLE_SECOND_PORT : integer;

  signal wr_index_aux : integer range 0 to MEM_DEPTH -1 := 0;
  signal rd_index_aux : integer range 0 to MEM_DEPTH -1 := 0;
  signal wr_index : integer range 0 to MEM_DEPTH -1 := 0;
  signal rd_index : integer range 0 to MEM_DEPTH -1 := 0;

  signal mem_addr_a : std_logic_vector(ADD_WIDTH-1 downto 0) := (others => '0');
  signal mem_addr_b : std_logic_vector(ADD_WIDTH-1 downto 0);
  signal mem_in_a : std_logic_vector(MEM_WIDTH - 1 downto 0);
  signal mem_in_b : std_logic_vector(MEM_WIDTH - 1 downto 0);
  signal mem_out_a : std_logic_vector(MEM_WIDTH - 1 downto 0);
  signal mem_out_b : std_logic_vector(MEM_WIDTH - 1 downto 0);

  signal used_data : integer range MEM_DEPTH - 1 downto 0 := 0;

  --------------------------------
  -- functions
  --------------------------------
  function get_read_index( 
    read_index : integer ;
    write_index : integer := 0;
    fi_delay : integer := 0
    ) return integer is
    variable o_rd_index : integer := 0;
    begin
    -- if g_LOGIC_TYPE = "fifo" then
    --   if read_index < MEM_DEPTH - 1 then
    --     o_rd_index := read_index + 1;
    --   else
    --     o_rd_index := 0;
    --   end if;
    -- elsif g_LOGIC_TYPE = "pipeline" then
      if write_index - fi_delay >= 0 then
        o_rd_index := write_index - fi_delay;
      else
        o_rd_index := (MEM_DEPTH - 1) - (fi_delay - 1)  + write_index;
      end if;
    -- else
    --   -- ERROR
    -- end if;
    return o_rd_index;

  end function;

  function get_write_index(write_index : integer; index_inc : integer := 1) return integer is
    variable o_wr_index_aux : integer := 0;
    begin
    if write_index < MEM_DEPTH - 1 then
      o_wr_index_aux := write_index + index_inc;
    else
      o_wr_index_aux := 0;
    end if;
    return o_wr_index_aux;
  end function;
  --------------------------------
  -- end functions
  --------------------------------
begin
  
  -- mem_in_b <= (others => '0');
  mem_addr_a <= std_logic_vector(to_unsigned( wr_index , ADD_WIDTH ));
  mem_addr_b <=std_logic_vector(to_unsigned( rd_index , ADD_WIDTH ));

  MEM_CTRL: process(clk) begin
    if rising_edge(clk) then
      if rst = '1' then
        -- mem <= (others => (others => '0'));
        -- mem_dv <= (others => '0');
        rd_index_aux <= get_read_index(rd_index_aux,wr_index_aux,g_MEM_DEPTH);
        wr_index_aux <= 0;
        o_empty       <= '1';
        o_empty_next  <= '1';
        o_full        <= '0';
        o_full_next   <= '0';
        used_data <= 0;
      else


        --------------------------------
        -- INPUT SIGNALS CTRL
        --------------------------------

        if used_data < 1 then
          o_empty       <= '1';
          o_empty_next  <= '1';
          o_full        <= '0';
          o_full_next   <= '0';
        elsif used_data < 2 then
          o_empty       <= '0';
          o_empty_next  <= '1';
          o_full        <= '0';
          o_full_next   <= '0';
        elsif used_data < g_MEM_DEPTH - 2  then
          o_empty       <= '0';
          o_empty_next  <= '0';
          o_full        <= '0';
          o_full_next   <= '1';
        elsif used_data < g_MEM_DEPTH - 1  then
          o_empty       <= '0';
          o_empty_next  <= '0';
          o_full        <= '1';
          o_full_next   <= '1';
        else
          --ERROR used more than space
        end if;

        --------------------------------
        -- INPUT SIGNALS CTRL
        --------------------------------

        --------------------------------
        -- index  CTRL
        --------------------------------
        wr_index_aux <= get_write_index(wr_index_aux,1);
        rd_index_aux <= get_read_index(rd_index_aux,wr_index_aux + 1,g_PL_DELAY_CYCLES);
        
        if i_freeze = '0' then -- normal
          wr_index <= get_write_index(wr_index_aux,0);
          rd_index <= get_read_index(rd_index_aux,wr_index_aux + 1,g_PL_DELAY_CYCLES);
        else -- external
          wr_index <= to_integer(unsigned(i_addr_b));
          rd_index <= to_integer(unsigned(i_addr_b));
        end if;


      end if;
    end if;
  end process MEM_CTRL;

end architecture beh;
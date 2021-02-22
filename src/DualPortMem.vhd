--------------------------------------------------------------------------------
--  UMass , Physics Department
--  Guillermo Loustau de Linares
--  gloustau@cern.ch
--
--  Module: Dual Port memory
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

entity DualPortMem is
  generic(
    g_MEMORY_TYPE         : string := "auto";
    g_ENABLE_SECOND_PORT  : std_logic := '0';
    g_RAM_WIDTH           : integer := 8;
    g_ADD_WIDTH           : integer := 32
  );
  port (
    clk : in std_logic;
    ena : in std_logic;
    -- Port A
    i_addr_a    : in std_logic_vector(g_ADD_WIDTH-1 downto 0);
    i_din_a     : in std_logic_vector(g_RAM_WIDTH-1 downto 0);
    i_wr_nrd_a  : in  std_logic;
    o_dout_a    : out std_logic_vector(g_RAM_WIDTH-1 downto 0);
    -- Port B
    i_addr_b    : in std_logic_vector(g_ADD_WIDTH-1 downto 0);
    i_din_b     : in std_logic_vector(g_RAM_WIDTH-1 downto 0);
    i_wr_nrd_b  : in  std_logic;
    o_dout_b    : out std_logic_vector(g_RAM_WIDTH-1 downto 0)
  );
end entity DualPortMem;

architecture beh of DualPortMem is
  type mem_ram_t is array (2**g_ADD_WIDTH-1 downto 0) of std_logic_vector(g_RAM_WIDTH-1 downto 0);	
  signal mem : mem_ram_t;

  attribute RAM_STYLE : string;
  attribute RAM_STYLE of mem : signal is g_MEMORY_TYPE;
begin
  RD_A: process(clk)
  begin
    if rising_edge(clk) then
      -- if rst_a = '1'' then
      -- else
        if ena = '1' then
          o_dout_a <= mem(to_integer(unsigned(i_addr_a)));
        end if;
      -- end if;
    end if;
  end process RD_A;

  WR_A: process(clk)
  begin
    if rising_edge(clk) then
      -- if rst = '1' then
      -- else
        if ena = '1' and i_wen_a = '1' then
          mem(to_integer(unsigned(i_addr_a))) <= i_din_a;
        end if;
      -- end if;
    end if;
  end process WR_A;

  -- SECOND_PORT: if g_ENABLE_SECOND_PORT = '1' generate
  
  --   RD_B: process(clk)
  --   begin
  --     if rising_edge(clk) then
  --       if rst = '1'' then
  --       else
  --         if ena = '1' then
  --           o_dout_b <= mem(to_integer(unsigned(i_addr_b)));
  --         end if;
  --       end if;
  --     end if;
  --   end process RD_B;

  --   WR_B: process(clk)
  --   begin
  --     if rising_edge(clk) then
  --       if rst = '1' then
  --       else
  --         if ena = '1' and i_wen_a = '1' then
  --           mem(to_integer(unsigned(i_addr_b))) <= i_din_b;
  --         end if;
  --       end if;
  --     end if;
  --   end process WR_B;
  -- end generate SECOND_PORT;
  
  
end architecture beh;
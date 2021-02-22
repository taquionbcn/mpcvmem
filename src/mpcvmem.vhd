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

entity mpcvmem is
  generic(
    g_SIMULATION          : std_logic := '0';
    g_LOGIC_TYPE          : string := "fifo"; -- fifo, pipeline, ram
    g_FIFO_TYPE           : string := "normal"; -- normal , read_ahead
    g_MEMORY_TYPE         : string := "auto"; -- auto, ultra, block, distributed
    --
    g_SECOND_PORT         : string := "none"; -- none, normal
    --
    g_PIPELINE_IN_REGS    : natural := 0;
    g_PIPELINE_OUT_REGS   : natural := 0;

    g_RAM_WIDTH           : integer := 64;
    g_RAM_DEPTH           : integer := 9600     -- maximum depth of the ram, also the maximum delay
  );
  port (
    clk                   : in std_logic;
    rst                   : in std_logic;
    ena                   : in std_logic := '1';
    -- Port A
    i_addr_a              : in  std_logic_vector(g_RAM_DEPTH-1 downto 0):= (others => '0');
    i_din_a               : in  std_logic_vector(g_RAM_WIDTH - 1 downto 0) := (others => '0');
    i_dv_in_a             : in  std_logic := '1';
    o_dout_a              : out std_logic_vector(g_RAM_WIDTH - 1 downto 0);
    o_dv_out_a             : out std_logic := '1';

    -- Port B
    i_addr_b              : in  std_logic_vector(g_RAM_DEPTH-1 downto 0):= (others => '0');
    i_din_b               : in  std_logic_vector(g_RAM_WIDTH - 1 downto 0) := (others => '0');
    i_dv_in_b             : in  std_logic := '1';
    o_dout_b              : out std_logic_vector(g_RAM_WIDTH - 1 downto 0);
    o_dv_out_b             : out std_logic := '1';

    -- Flags
    o_empty               : out std_logic;
    o_empty_next          : out std_logic;
    o_full                : out std_logic;
    o_full_next           : out std_logic;
    -- used counter
    o_used                : out integer range g_RAM_DEPTH - 1 downto 0;
    -- The delay can be changed by the offset and resetting the module
    i_delay               : in integer range g_RAM_DEPTH - 1 downto 0 := g_RAM_DEPTH-1  
  );
end entity mpcvmem;

architecture beh of mpcvmem is

  component DualPortMem is
    generic(
      g_MEMORY_TYPE         : string := "auto";
      g_ENABLE_SECOND_PORT  : std_logic := '0';
      g_RAM_WIDTH           : integer := 16;
      g_RAM_DEPTH           : integer := 1
    );
    port (
      clk         : in std_logic;
      ena         : in std_logic;
      -- Port A
      i_addr_a    : in std_logic_vector(g_RAM_DEPTH-1 downto 0);
      i_din_a     : in std_logic_vector(g_RAM_WIDTH-1 downto 0);
      o_dout_a    : out std_logic_vector(g_RAM_WIDTH-1 downto 0);
      -- Port B
      i_addr_b    : in std_logic_vector(g_RAM_DEPTH-1 downto 0);
      i_din_b     : in std_logic_vector(g_RAM_WIDTH-1 downto 0);
      o_dout_b    : out std_logic_vector(g_RAM_WIDTH-1 downto 0);
      -- 
      i_wen_a     : in std_logic;
      i_wen_b     : in std_logic
    );
  end component DualPortMem;
  
begin
  
  PIPE_GEN : if g_LOGIC_TYPE = "pipeline" generate

    RAM_MEM : DualPortMem
      generic map(
        g_MEMORY_TYPE => g_MEMORY_TYPE
        g_ENABLE_SECOND_PORT => g_ENABLE_SECOND_PORT,
        g_RAM_WIDTH => g_RAM_WIDTH,
        g_RAM_DEPTH => g_RAM_DEPTH
      )
      port map(
        clk         => clk,
        ena         => 
        -- Port A
        i_addr_a    => 
        i_din_a     => 
        o_dout_a    => 
        -- Port B
        i_addr_b    => 
        i_din_b     => 
        o_dout_b    => 
        -- 
        i_wen_a     => 
        i_wen_b     => 
      );




    case_options <= i_wr & mem_dv(rd_index);

    MEM_PROC: process(clk)
    begin
      if rising_edge(clk) then
        if rst = '1' then
          -- mem <= (others => (others => '0'));
          mem_dv <= (others => '0');
          rd_index <= get_read_index(rd_index,wr_index);
          wr_index <= 0;
          o_empty       <= '1';
          o_empty_next  <= '1';
          o_full        <= '0';
          o_full_next   <= '0';
          used_data <= 0;
        else


          --------------------------------
          -- PIPELINES CTRL
          --------------------------------
          -- if g_PIPELINE_IN_REGS = 0 then
          -- else
          -- end if;
          -- if g_PIPELINE_OUT_REGS = 0 then
          -- else
          -- end if;
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
          elsif used_data < g_RAM_DEPTH - 2  then
            o_empty       <= '0';
            o_empty_next  <= '0';
            o_full        <= '0';
            o_full_next   <= '1';
          elsif used_data < g_RAM_DEPTH - 1  then
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
          if ena = '1' then
            mem_dv(wr_index) <= i_wr;
            mem(wr_index) <= int_wr_data;
          end if;

          if ena = '1' then
            if mem_dv(rd_index) = '1' then
              output_pipeline(g_PIPELINE_OUT_REGS) <= mem(rd_index);
            else
              output_pipeline(g_PIPELINE_OUT_REGS) <= (others => '0');
            end if;
            output_dv_pipeline(g_PIPELINE_OUT_REGS) <= mem_dv(rd_index);
          end if;
          -- case case_options is
          --   when b"00" => -- idle

          --   when b"10" => -- write

          --     -- mem_dv(wr_index) <= i_wr;
          --     -- mem(wr_index) <= int_wr_data;
          --     used_data <= used_data + 1;

          --   when b"01" => -- read
            
          --     -- o_rd_data <= mem(rd_index);
          --     -- o_rd_dv <= mem_dv(rd_index);
          --     -- mem_dv(rd_index) <= '0';
          --     used_data <= used_data - 1;
              
          --   when b"11" => -- read & write 

          --     -- o_rd_data <= mem(rd_index);
          --     -- o_rd_dv <= mem_dv(rd_index);
          --     mem_dv(wr_index) <= '1';
          --     mem(wr_index) <= int_wr_data;

          --   when others =>
          --     -- ERROR
            
          -- end case;
          
          --------------------------------
          -- index  CTRL
          --------------------------------
          wr_index <= get_write_index(wr_index);
          rd_index <= get_read_index(rd_index,wr_index + 1,i_delay);

        end if;
      end if;
    end process MEM_PROC;
    
  end generate;
  
  
end architecture beh;
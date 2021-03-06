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

entity mpcvm_top is
  generic(
    g_SIMULATION          : std_logic := '0'; -- deprecated( only was to force proper simulation in vivado)
    --
    g_LOGIC_TYPE          : string := "fifo"; -- fifo, pipeline, ram
    g_READ_MODE           : string := "normal"; -- normal , read_ahead
    g_MEMORY_TYPE         : string := "auto"; -- auto, ultra, block, distributed
    g_DV_TYPE             : string := "int"; -- int , ext
    -- MEMORY PARAMETERS
    g_MEMORY_STRUCTURE    : string := "SDP";
    g_SECOND_PORT         : string := "none"; -- none, normal, monitor
    -- FIFO 
    -- PIPELINE
    g_PL_DELAY_CYCLES     : integer := 0;
    -- RAM
    -- IN/OUT PARAMETERS
    g_IN_PIPELINE         : natural := 0;
    g_OUT_PIPELINE        : natural := 0;
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
    i_addr_a              : in  std_logic_vector(integer(ceil(log2(real(g_MEM_DEPTH))))-1 downto 0):= (others => '0');
    i_din_a               : in  std_logic_vector(g_MEM_WIDTH - 1 downto 0) := (others => '0');
    i_dv_in_a             : in  std_logic := '1';
    o_dout_a              : out std_logic_vector(g_MEM_WIDTH - 1 downto 0);
    o_dv_out_a            : out std_logic := '1';

    -- Port B
    i_addr_b              : in  std_logic_vector(integer(ceil(log2(real(g_MEM_DEPTH))))-1 downto 0):= (others => '0');
    i_din_b               : in  std_logic_vector(g_MEM_WIDTH - 1 downto 0) := (others => '0');
    i_dv_in_b             : in  std_logic := '1';
    o_dout_b              : out std_logic_vector(g_MEM_WIDTH - 1 downto 0);
    o_dv_out_b             : out std_logic := '1';

    -- Flags
    o_empty               : out std_logic;
    o_empty_next          : out std_logic;
    o_full                : out std_logic;
    o_full_next           : out std_logic;
    -- used counter
    o_used                : out integer 
    -- range integer(log2(real(g_MEM_DEPTH))) - 1 downto 0)
    -- The delay can be changed by the offset and resetting the module
    -- i_delay               : in integer range g_RAM_DEPTH - 1 downto 0 := g_RAM_DEPTH-1  
  );
end entity mpcvm_top;

architecture beh of mpcvm_top is


  --------------------------------
  -- constants
  --------------------------------
  constant ADD_WIDTH : integer := init_add_width(g_MEM_DEPTH,g_ADD_WIDTH);--integer(ceil(log2(real(g_MEM_DEPTH))));
  constant MEM_DEPTH : integer := init_mem_depth(g_MEM_DEPTH,g_ADD_WIDTH);
  constant MEM_WIDTH : integer := init_mem_width(g_MEM_WIDTH,g_DV_TYPE);--g_MEM_WIDTH + 1;
  --------------------------------
  -- signals
  --------------------------------
  signal ena_a, ena_b : std_logic;
  signal ena_pipes_a : std_logic_vector(g_OUT_PIPELINE - 1 downto 0);
  signal ena_pipes_b : std_logic_vector(g_OUT_PIPELINE - 1 downto 0);
  type my_pipes is array (g_OUT_PIPELINE-1 downto 0) of std_logic_vector(MEM_WIDTH-1 downto 0);
  signal data_pipes_A : my_pipes;
  signal data_pipes_B : my_pipes;

  -- signal ENABLE_SECOND_PORT : integer;

  -- signal wr_index_aux : integer range 0 to MEM_DEPTH -1 := 0;
  -- signal rd_index_aux : integer range 0 to MEM_DEPTH -1 := 0;
  -- signal wr_index : integer range 0 to MEM_DEPTH -1 := 0;
  -- signal rd_index : integer range 0 to MEM_DEPTH -1 := 0;

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
  -- function get_read_index( 
  --   read_index : integer ;
  --   write_index : integer := 0;
  --   fi_delay : integer := 0
  --   ) return integer is
  --   variable o_rd_index : integer := 0;
  --   begin
  --   if g_LOGIC_TYPE = "fifo" then
  --     if read_index < MEM_DEPTH - 1 then
  --       o_rd_index := read_index + 1;
  --     else
  --       o_rd_index := 0;
  --     end if;
  --   elsif g_LOGIC_TYPE = "pipeline" then
  --     if write_index - fi_delay >= 0 then
  --       o_rd_index := write_index - fi_delay;
  --     else
  --       o_rd_index := (MEM_DEPTH - 1) - (fi_delay - 1)  + write_index;
  --     end if;
  --   else
  --     -- ERROR
  --   end if;
  --   return o_rd_index;

  -- end function;

  -- function get_write_index(write_index : integer; index_inc : integer := 1) return integer is
  --   variable o_wr_index_aux : integer := 0;
  --   begin
  --   if write_index < MEM_DEPTH - 1 then
  --     o_wr_index_aux := write_index + index_inc;
  --   else
  --     o_wr_index_aux := 0;
  --   end if;
  --   return o_wr_index_aux;
  -- end function;
  --------------------------------
  -- end functions
  --------------------------------
begin

  IF_DV_DATA: if g_DV_TYPE = "int" generate
    
  end generate IF_DV_DATA;

  NO_IN_PL_GEN : if g_IN_PIPELINE = 0 generate
    -- in_ctrl_NO: process(clk)
    -- begin
    --   if rising_edge(clk) then
    --     if i_dv_in_a = '1' then
          mem_in_a <= i_din_a & i_dv_in_a;
          -- mem_in_b <= i_din_b & i_dv_in_b;
    --     else
    --       mem_in_a <= (others => '0');
    --     end if;
    --   end if;
    -- end process in_ctrl_NO;
    
  end generate NO_IN_PL_GEN;
  
    PIPE_GEN : if g_LOGIC_TYPE = "pipeline" generate
      constant PL_DELAY : integer := g_PL_DELAY_CYCLES - g_IN_PIPELINE - g_OUT_PIPELINE ;
      -- constant MEM_DEPTH : integer := 2**ADD_WIDTH;

    begin
      mpcv_mPL_CTRLem : entity mpcvmem_lib.mpcvm_pl_ctrl
      generic map(
        -- g_LOGIC_TYPE    => "pipeline",
        -- g_MEMORY_TYPE   => g_MEMORY_TYPE,
        -- g_MEMORY_STRUCTURE => g_MEMORY_STRUCTURE,
        -- g_SECOND_PORT => "monitor",

        g_PL_DELAY_CYCLES => PL_DELAY,--TOTAL_DELAY_CYCLES,
        -- g_PL_LATENCY    => 2,
        g_ADD_WIDTH     => ADD_WIDTH,
        g_MEM_WIDTH     => MEM_WIDTH
        -- g_MEM_DEPTH     => DATA_DEPTH
      )
      port map(
        clk           => clk,
        rst           => rst,
        ena           => ena,
        --
        i_freeze      => i_freeze,
        i_ext_ctrl    => i_ext_ctrl,
        -- Port A
        i_addr_a      => i_addr_a,
        o_addr_a      => mem_addr_a,
       
        -- Port B
        i_addr_b      => i_addr_b,
        o_addr_b      => mem_addr_b, -- apb_rd_addr_o,  
      
        -- Flags
        o_empty       => o_empty,
        o_empty_next  => o_empty_next,
        o_full        => o_full,
        o_full_next   => o_full_next,
        -- used counter
        o_used        => o_used
      );
      -- mem_in_b <= (others => '0');
      -- mem_addr_a <= std_logic_vector(to_unsigned( wr_index , ADD_WIDTH ));
      -- mem_addr_b <=std_logic_vector(to_unsigned( rd_index , ADD_WIDTH ));

      -- MEM_CTRL: process(clk) begin
      --   if rising_edge(clk) then
      --     if rst = '1' then
      --       -- mem <= (others => (others => '0'));
      --       -- mem_dv <= (others => '0');
      --       rd_index_aux <= get_read_index(rd_index_aux,wr_index_aux,g_MEM_DEPTH);
      --       wr_index_aux <= 0;
      --       o_empty       <= '1';
      --       o_empty_next  <= '1';
      --       o_full        <= '0';
      --       o_full_next   <= '0';
      --       used_data <= 0;
      --     else


      --       --------------------------------
      --       -- INPUT SIGNALS CTRL
      --       --------------------------------

      --       if used_data < 1 then
      --         o_empty       <= '1';
      --         o_empty_next  <= '1';
      --         o_full        <= '0';
      --         o_full_next   <= '0';
      --       elsif used_data < 2 then
      --         o_empty       <= '0';
      --         o_empty_next  <= '1';
      --         o_full        <= '0';
      --         o_full_next   <= '0';
      --       elsif used_data < g_MEM_DEPTH - 2  then
      --         o_empty       <= '0';
      --         o_empty_next  <= '0';
      --         o_full        <= '0';
      --         o_full_next   <= '1';
      --       elsif used_data < g_MEM_DEPTH - 1  then
      --         o_empty       <= '0';
      --         o_empty_next  <= '0';
      --         o_full        <= '1';
      --         o_full_next   <= '1';
      --       else
      --         --ERROR used more than space
      --       end if;

      --       --------------------------------
      --       -- INPUT SIGNALS CTRL
      --       --------------------------------

      --       --------------------------------
      --       -- index  CTRL
      --       --------------------------------
      --       wr_index_aux <= get_write_index(wr_index_aux,1);
      --       rd_index_aux <= get_read_index(rd_index_aux,wr_index_aux + 1,PL_DELAY);
            
      --       if i_freeze = '0' then -- normal
      --         wr_index <= get_write_index(wr_index_aux,0);
      --         rd_index <= get_read_index(rd_index_aux,wr_index_aux + 1,PL_DELAY);
      --       else -- external
      --         wr_index <= to_integer(unsigned(i_addr_b));
      --         rd_index <= to_integer(unsigned(i_addr_b));
      --       end if;


      --     end if;
      --   end if;
      -- end process MEM_CTRL;

    end generate;

    --------------------------------
    -- PIPELINES CTRL
    --------------------------------
    NO_OUT_PL_GEN: if g_OUT_PIPELINE = 0 generate
      -- 1 clk latency
      data_no_pl: process(clk)
      begin
        if rising_edge(clk) then
          if rst = '1' then
            o_dout_a <= (others => '0');        
            o_dout_b <= (others => '0');  
            o_dv_out_a <= '0';
            o_dv_out_b <= '0';      
          else
            o_dout_a <= mem_out_a;        
            o_dout_b <= mem_out_b;  
            o_dv_out_a <= mem_out_a(0);
            o_dv_out_b <= mem_out_b(0); 
          end if;
        end if;
      end process data_no_pl;

      -- 0 clk latency
      -- o_dout_a <= mem_out_b;
    end generate NO_OUT_PL_GEN;

    SDP: if g_MEMORY_STRUCTURE = "SDP" generate

      ena_a <= ena;
      ena_b <= ena;

      PL_ULTRA: if g_MEMORY_TYPE = "ultra" generate

        RAM_MEM : entity mpcvmem_lib.mpcvm_tdp
          generic map(
            g_MEMORY_TYPE => g_MEMORY_TYPE,
            g_MEMORY_STRUCTURE => "SDP",
            g_ENABLE_SECOND_PORT => '1',
            -- g_OUT_PIPELINE => 2,
            g_RAM_WIDTH => MEM_WIDTH,
            g_ADD_WIDTH => ADD_WIDTH,
            g_RAM_DEPTH => MEM_DEPTH
          )
          port map(
            clk         => clk,
            rst         => rst,
            -- Port A
            ena_a        => ena_a,
            i_addr_a     => mem_addr_a,--std_logic_vector(to_unsigned(mem_addr_a)); 
            i_din_a      => mem_in_a,
            i_wr_nrd_a   => '1',
            o_dout_a     => mem_out_a,
            -- Port B 
            ena_b        => ena_b,
            i_addr_b     => mem_addr_b,--std_logic_vector(to_unsigned(mem_addr_b));
            i_din_b      => (others => '0'),--mem_in_b,
            i_wr_nrd_b   => '0',
            o_dout_b     => mem_out_b
          );
      end generate PL_ULTRA;

      

      OUT_PL_GEN: if g_OUT_PIPELINE > 0 generate
   
        ena_b0: process(clk) begin
          if rising_edge(clk) then
            ena_pipes_b(0) <= ena_b;
            if g_OUT_PIPELINE > 1 then
              for i in 1 to g_OUT_PIPELINE - 1 loop
                ena_pipes_b(i) <= ena_pipes_b(i-1);
              end loop;
            end if;
          end if;
        end process ena_b0;
  
        proc0_B: process(clk)
        begin
          if rising_edge(clk) then
            -- data pipleine
            if (ena_pipes_b(0) = '1') then
              data_pipes_B(0) <= mem_out_b;
              if g_OUT_PIPELINE > 1 then
                for j in 1 to g_OUT_PIPELINE-1 loop
                  if (ena_pipes_b(j) = '1') then
                    data_pipes_B(j) <= data_pipes_B(j-1);
                  end if;
                end loop;
              end if;
            end if;
            -- data out 
            if (rst = '1') then
              o_dout_b <= (others => '0');
              o_dv_out_b <= '0';
            elsif (ena_pipes_b(g_OUT_PIPELINE - 1) = '1') then
              o_dout_b <= data_pipes_B(g_OUT_PIPELINE-1)(MEM_WIDTH -1 downto 1);
              o_dv_out_b <= data_pipes_B(g_OUT_PIPELINE-1)(0);
            end if;
          end if;
        end process proc0_B;
        
  
      end generate OUT_PL_GEN;
    end generate;

    TDP: if g_MEMORY_STRUCTURE = "TDP" generate
      PL_ULTRA: if g_MEMORY_TYPE = "ultra" generate

        RAM_MEM : entity mpcvmem_lib.mpcvm_tdp
          generic map(
            g_MEMORY_TYPE => g_MEMORY_TYPE,
            g_MEMORY_STRUCTURE => "SDP",
            g_ENABLE_SECOND_PORT => '1',
            -- g_OUT_PIPELINE => 2,
            g_RAM_WIDTH => MEM_WIDTH,
            g_ADD_WIDTH => ADD_WIDTH,
            g_RAM_DEPTH => MEM_DEPTH
          )
          port map(
            clk         => clk,
            rst         => rst,
            -- Port A
            ena_a        => ena_a,
            i_addr_a     => mem_addr_a,--std_logic_vector(to_unsigned(mem_addr_a)); 
            i_din_a      => mem_in_a,
            i_wr_nrd_a   => '1',
            o_dout_a     => mem_out_a,
            -- Port B 
            ena_b        => ena_b,
            i_addr_b     => mem_addr_b,--std_logic_vector(to_unsigned(mem_addr_b));
            i_din_b      => (others => '0'),--mem_in_b,
            i_wr_nrd_b   => '0',
            o_dout_b     => mem_out_b
          );
      end generate PL_ULTRA;

      ena_a <= ena;
      ena_b <= ena;

      NO_OUT_PL_GEN: if g_OUT_PIPELINE = 0 generate
        -- 1 clk latency
        data_no_pl: process(clk)
        begin
          if rising_edge(clk) then
            if rst = '1' then
              o_dout_a <= (others => '0');        
              o_dout_b <= (others => '0');  
              o_dv_out_a <= '0';
              o_dv_out_b <= '0';      
            else
              o_dout_a <= mem_out_a;        
              o_dout_b <= mem_out_b;  
              o_dv_out_a <= mem_out_a(0);
              o_dv_out_b <= mem_out_b(0); 
            end if;
          end if;
        end process data_no_pl;
  
        -- 0 clk latency
        -- o_dout_a <= mem_out_b;
      end generate NO_OUT_PL_GEN;
  
      
  
      OUT_PL_GEN: if g_OUT_PIPELINE > 0 generate
        -- enable pl
        -- ena_a0: process(clk) begin
        --   if rising_edge(clk) then
        --     ena_pipes_a(0) <= ena_a;
        --     for i in 1 to g_OUT_PIPELINE loop
        --       ena_pipes_a(i) <= ena_pipes_a(i-1);
        --     end loop;
        --   end if;
        -- end process ena_a0;      
        
        ena_b0: process(clk) begin
          if rising_edge(clk) then
            ena_pipes_b(0) <= ena_b;
            if g_OUT_PIPELINE > 1 then
              for i in 1 to g_OUT_PIPELINE - 1 loop
                ena_pipes_b(i) <= ena_pipes_b(i-1);
              end loop;
            end if;
          end if;
        end process ena_b0;
  
        -- data pl
        -- proc0_A: process(clk)
        -- begin
        --   if rising_edge(clk) then
        --     if (ena_pipes_a(0) = '1') then
        --       data_pipes_A(0) <= mem_out_b;
        --       for j in 1 to g_OUT_PIPELINE-1 loop
        --         if (ena_pipes_a(j) = '1') then
        --             data_pipes_A(j) <= data_pipes_A(j-1);
        --         end if;
        --       end loop;
        --     end if;
        --     if (rst = '1') then
        --       o_dout_a <= (others => '0');
        --       o_dv_out_a <= '0';
        --     elsif (ena_pipes_a(g_OUT_PIPELINE) = '1') then
        --       o_dout_a <= data_pipes_A(g_OUT_PIPELINE-1)(MEM_WIDTH -1 downto 1);
        --       o_dv_out_a <= data_pipes_A(g_OUT_PIPELINE-1)(0);
        --     end if;
        --   end if;
        -- end process proc0_A;
  
        proc0_B: process(clk)
        begin
          if rising_edge(clk) then
            -- data pipleine
            if (ena_pipes_b(0) = '1') then
              data_pipes_B(0) <= mem_out_b;
              if g_OUT_PIPELINE > 1 then
                for j in 1 to g_OUT_PIPELINE-1 loop
                  if (ena_pipes_b(j) = '1') then
                    data_pipes_B(j) <= data_pipes_B(j-1);
                  end if;
                end loop;
              end if;
            end if;
            -- data out 
            if (rst = '1') then
              o_dout_b <= (others => '0');
              o_dv_out_b <= '0';
            elsif (ena_pipes_b(g_OUT_PIPELINE - 1) = '1') then
              o_dout_b <= data_pipes_B(g_OUT_PIPELINE-1)(MEM_WIDTH -1 downto 1);
              o_dv_out_b <= data_pipes_B(g_OUT_PIPELINE-1)(0);
            end if;
          end if;
        end process proc0_B;
        
  
      end generate OUT_PL_GEN;
    end generate;

    SDP_2: if g_MEMORY_STRUCTURE = "SDP_2" generate

      -- signal ena_pipes : std_logic_vector(g_OUT_PIPELINE downto 0);
      -- signal data_pipes : my_pipes;

      begin

      PL_ULTRA: if g_MEMORY_TYPE = "ultra" generate

        RAM_MEM : entity mpcvmem_lib.mpcvm_sdp
          generic map(
            g_MEMORY_TYPE => g_MEMORY_TYPE,
            -- g_MEMORY_STRUCTURE => "SDP",
            g_OUT_PIPELINE => 2,
            -- g_OUT_PIPELINE => 2,
            g_RAM_WIDTH   => MEM_WIDTH,
            g_ADD_WIDTH   => ADD_WIDTH,
            g_RAM_DEPTH   => MEM_DEPTH
          )
          port map(
            clk           => clk,
            rst           => rst,
            -- Port A
            ena           => ena,
            i_addr_a      => mem_addr_a,--std_logic_vector(to_unsigned(mem_addr_a)); 
            i_din_a       => mem_in_a,
            i_wr_nrd_a    => '1',
            -- o_dout_a     => mem_out_a,
            -- Port B 
            -- ena_b        => ena_b,
            i_addr_b      => mem_addr_b,--std_logic_vector(to_unsigned(mem_addr_b));
            -- i_din_b      => (others => '0'),--mem_in_b,
            -- i_wr_nrd_b   => '0',
            o_dout_b      => mem_out_b
          );
      end generate PL_ULTRA;

        o_dout_b   <= mem_out_b(MEM_WIDTH -1 downto 1);
        o_dv_out_b <= mem_out_b(0);

    end generate;

    -- XPM: if g_MEMORY_STRUCTURE = "XPM" generate

    --   xpm_memory_sdpram_inst : xpm_memory_sdpram
    --   generic map (
    --      ADDR_WIDTH_A => ADD_WIDTH,               -- DECIMAL
    --      ADDR_WIDTH_B => ADD_WIDTH,               -- DECIMAL
    --      AUTO_SLEEP_TIME => 0,            -- DECIMAL
    --      BYTE_WRITE_WIDTH_A => MEM_WIDTH,--32,        -- DECIMAL
    --      CASCADE_HEIGHT => 0,             -- DECIMAL
    --      CLOCKING_MODE => "common_clock", -- String
    --      ECC_MODE => "no_ecc",            -- String
    --      MEMORY_INIT_FILE => "none",      -- String
    --      MEMORY_INIT_PARAM => "0",        -- String
    --      MEMORY_OPTIMIZATION => "true",   -- String
    --      MEMORY_PRIMITIVE => "ultra",      -- String
    --      MEMORY_SIZE => MEM_WIDTH * MEM_DEPTH,             -- DECIMAL
    --      MESSAGE_CONTROL => 0,            -- DECIMAL
    --      READ_DATA_WIDTH_B => MEM_WIDTH,--32,         -- DECIMAL
    --      READ_LATENCY_B => 2,             -- DECIMAL
    --      READ_RESET_VALUE_B => "0",       -- String
    --      RST_MODE_A => "SYNC",            -- String
    --      RST_MODE_B => "SYNC",            -- String
    --      SIM_ASSERT_CHK => 0,             -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
    --      USE_EMBEDDED_CONSTRAINT => 0,    -- DECIMAL
    --      USE_MEM_INIT => 1,               -- DECIMAL
    --      WAKEUP_TIME => "disable_sleep",  -- String
    --      WRITE_DATA_WIDTH_A => MEM_WIDTH,--32,        -- DECIMAL
    --      WRITE_MODE_B => "read_first"      -- String
    --   )
    --   port map (
    --     --  dbiterrb => dbiterrb,             -- 1-bit output: Status signal to indicate double bit error occurrence
    --                                        -- on the data output of port B.
   
    --      doutb => mem_out_b,--doutb,                   -- READ_DATA_WIDTH_B-bit output: Data output for port B read operations.
    --     --  sbiterrb => sbiterrb,             -- 1-bit output: Status signal to indicate single bit error occurrence
    --                                        -- on the data output of port B.
   
    --      addra => mem_addr_a,                   -- ADDR_WIDTH_A-bit input: Address for port A write operations.
    --      addrb => mem_addr_b,                   -- ADDR_WIDTH_B-bit input: Address for port B read operations.
    --      clka => clk,                     -- 1-bit input: Clock signal for port A. Also clocks port B when
    --                                        -- parameter CLOCKING_MODE is "common_clock".
   
    --      clkb => clk,                     -- 1-bit input: Clock signal for port B when parameter CLOCKING_MODE is
    --                                        -- "independent_clock". Unused when parameter CLOCKING_MODE is
    --                                        -- "common_clock".
   
    --      dina => mem_in_a,--dina,                     -- WRITE_DATA_WIDTH_A-bit input: Data input for port A write operations.
    --      ena => ena,                       -- 1-bit input: Memory enable signal for port A. Must be high on clock
    --                                        -- cycles when write operations are initiated. Pipelined internally.
   
    --      enb => ena,                       -- 1-bit input: Memory enable signal for port B. Must be high on clock
    --                                        -- cycles when read operations are initiated. Pipelined internally.
   
    --      injectdbiterra => '0',--injectdbiterra, -- 1-bit input: Controls double bit error injection on input data when
    --                                        -- ECC enabled (Error injection capability is not available in
    --                                        -- "decode_only" mode).
   
    --      injectsbiterra => '0',--injectsbiterra, -- 1-bit input: Controls single bit error injection on input data when
    --                                        -- ECC enabled (Error injection capability is not available in
    --                                        -- "decode_only" mode).
   
    --      regceb => '1',--regceb,                 -- 1-bit input: Clock Enable for the last register stage on the output
    --                                        -- data path.
   
    --      rstb => rst,                     -- 1-bit input: Reset signal for the final port B output register
    --                                        -- stage. Synchronously resets output port doutb to the value specified
    --                                        -- by parameter READ_RESET_VALUE_B.
   
    --      sleep => '0',--sleep,                   -- 1-bit input: sleep signal to enable the dynamic power saving feature.
    --      wea => b"1" --wea                        -- WRITE_DATA_WIDTH_A/BYTE_WRITE_WIDTH_A-bit input: Write enable vector
    --                                        -- for port A input data port dina. 1 bit wide when word-wide writes
    --                                        -- are used. In byte-wide write configurations, each bit controls the
    --                                        -- writing one byte of dina to address addra. For example, to
    --                                        -- synchronously write only bits [15-8] of dina when WRITE_DATA_WIDTH_A
    --                                        -- is 32, wea would be 4'b0010.
   
    --   );
    --   o_dout_b   <= mem_out_b(MEM_WIDTH -1 downto 1);
    --   o_dv_out_b <= mem_out_b(0);
    -- end generate XPM;
    
  
  
  
end architecture beh;
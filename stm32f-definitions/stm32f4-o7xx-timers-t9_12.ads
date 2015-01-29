------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                S T M 32 F 4 . O7xx . T i m e r s . T 9 _ 12              --
--                                                                          --
--                           H a r d w ar e  S p e c                        --
--                                                                          --
--                     Copyright (C) 2014, Jan de Kruyf                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                      (email: jan.de.kruyf@gmail.com)                     --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides type definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.
--
--  chapter 19.4   TIM9 and TIM12 registers       RM0090 Reference manual


pragma Restrictions (No_Elaboration_Code);

with STM32F4.O7xx.Timers.T2_5;

package STM32F4.O7xx.Timers.T9_12 is
   
   type Cr1_Register is record  --   control register 1 
      CEN,           -- Counter enable
      UDIS,          -- Update disable
      URS,           -- Update request source
      OPM  : Bits_1; -- One pulse mode
      Res1 : Reserved (4 .. 6) := (others => 0);
      ARPE : Bits_1; -- Auto-reload preload enable
      CKD  : Bits_2; -- Clock division
      Res2 : Reserved (10 .. 31) := (others => 0);
   end record;
   
   for Cr1_Register use record
      CEN  at 0 range 0 .. 0;
      UDIS at 0 range 1 .. 1;
      URS  at 0 range 2 .. 2;
      OPM  at 0 range 3 .. 3;
      Res1 at 0 range 4 .. 6;
      ARPE at 0 range 7 .. 7;
      CKD  at 0 range 8 .. 9;
      Res2 at 0 range 10 .. 31;
   end record;
   
   
   type SMCR_Register is record   -- slave mode control register
      SMS   : Bits_3; -- Slave mode selection
      Res1 : Reserved (3 .. 3) := (others => 0);
      TS    : Bits_3; -- Trigger selection
      MSM   : Bits_1; -- Master/slave mode
      Res2 : Reserved (8 .. 31) := (others => 0);
   end record;
   
   for SMCR_Register use record
      SMS  at 0 range 0 .. 2;
      Res1 at 0 range 3 .. 3;
      TS   at 0 range 4 .. 6;
      MSM  at 0 range 7 .. 7;
      Res2 at 0 range 8 .. 31;
   end record;
   
   
   type DIER_Register is new STM32F4.O7xx.Timers.T2_5.DIER_Register;
   
        
   type SR_Register is record  -- status register 
      UIF,                -- Update interrupt flag
      CC1IF,              -- CC1IF
      CC2IF    : Bits_1;  -- CC2IF
      Res1     : Reserved (3 .. 5) := (others => 0);
      TIF      : Bits_1;  -- Trigger interrupt flag
      Res3     : Reserved (7 .. 8) := (others => 0);
      CC1OF,              -- Capture/Compare 1 overcapture flag
      CC2OF    : Bits_1;  -- Capture/Compare 2 overcapture flag
      Res2     : Reserved (11 .. 31) := (others => 0);
   end record;
   
   for SR_Register use record
      UIF    at 0 range 0 .. 0;	
      CC1IF  at 0 range 1 .. 1;	
      CC2IF  at 0 range 2 .. 2;	
      Res1   at 0 range 3 .. 5;
      TIF    at 0 range 6 .. 6;	
      Res3   at 0 range 7 .. 8;
      CC1OF  at 0 range 9 .. 9; 
      CC2OF  at 0 range 10 .. 10;
      Res2   at 0 range 11 .. 31;
   end record;
   
   
   type EGR_Register is record  -- event generation register
      UG,                 -- Update generation
      CC1G,               -- Capture/Compare 1 generation
      CC2G     : Bits_1;  -- Capture/Compare 2 generation
      Res1     : Reserved (3 .. 5) := (others => 0);
      TG       : Bits_1;  -- Trigger generation
      Res2     : Reserved (7 .. 31) := (others => 0);
   end record;
   
   for EGR_Register use record
      UG    at 0 range 0 .. 0;	
      CC1G  at 0 range 1 .. 1;	
      CC2G  at 0 range 2 .. 2;
      Res1  at 0 range 3 .. 5;
      TG    at 0 range 6 .. 6;	
      Res2  at 0 range 7 .. 31;
   end record;
   
   
   --type Mode_Type is (Input_Capture, Output_Compare);
   type CCMR1_Register is record  
      -- capture/compare mode register 1
      CC1S    : Bits_2;             -- Capture/Compare 1 selection
      --case Mode is
	 --when Input_Capture   =>
	    IC1PSC  : Bits_2;  -- Input capture 1 prescaler
	    IC1F    : Bits_4;  -- Input capture 1 filter
	    iCC2S   : Bits_2;  -- Capture/Compare 2 selection
	    IC2PSC  : Bits_2;  -- Input capture 2 prescaler
	    IC2F    : Bits_4;  -- Input capture 2 filter  
	    Res5    : Reserved (16 .. 31) := (others => 0);
      --  	 when Output_Compare  =>
      --  	    OC1FE,             -- Output Compare 1 fast enable
      --  	    OC1PE   : Bits_1;  -- Output Compare 1 preload enable
      --  	    OC1M    : Bits_3;  -- Output Compare 1 mode
      --  	    Res1    : Reserved (7 .. 7) := (others => 0);
      --  	    OCC2S   : BITS_2;  -- Capture/Compare 2 selection
      --  	    OC2FE,             -- Output Compare 2 fast enable
      --  	    OC2PE   : Bits_1;  -- Output Compare 2 preload enable
      --  	    OC2M    : Bits_3;  -- Output Compare 2 mode
      --  	    Res2    : Reserved (15 .. 31) := (others => 0);
      --  end case;
   end record;
   
   for CCMR1_Register use record
      CC1S   at 0 range 0 .. 1;	
      
      IC1PSC at 0 range 2 .. 3;
      IC1F   at 0 range 4 .. 7;
      iCC2S  at 0 range 8 .. 9;
      IC2PSC at 0 range 10 .. 11;
      IC2F   at 0 range 12 .. 15;
      Res5   at 0 range 16 .. 31;
      --  OC1FE  at 0 range 2 .. 2;
      --  OC1PE  at 0 range 3 .. 3;
      --  OC1M   at 0 range 4 .. 6;
      --  Res1   at 0 range 7 .. 7;
      --  oCC2S  at 0 range 8 .. 9;
      --  OC2FE  at 0 range 10 .. 10;
      --  OC2PE  at 0 range 11 .. 11;
      --  OC2M   at 0 range 12 .. 14;
      --  Res2   at 0 range 15 .. 31;
   end record;
   
   --  type Mode_Type is (Input_Capture, Output_Compare);
   --  type CCMR1_Register (Mode : Mode_Type := Output_Compare) is record  
   --     -- capture/compare mode register 1
   --     CC1S    : Bits_2;             -- Capture/Compare 1 selection
   --     case Mode is
   --  	 when Input_Capture   =>
   --  	    IC1PSC  : Bits_2;  -- Input capture 1 prescaler
   --  	    IC1F    : Bits_4;  -- Input capture 1 filter
   --  	    iCC2S   : Bits_2;  -- Capture/Compare 2 selection
   --  	    IC2PSC  : Bits_2;  -- Input capture 2 prescaler
   --  	    IC2F    : Bits_4;  -- Input capture 2 filter  
   --  	    Res5    : Reserved (16 .. 31) := (others => 0);
   --  	 when Output_Compare  =>
   --  	    OC1FE,             -- Output Compare 1 fast enable
   --  	    OC1PE   : Bits_1;  -- Output Compare 1 preload enable
   --  	    OC1M    : Bits_3;  -- Output Compare 1 mode
   --  	    Res1    : Reserved (7 .. 7) := (others => 0);
   --  	    OCC2S   : BITS_2;  -- Capture/Compare 2 selection
   --  	    OC2FE,             -- Output Compare 2 fast enable
   --  	    OC2PE   : Bits_1;  -- Output Compare 2 preload enable
   --  	    OC2M    : Bits_3;  -- Output Compare 2 mode
   --  	    Res2    : Reserved (15 .. 31) := (others => 0);
   --     end case;
   --  end record;
   
   --  for CCMR1_Register use record
   --     CC1S   at 0 range 0 .. 1;	
      
   --     IC1PSC at 0 range 2 .. 3;
   --     IC1F   at 0 range 4 .. 7;
   --     iCC2S  at 0 range 8 .. 9;
   --     IC2PSC at 0 range 10 .. 11;
   --     IC2F   at 0 range 12 .. 15;
   --     Res5   at 0 range 16 .. 31;
   --     OC1FE  at 0 range 2 .. 2;
   --     OC1PE  at 0 range 3 .. 3;
   --     OC1M   at 0 range 4 .. 6;
   --     Res1   at 0 range 7 .. 7;
   --     oCC2S  at 0 range 8 .. 9;
   --     OC2FE  at 0 range 10 .. 10;
   --     OC2PE  at 0 range 11 .. 11;
   --     OC2M   at 0 range 12 .. 14;
   --     Res2   at 0 range 15 .. 31;
   --  end record;
   
   
   type CCER_Register is record  -- capture/compare enable register 
      CC1E,               -- Capture/Compare 1 output enable
      CC1P      : Bits_1; -- Capture/Compare 1 output polarity
      Res1      : Reserved (2 .. 2) := (others => 0);
      CC1NP,              -- Capture/Compare 1 complementary output polarity
      CC2E,               -- Capture/Compare 2 output enable
      CC2P      : Bits_1; -- Capture/Compare 2 output polarity
      Res3      : Reserved (6 .. 6) := (others => 0);
      CC2NP     : Bits_1; -- Capture/Compare 2 complementary output polarity
      Res2      : Reserved (8 .. 31) := (others => 0);
   end record;
   
   for CCER_Register use record
      CC1E   at 0 range 0 .. 0;	 
      CC1P   at 0 range 1 .. 1;	
      Res1   at 0 range 2 .. 2;
      CC1NP  at 0 range 3 .. 3;	 
      CC2E   at 0 range 4 .. 4;	 
      CC2P   at 0 range 5 .. 5;	
      Res3   at 0 range 6 .. 6;
      CC2NP  at 0 range 7 .. 7;  
      Res2   at 0 range 8 .. 31;
   end record;
   
   
   subtype CNT_Register is Half_Word;  -- counter
   subtype PSC_Register is Half_Word;  -- prescaler 
   subtype ARR_Register is Half_Word;  -- auto-reload register
   subtype CCR_Register is Half_Word;  -- capture/compare register
   
   
   type Cntr_Nr_Type is new Positive range 9 .. 12;
   
   -----------------------------------------------------------
   -- this is the top of the timer / counter definition     --
   -- usage example:                                        --
   --   T9 : STM32F4.O7xx.Timers.T9_12.Timer_Register;  --
   -----------------------------------------------------------
   
   type Timer_Register is record
      CR1         : Cr1_Register;  -- control register 1 
      SMCR        : SMCR_Register;  -- slave mode control register
      DIER        : DIER_Register;  -- DMA/interrupt enable register
      SR          : SR_Register;    -- status register 
      EGR         : EGR_Register;   -- event generation register
      CCMR1       : CCMR1_Register; -- capture/compare mode register 1
      Reserved0   : Bits_32x1;
      CCER        : CCER_Register;  -- capture/compare enable register 
      CNT         : CNT_Register;   -- counter
      PSC         : PSC_Register;   -- prescaler 
      ARR         : ARR_Register;   -- auto-reload register
      Reserved1   : Bits_32x1;
      CCR1        : CCR_Register;  -- capture/compare register 1
      CCR2        : CCR_Register;  -- capture/compare register 1
   end record;
   
   for Timer_Register use record
      CR1   at 0   range 0 .. 31;
      SMCR  at 8   range 0 .. 31;
      DIER  at 12  range 0 .. 31;
      SR    at 16  range 0 .. 31;
      EGR   at 20  range 0 .. 31;
      CCMR1 at 24  range 0 .. 31;
      Reserved0 at 28 range 0 .. 31;
      CCER  at 32  range 0 .. 31;
      CNT   at 36  range 0 .. 31;
      PSC   at 40  range 0 .. 31;
      ARR   at 44  range 0 .. 31;
      Reserved1 at 48 range 0 .. 31;
      CCR1  at 52  range 0 .. 31;
      CCR2  at 56  range 0 .. 31;
   end record;
   
end STM32F4.O7xx.Timers.T9_12;

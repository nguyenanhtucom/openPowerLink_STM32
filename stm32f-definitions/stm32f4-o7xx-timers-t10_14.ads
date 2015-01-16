------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--               S T M 32 F 4 . O7xx . T i m e r s . T 10 _ 14              --
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
--  chapter 19.5   TIM10/11/13/14 registers       RM0090 Reference manual



pragma Restrictions (No_Elaboration_Code);

package STM32F4.O7xx.Timers.T10_14 is
   
   type Cr1_Register is record  --   control register 1 
      CEN,           -- Counter enable
      UDIS,          -- Update disable
      URS  : Bits_1;  -- Update request source
      Res1 : Reserved (3 .. 6) := (others => 0);
      ARPE : Bits_1; -- Auto-reload preload enable
      CKD  : Bits_2; -- Clock division
      Res2 : Reserved (10 .. 31) := (others => 0);
   end record;
   
   for Cr1_Register use record
      CEN  at 0 range 0 .. 0;
      UDIS at 0 range 1 .. 1;
      URS  at 0 range 2 .. 2;
      Res1 at 0 range 3 .. 6;
      ARPE at 0 range 7 .. 7;
      CKD  at 0 range 8 .. 9;
      Res2 at 0 range 10 .. 31;
   end record;
   
   
   type DIER_Register is record   --  DMA/interrupt enable register
      UIE,              -- Update interrupt enable
      CC1IE   : Bits_1; -- Capture/Compare 1 interrupt enable
      Res1    : Reserved (2 .. 31) := (others => 0);
   end record;
   
   for DIER_Register use record
      UIE    at 0 range 0 .. 0;
      CC1IE  at 0 range 1 .. 1;
      Res1   at 0 range 2 .. 31;
   end record;
   
   
   type SR_Register is record  -- status register 
      UIF,                -- Update interrupt flag
      CC1IF    : Bits_1;  -- CC1IF
      Res1     : Reserved (2 .. 8) := (others => 0);
      CC1OF    : Bits_1;  -- Capture/Compare 1 overcapture flag
      Res2     : Reserved (10 .. 31) := (others => 0);
   end record;
   
   for SR_Register use record
      UIF    at 0 range 0 .. 0;	
      CC1IF  at 0 range 1 .. 1;	
      Res1   at 0 range 2 .. 8;
      CC1OF  at 0 range 9 .. 9; 
      Res2   at 0 range 10 .. 31;
   end record;
   
   
   type EGR_Register is record  -- event generation register
      UG,                 -- Update generation
      CC1G      : Bits_1; -- Capture/Compare 1 generation
      Res2      : Reserved (2 .. 31) := (others => 0);
   end record;
   
   for EGR_Register use record
      UG    at 0 range 0 .. 0;	
      CC1G  at 0 range 1 .. 1;	
      Res2  at 0 range 2 .. 31;
   end record;
   
   
      --type Mode_Type is (Input_Capture, Output_Compare);
   type CCMR1_Register is record  
      -- capture/compare mode register 1
      CC1S    : Bits_2;             -- Capture/Compare 1 selection
      --case Mode is
	 --when Input_Capture   =>
	    IC1PSC  : Bits_2;  -- Input capture 1 prescaler
	    IC1F    : Bits_4;  -- Input capture 1 filter
	    Res1    : Reserved (8 .. 31) := (others => 0);
      --  	 when Output_Compare  =>
      --  	    OC1FE,             -- Output Compare 1 fast enable
      --  	    OC1PE   : Bits_1;  -- Output Compare 1 preload enable
      --  	    OC1M    : Bits_3;  -- Output Compare 1 mode
      --  	    Res2    : Reserved (7 .. 31) := (others => 0);
      --  end case;
   end record;
      
   for CCMR1_Register use record
      CC1S   at 0 range 0 .. 1;	
      
      IC1PSC at 0 range 2 .. 3;
      IC1F   at 0 range 4 .. 7;
      Res1   at 0 range 8 .. 31;
      
      --  OC1FE  at 0 range 2 .. 2;
      --  OC1PE  at 0 range 3 .. 3;
      --  OC1M   at 0 range 4 .. 6;
      --  Res2   at 0 range 7 .. 31;
   end record;
   
    
   --     type Mode_Type is (Input_Capture, Output_Compare);
   --  type CCMR1_Register (Mode : Mode_Type := Output_Compare) is record  
   --     -- capture/compare mode register 1
   --     CC1S    : Bits_2;             -- Capture/Compare 1 selection
   --     case Mode is
   --  	 when Input_Capture   =>
   --  	    IC1PSC  : Bits_2;  -- Input capture 1 prescaler
   --  	    IC1F    : Bits_4;  -- Input capture 1 filter
   --  	    Res1    : Reserved (8 .. 31) := (others => 0);
   --  	 when Output_Compare  =>
   --  	    OC1FE,             -- Output Compare 1 fast enable
   --  	    OC1PE   : Bits_1;  -- Output Compare 1 preload enable
   --  	    OC1M    : Bits_3;  -- Output Compare 1 mode
   --  	    Res2    : Reserved (7 .. 31) := (others => 0);
   --     end case;
   --  end record;
      
   --  for CCMR1_Register use record
   --     CC1S   at 0 range 0 .. 1;	
      
   --     IC1PSC at 0 range 2 .. 3;
   --     IC1F   at 0 range 4 .. 7;
   --     Res1   at 0 range 8 .. 31;
      
   --     OC1FE  at 0 range 2 .. 2;
   --     OC1PE  at 0 range 3 .. 3;
   --     OC1M   at 0 range 4 .. 6;
   --     Res2   at 0 range 7 .. 31;
   --  end record;
   
  
   type CCER_Register is record  -- capture/compare enable register 
      CC1E,               -- Capture/Compare 1 output enable
      CC1P      : Bits_1; -- Capture/Compare 1 output polarity
      Res1      : Reserved (2 .. 2) := (others => 0);
      CC1NP     : Bits_1; -- Capture/Compare 1 complementary output polarity
      Res2      : Reserved (4 .. 31) := (others => 0);
   end record;
   
   for CCER_Register use record
      CC1E   at 0 range 0 .. 0;	 
      CC1P   at 0 range 1 .. 1;	
      Res1   at 0 range 2 .. 2;
      CC1NP  at 0 range 3 .. 3;
      Res2   at 0 range 4 .. 31;
   end record;
   
   
   subtype CNT_Register is Half_Word;  -- counter
   subtype PSC_Register is Half_Word;  -- prescaler 
   subtype ARR_Register is Half_Word;  -- auto-reload register
   subtype CCR_Register is Half_Word;  -- capture/compare register
   
   
   type TIM11_OR_Register is record  -- TIM11 option register 1
      TI1_RMP : Bits_2;    -- TIM11 Input 1 remapping capability
      Res2    : Reserved (2 .. 31) := (others => 0);
   end record;
   
   for TIM11_OR_Register use record
      TI1_RMP at 0 range 0 .. 1;
      Res2    at 0 range 2 .. 31;
   end record;
   
   
   type Cntr_Nr_Type is new Positive range 10 .. 14;
    
   ------------------------------------------------------------
   -- this is the top of the timer / counter definition      --
   -- usage example:                                         --
   --  T11 : STM32F4.O7xx.Timers.T10_14.Timer_Register (11); --
   ------------------------------------------------------------
   
   type Timer_Register (Cntr : Cntr_Nr_Type := 10) is record
      pragma Compile_Time_Error 
	(Cntr = 12, "Timer 12 is in STM32F4.O7xx.Timers.T9_12");
      CR1         : Cr1_Register;  -- control register 1 
      DIER        : DIER_Register;  -- DMA/interrupt enable register
      SR          : SR_Register;    -- status register 
      EGR         : EGR_Register;   -- event generation register
      CCMR1       : CCMR1_Register; -- capture/compare mode register 1
      CCER        : CCER_Register;  -- capture/compare enable register 
      CNT         : CNT_Register;   -- counter
      PSC         : PSC_Register;   -- prescaler 
      ARR         : ARR_Register;   -- auto-reload register
      CCR1        : CCR_Register;  -- capture/compare register 1
      case Cntr is
	 when 11      =>
	    TIM11_OR : TIM11_OR_Register; -- TIM11 option register 1
	 when others  =>
	    null;
      end case;
   end record;
   
   for Timer_Register use record
      CR1   at 0   range 0 .. 31;
      DIER  at 12  range 0 .. 31;
      SR    at 16  range 0 .. 31;
      EGR   at 20  range 0 .. 31;
      CCMR1 at 24  range 0 .. 31;
      CCER  at 32  range 0 .. 31;
      CNT   at 36  range 0 .. 31;
      PSC   at 40  range 0 .. 31;
      ARR   at 44  range 0 .. 31;
      CCR1  at 52  range 0 .. 31;
      TIM11_OR at 80 range 0 .. 31;
   end record;
   
end STM32F4.O7xx.Timers.T10_14;

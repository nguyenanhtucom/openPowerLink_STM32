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

with STM32F4.o7xx.Timers.T1_8;
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
      Res2 : Reserved (10 .. 15) := (others => 0);
   end record;
   
   for Cr1_Register use record
      CEN  at 0 range 0 .. 0;
      UDIS at 0 range 1 .. 1;
      URS  at 0 range 2 .. 2;
      OPM  at 0 range 3 .. 3;
      Res1 at 0 range 4 .. 6;
      ARPE at 0 range 7 .. 7;
      CKD  at 0 range 8 .. 9;
      Res2 at 0 range 10 .. 15;
   end record;
   
   
   type SMCR_Register is record   -- slave mode control register
      SMS   : Bits_3; -- Slave mode selection
      Res1 : Reserved (3 .. 3) := (others => 0);
      TS    : Bits_3; -- Trigger selection
      MSM   : Bits_1; -- Master/slave mode
      Res2 : Reserved (8 .. 15) := (others => 0);
   end record;
   
   for SMCR_Register use record
      SMS  at 0 range 0 .. 2;
      Res1 at 0 range 3 .. 3;
      TS   at 0 range 4 .. 6;
      MSM  at 0 range 7 .. 7;
      Res2 at 0 range 8 .. 15;
   end record;
   
   
   subtype DIER_Register is STM32F4.O7xx.Timers.T2_5.DIER_Register;
   
        
   type SR_Register is record  -- status register 
      UIF,                -- Update interrupt flag
      CC1IF,              -- CC1IF
      CC2IF    : Bits_1;  -- CC2IF
      Res1     : Reserved (3 .. 5) := (others => 0);
      TIF      : Bits_1;  -- Trigger interrupt flag
      Res3     : Reserved (7 .. 8) := (others => 0);
      CC1OF,              -- Capture/Compare 1 overcapture flag
      CC2OF    : Bits_1;  -- Capture/Compare 2 overcapture flag
      Res2     : Reserved (11 .. 15) := (others => 0);
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
      Res2   at 0 range 11 .. 15;
   end record;
   
   
   type EGR_Register is record  -- event generation register
      UG,                 -- Update generation
      CC1G,               -- Capture/Compare 1 generation
      CC2G     : Bits_1;  -- Capture/Compare 2 generation
      Res1     : Reserved (3 .. 5) := (others => 0);
      TG       : Bits_1;  -- Trigger generation
      Res2     : Reserved (7 .. 15) := (others => 0);
   end record;
   
   for EGR_Register use record
      UG    at 0 range 0 .. 0;	
      CC1G  at 0 range 1 .. 1;	
      CC2G  at 0 range 2 .. 2;
      Res1  at 0 range 3 .. 5;
      TG    at 0 range 6 .. 6;	
      Res2  at 0 range 7 .. 15;
   end record;
   
   subtype CCMR1_Capture_Register is
     STM32F4.O7xx.Timers.T1_8.CCMR1_Capture_Register;
   
   
   type CCER_Register is record  -- capture/compare enable register 
      CC1E,               -- Capture/Compare 1 output enable
      CC1P      : Bits_1; -- Capture/Compare 1 output polarity
      Res1      : Reserved (2 .. 2) := (others => 0);
      CC1NP,              -- Capture/Compare 1 complementary output polarity
      CC2E,               -- Capture/Compare 2 output enable
      CC2P      : Bits_1; -- Capture/Compare 2 output polarity
      Res3      : Reserved (6 .. 6) := (others => 0);
      CC2NP     : Bits_1; -- Capture/Compare 2 complementary output polarity
      Res2      : Reserved (8 .. 15) := (others => 0);
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
      Res2   at 0 range 8 .. 15;
   end record;
   
   
   subtype CNT_Register is Half_Word;  -- counter
   subtype PSC_Register is Half_Word;  -- prescaler 
   subtype ARR_Register is Half_Word;  -- auto-reload register
   subtype CCR_Register is Half_Word;  -- capture/compare register
   
   

   
   -----------------------------------------------------------
   -- this is the top of the timer / counter definition     --
   -----------------------------------------------------------
   
   type Timer_Register is record
      CR1         : Cr1_Register;   -- control register 1 
      Res0        : Bits_16;
      Reserved2   : Bits_32x1;
      SMCR        : SMCR_Register;  -- slave mode control register
      Res2        : Bits_16;
      DIER        : DIER_Register;  -- DMA/interrupt enable register
      Res3        : Bits_16;
      SR          : SR_Register;    -- status register 
      Res4        : Bits_16;
      EGR         : EGR_Register;   -- event generation register
      Res5        : Bits_16;
      CCMR1       : CCMR1_Capture_Register; -- capt/compare mode register 1
      Res6        : Bits_16;
      Reserved0   : Bits_32x1;
      CCER        : CCER_Register;  -- capture/compare enable register 
      Res8        : Bits_16;
      CNT         : CNT_Register;   -- counter
      Res9        : Bits_16;
      PSC         : PSC_Register;   -- prescaler 
      Res10       : Bits_16;
      ARR         : ARR_Register;   -- auto-reload register
      Res11       : Bits_16;
      Reserved1   : Bits_32x1;
      CCR1        : CCR_Register;  -- capture/compare register 1
      Res13       : Bits_16;
      CCR2        : CCR_Register;  -- capture/compare register 1
      Res14       : Bits_16;
   end record;
   
   for Timer_Register use record
      CR1   at 0   range 0 .. 15;
      Res0  at 2   range 0 .. 15;
      Reserved2 at 4   range 0 .. 31;
      SMCR  at 8   range 0 .. 15;
      Res2  at 10  range 0 .. 15;
      DIER  at 12  range 0 .. 15;
      Res3  at 14  range 0 .. 15;
      SR    at 16  range 0 .. 15;
      Res4  at 18  range 0 .. 15;
      EGR   at 20  range 0 .. 15;
      Res5  at 22  range 0 .. 15;
      CCMR1 at 24  range 0 .. 15;
      Res6  at 26  range 0 .. 15;
      Reserved0 at 28 range 0 .. 31;
      CCER  at 32  range 0 .. 15;
      Res8  at 34  range 0 .. 15;
      CNT   at 36  range 0 .. 15;
      Res9  at 38  range 0 .. 15;
      PSC   at 40  range 0 .. 15;
      Res10 at 42  range 0 .. 15;
      ARR   at 44  range 0 .. 15;
      Res11 at 46  range 0 .. 15;
      Reserved1 at 48 range 0 .. 31;
      CCR1  at 52  range 0 .. 15;
      Res13 at 54  range 0 .. 15;
      CCR2  at 56  range 0 .. 15;
      Res14 at 58  range 0 .. 15;
   end record;
   
end STM32F4.O7xx.Timers.T9_12;

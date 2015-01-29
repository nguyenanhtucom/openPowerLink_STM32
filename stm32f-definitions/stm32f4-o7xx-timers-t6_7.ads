------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                 S T M 32 F 4 . O7xx . T i m e r s . T 6 _ 7              --
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
--  chapter 20.4   TIM6&TIM7 registers       RM0090 Reference manual



pragma Restrictions (No_Elaboration_Code);

package STM32F4.O7xx.Timers.T6_7 is
   
   type Cr1_Register  is record   --   control register 1 
      CEN,           -- Counter enable
      UDIS,          -- Update disable
      URS,           -- Update request source
      OPM  : Bits_1; -- One pulse mode
      Res1 : Reserved (4 .. 6) := (others => 0);
      ARPE : Bits_1; -- Auto-reload preload enable
      Res2 : Reserved (8 .. 31) := (others => 0);
   end record;
   
   for Cr1_Register use record
      CEN  at 0 range 0 .. 0;
      UDIS at 0 range 1 .. 1;
      URS  at 0 range 2 .. 2;
      OPM  at 0 range 3 .. 3;
      Res1 at 0 range 4 .. 6;
      ARPE at 0 range 7 .. 7;
      Res2 at 0 range 8 .. 31;
   end record;
   --pragma Atomic (Cr1_Register);
   
   
   type Cr2_Register is record
      Res1  : Reserved (0 .. 3) := (others => 0);
      MMS   : Bits_3; -- Master mode selection
      Res2  : Reserved (7 .. 31) := (others => 0);
   end record;
   
   for Cr2_Register use record
      Res1  at 0 range 0 .. 3;
      MMS   at 0 range 4 .. 6;
      Res2  at 0 range 7 .. 31;
   end record;
   
   
   type DIER_Register is record   --  DMA/interrupt enable register
      UIE   : Bits_1;    -- Update interrupt enable
      Res1  : Reserved (1 .. 7) := (others => 0);
      UDE   : Bits_1;    -- Update DMA request enable
      Res2  : Reserved (9 .. 31) := (others => 0);
   end record;
  
   for DIER_Register use record
      UIE    at 0 range 0 .. 0;
      Res1   at 0 range 1 .. 7;
      UDE    at 0 range 8 .. 8; 
      Res2   at 0 range 9 .. 31;
   end record;
   --pragma Atomic (DIER_Register);
   
   type SR_Register is record  -- status register 
      UIF      : Bits_1;  -- Update interrupt flag
      Res2     : Reserved (1 .. 31) := (others => 0);
   end record;
   
   for SR_Register use record
      UIF    at 0 range 0 .. 0;	
      Res2   at 0 range 1 .. 31;
   end record;
   
   
   type EGR_Register is record  -- event generation register
      UG        : Bits_1;  -- Update generation
      Res2      : Reserved (1 .. 31) := (others => 0);
   end record;
   
   for EGR_Register use record
      UG    at 0 range 0 .. 0;
      Res2  at 0 range 1 .. 31;
   end record;
   
   
   subtype CNT_Register is Half_Word;  -- counter
   subtype PSC_Register is Half_Word;  -- prescaler 
   subtype ARR_Register is Half_Word;  -- auto-reload register
   
   type RESERVED0_Register is array (0 .. 2) of aliased Bits_32x1;
     
   type Timer_Register is record
      CR1         : Cr1_Register;  -- control register 1 
      CR2         : Cr2_Register;  -- control register 2 
      Res1        : Bits_32x1;
      DIER        : DIER_Register;  -- DMA/interrupt enable register
      SR          : SR_Register;    -- status register 
      EGR         : EGR_Register;   -- event generation register
      Res0        : RESERVED0_Register;
      CNT         : CNT_Register;   -- counter
      PSC         : PSC_Register;   -- prescaler 
      ARR         : ARR_Register;   -- auto-reload register
   end record;
   
   for Timer_Register use record
      CR1   at 0   range 0 .. 31;
      CR2   at 4   range 0 .. 31;
      Res1  at 8   range 0 .. 31;
      DIER  at 12  range 0 .. 31;
      SR    at 16  range 0 .. 31;
      EGR   at 20  range 0 .. 31;
      Res0  at 24  range 0 .. 95;
      CNT   at 36  range 0 .. 31;
      PSC   at 40  range 0 .. 31;
      ARR   at 44  range 0 .. 31;
   end record;
   
end STM32F4.O7xx.Timers.T6_7;

------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                        S T M 32 F 4 . O7xx . W w d                       --
--                                                                          --
--                           H a r d w ar e  S p e c                        --
--                                                                          --
--                     Copyright (C) 2015, Jan de Kruyf                     --
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
--  chapter 22.6    WWDG registers       RM0090 Reference manual
--

pragma Restrictions (No_Elaboration_Code);

package STM32F4.o7xx.Wwdg is

   --------------------------------------------------
   -- constants for use with the dma definitions   --
   --  in the child packages                       --
   --------------------------------------------------

   Enable    : constant Bits_1 := 1;
   On        : constant Bits_1 := 1;
   Off       : constant Bits_1 := 0;
   Ckcc_Div1 : constant Bits_2 := 0;
   Ckcc_Div2 : constant Bits_2 := 1;
   Ckcc_Div4 : constant Bits_2 := 2;
   Ckcc_Div8 : constant Bits_2 := 3;

   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------

   type CR_Register is record
      -- WWDG Control register
      Res0	: Reserved (8 .. 31) := (others => 0);
      WDGA	: Bits_1;
      --  Activation bit
      --- On, Off
      T		: Bits_7;
      --  7-bit counter
   end record;

   for CR_Register use record
      Res0 at 0 range 8 .. 31;
      WDGA at 0 range 7 ..  7;
      T    at 0 range 0 ..  6;
   end record;
   
   
   type CFR_Register is record
      -- WWDG Configuration register
      Res0	: Reserved (10 .. 31) := (others => 0);
      EWI	: Bits_1;
      --  Early wakeup interrupt
      --- Enable, off
      WDGTB	: Bits_2;
      --  time base of the prescaler
      --- Ckcc_Div1, Ckcc_Div2, Ckcc_Div4, Ckcc_Div8
      W		: Bits_7;
      --  7-bit window value
   end record;

   for CFR_Register use record
      Res0  at 0 range 10 .. 31;
      EWI   at 0 range  9 ..  9;
      WDGTB at 0 range  7 ..  8;
      W     at 0 range  0 ..  6;
   end record;
   
   
   type SR_Register is record
      -- WWDG Status register
      Res0	: Reserved (1 .. 31) := (others => 0);
      EWIF	: Bits_1;
      --  Early wakeup interrupt flag
      --- On, Off
   end record;

   for SR_Register use record
      Res0 at 0 range 1 .. 31;
      EWIF at 0 range 0 ..  0;
   end record;
   
   
   type WWDG_TypeDef is record
      CR	: CR_Register;
      CFR	: CFR_Register;
      SR	: SR_Register;
   end record;
   pragma Convention (C_Pass_By_Copy, WWDG_TypeDef);

   for WWDG_TypeDef use record
      CR  at 0 range 0 .. 31;
      CFR at 4 range 0 .. 31;
      SR  at 8 range 0 .. 31;
   end record;

end STM32F4.o7xx.Wwdg;

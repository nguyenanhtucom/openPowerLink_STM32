------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                        S T M 32 F 4 . O7xx . I w d g                     --
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
--  chapter 21.4    IWDG registers       RM0090 Reference manual
--

pragma Restrictions (No_Elaboration_Code);

package STM32F4.o7xx.Iwdg is

   --------------------------------------------------
   -- constants for use with the iwdg definitions  --
   --  in the client packages                      --
   --------------------------------------------------
   
   Open          : constant Bits_1 := 0;
   Locked        : constant Bits_1 := 1;
   Div_4         : constant Bits_3 := 0;
   Div_8         : constant Bits_3 := 1;
   Div_16        : constant Bits_3 := 2;
   Div_32        : constant Bits_3 := 3;
   Div_64        : constant Bits_3 := 4;
   Div_128       : constant Bits_3 := 5;
   Div_256       : constant Bits_3 := 6;
   Init          : constant Bits_16 := 16#CCCC#;
   Update        : constant Bits_16 := 16#AAAA#;
   Access_En     : constant Bits_16 := 16#5555#;
   
   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------
   
   type KR_Register is record
      -- Key register 
      Res0	: Reserved (16 .. 31) := (others => 0);
      Key       : Bits_16;
      --  Key value (write only, read 0000h)
      --- Init, Update, Access_En
   end record;
   
   for KR_Register use record
      Res0 at 0 range 16 .. 31;
      Key  at 0 range 0 ..  15;
   end record;
   
   
   type PR_Register is record
      -- Prescaler register
      Res0	: Reserved (3 .. 31) := (others => 0);
      Pr        : Bits_3;
      --  Prescaler divider
      --- Div_4, Div_8, Div_16, Div_32, Div_64, Div_128, Div_256
   end record;
   
   for PR_Register use record
      Res0 at 0 range 3 .. 31;
      Pr   at 0 range 0 ..  2;
   end record;
   
   
   type RLR_Register is record
      -- Reload register
      Res0	: Reserved (12 .. 31) := (others => 0);
      Rl        : Bits_12;
      --  Watchdog counter reload value
      --- ?
   end record;
   
   for RLR_Register use record
      Res0 at 0 range 12 .. 31;
      Rl   at 0 range 0  .. 11;
   end record;
   
   
   type SR_Register is record
      -- Status register 
      Res0	: Reserved (2 .. 31) := (others => 0);
      Rvu       : Bits_1;
      --  Watchdog counter reload value update
      --- Open, Locked
      Pvu       : Bits_1;
      --  Watchdog prescaler value update
      --- Open, Locked
   end record;
   
   for SR_Register use record
      Res0 at 0 range 2 .. 31;
      Rvu  at 0 range 1 ..  1;
      Pvu  at 0 range 0 ..  0;
   end record;
   
   
   type IWDG_TypeDef is record
      KR     : KR_Register;
      PR     : PR_Register;
      RLR    : RLR_Register;
      SR     : SR_Register;
   end record;
   
   for IWDG_TypeDef use record
      KR     at 0  range 0 .. 31;
      PR     at 4  range 0 .. 31;
      RLR    at 8  range 0 .. 31;
      SR     at 12 range 0 .. 31;
   end record;
   
end STM32F4.o7xx.Iwdg;

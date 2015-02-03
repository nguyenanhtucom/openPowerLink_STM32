------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                      S T M 32 F 4 . O7xx . S y s c f g                   --
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
--  chapter 9.3    SYSCFG registers       RM0090 Reference manual
--

pragma Restrictions (No_Elaboration_Code);

package STM32F4.O7xx.Syscfg is

   ---------------------------------------------------
   -- constants for use with the syscfg definitions --
   --  in the client packages                       --
   ---------------------------------------------------
   
   Main_Flash   : constant Bits_2 := 0;
   System_Flash : constant Bits_2 := 1;
   Fsmc_Bank1   : constant Bits_2 := 2;
   Sram1        : constant Bits_2 := 3;
   MII          : constant Bits_1 := 0;
   RMII         : constant Bits_1 := 1;
   Ready        : constant Bits_1 := 1;
   Off          : constant Bits_1 := 0;
   Pd_Mode      : constant Bits_1 := 0;
   Enabled      : constant Bits_1 := 1;
   Pa           : constant Bits_4 := 0;
   Pb           : constant Bits_4 := 1;
   Pc           : constant Bits_4 := 2;
   Pd           : constant Bits_4 := 3;
   Pe           : constant Bits_4 := 4;
   Pf           : constant Bits_4 := 5;
   Pg           : constant Bits_4 := 6;
   Ph           : constant Bits_4 := 7;
   Pi           : constant Bits_4 := 8;
   
   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------
   
   type MEMRMP_Register is record
      --  memory remap register 
      Res0     : Reserved (2 .. 31) := (others => 0);
      MEM_MODE : Bits_2;
      --  Memory mapping selection
      --- Main_Flash, System_Flash, Fsmc_Bank1, Sram1
   end record;
   
   for MEMRMP_Register use record
      Res0      at 0 range 2 .. 31;
      MEM_MODE  at 0 range 0 ..  1;
   end record;
   
   
   type PMC_Register is record
      --  peripheral mode configuration register
      Res0         : Reserved (24 .. 31) := (others => 0);
      MII_RMII_SEL : Bits_1;
      --  Ethernet PHY interface selection
      --- MII, RMII
      Res1         : Reserved (0 .. 22) := (others => 0);
   end record;
   
   for PMC_Register use record
      Res0         at 0 range 24 .. 31;
      MII_RMII_SEL at 0 range 23 .. 23;
      Res1         at 0 range  0 .. 22;
   end record;
   
   
   type CMPCR_Register is record
      -- Compensation cell control register 
      Res0     : Reserved (9 .. 31) := (others => 0);
      READY    : Bits_1;
      --  Compensation cell ready flag
      --- Ready, Off
      Res1     : Reserved (1 ..  7) := (others => 0);
      CMP_PD   : Bits_1;
      -- Compensation cell power-down
      --- Pd_Mode, enabled
   end record;
   
   for CMPCR_Register use record
      Res0     at 0 range  9 .. 31;
      READY    at 0 range  8 ..  8;
      Res1     at 0 range  1 ..  7;
      CMP_PD   at 0 range  0 ..  0;
   end record;
   
   type EXTIC1_Array is array (0 ..  3) of Bits_4 with Pack, Size => 16;
   type EXTIC2_Array is array (4 ..  7) of Bits_4 with Pack, Size => 16;
   type EXTIC3_Array is array (8 .. 11) of Bits_4 with Pack, Size => 16;
   type EXTIC4_Array is array (12.. 15) of Bits_4 with Pack, Size => 16;
   
   type RESERVED_Register is array (0 .. 1) of aliased Bits_32x1;
   
   
   type SYSCFG_TypeDef is record
      MEMRMP : MEMRMP_Register;
      PMC    : PMC_Register;
      EXTIC1 : EXTIC1_Array;
      -- array of ext. interrupts 0 .. 3,
      --- Pa, Pb, Pc, Pd, Pe, Pf, Pg, Ph, Pi
      Res1   : Bits_16;
      EXTIC2 : EXTIC2_Array;
      -- array of ext. interrupts 4 .. 7,
      --- Pa, Pb, Pc, Pd, Pe, Pf, Pg, Ph, Pi
      Res2   : Bits_16;
      EXTIC3 : EXTIC3_Array;
      -- array of ext. interrupts 8 .. 11,
      --- Pa, Pb, Pc, Pd, Pe, Pf, Pg, Ph, Pi
      Res3   : Bits_16;
      EXTIC4 : EXTIC4_Array;
      -- array of ext. interrupts 12 .. 15,
      --- Pa, Pb, Pc, Pd, Pe, Pf, Pg, Ph, Pi
      Res4   : Bits_16;
      Res5   : RESERVED_Register;
      CMPCR  : CMPCR_Register;
   end record;
   
   for SYSCFG_TypeDef use record
      MEMRMP at   0 range 0 .. 31;
      PMC    at   4 range 0 .. 31;
      EXTIC1 at   8 range 0 .. 15;
      Res1   at  10 range 0 .. 15;
      EXTIC2 at  12 range 0 .. 15;
      Res2   at  14 range 0 .. 15;
      EXTIC3 at  16 range 0 .. 15;
      Res3   at  18 range 0 .. 15;
      EXTIC4 at  20 range 0 .. 15;
      Res4   at  22 range 0 .. 15;
      Res5   at  24 range 0 .. 63;
      CMPCR  at  32 range 0 .. 31;
   end record;
   
end STM32F4.O7xx.Syscfg;

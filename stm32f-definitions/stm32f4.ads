------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides type definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

pragma Restrictions (No_Elaboration_Code);

with Interfaces;

package STM32F4 is

   type Word      is new Interfaces.Unsigned_32;  -- for shift/rotate
   type Half_Word is new Interfaces.Unsigned_16;  -- for shift/rotate
   type Byte      is new Interfaces.Unsigned_8;   -- for shift/rotate

   type Bits_1  is mod 2**1 with Size => 1;
   type Bits_2  is mod 2**2 with Size => 2;
   type Bits_3  is mod 2**3 with Size => 3;
   type Bits_4  is mod 2**4 with Size => 4;
   type Bits_5  is mod 2**5 with Size => 5;
   type Bits_6  is mod 2**6 with Size => 6;
   type Bits_7  is mod 2**7 with Size => 7;
   type Bits_9  is mod 2**9 with Size => 9;
   type Bits_11 is mod 2**11 with Size => 11;
   type Bits_12 is mod 2**12 with Size => 12;
   type Bits_13 is mod 2**13 with Size => 13;
   type Bits_14 is mod 2**14 with Size => 14;
   type Bits_15 is mod 2**15 with Size => 15;
   type Bits_16 is mod 2**16 with Size => 16;
   type Bits_31 is mod 2**31 with Size => 31;
   type Bits_32 is mod 2**32 with Size => 32;
   type Bits_48 is mod 2**48 with Size => 48;
   
   type Bits_32x1 is array (0 .. 31) of Bits_1 with Pack, Size => 32;
   type Bits_16x1 is array (0 .. 15) of Bits_1 with Pack, Size => 16;
   type Bits_16x2 is array (0 .. 15) of Bits_2 with Pack, Size => 32;
   type Bits_8x4  is array (0 ..  7) of Bits_4 with Pack, Size => 32;
   
   type Reserved_Bit is range 0 .. 1 with Size => 1; 
   type Reserved is array (Natural range <>) of Reserved_Bit 
     with Component_Size => 1;

   --  Define address bases for the various system components

   Peripheral_Base : constant := 16#4000_0000#;

   APB1_Peripheral_Base : constant := Peripheral_Base;
   APB2_Peripheral_Base : constant := Peripheral_Base + 16#0001_0000#;
   AHB1_Peripheral_Base : constant := Peripheral_Base + 16#0002_0000#;
   AHB2_Peripheral_Base : constant := Peripheral_Base + 16#1000_0000#;

   GPIOA_Base           : constant := AHB1_Peripheral_Base + 16#0000#;
   GPIOB_Base           : constant := AHB1_Peripheral_Base + 16#0400#;
   GPIOC_Base           : constant := AHB1_Peripheral_Base + 16#0800#;
   GPIOD_Base           : constant := AHB1_Peripheral_Base + 16#0C00#;

   --FLASH_Base           : constant := AHB1_Peripheral_Base + 16#3C00#;
   RCC_Base             : constant := AHB1_Peripheral_Base + 16#3800#;

   PWR_Base             : constant := APB1_Peripheral_Base + 16#7000#;

   --USART1_Base          : constant := APB2_Peripheral_Base + 16#1000#;
   SYSCFG_Base          : constant := APB2_Peripheral_Base + 16#3800#;
   EXTI_Base            : constant := APB2_Peripheral_Base + 16#3C00#;

end STM32F4;

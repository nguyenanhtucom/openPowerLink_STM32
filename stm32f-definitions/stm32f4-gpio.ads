------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                         S T M 32 F 4 . O7xx . D m a                      --
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
--                        This file is a modified                           --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides register definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

pragma Restrictions (No_Elaboration_Code);

package STM32F4.GPIO is

   --  MODER constants
   Input        : constant Bits_2 := 0;
   Output       : constant Bits_2 := 1;
   Alt_Func     : constant Bits_2 := 2;
   Analog       : constant Bits_2 := 3;

   --  OTYPER constants
   Push_Pull    : constant Bits_1 := 0; -- Push/pull
   Open_Drain   : constant Bits_1 := 1; -- Open drain

   --  OSPEEDR constants
   Speed_2MHz   : constant Bits_2 := 0; -- Low speed
   Speed_25MHz  : constant Bits_2 := 1; -- Medium speed
   Speed_50MHz  : constant Bits_2 := 2; -- Fast speed
   Speed_100MHz : constant Bits_2 := 3; -- High speed on 30pF, 80MHz on 15

   --  PUPDR constants
   No_Pull      : constant Bits_2 := 0;
   Pull_Up      : constant Bits_2 := 1;
   Pull_Down    : constant Bits_2 := 2;

   --  AFL constants
   AF_USART1    : constant Bits_4 := 7;

   --  Reset constants
   GPIOA_Reset       : constant Word := 16#A800_0000#;
   GPIOB_Reset       : constant Word := 16#0000_0280#;
   GPIO_Others_Reset : constant Word := 16#0000_0000#;
   
   --  pin constants
   Bs0, Dr0    : constant Natural := 0;
   Bs1, Dr1    : constant Natural := 1;
   Bs2, Dr2    : constant Natural := 2;
   Bs3, Dr3    : constant Natural := 3;
   Bs4, Dr4    : constant Natural := 4;
   Bs5, Dr5    : constant Natural := 5;
   Bs6, Dr6    : constant Natural := 6;
   Bs7, Dr7    : constant Natural := 7;
   Bs8, Dr8    : constant Natural := 8;
   Bs9, Dr9    : constant Natural := 9;
   Bs10, Dr10  : constant Natural := 10;
   Bs11, Dr11  : constant Natural := 11;
   Bs12, Dr12  : constant Natural := 12;
   Bs13, Dr13  : constant Natural := 13;
   Bs14, Dr14  : constant Natural := 14;
   Bs15, Dr15  : constant Natural := 15;
   
   Br0   : constant Natural := 16; 
   Br1   : constant Natural := 17;
   Br2   : constant Natural := 18;
   Br3   : constant Natural := 19;
   Br4   : constant Natural := 20;
   Br5   : constant Natural := 21;
   Br6   : constant Natural := 22;
   Br7   : constant Natural := 23;
   Br8   : constant Natural := 24;
   Br9   : constant Natural := 25;
   Br10  : constant Natural := 26;
   Br11  : constant Natural := 27;
   Br12  : constant Natural := 28;
   Br13  : constant Natural := 29;
   Br14  : constant Natural := 30;
   Br15  : constant Natural := 31;
   
   --  AF constants
   Af0    : constant Bits_4 := 0;
   Af1    : constant Bits_4 := 1;
   Af2    : constant Bits_4 := 2;
   Af3    : constant Bits_4 := 3;
   Af4    : constant Bits_4 := 4;
   Af5    : constant Bits_4 := 5;
   Af6    : constant Bits_4 := 6;
   Af7    : constant Bits_4 := 7;
   Af8    : constant Bits_4 := 8;
   Af9    : constant Bits_4 := 9;
   Af10   : constant Bits_4 := 10;
   Af11   : constant Bits_4 := 11;
   Af12   : constant Bits_4 := 12;
   Af13   : constant Bits_4 := 13;
   Af14   : constant Bits_4 := 14;
   Af15   : constant Bits_4 := 15;
   
   
   
   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------
   
   -- type Bits_32x1 is array (0 .. 31) of Bits_1 with Pack, Size => 32;
   -- type Bits_16x2 is array (0 .. 15) of Bits_2 with Pack, Size => 32;
   -- type Bits_8x4  is array (0 ..  7) of Bits_4 with Pack, Size => 32;
   
   subtype Bit_Array is  Bits_32x1;
   -- for temp vars
   -- use Bs.. constants to set (1) and reset (0) individual bits
   -- then 'or' or 'and' with ODR, with unchecked conversion
   subtype Set_reset_Bit_Array is  Bits_32x1;
   -- for temp vars
   -- use Br.. and Bs.. constants to se and reset individual bits
   -- then copy with unchecked conversion to BSRR
   
   type AFRL_Register is array (0 ..  7) of Bits_4 with Pack, Size => 32;
   type AFRH_Register is array (8 .. 15) of Bits_4 with Pack, Size => 32;
   
   type GPIO_Register is record
      MODER   : Bits_16x2;  
      --  mode register
      --- Input, Output, Alt_Func, Analog
      OTYPER  : Bits_16x1;  
      --  output type register
      --- Push_Pull, Open_Drain
      Res0    : Bits_16;
      OSPEEDR : Bits_16x2;  
      --  output speed register
      --- Speed_2MHz, Speed_25MHz, Speed_50MHz, Speed_100MHz
      PUPDR   : Bits_16x2;  
      --  pull-up/pull-down register
      --- No_Pull, Pull_Up, Pull_Down 
      IDR     : Bits_16;       
      --  input data register
      Res1    : Bits_16;
      ODR     : Bits_16;       
      --  output data register
      Res2    : Bits_16;
      BSRR    : Word;       
      --  bit set/reset register
      LCKR    : Bits_16;       
      --  configuration lock register
      Res3    : Bits_16;
      AFRL    : AFRL_Register;   
      --  alternate function low register
      --- Af0 .. Af15
      AFRH    : AFRH_Register;   
      --  alternate function high register
      --- Af0 .. Af15
   end record;

   for GPIO_Register use record
      MODER   at 0  range 0 .. 31;
      OTYPER  at 4  range 0 .. 15;
      Res0    at 6  range 0 .. 15;
      OSPEEDR at 8  range 0 .. 31;
      PUPDR   at 12 range 0 .. 31;
      IDR     at 16 range 0 .. 15;
      Res1    at 18 range 0 .. 15;
      ODR     at 20 range 0 .. 15;
      Res2    at 22 range 0 .. 15;
      BSRR    at 24 range 0 .. 31;
      LCKR    at 28 range 0 .. 15;
      Res3    at 30 range 0 .. 15;
      AFRL    at 32 range 0 .. 31;
      AFRH    at 36 range 0 .. 31;
   end record;

end STM32F4.GPIO;

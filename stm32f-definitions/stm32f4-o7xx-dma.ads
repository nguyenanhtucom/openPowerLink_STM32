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
------------------------------------------------------------------------------

--  This file provides type definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.
--
--  chapter 10.5    DMA registers       RM0090 Reference manual
--

pragma Restrictions (No_Elaboration_Code);

-- with System;

package STM32F4.o7xx.Dma is
   
   --------------------------------------------------
   -- constants for use with the dma definitions   --
   --  in the child packages                       --
   --------------------------------------------------

   
   
   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------
   
   type LISR_Register is record
      -- Bit definition for DMA low interrupt status register --
      Res0    : Reserved (28 .. 31) := (others => 0);
      TCIF3   : Bits_1;
      -- 
      HTIF3   : Bits_1;
      -- 
      TEIF3   : Bits_1;
      -- 
      DMEIF3  : Bits_1;
      -- 
      Res1    : Reserved (23 .. 23) := (others => 0);
      FEIF3   : Bits_1;
      -- 
      TCIF2   : Bits_1;
      -- 
      HTIF2   : Bits_1;
      -- 
      TEIF2   : Bits_1;
      -- 
      DMEIF2  : Bits_1;
      -- 
      Res2    : Reserved (17 .. 17) := (others => 0);
      FEIF2   : Bits_1;
      -- 
      Res3    : Reserved (12 .. 15) := (others => 0);
      TCIF1   : Bits_1;
      -- 
      HTIF1   : Bits_1;
      -- 
      TEIF1   : Bits_1;
      -- 
      DMEIF1  : Bits_1;
      -- 
      Res4    : Reserved (7 .. 7) := (others => 0);
      FEIF1   : Bits_1;
      -- 
      TCIF0   : Bits_1;
      -- 
      HTIF0   : Bits_1;
      -- 
      TEIF0   : Bits_1;
      -- 
      DMEIF0  : Bits_1;
      -- 
      Res5    : Reserved (1 .. 1) := (others => 0);
      FEIF0   : Bits_1;
      -- 
   end record;
   
   for LISR_Register use record
      Res0    at 0 range 28 .. 31;
      TCIF3   at 0 range 27 .. 27;
      HTIF3   at 0 range 26 .. 26;
      TEIF3   at 0 range 25 .. 25;
      DMEIF3  at 0 range 24 .. 24;
      Res1    at 0 range 23 .. 23;
      FEIF3   at 0 range 22 .. 22;
      TCIF2   at 0 range 21 .. 21;
      HTIF2   at 0 range 20 .. 20;
      TEIF2   at 0 range 19 .. 19;
      DMEIF2  at 0 range 18 .. 18;
      Res2    at 0 range 17 .. 17;
      FEIF2   at 0 range 16 .. 16;
      Res3    at 0 range 12 .. 15;
      TCIF1   at 0 range 11 .. 11;
      HTIF1   at 0 range 10 .. 10;
      TEIF1   at 0 range  9 ..  9;
      DMEIF1  at 0 range  8 ..  8;
      Res4    at 0 range  7 ..  7;
      FEIF1   at 0 range  6 ..  6;
      TCIF0   at 0 range  5 ..  5;
      HTIF0   at 0 range  4 ..  4;
      TEIF0   at 0 range  3 ..  3;
      DMEIF0  at 0 range  2 ..  2;
      Res5    at 0 range  1 ..  1;
      FEIF0   at 0 range  0 ..  0;
   end record;
   
   
   type HISR_Register is record
      
   end record;
   
   for HISR_Register use record
      
   end record;
   
   
   type LIFCR_Register is record
      
   end record;
   
   for LIFCR_Register use record
      
   end record;
   
   
   type HIFCR_Register is record
      
   end record;
   
   for HIFCR_Register use record
      
   end record;
   
   
   type CR_Register is record
      
   end record;
   
   for CR_Register use record
      
   end record;
   
   
   type NDTR_Register is record
      
   end record;
   
   for NDTR_Register use record
      
   end record;
   
   
   type PAR_Register is record
      
   end record;
   
   for PAR_Register use record
      
   end record;
   
   
   type M0AR_Register is record
      
   end record;
   
   for M0AR_Register use record
      
   end record;
   
   
   type M1AR_Register is record
      
   end record;
   
   for M1AR_Register use record
      
   end record;
   
   
   type FCR_Register is record
      
   end record;
   
   for FCR_Register use record
      
   end record;
   
   
   
   type DMA_Stream_TypeDef is record
      -- DMA controller  --
      CR   : CR_Register;
      -- DMA stream x configuration register
      NDTR : NDTR_Register;
      -- DMA stream x number of data register
      PAR  : PAR_Register;
      -- DMA stream x peripheral address register
      M0AR : M0AR_Register;
      -- DMA stream x memory 0 address register
      M1AR : M1AR_Register;
      -- DMA stream x memory 1 address register
      FCR  : FCR_Register;
      -- DMA stream x FIFO control register
      end record;
   pragma Convention (C_Pass_By_Copy, DMA_Stream_TypeDef);  -- dma.h:329
   
   for DMA_Stream_TypeDef use record
      CR   at   0  range 0 ..    31;    -- starts at 16 for the first one
      NDTR at   4  range 0 ..    31;
      PAR  at   8  range 0 ..    31;
      M0AR at   12 range 0 ..    31;
      M1AR at   16 range 0 ..    31;
      FCR  at   20 range 0 ..    31;
   end record;
   
   
   type DMA_TypeDef is record
      LISR  : LISR_Register;
      -- DMA low interrupt status register
      HISR  : HISR_Register;
      -- DMA high interrupt status register
      LIFCR : LIFCR_Register;
      -- DMA low interrupt flag clear register
      HIFCR : HIFCR_Register;
      -- DMA high interrupt flag clear register
      S0    : DMA_Stream_TypeDef;
      -- DMA stream 0
      S1    : DMA_Stream_TypeDef;
      -- DMA stream 1
      S2    : DMA_Stream_TypeDef;
      -- DMA stream 2
      S3    : DMA_Stream_TypeDef;
      -- DMA stream 3
      S4    : DMA_Stream_TypeDef;
      -- DMA stream 4
      S5    : DMA_Stream_TypeDef;
      -- DMA stream 5
      S6    : DMA_Stream_TypeDef;
      -- DMA stream 6
      S7    : DMA_Stream_TypeDef;
      -- DMA stream 7
   end record;
   pragma Convention (C_Pass_By_Copy, DMA_TypeDef);  -- dma.h:337
   
   for DMA_TypeDef use record
      LISR  at   0  range 0 ..    31;
      HISR  at   4  range 0 ..    31;
      LIFCR at   8  range 0 ..    31;
      HIFCR at   12 range 0 ..    31;
      S0    at   16 range 0 ..    191;
      S1    at   40 range 0 ..    191;
      S2    at   64 range 0 ..    191;
      S3    at   88 range 0 ..    191;
      S4    at  112 range 0 ..    191;
      S5    at  136 range 0 ..    191;
      S6    at  160 range 0 ..    191;
      S7    at  184 range 0 ..    191;
   end record;
   
end STM32F4.o7xx.Dma;

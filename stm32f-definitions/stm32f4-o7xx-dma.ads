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

package STM32F4.o7xx.Dma is

   --------------------------------------------------
   -- constants for use with the dma definitions   --
   --  in the client packages                      --
   --------------------------------------------------

   Xfer_Complete  : constant Bits_1 := 1;
   On             : constant Bits_1 := 1;
   Off            : constant Bits_1 := 0;
   Half_Xfer      : constant Bits_1 := 1;
   Xfer_Error     : constant Bits_1 := 1;
   Dm_Error       : constant Bits_1 := 1;
   Fifo_Error     : constant Bits_1 := 1;
   Clearit        : constant Bits_1 := 1;
   Sel_Ch0        : constant Bits_3 := 0;
   Sel_Ch1        : constant Bits_3 := 1;
   Sel_Ch2        : constant Bits_3 := 2;
   Sel_Ch3        : constant Bits_3 := 3;
   Sel_Ch4        : constant Bits_3 := 4;
   Sel_Ch5        : constant Bits_3 := 5;
   Sel_Ch6        : constant Bits_3 := 6;
   Sel_Ch7        : constant Bits_3 := 7;
   Single         : constant Bits_2 := 0;
   Incr_4beats    : constant Bits_2 := 1;
   Incr_8beats    : constant Bits_2 := 2;
   Incr_16beats   : constant Bits_2 := 3;
   Mem0           : constant Bits_1 := 0;
   Mem1           : constant Bits_1 := 1;
   Low            : constant Bits_2 := 0;
   Medium         : constant Bits_2 := 1;
   High           : constant Bits_2 := 2;
   Very_High      : constant Bits_2 := 3;
   Psize          : constant Bits_1 := 0;
   Bit32          : constant Bits_1 := 1;
   Byte           : constant Bits_2 := 0;
   H_Word         : constant Bits_2 := 1;
   Word           : constant Bits_2 := 2;
   Post_Inc       : constant Bits_1 := 1;
   Enable         : constant Bits_1 := 1;
   Periph_To_Mem  : constant Bits_2 := 0;
   Mem_To_Periph  : constant Bits_2 := 1;
   Mem_To_Mem     : constant Bits_2 := 2;
   Dma_Contrld    : constant Bits_1 := 0;
   Periph_Contrld : constant Bits_1 := 1;

   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------

   type LISR_Register is record
      -- Bit definition for DMA low interrupt status register --
      Res0	: Reserved (28 .. 31) := (others => 0);
      TCIF3	: Bits_1;
      -- Stream x transfer complete interrupt flag
      --- Xfer_Complete, Off
      HTIF3	: Bits_1;
      --  Stream x half transfer interrupt flag
      --- Half_Xfer, Off
      TEIF3	: Bits_1;
      --  Stream x transfer error interrupt flag
      --- Xfer_Error, Off
      DMEIF3	: Bits_1;
      -- Stream x direct mode error interrupt flag
      --- Dm_Error, Off
      Res1	: Reserved (23 .. 23) := (others => 0);
      FEIF3	: Bits_1;
      --  Stream x FIFO error interrupt flag
      --- Fifo_Error. Off
      TCIF2	: Bits_1;
      -- Stream x transfer complete interrupt flag
      --- Xfer_Complete, Off
      HTIF2	: Bits_1;
      -- Stream x half transfer interrupt flag
      --- Half_Xfer, Off
      TEIF2	: Bits_1;
      -- Stream x transfer error interrupt flag
      --- Xfer_Error, Off
      DMEIF2	: Bits_1;
      -- Stream x direct mode error interrupt flag
      --- Dm_Error, Off
      Res2	: Reserved (17 .. 17) := (others => 0);
      FEIF2	: Bits_1;
      -- Stream x FIFO error interrupt flag
      --- Fifo_Error. Off
      Res3	: Reserved (12 .. 15) := (others => 0);
      TCIF1	: Bits_1;
      -- Stream x transfer complete interrupt flag
      --- Xfer_Complete, Off
      HTIF1	: Bits_1;
      -- Stream x half transfer interrupt flag
      --- Half_Xfer, Off
      TEIF1	: Bits_1;
      -- Stream x transfer error interrupt flag
      --- Xfer_Error, Off
      DMEIF1	: Bits_1;
      -- Stream x direct mode error interrupt flag
      --- Dm_Error, Off
      Res4	: Reserved (7 .. 7) := (others => 0);
      FEIF1	: Bits_1;
      -- Stream x FIFO error interrupt flag
      --- Fifo_Error. Off
      TCIF0	: Bits_1;
      -- Stream x transfer complete interrupt flag
      --- Xfer_Complete, Off
      HTIF0	: Bits_1;
      -- Stream x half transfer interrupt flag
      --- Half_Xfer, Off
      TEIF0	: Bits_1;
      -- Stream x transfer error interrupt flag
      --- Xfer_Error, Off
      DMEIF0	: Bits_1;
      -- Stream x direct mode error interrupt flag
      --- Dm_Error, Off
      Res5	: Reserved (1 .. 1) := (others => 0);
      FEIF0	: Bits_1;
      -- Stream x FIFO error interrupt flag
      --- Fifo_Error. Off
   end record;

   for LISR_Register use record
      Res0   at 0 range 28 .. 31;
      TCIF3  at 0 range 27 .. 27;
      HTIF3  at 0 range 26 .. 26;
      TEIF3  at 0 range 25 .. 25;
      DMEIF3 at 0 range 24 .. 24;
      Res1   at 0 range 23 .. 23;
      FEIF3  at 0 range 22 .. 22;
      TCIF2  at 0 range 21 .. 21;
      HTIF2  at 0 range 20 .. 20;
      TEIF2  at 0 range 19 .. 19;
      DMEIF2 at 0 range 18 .. 18;
      Res2   at 0 range 17 .. 17;
      FEIF2  at 0 range 16 .. 16;
      Res3   at 0 range 12 .. 15;
      TCIF1  at 0 range 11 .. 11;
      HTIF1  at 0 range 10 .. 10;
      TEIF1  at 0 range  9 ..  9;
      DMEIF1 at 0 range  8 ..  8;
      Res4   at 0 range  7 ..  7;
      FEIF1  at 0 range  6 ..  6;
      TCIF0  at 0 range  5 ..  5;
      HTIF0  at 0 range  4 ..  4;
      TEIF0  at 0 range  3 ..  3;
      DMEIF0 at 0 range  2 ..  2;
      Res5   at 0 range  1 ..  1;
      FEIF0  at 0 range  0 ..  0;
   end record;

   type HISR_Register is record
      -- Bit definition for DMA high interrupt status register --
      Res0	: Reserved (28 .. 31) := (others => 0);
      TCIF7	: Bits_1;
      -- Stream x transfer complete interrupt flag
      --- Xfer_Complete, Off
      HTIF7	: Bits_1;
      --  Stream x half transfer interrupt flag
      --- Half_Xfer, Off
      TEIF7	: Bits_1;
      --  Stream x transfer error interrupt flag
      --- Xfer_Error, Off
      DMEIF7	: Bits_1;
      -- Stream x direct mode error interrupt flag
      --- Dm_Error, Off
      Res1	: Reserved (23 .. 23) := (others => 0);
      FEIF7	: Bits_1;
      --  Stream x FIFO error interrupt flag
      --- Fifo_Error. Off
      TCIF6	: Bits_1;
      -- Stream x transfer complete interrupt flag
      --- Xfer_Complete, Off
      HTIF6	: Bits_1;
      -- Stream x half transfer interrupt flag
      --- Half_Xfer, Off
      TEIF6	: Bits_1;
      -- Stream x transfer error interrupt flag
      --- Xfer_Error, Off
      DMEIF6	: Bits_1;
      -- Stream x direct mode error interrupt flag
      --- Dm_Error, Off
      Res2	: Reserved (17 .. 17) := (others => 0);
      FEIF6	: Bits_1;
      -- Stream x FIFO error interrupt flag
      --- Fifo_Error. Off
      Res3	: Reserved (12 .. 15) := (others => 0);
      TCIF5	: Bits_1;
      -- Stream x transfer complete interrupt flag
      --- Xfer_Complete, Off
      HTIF5	: Bits_1;
      -- Stream x half transfer interrupt flag
      --- Half_Xfer, Off
      TEIF5	: Bits_1;
      -- Stream x transfer error interrupt flag
      --- Xfer_Error, Off
      DMEIF5	: Bits_1;
      -- Stream x direct mode error interrupt flag
      --- Dm_Error, Off
      Res4	: Reserved (7 .. 7) := (others => 0);
      FEIF5	: Bits_1;
      -- Stream x FIFO error interrupt flag
      --- Fifo_Error. Off
      TCIF4	: Bits_1;
      -- Stream x transfer complete interrupt flag
      --- Xfer_Complete, Off
      HTIF4	: Bits_1;
      -- Stream x half transfer interrupt flag
      --- Half_Xfer, Off
      TEIF4	: Bits_1;
      -- Stream x transfer error interrupt flag
      --- Xfer_Error, Off
      DMEIF4	: Bits_1;
      -- Stream x direct mode error interrupt flag
      --- Dm_Error, Off
      Res5	: Reserved (1 .. 1) := (others => 0);
      FEIF4	: Bits_1;
      -- Stream x FIFO error interrupt flag
      --- Fifo_Error. Off
   end record;

   for HISR_Register use record
      Res0   at 0 range 28 .. 31;
      TCIF7  at 0 range 27 .. 27;
      HTIF7  at 0 range 26 .. 26;
      TEIF7  at 0 range 25 .. 25;
      DMEIF7 at 0 range 24 .. 24;
      Res1   at 0 range 23 .. 23;
      FEIF7  at 0 range 22 .. 22;
      TCIF6  at 0 range 21 .. 21;
      HTIF6  at 0 range 20 .. 20;
      TEIF6  at 0 range 19 .. 19;
      DMEIF6 at 0 range 18 .. 18;
      Res2   at 0 range 17 .. 17;
      FEIF6  at 0 range 16 .. 16;
      Res3   at 0 range 12 .. 15;
      TCIF5  at 0 range 11 .. 11;
      HTIF5  at 0 range 10 .. 10;
      TEIF5  at 0 range  9 ..  9;
      DMEIF5 at 0 range  8 ..  8;
      Res4   at 0 range  7 ..  7;
      FEIF5  at 0 range  6 ..  6;
      TCIF4  at 0 range  5 ..  5;
      HTIF4  at 0 range  4 ..  4;
      TEIF4  at 0 range  3 ..  3;
      DMEIF4 at 0 range  2 ..  2;
      Res5   at 0 range  1 ..  1;
      FEIF4  at 0 range  0 ..  0;
   end record;

   type LIFCR_Register is record
      -- Bit definition for DMA low interrupt flag clear register --
      Res0	: Reserved (28 .. 31) := (others => 0);
      CTCIF3	: Bits_1;
      -- Stream x clear transfer complete interrupt flag
      --- Clearit, Off
      CHTIF3	: Bits_1;
      --  Stream x clear half transfer interrupt flag
      --- Clearit, Off
      CTEIF3	: Bits_1;
      --  Stream x clear transfer error interrupt flag
      --- Clearit, Off
      CDMEIF3	: Bits_1;
      -- Stream x clear direct mode error interrupt flag
      --- Clearit, Off
      Res1	: Reserved (23 .. 23) := (others => 0);
      CFEIF3	: Bits_1;
      --  Stream x clear FIFO error interrupt flag
      --- Clearit, Off
      CTCIF2	: Bits_1;
      -- Stream x clear transfer complete interrupt flag
      --- Clearit, Off
      CHTIF2	: Bits_1;
      -- Stream x clear half transfer interrupt flag
      --- Clearit, Off
      CTEIF2	: Bits_1;
      -- Stream x clear transfer error interrupt flag
      --- Clearit, Off
      CDMEIF2	: Bits_1;
      -- Stream x clear direct mode error interrupt flag
      --- Clearit, Off
      Res2	: Reserved (17 .. 17) := (others => 0);
      CFEIF2	: Bits_1;
      -- Stream x clear FIFO error interrupt flag
      --- Clearit, Off
      Res3	: Reserved (12 .. 15) := (others => 0);
      CTCIF1	: Bits_1;
      -- Stream x clear transfer complete interrupt flag
      --- Clearit, Off
      CHTIF1	: Bits_1;
      -- Stream x clear half transfer interrupt flag
      --- Clearit, Off
      CTEIF1	: Bits_1;
      -- Stream x clear transfer error interrupt flag
      --- Clearit, Off
      CDMEIF1	: Bits_1;
      -- Stream x clear direct mode error interrupt flag
      --- Clearit, Off
      Res4	: Reserved (7 .. 7) := (others => 0);
      CFEIF1	: Bits_1;
      -- Stream x clear FIFO error interrupt flag
      --- Clearit, Off
      CTCIF0	: Bits_1;
      -- Stream x clear transfer complete interrupt flag
      --- Clearit, Off
      CHTIF0	: Bits_1;
      -- Stream x clear half transfer interrupt flag
      --- Clearit, Off
      CTEIF0	: Bits_1;
      -- Stream x clear transfer error interrupt flag
      --- Clearit, Off
      CDMEIF0	: Bits_1;
      -- Stream x clear direct mode error interrupt flag
      --- Clearit, Off
      Res5	: Reserved (1 .. 1) := (others => 0);
      CFEIF0	: Bits_1;
      -- Stream x clear FIFO error interrupt flag
      --- Clearit, Off
   end record;

   for LIFCR_Register use record
      Res0    at 0 range 28 .. 31;
      CTCIF3  at 0 range 27 .. 27;
      CHTIF3  at 0 range 26 .. 26;
      CTEIF3  at 0 range 25 .. 25;
      CDMEIF3 at 0 range 24 .. 24;
      Res1    at 0 range 23 .. 23;
      CFEIF3  at 0 range 22 .. 22;
      CTCIF2  at 0 range 21 .. 21;
      CHTIF2  at 0 range 20 .. 20;
      CTEIF2  at 0 range 19 .. 19;
      CDMEIF2 at 0 range 18 .. 18;
      Res2    at 0 range 17 .. 17;
      CFEIF2  at 0 range 16 .. 16;
      Res3    at 0 range 12 .. 15;
      CTCIF1  at 0 range 11 .. 11;
      CHTIF1  at 0 range 10 .. 10;
      CTEIF1  at 0 range  9 ..  9;
      CDMEIF1 at 0 range  8 ..  8;
      Res4    at 0 range  7 ..  7;
      CFEIF1  at 0 range  6 ..  6;
      CTCIF0  at 0 range  5 ..  5;
      CHTIF0  at 0 range  4 ..  4;
      CTEIF0  at 0 range  3 ..  3;
      CDMEIF0 at 0 range  2 ..  2;
      Res5    at 0 range  1 ..  1;
      CFEIF0  at 0 range  0 ..  0;
   end record;

   type HIFCR_Register is record
      -- Bit definition for DMA high interrupt flag clear register --
      Res0	: Reserved (28 .. 31) := (others => 0);
      CTCIF7	: Bits_1;
      -- Stream x clear transfer complete interrupt flag
      --- Clearit, Off
      CHTIF7	: Bits_1;
      --  Stream x clear half transfer interrupt flag
      --- Clearit, Off
      CTEIF7	: Bits_1;
      --  Stream x clear transfer error interrupt flag
      --- Clearit, Off
      CDMEIF7	: Bits_1;
      -- Stream x clear direct mode error interrupt flag
      --- Clearit, Off
      Res1	: Reserved (23 .. 23) := (others => 0);
      CFEIF7	: Bits_1;
      --  Stream x clear FIFO error interrupt flag
      --- Clearit, Off
      CTCIF6	: Bits_1;
      -- Stream x clear transfer complete interrupt flag
      --- Clearit, Off
      CHTIF6	: Bits_1;
      -- Stream x clear half transfer interrupt flag
      --- Clearit, Off
      CTEIF6	: Bits_1;
      -- Stream x clear transfer error interrupt flag
      --- Clearit, Off
      CDMEIF6	: Bits_1;
      -- Stream x clear direct mode error interrupt flag
      --- Clearit, Off
      Res2	: Reserved (17 .. 17) := (others => 0);
      CFEIF6	: Bits_1;
      -- Stream x clear FIFO error interrupt flag
      --- Clearit, Off
      Res3	: Reserved (12 .. 15) := (others => 0);
      CTCIF5	: Bits_1;
      -- Stream x clear transfer complete interrupt flag
      --- Clearit, Off
      CHTIF5	: Bits_1;
      -- Stream x clear half transfer interrupt flag
      --- Clearit, Off
      CTEIF5	: Bits_1;
      -- Stream x clear transfer error interrupt flag
      --- Clearit, Off
      CDMEIF5	: Bits_1;
      -- Stream x clear direct mode error interrupt flag
      --- Clearit, Off
      Res4	: Reserved (7 .. 7) := (others => 0);
      CFEIF5	: Bits_1;
      -- Stream x clear FIFO error interrupt flag
      --- Clearit, Off
      CTCIF4	: Bits_1;
      -- Stream x clear transfer complete interrupt flag
      --- Clearit, Off
      CHTIF4	: Bits_1;
      -- Stream x clear half transfer interrupt flag
      --- Clearit, Off
      CTEIF4	: Bits_1;
      -- Stream x clear transfer error interrupt flag
      --- Clearit, Off
      CDMEIF4	: Bits_1;
      -- Stream x clear direct mode error interrupt flag
      --- Clearit, Off
      Res5	: Reserved (1 .. 1) := (others => 0);
      CFEIF4	: Bits_1;
      -- Stream x clear FIFO error interrupt flag
      --- Clearit, Off
   end record;

   for HIFCR_Register use record
      Res0    at 0 range 28 .. 31;
      CTCIF7  at 0 range 27 .. 27;
      CHTIF7  at 0 range 26 .. 26;
      CTEIF7  at 0 range 25 .. 25;
      CDMEIF7 at 0 range 24 .. 24;
      Res1    at 0 range 23 .. 23;
      CFEIF7  at 0 range 22 .. 22;
      CTCIF6  at 0 range 21 .. 21;
      CHTIF6  at 0 range 20 .. 20;
      CTEIF6  at 0 range 19 .. 19;
      CDMEIF6 at 0 range 18 .. 18;
      Res2    at 0 range 17 .. 17;
      CFEIF6  at 0 range 16 .. 16;
      Res3    at 0 range 12 .. 15;
      CTCIF5  at 0 range 11 .. 11;
      CHTIF5  at 0 range 10 .. 10;
      CTEIF5  at 0 range  9 ..  9;
      CDMEIF5 at 0 range  8 ..  8;
      Res4    at 0 range  7 ..  7;
      CFEIF5  at 0 range  6 ..  6;
      CTCIF4  at 0 range  5 ..  5;
      CHTIF4  at 0 range  4 ..  4;
      CTEIF4  at 0 range  3 ..  3;
      CDMEIF4 at 0 range  2 ..  2;
      Res5    at 0 range  1 ..  1;
      CFEIF4  at 0 range  0 ..  0;
   end record;

   type CR_Register is record
      Res0	: Reserved (28 .. 31) := (others => 0);
      CHSEL	: Bits_3;
      --  Channel selection
      --- Sel_Ch0, Sel_Ch1, Sel_Ch2, Sel_Ch3, 
      --- Sel_Ch4, Sel_Ch5, Sel_Ch6, Sel_Ch7
      MBURST	: Bits_2;
      --  Memory burst transfer configuration
      --- Single, Incr_4beats, Incr_8beats, Incr_16beats
      PBURST	: Bits_2;
      --  Peripheral burst transfer configuration
      --- Single, Incr_4beats, Incr_8beats, Incr_16beats
      Res1	: Reserved (20 .. 20) := (others => 0);
      CT	: Bits_1;
      --  Current target (only in double buffer mode)
      --- Mem1, Mem0
      DBM	: Bits_1;
      --  Double buffer mode
      --- On, Off
      PL	: Bits_2;
      --  Priority level
      --- Low, Medium, High, Very_High
      PINCOS	: Bits_1;
      --  Peripheral increment offset size
      --- Psize, Bit32
      MSIZE	: Bits_2;
      --  Memory data size
      --- Byte, H_Word, Word
      PSIZE	: Bits_2;
      --  Peripheral data size
      --- Byte, H_Word, Word
      MINC	: Bits_1;
      --  Memory increment mode
      --- Post_Inc, Off
      PINC	: Bits_1;
      --  Peripheral increment mode
      --- Post_Inc, Off
      CIRC	: Bits_1;
      --  Circular mode
      --- Enable, Off
      DIR	: Bits_2;
      --  Data transfer direction
      --- Periph_To_Mem, Mem_To_Periph, Mem_To_Mem
      PFCTRL	: Bits_1;
      --  Peripheral flow controller
      --- Dma_Contrld, Periph_Contrld
      TCIE	: Bits_1;
      --  Transfer complete interrupt enable
      --- Enable, Off
      HTIE	: Bits_1;
      --  Half transfer interrupt enable
      --- Enable, Off
      TEIE	: Bits_1;
      --  Transfer error interrupt enable
      --- Enable, Off
      DMEIE	: Bits_1;
      --  Direct mode error interrupt enable
      --- Enable, Off
      EN	: Bits_1;
      --  Stream enable / flag stream ready when read low
      --- Enable, Off
   end record;

   for CR_Register use record
      Res0   at 0 range 28 .. 31;
      CHSEL  at 0 range 25 .. 27;
      MBURST at 0 range 23 .. 24;
      PBURST at 0 range 21 .. 22;
      Res1   at 0 range 20 .. 20;
      CT     at 0 range 19 .. 19;
      DBM    at 0 range 18 .. 18;
      PL     at 0 range 16 .. 17;
      PINCOS at 0 range 15 .. 15;
      MSIZE  at 0 range 13 .. 14;
      PSIZE  at 0 range 11 .. 12;
      MINC   at 0 range 10 .. 10;
      PINC   at 0 range  9 ..  9;
      CIRC   at 0 range  8 ..  8;
      DIR    at 0 range  6 ..  7;
      PFCTRL at 0 range  5 ..  5;
      TCIE   at 0 range  4 ..  4;
      HTIE   at 0 range  3 ..  3;
      TEIE   at 0 range  2 ..  2;
      DMEIE  at 0 range  1 ..  1;
      EN     at 0 range  0 ..  0;
   end record;

   type NDTR_Register is record
      -- DMA stream x number of data register
      Res0	: Reserved (16 .. 31) := (others => 0);
      NDT	: Bits_16;
      --  Number of data items to transfer
   end record;

   for NDTR_Register use record
      Res0 at 0 range 16 .. 31;
      NDT  at 0 range  0 .. 15;
   end record;

   subtype PAR_Register is Bits_32;
   -- DMA stream x peripheral address register
   -- Base address of the peripheral data register

   subtype M0AR_Register is Bits_32;
   -- DMA stream x memory 0 address register
   -- Base address of Memory area

   subtype M1AR_Register is Bits_32;
   -- DMA stream x memory 1 address register
   -- Base address of Memory area

   type FCR_Register is record
      Res0	: Reserved (8 .. 31) := (others => 0);
      FEIE	: Bits_1;
      --  FIFO error interrupt enable
      Res1	: Reserved (6 .. 6) := (others => 0);
      FS	: Bits_3;
      --  FIFO status
      DMDIS	: Bits_1;
      --  Direct mode disable
      FTH	: Bits_2;
      --  FIFO threshold selection
   end record;

   for FCR_Register use record
      Res0  at 0 range 8 .. 31;
      FEIE  at 0 range 7 ..  7;
      Res1  at 0 range 6 ..  6;
      FS    at 0 range 3 ..  5;
      DMDIS at 0 range 2 ..  2;
      FTH   at 0 range 0 ..  1;
   end record;

   type DMA_Stream_TypeDef is record
      -- DMA controller  --
      CR	: CR_Register;
      -- DMA stream x configuration register
      NDTR	: NDTR_Register;
      -- DMA stream x number of data register
      PAR	: PAR_Register;
      -- DMA stream x peripheral address register
      M0AR	: M0AR_Register;
      -- DMA stream x memory 0 address register
      M1AR	: M1AR_Register;
      -- DMA stream x memory 1 address register
      FCR	: FCR_Register;
      -- DMA stream x FIFO control register
   end record;
   pragma Convention (C_Pass_By_Copy, DMA_Stream_TypeDef);  -- dma.h:329

   for DMA_Stream_TypeDef use record
      CR   at  0 range 0 .. 31;    -- starts at 16 for the first one
      NDTR at  4 range 0 .. 31;
      PAR  at  8 range 0 .. 31;
      M0AR at 12 range 0 .. 31;
      M1AR at 16 range 0 .. 31;
      FCR  at 20 range 0 .. 31;
   end record;

   type DMA_TypeDef is record
      LISR	: LISR_Register;
      -- DMA low interrupt status register
      HISR	: HISR_Register;
      -- DMA high interrupt status register
      LIFCR	: LIFCR_Register;
      -- DMA low interrupt flag clear register
      HIFCR	: HIFCR_Register;
      -- DMA high interrupt flag clear register
      S0	: DMA_Stream_TypeDef;
      -- DMA stream 0
      S1	: DMA_Stream_TypeDef;
      -- DMA stream 1
      S2	: DMA_Stream_TypeDef;
      -- DMA stream 2
      S3	: DMA_Stream_TypeDef;
      -- DMA stream 3
      S4	: DMA_Stream_TypeDef;
      -- DMA stream 4
      S5	: DMA_Stream_TypeDef;
      -- DMA stream 5
      S6	: DMA_Stream_TypeDef;
      -- DMA stream 6
      S7	: DMA_Stream_TypeDef;
      -- DMA stream 7
   end record;
   pragma Convention (C_Pass_By_Copy, DMA_TypeDef);  -- dma.h:337

   for DMA_TypeDef use record
      LISR  at   0 range 0 ..  31;
      HISR  at   4 range 0 ..  31;
      LIFCR at   8 range 0 ..  31;
      HIFCR at  12 range 0 ..  31;
      S0    at  16 range 0 .. 191;
      S1    at  40 range 0 .. 191;
      S2    at  64 range 0 .. 191;
      S3    at  88 range 0 .. 191;
      S4    at 112 range 0 .. 191;
      S5    at 136 range 0 .. 191;
      S6    at 160 range 0 .. 191;
      S7    at 184 range 0 .. 191;
   end record;

end STM32F4.o7xx.Dma;

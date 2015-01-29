------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                         S T M 32 F 4 . O7xx . D a c                      --
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
--    chapter 14.5   DAC registers       RM0090 Reference manual
--





pragma Restrictions (No_Elaboration_Code);

with STM32F4.O7xx;

package STM32F4.O7xx.Dac is
   
   --------------------------------------------------
   -- constants for use with the dac definitions   --
   --  in the child packages                       --
   --------------------------------------------------
   
   --------------------------------
   --------------------------------
   --  control register (DAC_CR) --
   --------------------------------
   Disabled      : constant Bits_1  := 0;  -- bit 13, 29  
   Enabled       : constant Bits_1  := 1;
   -- DMAUDRIEx: DAC channel x DMA underrun interrupt enable
   
   --Disabled                  
   --Enabled                                -- bit 12, 28
   -- DMAENx: DAC channel x DMA enable
   
   Ampl_1         : constant Bits_4  := 0;  -- bit 11-8, 24-27
   Ampl_3         : constant Bits_4  := 1;
   Ampl_7         : constant Bits_4  := 2;
   Ampl_15        : constant Bits_4  := 3;
   Ampl_31        : constant Bits_4  := 4;
   Ampl_63        : constant Bits_4  := 5;
   Ampl_127       : constant Bits_4  := 6;
   Ampl_255       : constant Bits_4  := 7;
   Ampl_511       : constant Bits_4  := 8;
   Ampl_1023      : constant Bits_4  := 9;
   Ampl_2047      : constant Bits_4  := 10;
   Ampl_4095      : constant Bits_4  := 11;
   -- MAMPx[3:0]: DAC channel x mask/amplitude selector
     
   Wave_Disabled  : constant Bits_2  := 0;  -- bit 6-7, 22-23
   Noise_Enabled  : constant Bits_2  := 1;
   Triangle_Enabled : constant Bits_2  := 2;
   -- WAVEx[1:0]: DAC channel x noise/triangle wave generation enable
   
   T_6_TRGO       : constant Bits_3  := 0;  -- bit 3-5, 19-21
   T_8_TRGO	  : constant Bits_3  := 1;
   T_7_TRGO	  : constant Bits_3  := 2;
   T_5_TRGO	  : constant Bits_3  := 3;
   T_2_TRGO	  : constant Bits_3  := 4;
   T_4_TRGO	  : constant Bits_3  := 5;
   Ext_L9	  : constant Bits_3  := 6;
   Soft_Trig	  : constant Bits_3  := 7;
   -- TSELx[2:0]: DAC channel x trigger selection
   
   --Disabled                  
   --Enabled                                -- bit 2, 18
   -- TENx: DAC channel x trigger enable
   -- BOFFx: DAC channel x output buffer disable   bit 1, 17
   -- ENx: DAC channel x enable                    bit 0, 16

   
   ---------------------------------------------
   -- software trigger register (DAC_SWTRIGR) --
   ---------------------------------------------
   
   --Disabled                  
   --Enabled                                -- bit 0, 1
   -- SWTRIGx: DAC channel x software trigger
   
   ----------------------------------------------------------------
   ----------------------------------------------------------------
   
   type CR_Register is record  -- control register
      EN1           : Bits_1;  -- channel x enable				  
      BOFF1         : Bits_1;  -- channel x output buffer disable		  
      TEN1          : Bits_1;  -- channel x trigger enable			  
      TSEL1         : Bits_3;  -- channel x trigger selection			  
      WAVE1         : Bits_2;  -- channel x noise/triangle wave generation enable 
      MAMP1         : Bits_4;  -- channel x mask/amplitude selector		  
      DMAEN1        : Bits_1;  -- channel x DMA enable				  
      DMAUDRIE1     : Bits_1;  -- channel x DMA underrun interrupt enable	  
      Res1          : Reserved (14 .. 15) := (others => 0);                       
      EN2           : Bits_1;  -- channel x enable
      BOFF2         : Bits_1;  -- channel x output buffer disable
      TEN2          : Bits_1;  -- channel x trigger enable
      TSEL2         : Bits_3;  -- channel x trigger selection
      WAVE2         : Bits_2;  -- channel x noise/triangle wave generation enable
      MAMP2         : Bits_4;  -- channel x mask/amplitude selector
      DMAEN2        : Bits_1;  -- channel x DMA enable
      DMAUDRIE2     : Bits_1;  -- channel x DMA underrun interrupt enable
      Res2          : Reserved (30 .. 31) := (others => 0);                       
   end record;
   
   for CR_Register use record
      EN1       at 0 range 0 .. 0;
      BOFF1     at 0 range 1 .. 1;
      TEN1      at 0 range 2 .. 2;
      TSEL1     at 0 range 3 .. 5;
      WAVE1     at 0 range 6 .. 7;
      MAMP1     at 0 range 8 .. 11;  
      DMAEN1    at 0 range 12 .. 12;
      DMAUDRIE1 at 0 range 13 .. 13;
      Res1      at 0 range 14 .. 15;
      EN2       at 0 range 16 .. 16;
      BOFF2     at 0 range 17 .. 17;
      TEN2      at 0 range 18 .. 18;
      TSEL2     at 0 range 19 .. 21;
      WAVE2     at 0 range 22 .. 23;
      MAMP2     at 0 range 24 .. 27;
      DMAEN2    at 0 range 28 .. 28;  
      DMAUDRIE2 at 0 range 29 .. 29;  
      Res2      at 0 range 30 .. 31;
   end record;
   
   
   type SWTRIGR_Register is record  -- software trigger register
      SWTRIG1       : Bits_1;  -- DAC channel x software trigger
      SWTRIG2       : Bits_1;  -- DAC channel x software trigger
      Res2          : Reserved (2 .. 31) := (others => 0);                     
   end record;
   
   for SWTRIGR_Register use record 
      SWTRIG1   at 0 range 0 .. 0;
      SWTRIG2   at 0 range 1 .. 1;
      Res2      at 0 range 2 .. 31;
   end record;
   
   
   type DHR12R_Register is record  -- 12-bit right-aligned data holding register
      Data12_Ra     : Bits_12;  --  channel 12-bit right-aligned data
      Res2          : Reserved (12 .. 31) := (others => 0);  
   end record;
      
   for DHR12R_Register use record
      Data12_Ra at 0 range 0 .. 11;
      Res2      at 0 range 12 .. 31;
   end record;
   
   
   type DHR12L_Register is record  -- 12-bit left-aligned data holding register
      Res1          : Reserved (0 .. 3) := (others => 0);
      Data12_La     : Bits_12;  --  channel 12-bit Left-aligned data
      Res2          : Reserved (16 .. 31) := (others => 0);  
   end record;
      
   for DHR12L_Register use record
      Res1      at 0 range 0 .. 3;
      Data12_La at 0 range 4 .. 15;
      Res2      at 0 range 16 .. 31;
   end record;
   
   
   type DHR8R_Register is record  -- 8-bit right-aligned data holding register
      Data8_Ra      : Byte;  --  channel 8-bit right-aligned data
      Res2          : Reserved (8 .. 31) := (others => 0);  
   end record;
      
   for DHR8R_Register use record
      Data8_Ra  at 0 range 0 .. 7;
      Res2      at 0 range 8 .. 31;
   end record;
   
   
   type DHR12RD_Register is record --12-bit dual right-aligned data holding reg
      Data12_Ra_C1  : Bits_12;  --  channel 12-bit right-aligned data
      Res1          : Reserved (12 .. 15) := (others => 0);
      Data12_Ra_C2  : Bits_12;  --  channel 12-bit right-aligned data
      Res2          : Reserved (28 .. 31) := (others => 0);  
   end record;
     
   for DHR12RD_Register use record
      Data12_Ra_C1 at 0 range 0 .. 11;
      Res1         at 0 range 12 .. 15;
      Data12_Ra_C2 at 0 range 16 .. 27;
      Res2         at 0 range 28 .. 31;
   end record;
   
   
   type DHR12LD_Register is record -- 12-bit dual left-aligned data holding reg
      Res1          : Reserved (0 .. 3) := (others => 0);
      Data12_La_C1  : Bits_12;  --  channel 12-bit Left-aligned data
      Res2          : Reserved (16 .. 19) := (others => 0);  
      Data12_La_C2  : Bits_12;  --  channel 12-bit Left-aligned data
   end record;
   
   for DHR12LD_Register use record
      Res1         at 0 range 0 .. 3;
      Data12_La_C1 at 0 range 4 .. 15;
      Res2         at 0 range 16 .. 19;
      Data12_La_C2 at 0 range 20 .. 31;
   end record;
   
   
   type DHR8RD_Register is record -- 8-bit dual right aligned data holding reg
      Data8_Ra_C1   : Byte;  --  channel 8-bit right-aligned data
      Data8_Ra_C2   : Byte;  --  channel 8-bit right-aligned data
      Res2          : Reserved (16 .. 31) := (others => 0);  
   end record;
   
   for DHR8RD_Register use record
      Data8_Ra_C1  at 0 range 0 .. 7;
      Data8_Ra_C2  at 0 range 8 .. 15;
      Res2         at 0 range 16 .. 31;
   end record;
   
   
   type DOR_Register is record -- channelx data output register 
      Data          : Bits_12;  --   channel x data output
      Res2          : Reserved (16 .. 31) := (others => 0); 
   end record;
   
   for DOR_Register use record
      Data         at 0 range 0 .. 11;
      Res2         at 0 range 16 .. 31;
   end record;
      
   
   type SR_Register is record  -- status register
      Res1          : Reserved (0 .. 12) := (others => 0);
      DMAUDR1       : Bits_1;  -- channel1 DMA underrun flag
      Res2          : Reserved (14 .. 28) := (others => 0); 
      DMAUDR2       : Bits_1;  -- channel2 DMA underrun flag
      Res3          : Reserved (30 .. 31) := (others => 0); 
   end record;
   
   for SR_Register use record
      Res1         at 0 range 0 .. 12;
      DMAUDR1      at 0 range 13 .. 13;
      Res2         at 0 range 14 .. 28;
      DMAUDR2      at 0 range 29 .. 29;  
      Res3         at 0 range 30 .. 31;
   end record;
      
   type Dac_Register is record
      CR          : CR_Register; -- control register
      SWTRIGR     : SWTRIGR_Register; -- software trigger register
      DHR12R1     : DHR12R_Register;  -- 12-bit right-aligned data holding register1
      DHR12L1     : DHR12L_Register;  -- 12-bit left-aligned data holding register1
      DHR8R1      : DHR8R_Register;   -- 8-bit right-aligned data holding register1
      DHR12R2     : DHR12R_Register;  -- 12-bit right-aligned data holding register2
      DHR12L2     : DHR12L_Register;  -- 12-bit left-aligned data holding register2
      DHR8R2      : DHR8R_Register;   -- 8-bit right-aligned data holding register2
      DHR12RD     : DHR12RD_Register; -- 12-bit dual right-aligned data holding reg
      DHR12LD     : DHR12LD_Register; -- 12-bit dual left-aligned data holding reg
      DHR8RD      : DHR8RD_Register;  -- 8-bit dual right aligned data holding reg
      DOR1        : DOR_Register;     -- channel1 data output register
      DOR2        : DOR_Register;     -- channel2 data output register
      SR          : SR_Register;      -- status register
   end record;
     
   for Dac_Register use record
      CR          at 0   range 0 .. 31;
      SWTRIGR     at 4   range 0 .. 31;
      DHR12R1     at 8   range 0 .. 31;
      DHR12L1     at 12  range 0 .. 31;
      DHR8R1      at 16  range 0 .. 31;
      DHR12R2     at 20  range 0 .. 31;
      DHR12L2     at 24  range 0 .. 31;
      DHR8R2      at 28  range 0 .. 31;
      DHR12RD     at 32  range 0 .. 31;
      DHR12LD     at 36  range 0 .. 31;
      DHR8RD      at 40  range 0 .. 31;
      DOR1        at 44  range 0 .. 31;
      DOR2        at 48  range 0 .. 31;
      SR          at 52  range 0 .. 31;
   end record;
   
      
      
     
end STM32F4.O7xx.Dac;


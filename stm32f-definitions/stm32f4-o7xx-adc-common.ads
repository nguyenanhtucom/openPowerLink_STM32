------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                S T M 32 F 4 . O7xx . A d c . C o m m o n                 --
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
--  chapter 13.13   ADC registers       RM0090 Reference manual
--


pragma Restrictions (No_Elaboration_Code);

package STM32F4.O7xx.Adc.Common is
   
   type CSR_Register is record  --   common status register 
      AWD1,                 -- ADC1 Analog watchdog flag
      EOC1,                 -- ADC1 Regular channel end of conversion
      JEOC1,                -- ADC1 Injected channel end of conversion
      JSTRT1,               -- ADC1 Injected channel start flag
      STRT1,                -- ADC1 Regular channel start flag
      OVR1     : Bits_1;    -- ADC1 Overrun
      Res1     : Reserved (6 .. 7) := (others => 0);
      AWD2,                 -- ADC2 Analog watchdog flag
      EOC2,                 -- ADC2 Regular channel end of conversion
      JEOC2,                -- ADC2 Injected channel end of conversion
      JSTRT2,               -- ADC2 Injected channel start flag
      STRT2,                -- ADC2 Regular channel start flag
      OVR2     : Bits_1;    -- ADC2 Overrun
      Res2     : Reserved (14 .. 15) := (others => 0);
      AWD3,                 -- ADC3 Analog watchdog flag
      EOC3,                 -- ADC3 Regular channel end of conversion
      JEOC3,                -- ADC3 Injected channel end of conversion
      JSTRT3,               -- ADC3 Injected channel start flag
      STRT3,                -- ADC3 Regular channel start flag
      OVR3     : Bits_1;    -- ADC3 Overrun
      Res3     : Reserved (22 .. 31) := (others => 0);
   end record;
     
   for CSR_Register use record
      AWD1    at 0 range 0 .. 0;
      EOC1    at 0 range 1 .. 1;
      JEOC1   at 0 range 2 .. 2;
      JSTRT1  at 0 range 3 .. 3;
      STRT1   at 0 range 4 .. 4;
      OVR1    at 0 range 5 .. 5;
      Res1    at 0 range 6 .. 7;
      AWD2    at 0 range 8 .. 8;  
      EOC2    at 0 range 9 .. 9;  
      JEOC2   at 0 range 10 .. 10;
      JSTRT2  at 0 range 11 .. 11;
      STRT2   at 0 range 12 .. 12;
      OVR2    at 0 range 13 .. 13;
      Res2    at 0 range 14 .. 15;
      AWD3    at 0 range 16 .. 16;
      EOC3    at 0 range 17 .. 17;
      JEOC3   at 0 range 18 .. 18;  
      JSTRT3  at 0 range 19 .. 19;  
      STRT3   at 0 range 20 .. 20;
      OVR3    at 0 range 21 .. 21;
      Res3    at 0 range 22 .. 31;
   end record;
   
   
   type CCR_Register is record  --  common control register
      MULTI    : Bits_5;  -- Multi ADC mode selection
      Res1     : Reserved (5 .. 7) := (others => 0);
      SDELAY   : Bits_4;  -- Delay between 2 sampling phases
      Res2     : Reserved (12 .. 12) := (others => 0);
      DDS      : Bits_1;   -- DMA disable selection for multi ADC mode
      DMA      : Bits_2;   -- Direct memory access mode for multi ADC mode
      ADCPRE   : Bits_2;   -- ADC prescaler
      Res3     : Reserved (18 .. 21) := (others => 0);
      VBATE    : Bits_1;   -- VBAT enable
      TSVREFE  : Bits_1;   -- Temperature sensor and VREFINT enable
      Res4     : Reserved (24 .. 31) := (others => 0);
   end record;
   
   for CCR_Register use record
      MULTI   at 0 range 0 .. 4;
      Res1    at 0 range 5 .. 7;
      SDELAY  at 0 range 8 .. 11;
      Res2    at 0 range 12 .. 12;
      DDS     at 0 range 13 .. 13;
      DMA     at 0 range 14 .. 15;
      ADCPRE  at 0 range 16 .. 17;
      Res3    at 0 range 18 .. 21;
      VBATE   at 0 range 22 .. 22;
      TSVREFE at 0 range 23 .. 23;
      Res4    at 0 range 24 .. 31;
   end record;
   
   
   type CDR_Register is record  -- common regular data register for 
				-- dual and triple modes
      DATA1  : Half_Word;  --  1st data item of a pair of regular conversions
      DATA2  : Half_Word;  --  2nd data item of a pair of regular conversions
   end record;
   
   for CDR_Register use record
      DATA1  at 0 range 0 .. 15;
      DATA2  at 0 range 16 .. 31;
   end record;
   
      
   ----------------------------------------------------
   -- this is the top of the ADC Common definition   --
   -- usage example:                                 --
   --   ADCC : STM32F4.O7xx.Adc.Common.Adcc_Register --
   ----------------------------------------------------
   
   type Adcc_Register is record
      CSR      : CSR_Register;   -- common status register 
      CCR      : CCR_Register;   -- common control register
      CDR      : CDR_Register;   -- common regular data register for 
				 -- dual and triple modes
   end record;
   
   for Adcc_Register use record
      CSR     at 0   range 0 .. 31;
      CCR     at 4   range 0 .. 31;
      CDR     at 8   range 0 .. 31;
   end record;
      
end STM32F4.O7xx.Adc.Common;

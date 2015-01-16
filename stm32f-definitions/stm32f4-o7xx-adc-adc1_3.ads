------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                  S T M 32 F 4 . O7xx . A d c . A d c 1 _ 3               --
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

package STM32F4.O7xx.Adc.Adc1_3 is
   
   type SR_Register is record  --   status register 
      AWD,                 -- Analog watchdog flag
      EOC,                 -- Regular channel end of conversion
      JEOC,                -- Injected channel end of conversion
      JSTRT,               -- Injected channel start flag
      STRT,                -- Regular channel start flag
      OVR     : Bits_1;    -- Overrun
      Res1    : Reserved (6 .. 31) := (others => 0);
   end record;
   
   for SR_Register use record
      AWD    at 0 range 0 .. 0;
      EOC    at 0 range 1 .. 1;
      JEOC   at 0 range 2 .. 2;
      JSTRT  at 0 range 3 .. 3;
      STRT   at 0 range 4 .. 4;
      OVR    at 0 range 5 .. 5;
      Res1   at 0 range 6 .. 31;
   end record;
   
   
   type CR1_Register is record  --    control register 1 
      AWDCH    : Bits_5;  -- Analog watchdog channel select bits
      EOCIE,              -- Interrupt enable for EOC
      AWDIE,              -- Analog watchdog interrupt enable
      JEOCIE,             -- Interrupt enable for injected channels
      SCAN,               -- Scan mode
      AWDSGL,             -- Enable the watchdog on a single channel in scan mode
      JAUTO,              -- Automatic injected group conversion
      DISCEN,             -- Discontinuous mode on regular channels
      JDISCEN  : Bits_1;  -- Discontinuous mode on injected channels
      DISCNUM  : Bits_3;  -- Discontinuous mode channel count
      Res1     : Reserved (16 .. 21) := (others => 0);
      JAWDEN,             -- Analog watchdog enable on injected channels
      AWDEN    : Bits_1;  -- Analog watchdog enable on regular channels
      RES      : Bits_2;  -- Resolution
      OVRIE    : Bits_1;  -- Overrun interrupt enable
      Res2     : Reserved (27 .. 31) := (others => 0);
   end record;
   
   for CR1_Register use record
      AWDCH   at 0 range 0 .. 4;	
      EOCIE   at 0 range 5 .. 5;	 
      AWDIE   at 0 range 6 .. 6;	 
      JEOCIE  at 0 range 7 .. 7;  
      SCAN    at 0 range 8 .. 8;  
      AWDSGL  at 0 range 9 .. 9;  
      JAUTO   at 0 range 10 .. 10;
      DISCEN  at 0 range 11 .. 11;
      JDISCEN at 0 range 12 .. 12;
      DISCNUM at 0 range 13 .. 15;
      Res1    at 0 range 16 .. 21;
      JAWDEN  at 0 range 22 .. 22;
      AWDEN   at 0 range 23 .. 23;
      RES     at 0 range 24 .. 25;
      OVRIE   at 0 range 26 .. 26;
      Res2    at 0 range 27 .. 31;
   end record;
	
   
   type CR2_Register is record   --  control register 2 
      ADON,                -- A/D Converter ON / OFF
      CONT      : Bits_1;  -- Continuous conversion
      Res1      : Reserved (2 .. 7) := (others => 0);
      DMA,                 -- Direct memory access mode (for single ADC mode)
      DDS,                 -- DMA disable selection (for single ADC mode)
      EOCS,                -- End of conversion selection
      ALIGN     : Bits_1;  -- Data alignment
      Res2      : Reserved (12 .. 15) := (others => 0);
      JEXTSEL   : Bits_4;  -- External event select for injected group
      JEXTEN    : Bits_2;  -- External trigger enable for injected channels
      JSWSTART  : Bits_1;  -- Start conversion of injected channels
      Res3      : Reserved (23 .. 23) := (others => 0);
      EXTSEL    : Bits_4;  -- External event select for regular group
      EXTEN     : Bits_2;  -- External trigger enable for regular channels
      SWSTART   : Bits_1;  -- Start conversion of regular channels
      Res4      : Reserved (31 .. 31) := (others => 0);
   end record;
   
   for CR2_Register use record
      ADON     at 0 range 0 .. 0;
      CONT     at 0 range 1 .. 1;
      Res1     at 0 range 2 .. 7;
      DMA      at 0 range 8 .. 8;  
      DDS      at 0 range 9 .. 9;  
      EOCS     at 0 range 10 .. 10;
      ALIGN    at 0 range 11 .. 11;
      Res2     at 0 range 12 .. 15;
      JEXTSEL  at 0 range 16 .. 19;
      JEXTEN   at 0 range 20 .. 21;
      JSWSTART at 0 range 22 .. 22;
      Res3     at 0 range 23 .. 23;
      EXTSEL   at 0 range 24 .. 27;
      EXTEN    at 0 range 28 .. 29;
      SWSTART  at 0 range 30 .. 30;
      Res4     at 0 range 31 .. 31;
   end record;
      
   
   type SMPR1_Register is record  -- sample time register 1
      SMP10,
      SMP11,
      SMP12,
      SMP13,
      SMP14,
      SMP15,
      SMP16,
      SMP17, 
      SMP18    : Bits_3;  --    Channel x sampling time selection  
      Res4      : Reserved (27 .. 31) := (others => 0);
   end record;
   
   for SMPR1_Register use record
      SMP10  at 0 range 0 .. 2;
      SMP11  at 0 range 3 .. 5;
      SMP12  at 0 range 6 .. 8;
      SMP13  at 0 range 9 .. 11;
      SMP14  at 0 range 12 .. 14;
      SMP15  at 0 range 15 .. 17;
      SMP16  at 0 range 18 .. 20;
      SMP17  at 0 range 21 .. 23;
      SMP18  at 0 range 24 .. 26;
      Res4   at 0 range 27 .. 31;
   end record;
   
   
   type SMPR2_Register is record  -- sample time register 2
      SMP0,
      SMP1,
      SMP2,
      SMP3,
      SMP4,
      SMP5,
      SMP6,
      SMP7, 
      SMP8,
      SMP9      : Bits_3;  --    Channel x sampling time selection  
      Res4      : Reserved (30 .. 31) := (others => 0);
   end record;
   
   for SMPR2_Register use record
      SMP0  at 0 range 0 .. 2;
      SMP1  at 0 range 3 .. 5;
      SMP2  at 0 range 6 .. 8;
      SMP3  at 0 range 9 .. 11;
      SMP4  at 0 range 12 .. 14;
      SMP5  at 0 range 15 .. 17;
      SMP6  at 0 range 18 .. 20;
      SMP7  at 0 range 21 .. 23;
      SMP8  at 0 range 24 .. 26;
      SMP9  at 0 range 27 .. 29;
      Res4  at 0 range 30 .. 31;
   end record;
   
   
   type JOFR_Register is record  -- injected channel data offset register
      JOFFSET    : Bits_12;    -- Data offset for injected channel x
      Res4       : Reserved (12 .. 31) := (others => 0);
   end record;
   
   for JOFR_Register use record
      JOFFSET  at 0 range 0 .. 11;
      Res4     at 0 range 12 .. 31;
   end record;
   
   
   type HTR_Register is record  --  watchdog higher threshold register
      HT         : Bits_12;    -- Analog watchdog higher threshold
      Res4       : Reserved (12 .. 31) := (others => 0);
   end record;
   
   for HTR_Register use record
      HT       at 0 range 0 .. 11;
      Res4     at 0 range 12 .. 31;
   end record;
   
   
   type LTR_Register is record  --  watchdog lower threshold register
      LT         : Bits_12;    -- Analog watchdog higher threshold
      Res4       : Reserved (12 .. 31) := (others => 0);
   end record;
   
   for LTR_Register use record
      LT       at 0 range 0 .. 11;
      Res4     at 0 range 12 .. 31;
   end record;
   
   
   type SQR1_Register is record  --  regular sequence register 1
      SQ13        : Bits_5;    -- 13th conversion in regular sequence
      SQ14        : Bits_5;    -- 14th conversion in regular sequence
      SQ15        : Bits_5;    -- 15th conversion in regular sequence
      SQ16        : Bits_5;    -- 16th conversion in regular sequence
      L           : Bits_4;    -- Regular channel sequence length
      Res4        : Reserved (24 .. 31) := (others => 0);
   end record;
     
   for SQR1_Register use record
      SQ13     at 0 range 0 .. 4;
      SQ14     at 0 range 5 .. 9;
      SQ15     at 0 range 10 .. 14;
      SQ16     at 0 range 15 .. 19;
      L        at 0 range 20 .. 23;
      Res4     at 0 range 24 .. 31;
   end record;
   
   
   type SQR2_Register is record  --  regular sequence register 2
      SQ7        : Bits_5;    -- 7th conversion in regular sequence
      SQ8        : Bits_5;    -- 8th conversion in regular sequence
      SQ9        : Bits_5;    -- 9th conversion in regular sequence
      SQ10        : Bits_5;    -- 10th conversion in regular sequence
      SQ11        : Bits_5;    -- 11th conversion in regular sequence
      SQ12        : Bits_5;    -- 12th conversion in regular sequence
      Res4        : Reserved (30 .. 31) := (others => 0);
   end record;
     
   for SQR2_Register use record
      SQ7     at 0 range 0 .. 4;
      SQ8     at 0 range 5 .. 9;
      SQ9     at 0 range 10 .. 14;
      SQ10     at 0 range 15 .. 19;
      SQ11     at 0 range 20 .. 24;
      SQ12     at 0 range 25 .. 29;
      Res4     at 0 range 30 .. 31;
   end record;
      
   
   type SQR3_Register is record  --  regular sequence register 3
      SQ1        : Bits_5;    -- 1th conversion in regular sequence
      SQ2        : Bits_5;    -- 2th conversion in regular sequence
      SQ3        : Bits_5;    -- 3th conversion in regular sequence
      SQ4        : Bits_5;    -- 4th conversion in regular sequence
      SQ5        : Bits_5;    -- 5th conversion in regular sequence
      SQ6        : Bits_5;    -- 6th conversion in regular sequence
      Res4       : Reserved (30 .. 31) := (others => 0);
   end record;
     
   for SQR3_Register use record
      SQ1     at 0 range 0 .. 4;
      SQ2     at 0 range 5 .. 9;
      SQ3     at 0 range 10 .. 14;
      SQ4     at 0 range 15 .. 19;
      SQ5     at 0 range 20 .. 24;
      SQ6     at 0 range 25 .. 29;
      Res4    at 0 range 30 .. 31;
   end record;
      
   
   type JSQR_Register is record  --   injected sequence register
      JSQ1       : Bits_5;    -- 1st conversion in injected sequence
      JSQ2       : Bits_5;    -- 2st conversion in injected sequence
      JSQ3       : Bits_5;    -- 3st conversion in injected sequence
      JSQ4       : Bits_5;    -- 4st conversion in injected sequence
      JL         : Bits_2;    -- Injected sequence length
      Res4       : Reserved (22 .. 31) := (others => 0);
   end record;
   
   for JSQR_Register use record
      JSQ1     at 0 range 0 .. 4;
      JSQ2     at 0 range 5 .. 9;
      JSQ3     at 0 range 10 .. 14;
      JSQ4     at 0 range 15 .. 19;
      JL       at 0 range 20 .. 21;
      Res4     at 0 range 22 .. 31;
   end record;
   
   
   type JDR_Register is record  --   injected data register 
      JDATA      : Half_Word; --  Injected data
      Res4       : Reserved (16 .. 31) := (others => 0);
   end record;
   
   for JDR_Register use record
      JDATA    at 0 range 0 .. 15;
      Res4     at 0 range 16 .. 31;
   end record;
   
   
   type DR_Register is record  --   regular data register 
      DATA       : Half_Word; --  regular data
      Res4       : Reserved (16 .. 31) := (others => 0);
   end record;
   
   for DR_Register use record
      DATA    at 0 range 0 .. 15;
      Res4    at 0 range 16 .. 31;
   end record;
      
      
   ---------------------------------------------------
   -- this is the top of the ADC definition         --
   -- usage example:                                --
   --   ADC2 : STM32F4.O7xx.Adc.Adc1_3.Adc_Register --
   ---------------------------------------------------
   
   type Adc_Register is record
     SR       : SR_Register;    --   status register 
     CR1      : CR1_Register;   --   control register 1 
     CR2      : CR2_Register;   --   control register 2 
     SMPR1    : SMPR1_Register; --   sample time register 1
     SMPR2    : SMPR2_Register; --   sample time register 2
     JOFR1    : JOFR_Register;  --   injected channel data offset register 1
     JOFR2    : JOFR_Register;  --   injected channel data offset register 2
     JOFR3    : JOFR_Register;  --   injected channel data offset register 3
     JOFR4    : JOFR_Register;  --   injected channel data offset register 4
     HTR      : HTR_Register;   --   watchdog higher threshold register
     LTR      : LTR_Register;   --   watchdog lower threshold register
     SQR1     : SQR1_Register;  --   regular sequence register 1
     SQR2     : SQR2_Register;  --   regular sequence register 2
     SQR3     : SQR3_Register;  --   regular sequence register 3
     JSQR     : JSQR_Register;  --   injected sequence register
     JDR1     : JDR_Register;   --   injected data register 1
     JDR2     : JDR_Register;   --   injected data register 2
     JDR3     : JDR_Register;   --   injected data register 3
     JDR4     : JDR_Register;   --   injected data register 4
     DR       : DR_Register;    --   regular data register 
   end record;

   for Adc_Register use record
      SR     at 0   range 0 .. 31;
      CR1    at 4   range 0 .. 31;
      CR2    at 8   range 0 .. 31;
      SMPR1  at 12  range 0 .. 31;
      SMPR2  at 16  range 0 .. 31;
      JOFR1  at 20  range 0 .. 31;
      JOFR2  at 24  range 0 .. 31;
      JOFR3  at 28  range 0 .. 31;
      JOFR4  at 32  range 0 .. 31;
      HTR    at 36  range 0 .. 31;
      LTR    at 40  range 0 .. 31;
      SQR1   at 44  range 0 .. 31;
      SQR2   at 48  range 0 .. 31;
      SQR3   at 52  range 0 .. 31;
      JSQR   at 56  range 0 .. 31;
      JDR1   at 60  range 0 .. 31;
      JDR2   at 64  range 0 .. 31;
      JDR3   at 68  range 0 .. 31;
      JDR4   at 72  range 0 .. 31;
      DR     at 76  range 0 .. 31;
   end record;
      
end STM32F4.O7xx.Adc.Adc1_3;

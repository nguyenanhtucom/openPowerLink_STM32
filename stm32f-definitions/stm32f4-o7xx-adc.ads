------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                         S T M 32 F 4 . O7xx . A d c                      --
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

package STM32F4.O7xx.Adc is
   
   --------------------------------------------------
   -- constants for use with the adc definitions   --
   --  in the child packages                       --
   --------------------------------------------------
   
   -------------------------------
   -- ADC_SR status register    --
   -------------------------------
   No_Overrun    : constant Bits_1  := 0;  --  bit 5
   Overrun       : constant Bits_1  := 1;
   -- OVR: Overrun
   
   Not_Started   : constant Bits_1  := 0;  -- bit 4
   Has_Started   : constant Bits_1  := 1;
   --  STRT: Regular channel start flag 
   
   --Not_Started                              bit 3
   --Has_Started
   -- JSTRT: Injected channel start flag
   
   Not_Complete  : constant Bits_1  := 0; --  bit 2
   Complete      : constant Bits_1  := 1;
   --JEOC: Injected channel end of conversion
   
   --Not_Complete                             bit 1
   --Complete
   -- EOC: Regular channel end of conversion
   
   No_Event      : constant Bits_1  := 0; --  bit 0
   Event         : constant Bits_1  := 1;
   -- AWD: Analog watchdog flag
   
   
   --------------------------------
   -- ADC_CR1 control register 1 --
   --------------------------------
   Disabled      : constant Bits_1  := 0;  -- bit 26  
   Enabled       : constant Bits_1  := 1;
   -- OVRIE: Overrun interrupt enable
       
   Bits12       : constant Bits_2  := 0;  -- bit 24-25
   Bits10       : constant Bits_2  := 1;
   Bits8        : constant Bits_2  := 2;  
   Bits6        : constant Bits_2  := 3; 
   -- RES[1:0]: Resolution
   
   --Disabled                  
   --Enabled                                  bit 23
   -- AWDEN: Analog watchdog enable on   regular channels
   
   --Disabled                  
   --Enabled                                  bit 22
   -- JAWDEN: Analog watchdog enable on injected channels
   
   Channels_1    : constant Bits_3  := 0;
   Channels_2    : constant Bits_3  := 1;  -- bit 13-15
   Channels_3    : constant Bits_3  := 2;
   Channels_4    : constant Bits_3  := 3;
   Channels_5    : constant Bits_3  := 4;
   Channels_6    : constant Bits_3  := 5;
   Channels_7    : constant Bits_3  := 6;
   Channels_8    : constant Bits_3  := 7;
   -- DISCNUM[2:0]: Discontinuous mode channel count
   
   --Disabled                                 bit 12
   --Enabled
   -- JDISCEN: Discontinuous mode on injected channels
   
   --Disabled                                 bit 11
   --Enabled
   -- DISCEN: Discontinuous mode on regular channels
   
   --Disabled                                 bit 10
   --Enabled
   -- JAUTO: Automatic injected group conversion
   
   All_Channels  : constant Bits_1  := 0;  -- bit 9
   One_Channel   : constant Bits_1  := 1;
   -- AWDSGL: Enable the watchdog on a single channel in scan mode
   
   --Disabled                                 bit 8
   --Enabled
   -- SCAN: Scan mode
   
   --Disabled                                 bit 7
   --Enabled
   -- JEOCIE: Interrupt enable for injected channels
   
   --Disabled                                 bit 6
   --Enabled
   -- AWDIE: Analog watchdog interrupt enable
   
   --Disabled                                 bit 5
   --Enabled
   -- EOCIE: Interrupt enable for EOC
   
   Channel_0     : constant Bits_5  := 0;  -- bit 0-4
   Channel_1     : constant Bits_5  := 1;
   Channel_2     : constant Bits_5  := 2;
   Channel_3     : constant Bits_5  := 3;
   Channel_4     : constant Bits_5  := 4;
   Channel_5     : constant Bits_5  := 5;
   Channel_6     : constant Bits_5  := 6;
   Channel_7     : constant Bits_5  := 7;
   Channel_8     : constant Bits_5  := 8;
   Channel_9     : constant Bits_5  := 9;  
   Channel_10    : constant Bits_5  := 10;
   Channel_11    : constant Bits_5  := 11;
   Channel_12    : constant Bits_5  := 12;
   Channel_13    : constant Bits_5  := 13;
   Channel_14    : constant Bits_5  := 14;
   Channel_15    : constant Bits_5  := 15;
   Channel_16    : constant Bits_5  := 16;
   Channel_17    : constant Bits_5  := 17;
   Channel_18    : constant Bits_5  := 18;
   -- AWDCH[4:0]: Analog watchdog channel select bits
   
   
   ---------------------------------
   -- ADC_CR2  control register 2 --
   ---------------------------------
   Reset         : constant Bits_1  := 0;  -- bit 30
   Start         : constant Bits_1  := 1;
   -- SWSTART: Start conversion of regular channels
   
   --Disabled                                 bit 28-29
   Up_Edge       : constant Bits_2  := 1;
   Dwn_Edge      : constant Bits_2  := 2;
   Both          : constant Bits_2  := 3;
   -- EXTEN: External trigger enable for regular channels
   
   T1_Cc1        : constant Bits_4  := 0;  -- bit 24-27
   T1_Cc2        : constant Bits_4  := 1;
   T1_Cc3        : constant Bits_4  := 2;
   T2_Cc2        : constant Bits_4  := 3;
   T2_Cc3        : constant Bits_4  := 4;
   T2_Cc4        : constant Bits_4  := 5;
   T2_Trg0       : constant Bits_4  := 6;
   T3_Cc1        : constant Bits_4  := 7;
   T3_Trg0       : constant Bits_4  := 8;
   T4_Cc4        : constant Bits_4  := 9;
   T5_Cc1        : constant Bits_4  := 10;
   T5_Cc2        : constant Bits_4  := 11;
   T5_Cc3        : constant Bits_4  := 12;
   T8_Cc1        : constant Bits_4  := 13;
   T8_Trg0       : constant Bits_4  := 14;
   EXTI_Line11   : constant Bits_4  := 15;
   -- EXTSEL[3:0]: External event select for regular group
   
   --Reset                                    bit 22
   --Start
   -- JSWSTART: Start conversion of injected channels
   
   --Disabled                                 bit 20-21 
   --Up_Edge
   --Dwn_Edge 
   --Both
   -- JEXTEN: External trigger enable for injected channels
   
   T1_Cc4        : constant Bits_4  := 0;  -- bit 16-19
   T1_Trg0       : constant Bits_4  := 1;
   T2_Cc1        : constant Bits_4  := 2;
   --T2_Trg0       : constant Bits_4  := 3;
   T3_Cc2        : constant Bits_4  := 4;
   T3_Cc4        : constant Bits_4  := 5;
   T4_Cc1        : constant Bits_4  := 6;
   T4_Cc2        : constant Bits_4  := 7;
   T4_Cc3        : constant Bits_4  := 8;
   T4_Trg0       : constant Bits_4  := 9;
   T5_Cc4        : constant Bits_4  := 10;
   T5_Trg0       : constant Bits_4  := 11;
   T8_Cc2        : constant Bits_4  := 12;
   T8_Cc3        : constant Bits_4  := 13;
   T8_Cc4        : constant Bits_4  := 14;
   EXTI_Line15   : constant Bits_4  := 15;
   -- JEXTSEL[3:0]: External event select for injected group
   
   Right         : constant Bits_1  := 0;  -- bit 11
   Left          : constant Bits_1  := 1;
   -- ALIGN: Data alignment
   
   End_Sequence  : constant Bits_1  := 0;  -- bit 10
   Each_Convn    : constant Bits_1  := 1;
   -- EOCS: End of conversion selection
   
   Disable       : constant Bits_1  := 0;  -- bit 9
   Enable        : constant Bits_1  := 1;
   -- DDS: DMA disable selection (for single ADC mode)
   
   --Disabled                                 bit 8
   --Enabled
   -- DMA: Direct memory access mode (for single ADC mode)
   
   Single        : constant Bits_1  := 0;  -- bit 1
   Continuous    : constant Bits_1  := 1;
   -- CONT: Continuous conversion
   
   --Disable                                  bit 0
   --Enable
   -- ADON: A/D Converter ON / OFF
   
   
   -------------------------------------------------
   -- ADC_SMPR1 &2 sample time register 1 and 2  --
   -------------------------------------------------
   Cycles_3      : constant Bits_3  := 0;  -- bit 0-26
   Cycles_15     : constant Bits_3  := 1;
   Cycles_28     : constant Bits_3  := 2;
   Cycles_56     : constant Bits_3  := 3;
   Cycles_84     : constant Bits_3  := 4;
   Cycles_112    : constant Bits_3  := 5;
   Cycles_144    : constant Bits_3  := 6;
   Cycles_480    : constant Bits_3  := 7;
   -- SMPx[2:0]: Channel x sampling time selection
   
   
   ---------------------------------------------------------
   -- ADC_SQR1 &2 &3 regular sequence register 1, 2 and 3 --
   -- ADC_JSQR        injected sequence register          --
   ---------------------------------------------------------
   Convns_1      : constant Bits_4  := 0;  -- bit 20-23
   Convns_2      : constant Bits_4  := 1;
   Convns_3      : constant Bits_4  := 2;
   Convns_4      : constant Bits_4  := 3;
   Convns_5      : constant Bits_4  := 4;
   Convns_6      : constant Bits_4  := 5;
   Convns_7      : constant Bits_4  := 6;
   Convns_8      : constant Bits_4  := 7;
   Convns_9      : constant Bits_4  := 8;
   Convns_10     : constant Bits_4  := 9;  
   Convns_11     : constant Bits_4  := 10;
   Convns_12     : constant Bits_4  := 11;
   Convns_13     : constant Bits_4  := 12;
   Convns_14     : constant Bits_4  := 13;
   Convns_15     : constant Bits_4  := 14;
   Convns_16     : constant Bits_4  := 15;
   --  L[3:0]: Regular channel sequence length
   
   Convsns_1      : constant Bits_2  := 0;  -- bit 20-21 injected sequence reg
   Convsns_2      : constant Bits_2  := 1;
   Convsns_3      : constant Bits_2  := 2;
   Convsns_4      : constant Bits_2  := 3;
   --  JL[1:0]: Injected sequence length
   
   --Channel_0                              -- bit 15-19, 10-14, 5-9, 0-4
   --Channel_1                                    
   --Channel_2 
   --Channel_3 
   --Channel_4 
   --Channel_5 
   --Channel_6 
   --Channel_7 
   --Channel_8 
   --Channel_9 
   --Channel_10
   --Channel_11
   --Channel_12
   --Channel_13
   --Channel_14
   --Channel_15
   --Channel_16
   --Channel_17
   --Channel_18
   --  SQxx[4:0]: xxth conversion in regular sequence
   
   
   -------------------------------------
   -- ADC_CSR  Common status register --
   -------------------------------------
   -- use status register definitions.
   
   
   ---------------------------------------
   --  ADC_CCR  common control register --
   ---------------------------------------
   
   --Disabled                                 bit 22 and 23
   --Enabled
   -- TSVREFE: Temperature sensor and VREFINT enable
   -- VBATE: VBAT enable
   
   Freq_Div2     : constant Bits_2  := 0;  -- bit 16-17
   Freq_div4     : constant Bits_2  := 1;
   Freq_div6     : constant Bits_2  := 2;
   Freq_div8     : constant Bits_2  := 3;
   --  ADCPRE: ADC prescaler
   
   None          : constant Bits_2  := 0;  -- bit 14-15
   Dma_Mode1     : constant Bits_2  := 1;
   Dma_Mode2     : constant Bits_2  := 2;
   Dma_Mode3     : constant Bits_2  := 3;
   -- DMA: Direct memory access mode for multi ADC mode
   
   --Disabled                                 bit 13
   --Enabled
   -- DDS: DMA disable selection (for multi-ADC mode)
   
   Delay5        : constant Bits_4  := 0;  -- bit 8-11
   Delay6        : constant Bits_4  := 1;
   Delay7        : constant Bits_4  := 2;
   Delay8        : constant Bits_4  := 3;
   Delay9        : constant Bits_4  := 4;
   Delay10       : constant Bits_4  := 5;
   Delay11       : constant Bits_4  := 6;
   Delay12       : constant Bits_4  := 7;
   Delay13       : constant Bits_4  := 8;
   Delay14       : constant Bits_4  := 9;
   Delay15       : constant Bits_4  := 10;
   Delay16       : constant Bits_4  := 11;
   Delay17       : constant Bits_4  := 12;
   Delay18       : constant Bits_4  := 13;
   Delay19       : constant Bits_4  := 14;
   Delay20       : constant Bits_4  := 15;
   -- DELAY: Delay between 2 sampling phases
   
   Dm_I_Mode     : constant Bits_5  := 0;  -- bit 0-4
   Dm_Crsis_Mode : constant Bits_5  := 1;
   Dm_Crsat_Mode : constant Bits_5  := 2;   
   Dm_Is_Mode    : constant Bits_5  := 5;
   Dm_Rs_Mode    : constant Bits_5  := 6;
   Dm_Il_Mode    : constant Bits_5  := 7;
   Dm_At_Mode    : constant Bits_5  := 9;   
   Tm_Crsis_Mode : constant Bits_5  := 17;
   Tm_Crsat_Mode : constant Bits_5  := 18;   
   Tm_Is_Mode    : constant Bits_5  := 21;
   Tm_Rs_Mode    : constant Bits_5  := 22;
   Tm_Il_Mode    : constant Bits_5  := 23;
   Tm_At_Mode    : constant Bits_5  := 25;
   -- MULTI[4:0]: Multi ADC mode selection
   
end STM32F4.O7xx.Adc;

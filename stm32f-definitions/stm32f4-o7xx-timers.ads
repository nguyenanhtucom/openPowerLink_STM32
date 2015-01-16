------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                      S T M 32 F 4 . O7xx . T i m e r s                   --
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
--  chapter 2.2.21   Timers and watchdogs   STM32F405xx STM32F407xx Datasheet 
--  chapters 17, 18, 19 an 20   Timers      RM0090 Reference manual




pragma Restrictions (No_Elaboration_Code);

--with Interfaces;

package STM32F4.O7xx.Timers is
   
   --------------------------------------------------
   -- constants for use with the timer definitions --
   --  in the child packages                       --
   --------------------------------------------------
   
   ----------------------------------
   -- TIMx_CR1  control register 1 --
   ----------------------------------
   Tck_Int       : constant Bits_2  := 0;  -- bit 8-9
   Tck_Int_X2    : constant Bits_2  := 1;
   Tck_Int_X4    : constant Bits_2  := 2;
   -- CKD[1:0]: Clock division
   
   Not_Buffered  : constant Bits_1  := 0;  -- bit 7
   Buffered      : constant Bits_1  := 1;
   -- ARPE: Auto-reload preload enable
   
   Mode_1        : constant Bits_2  := 1;  -- bit 5-6
   Mode_2        : constant Bits_2  := 2;
   Mode_3        : constant Bits_2  := 3;
   -- CMS[1:0]: Center-aligned mode selection
   
   Up_Counter    : constant Bits_1  := 0;  -- bit 4
   Down_Counter  : constant Bits_1  := 1;
   -- DIR: Direction
   
   Not_Stopped   : constant Bits_1  := 0;  -- bit 3
   Stops         : constant Bits_1  := 1;
   -- OPM: One pulse mode
   
   Many          : constant Bits_1  := 0;  -- bit 2
   One_Only      : constant Bits_1  := 1;
   -- URS: Update request source
   
   UEV_Enabled   : constant Bits_1  := 0;  -- bit 1
   UEV_Disabled  : constant Bits_1  := 1;
   -- UDIS: Update disable
   
   Disabled      : constant Bits_1  := 0;  -- bit 0
   Enabled       : constant Bits_1  := 1;
   -- CEN: Counter enable
   
   
   ----------------------------------
   -- TIMx_CR2  control register 2 --
   ----------------------------------
   Ch1_Connected : constant Bits_1  := 0;  -- bit 7
   Ch123_Connctd : constant Bits_1  := 1;
   -- TI1S: TI1 selection
   
   Oc1ref        : constant Bits_3  := 4;  -- bit 4-6
   Oc2ref        : constant Bits_3  := 5;
   Oc3ref        : constant Bits_3  := 6;
   Oc4ref        : constant Bits_3  := 7;
   --  MMS[2:0]: Master mode selection
   
   CCx_Event     : constant Bits_1  := 0;  -- bit 3
   Update_Event  : constant Bits_1  := 1;
   -- CCDS: Capture/compare DMA selection
   
   COMG_Only     : constant Bits_1  := 0;  -- bit 2
   COMG_or_Edge  : constant Bits_1  := 1;
   --  CCUS: Capture/compare control update selection
   
   Not_Preloaded : constant Bits_1  := 0;  -- bit 0
   Preloaded     : constant Bits_1  := 1;
   -- CCPC: Capture/compare preloaded control
   
   --------------------------------------------
   -- TIMx_SMCR  slave mode control register --
   --------------------------------------------
   Non_Inverted  : constant Bits_1  := 0;  -- bit 15
   Inverted      : constant Bits_1  := 1;
   -- ETP: External trigger polarity
   
   --Disabled                                 bit 14
   --Enabled
   -- ECE: External clock enable
   
   Prescaler_Off : constant Bits_2  := 0;  -- bit 12-13
   Freq_div2     : constant Bits_2  := 1;
   Freq_div4     : constant Bits_2  := 2;
   Freq_div8     : constant Bits_2  := 3;
   -- ETPS[1:0]: External trigger prescaler
   
   S0_N0         : constant Bits_4  := 0;  -- bit 8-11
   S1_N2         : constant Bits_4  := 1;
   S1_N4         : constant Bits_4  := 2;
   S1_N8         : constant Bits_4  := 3;
   S2_N6         : constant Bits_4  := 4;
   S2_N8         : constant Bits_4  := 5;
   S4_N6         : constant Bits_4  := 6;
   S4_N8         : constant Bits_4  := 7;
   S8_N6         : constant Bits_4  := 8;
   S8_N8         : constant Bits_4  := 9;
   S16_N5        : constant Bits_4  := 10;
   S16_N6        : constant Bits_4  := 11;
   S16_N8        : constant Bits_4  := 12;
   S32_N5        : constant Bits_4  := 13;
   S32_N6        : constant Bits_4  := 14;
   S32_N8        : constant Bits_4  := 15;
   -- ETF[3:0]: External trigger filter
   
   No_Sync       : constant Bits_1  := 0;  -- bit 7
   Sync          : constant Bits_1  := 1;
   -- MSM: Master/slave mode
   
   Itr0          : constant Bits_3  := 0;  -- bit 4-6
   Itr1          : constant Bits_3  := 1;
   Itr2          : constant Bits_3  := 2;
   Itr3          : constant Bits_3  := 3;
   TI1F_ED       : constant Bits_3  := 4;
   TI1FP1        : constant Bits_3  := 5;
   TI2FP2        : constant Bits_3  := 6;
   ETRF          : constant Bits_3  := 7;
   -- TS[2:0]: Trigger selection
   
   --Disabled
   Enc_Mode1     : constant Bits_3  := 1;  -- bit 0-2
   Enc_Mode2     : constant Bits_3  := 2;
   Enc_Mode3     : constant Bits_3  := 3;
   Reset_Mode    : constant Bits_3  := 4;
   Gated_Mode    : constant Bits_3  := 5;
   Trigger_Mode  : constant Bits_3  := 6;
   Ext_Clk_Mode  : constant Bits_3  := 7;
   -- SMS: Slave mode selection
   
   
   ----------------------------------------------
   -- TIMx_DIER  DMA/interrupt enable register --
   ----------------------------------------------
   --Disabled                                 all bits
   --Enabled
   
   
   -----------------------------
   -- TIMx_SR status register --
   -----------------------------
   Clear        : constant Bits_1  := 0;  -- bit 9 - 12
   Overcapture  : constant Bits_1  := 1;
   -- CCxOF: Capture/Compare x overcapture flag
   
   No_Break     : constant Bits_1  := 0;  -- bit 7
   Break_Det    : constant Bits_1  := 1;
   -- BIF: Break interrupt flag
   
   No_Trig      : constant Bits_1  := 0;  -- bit 6
   Trig_Pend    : constant Bits_1  := 1;
   -- TIF: Trigger interrupt flag
   
   No_Com       : constant Bits_1  := 0;  -- bit 5
   Com_Pend     : constant Bits_1  := 1;
   -- COMIF: COM interrupt flag
   
   None         : constant Bits_1  := 0;  -- bit 1 - 4
   Match        : constant Bits_1  := 1;
   Captured     : constant Bits_1  := 1;
   -- CCxIF: Capture/Compare x interrupt flag
   
   --Clear        : constant Bits_1  := 0;  -- bit 0
   Update_Pend  : constant Bits_1  := 1;
   -- UIF: Update interrupt flag
   
   
   ----------------------------------------
   -- TIMx_EGR event generation register --
   ----------------------------------------
   Break        : constant Bits_1  := 1;  -- bit 7
   -- BG: Break generation
   
   Trigger      : constant Bits_1  := 1;  -- bit 6
   -- TG: Trigger generation

   Update       : constant Bits_1  := 1;  -- bit 5
   -- COMG: Capture/Compare control update generation
   
   Capture      : constant Bits_1  := 1;  -- bit 1 - 4
   -- CCxG: Capture/Compare x generation
   
   --Update                                  bit 0
   -- UG: Update generation
   
   -------------------------------------------------
   -- TIMx_CCMR1  capture/compare mode register 1 --
   -- output / compare mode                       --
   -------------------------------------------------
   --Disabled                                 bit 7 & 15
   --Enabled
   -- OCxCE: Output Compare x clear enable
   
   Frozen       : constant Bits_3  := 0;  -- bit 4-6 & 12-14
   Set_Chan1    : constant Bits_3  := 1;
   Reset_Chan1  : constant Bits_3  := 2;
   Toggle       : constant Bits_3  := 3;
   Force_Lo     : constant Bits_3  := 4;
   Force_Hi     : constant Bits_3  := 5;
   Pwm_Mode1    : constant Bits_3  := 6;
   Pwm_Mode2    : constant Bits_3  := 7;
   -- OCxM[2:0]: Output Compare x mode
   
   On_Disable   : constant Bits_1  := 0;  -- bit 3 & 11
   On_Enable    : constant Bits_1  := 1;
   -- OCxPE: Output Compare x preload enable
   
   --Disabled                                 bit 2 & 10
   --Enabled
   -- OCxFE: Output Compare x fast enable
   
   ----------------
   -- both modes --
   Cc1_Outp     : constant Bits_2  := 0;  -- bit 0-1
   Cc1_Inp_Ti1  : constant Bits_2  := 1;
   Cc1_Inp_Ti2  : constant Bits_2  := 2;
   Cc1_Inp_Trc  : constant Bits_2  := 3;
   -- CC1S: Capture/Compare 1 selection
   
   Cc2_Outp     : constant Bits_2  := 0;  -- bit 8-9
   Cc2_Inp_Ti1  : constant Bits_2  := 1;
   Cc2_Inp_Ti2  : constant Bits_2  := 2;
   Cc2_Inp_Trc  : constant Bits_2  := 3;
   -- CC2S[1:0]: Capture/Compare 2 selection
   
   ------------------------
   -- Input capture mode --
   
   --S0_N0                                   bit 4-7 & 12-15
   --S1_N2 
   --S1_N4 
   --S1_N8 
   --S2_N6 
   --S2_N8 
   --S4_N6 
   --S4_N8 
   --S8_N6 
   --S8_N8 
   --S16_N5
   --S16_N6
   --S16_N8
   --S32_N5
   --S32_N6
   --S32_N8
   -- ICxF: Input capture x filter
   
   --Prescaler_Off                           bit 2-3 & 10-11
   Every2       : constant Bits_2  := 1;
   Every4       : constant Bits_2  := 2;
   Every8       : constant Bits_2  := 3;
   -- ICxPSC[1:0]: Input capture x prescale
   
   
   --------------------------------------------------
   --  TIMx_CCMR2  capture/compare mode register 2 --
   --  identical to capture/compare mode register  --
   --  except as below                             --
   --------------------------------------------------
   -- both modes --
   Cc3_Outp     : constant Bits_2  := 0;  -- bit 0-1
   Cc3_Inp_Ti1  : constant Bits_2  := 1;
   Cc3_Inp_Ti2  : constant Bits_2  := 2;
   Cc3_Inp_Trc  : constant Bits_2  := 3;
   -- CC3S: Capture/Compare 3 selection
   
   Cc4_Outp     : constant Bits_2  := 0;  -- bit 8-9
   Cc4_Inp_Ti1  : constant Bits_2  := 1;
   Cc4_Inp_Ti2  : constant Bits_2  := 2;
   Cc4_Inp_Trc  : constant Bits_2  := 3;
   -- CC4S: Capture/Compare 4 selection
   
   
   ------------------------------------------------
   -- TIMx_CCER  capture/compare enable register --
   ------------------------------------------------
   Active_Hi   : constant Bits_1  := 0;  -- bit 3, 7, 11
   Active_Lo   : constant Bits_1  := 1;
   Both_Edges  : constant Bits_1  := 1;
   -- CCxNP: Capture/Compare x complementary output polarity
   
   Off         : constant Bits_1  := 0;  -- bit 2, 6, 10
   On          : constant Bits_1  := 1;
   -- CCxNE: Capture/Compare x complementary output enable
   
   --Active_Hi                              bit 1, 5, 9, 13
   --Active_Lo                           
   -- CCxP: Capture/Compare x output polarity
   
   Rising_Edge : constant Bits_1  := 0;  -- bit 1, 5, 9, 13
   Falling_Edge : constant Bits_1  := 1;
   -- CCxP: Capture/Compare x input polarity
   
   --Off                                    bit 0, 4, 8, 12
   --On
   -- CCxE: Capture/Compare x output enable
   --Disabled
   --Enabled
   -- CCxE: Capture/Compare x input enable
   
   
   ------------------------------------
   -- TIMx_DCR  DMA control register --
   ------------------------------------
   Xfer_1      : constant Bits_5  := 0;  -- bit 8-12
   Xfer_2      : constant Bits_5  := 1;
   Xfer_3      : constant Bits_5  := 2;
   Xfer_4      : constant Bits_5  := 3;
   Xfer_5      : constant Bits_5  := 4;
   Xfer_6      : constant Bits_5  := 5;
   Xfer_7      : constant Bits_5  := 6;
   Xfer_8      : constant Bits_5  := 7;
   Xfer_9      : constant Bits_5  := 8;
   Xfer_10     : constant Bits_5  := 9;
   Xfer_11     : constant Bits_5  := 10;
   Xfer_12     : constant Bits_5  := 11;
   Xfer_13     : constant Bits_5  := 12;
   Xfer_14     : constant Bits_5  := 13;
   Xfer_15     : constant Bits_5  := 14;
   Xfer_16     : constant Bits_5  := 15;
   Xfer_17     : constant Bits_5  := 16;
   Xfer_18     : constant Bits_5  := 17;
   -- DBL[4:0]: DMA burst length
   Cr1         : constant Bits_5  := 0;  -- bit 0-4
   Cr2         : constant Bits_5  := 1;
   Smcr        : constant Bits_5  := 2;
   -- DBA[4:0]: DMA base address
   
   -- option registers are not done --
     
   
   
   
end STM32F4.O7xx.Timers;

------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                 S T M 32 F 4 . O7xx . T i m e r s . T 1 _ 8              --
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
--  chapter 17.4  TIM1&TIM8 registers        RM0090 Reference manual


pragma Restrictions (No_Elaboration_Code);

package STM32F4.O7xx.Timers.T1_8 is
   
   type Cr1_Register is record  --   control register 1 
      CEN,           -- Counter enable
      UDIS,          -- Update disable
      URS,           -- Update request source
      OPM,           -- One pulse mode
      DIR  : Bits_1; -- Direction
      CMS  : Bits_2; -- Center-aligned mode selection
      ARPE : Bits_1; -- Auto-reload preload enable
      CKD  : Bits_2; -- Clock division
      Res1 : Reserved (10 .. 15) := (others => 0);
   end record;
   
   for Cr1_Register use record
      CEN  at 0 range 0 .. 0;
      UDIS at 0 range 1 .. 1;
      URS  at 0 range 2 .. 2;
      OPM  at 0 range 3 .. 3;
      DIR  at 0 range 4 .. 4;
      CMS  at 0 range 5 .. 6;
      ARPE at 0 range 7 .. 7;
      CKD  at 0 range 8 .. 9;
      Res1 at 0 range 10 .. 15;
      end record;
   
   
   type Cr2_Register is record
      CCPC  : Bits_1; -- Capture/compare preloaded control, T1 and T8 only
      Res1  : Reserved (1 .. 1) := (others => 0);
      CCUS,           -- Capture/compare control update selection, T1 and T8 only
      CCDS  : Bits_1; -- Capture/compare DMA selection
      MMS   : Bits_3; -- Master mode selection
      TI1S,           -- TI1 selection
      OIS1,           -- Output Idle state 1 (OC1 output), T1 and T8 only
      OIS1N,          -- Output Idle state 1 (OC1N output), T1 and T8 only
      OIS2,           -- Output Idle state 2 (OC2 output), T1 and T8 only
      OIS2N,          -- Output Idle state 3 (OC3 output), T1 and T8 only
      OIS3,           -- Output Idle state 3 (OC3 output), T1 and T8 only
      OIS3N,          -- Output Idle state 3 (OC3N output), T1 and T8 only
      OIS4  : Bits_1; -- Output Idle state 4 (OC4 output), T1 and T8 only
      Res2  : Reserved (15 .. 15) := (others => 0);
   end record;
   
   for Cr2_Register use record
      CCPC  at 0 range 0 .. 0;
      Res1  at 0 range 1 .. 1;
      CCUS  at 0 range 2 .. 2;
      CCDS  at 0 range 3 .. 3;
      MMS   at 0 range 4 .. 6;
      TI1S  at 0 range 7 .. 7;
      OIS1  at 0 range 8 .. 8;
      OIS1N at 0 range 9 .. 9;
      OIS2  at 0 range 10 .. 10;
      OIS2N at 0 range 11 .. 11;
      OIS3  at 0 range 12 .. 12;
      OIS3N at 0 range 13 .. 13;
      OIS4  at 0 range 14 .. 14;
      Res2  at 0 range 15 .. 15;
   end record;
   
   
   type SMCR_Register is record   -- slave mode control register
      SMS   : Bits_3; -- Slave mode selection
      Res1  : Reserved (3 .. 3) := (others => 0);
      TS    : Bits_3; -- Trigger selection
      MSM   : Bits_1; -- Master/slave mode
      ETF   : Bits_4; -- External trigger filter
      ETPS  : Bits_2; -- External trigger prescaler
      ECE,            -- External clock enable
      ETP   : Bits_1; -- External trigger polarity
   end record;
   
   for SMCR_Register use record
      SMS  at 0 range 0 .. 2;
      Res1 at 0 range 3 .. 3;
      TS   at 0 range 4 .. 6;
      MSM  at 0 range 7 .. 7;
      ETF  at 0 range 8 .. 11;
      ETPS at 0 range 12 .. 13;
      ECE  at 0 range 14 .. 14;
      ETP  at 0 range 15 .. 15;
   end record;
   
   
   type DIER_Register is record   --  DMA/interrupt enable register
      UIE,              -- Update interrupt enable
      CC1IE,            -- Capture/Compare 1 interrupt enable
      CC2IE,            -- Capture/Compare 2 interrupt enable
      CC3IE,            -- Capture/Compare 3 interrupt enable
      CC4IE,            -- Capture/Compare 4 interrupt enable
      COMIE,            -- COM interrupt enable
      TIE,              -- Trigger interrupt enable
      BIE,              -- Break interrupt enable
      UDE,              -- Update DMA request enable
      CC1DE,            -- Capture/Compare 1 DMA request enable
      CC2DE,            -- Capture/Compare 2 DMA request enable
      CC3DE,            -- Capture/Compare 3 DMA request enable
      CC4DE,            -- Capture/Compare 4 DMA request enable
      COMDE,            -- COM DMA request enable
      TDE    : Bits_1;  -- Trigger DMA request enable
      Res2   : Reserved (15 .. 15) := (others => 0);
   end record;
   
   for DIER_Register use record
      UIE    at 0 range 0 .. 0;
      CC1IE  at 0 range 1 .. 1;
      CC2IE  at 0 range 2 .. 2;
      CC3IE  at 0 range 3 .. 3;
      CC4IE  at 0 range 4 .. 4;
      COMIE  at 0 range 5 .. 5;
      TIE    at 0 range 6 .. 6;
      BIE    at 0 range 7 .. 7; 
      UDE    at 0 range 8 .. 8; 
      CC1DE  at 0 range 9 .. 9; 
      CC2DE  at 0 range 10 .. 10;
      CC3DE  at 0 range 11 .. 11;
      CC4DE  at 0 range 12 .. 12;
      COMDE  at 0 range 13 .. 13;
      TDE    at 0 range 14 .. 14;
      Res2   at 0 range 15 .. 15;
   end record;
   
      
   type SR_Register is record  -- status register 
      UIF,                -- Update interrupt flag
      CC1IF,              -- CC1IF
      CC2IF,              -- CC2IF
      CC3IF,              -- Capture/Compare 3 interrupt flag
      CC4IF,              -- Capture/Compare 4 interrupt flag
      COMIF,              -- COM interrupt flag
      TIF,                -- Trigger interrupt flag
      BIF      : Bits_1;  -- Break interrupt flag
      Res1     : Reserved (8 .. 8) := (others => 0);
      CC1OF,              -- Capture/Compare 1 overcapture flag
      CC2OF,              -- Capture/Compare 2 overcapture flag
      CC3OF,              -- Capture/Compare 3 overcapture flag
      CC4OF    : Bits_1;  -- Capture/Compare 4 overcapture flag
      Res2     : Reserved (13 .. 15) := (others => 0);
   end record;
   
   for SR_Register use record
      UIF    at 0 range 0 .. 0;	
      CC1IF  at 0 range 1 .. 1;	
      CC2IF  at 0 range 2 .. 2;	
      CC3IF  at 0 range 3 .. 3;	
      CC4IF  at 0 range 4 .. 4;	
      COMIF  at 0 range 5 .. 5;	
      TIF    at 0 range 6 .. 6;	
      BIF    at 0 range 7 .. 7; 
      Res1   at 0 range 8 .. 8; 
      CC1OF  at 0 range 9 .. 9; 
      CC2OF  at 0 range 10 .. 10;
      CC3OF  at 0 range 11 .. 11;
      CC4OF  at 0 range 12 .. 12;
      Res2   at 0 range 13 .. 15;
   end record;
   
   
   type EGR_Register is record  -- event generation register
      UG,                 -- Update generation
      CC1G,               -- Capture/Compare 1 generation
      CC2G,               -- Capture/Compare 2 generation
      CC3G,               -- Capture/Compare 3 generation
      CC4G,               -- Capture/Compare 4 generation
      COMG,               -- Capture/Compare control update generation
      TG,                 -- Trigger generation
      BG       : Bits_1;  -- Break generation
      Res1     : Reserved (8 .. 15) := (others => 0);
   end record;
   
   for EGR_Register use record
      UG    at 0 range 0 .. 0;	
      CC1G  at 0 range 1 .. 1;	
      CC2G  at 0 range 2 .. 2;	
      CC3G  at 0 range 3 .. 3;	
      CC4G  at 0 range 4 .. 4;	
      COMG  at 0 range 5 .. 5;	
      TG    at 0 range 6 .. 6;	
      BG    at 0 range 7 .. 7; 
      Res1  at 0 range  8 .. 15;
   end record;
   
   
   type Mode_Type is (Input_Capture, Output_Compare);
   
   type CCMR1_Register (Mode : Mode_Type := Output_Compare) is record  
      -- capture/compare mode register 1
      CC1S    : Bits_2;             -- Capture/Compare 1 selection
      case Mode is
	 when Input_Capture  =>
	    IC1PSC  : Bits_2;  -- Input capture 1 prescaler
	    IC1F    : Bits_4;  -- Input capture 1 filter
	    iCC2S   : Bits_2;  -- Capture/Compare 2 selection
	    IC2PSC  : Bits_2;  -- Input capture 2 prescaler
	    IC2F    : Bits_4;  -- Input capture 2 filter  
	 when Output_Compare =>
	    OC1FE,             -- Output Compare 1 fast enable
	    OC1PE   : Bits_1;  -- Output Compare 1 preload enable
	    OC1M    : Bits_3;  -- Output Compare 1 mode
	    OC1CE   : Bits_1;  -- Output Compare 1 clear enable
	    oCC2S   : Bits_2;  -- Capture/Compare 2 selection
	    OC2FE,             -- Output Compare 2 fast enable
	    OC2PE   : Bits_1;  -- Output Compare 2 preload enable
	    OC2M    : Bits_3;  -- Output Compare 2 mode
	    OC2CE   : Bits_1;  -- Output Compare 2 clear enable
      end case;
   end record;
   
   for CCMR1_Register use record
      CC1S   at 0 range 0 .. 1;	
      
      IC1PSC at 0 range 2 .. 3;
      IC1F   at 0 range 4 .. 7;
      iCC2S  at 0 range 8 .. 9;
      IC2PSC at 0 range 10 .. 11;
      IC2F   at 0 range 12 .. 15;
      
      OC1FE  at 0 range 2 .. 2;
      OC1PE  at 0 range 3 .. 3;
      OC1M   at 0 range 4 .. 6;
      OC1CE  at 0 range 7 .. 7;
      oCC2S  at 0 range 8 .. 9;
      OC2FE  at 0 range 10 .. 10;
      OC2PE  at 0 range 11 .. 11;
      OC2M   at 0 range 12 .. 14;
      OC2CE  at 0 range 15 .. 15;
   end record;
   
   
   type CCMR2_Register (Mode : Mode_Type := Output_Compare) is record  
      -- capture/compare mode register 2
      CC3S    : Bits_2;             -- Capture/Compare 3 selection
      case Mode is
	 when Input_Capture   =>
	    IC3PSC  : Bits_2;  -- Input capture 3 prescaler
	    IC3F    : Bits_4;  -- Input capture 3 filter
	    iCC4S   : Bits_2;  -- Capture/Compare 4 selection
	    IC4PSC  : Bits_2;  -- Input capture 4 prescaler
	    IC4F    : Bits_4;  -- Input capture 4 filter  
	 when Output_Compare  =>
	    OC3FE,             -- Output Compare 3 fast enable
	    OC3PE   : Bits_1;  -- Output Compare 3 preload enable
	    OC3M    : Bits_3;  -- Output Compare 3 mode
	    OC3CE   : Bits_1;  -- Output Compare 3 clear enable
	    oCC4S   : Bits_2;  -- Capture/Compare 4 selection
	    OC4FE,             -- Output Compare 4 fast enable
	    OC4PE   : Bits_1;  -- Output Compare 4 preload enable
	    OC4M    : Bits_3;  -- Output Compare 4 mode
	    OC4CE   : Bits_1;  -- Output Compare 4 clear enable
      end case;
   end record;
   
   for CCMR2_Register use record
      CC3S   at 0 range 0 .. 1;	
      
      IC3PSC at 0 range 2 .. 3;
      IC3F   at 0 range 4 .. 7;
      iCC4S  at 0 range 8 .. 9;
      IC4PSC at 0 range 10 .. 11;
      IC4F   at 0 range 12 .. 15;
      
      OC3FE  at 0 range 2 .. 2;
      OC3PE  at 0 range 3 .. 3;
      OC3M   at 0 range 4 .. 6;
      OC3CE  at 0 range 7 .. 7;
      oCC4S  at 0 range 8 .. 9;
      OC4FE  at 0 range 10 .. 10;
      OC4PE  at 0 range 11 .. 11;
      OC4M   at 0 range 12 .. 14;
      OC4CE  at 0 range 15 .. 15;
   end record;
   
   
   type CCER_Register is record  -- capture/compare enable register 
      CC1E,               -- Capture/Compare 1 output enable
      CC1P,               -- Capture/Compare 1 output polarity
      CC1NE,              -- Capture/Compare 1 complementary output enable
      CC1NP,              -- Capture/Compare 1 complementary output polarity
      CC2E,               -- Capture/Compare 2 output enable
      CC2P,               -- Capture/Compare 2 output polarity
      CC2NE,              -- Capture/Compare 2 complementary output enable
      CC2NP,              -- Capture/Compare 2 complementary output polarity
      CC3E,               -- Capture/Compare 3 output enable
      CC3P,               -- Capture/Compare 3 output polarity
      CC3NE,              -- Capture/Compare 3 complementary output enable
      CC3NP,              -- Capture/Compare 3 complementary output polarity
      CC4E,               -- Capture/Compare 4 output enable
      CC4P      : Bits_1; -- Capture/Compare 4 output polarity
      Res1      : Reserved (14 .. 15) := (others => 0);
   end record;
   
   for CCER_Register use record
      CC1E   at 0 range 0 .. 0;	 
      CC1P   at 0 range 1 .. 1;	 
      CC1NE  at 0 range 2 .. 2;	 
      CC1NP  at 0 range 3 .. 3;	 
      CC2E   at 0 range 4 .. 4;	 
      CC2P   at 0 range 5 .. 5;	 
      CC2NE  at 0 range 6 .. 6;	 
      CC2NP  at 0 range 7 .. 7;  
      CC3E   at 0 range 8 .. 8;  
      CC3P   at 0 range 9 .. 9;  
      CC3NE  at 0 range 10 .. 10;
      CC3NP  at 0 range 11 .. 11;
      CC4E   at 0 range 12 .. 12;
      CC4P   at 0 range 13 .. 13;
      Res1   at 0 range 14 .. 15;
   end record;
   
   
   subtype CNT_Register is Half_Word;  -- counter
   subtype PSC_Register is Half_Word;  -- prescaler 
   subtype ARR_Register is Half_Word;  -- auto-reload register
   subtype RCR_Register is Byte;       -- repetition counter register
   subtype CCR_Register is Half_Word;  -- capture/compare register
   
   
   type BDTR_Register is record  -- break and dead-time register 
      DTG       : Byte;    -- Dead-time generator setup
      LOCK      : Bits_2;  -- Lock configuration
      OSSI,                -- Off-state selection for Idle mode
      OSSR,                -- Off-state selection for Run mode
      BKE,                 -- Break enable
      BKP,                 -- Break polarity
      AOE,                 -- Automatic output enable
      MOE       : Bits_1;  -- Main output enable
   end record;
   
   for BDTR_Register use record
      DTG   at 0 range 0 .. 7;
      LOCK  at 0 range 8 .. 9;
      OSSI  at 0 range 10 .. 10;
      OSSR  at 0 range 11 .. 11;
      BKE   at 0 range 12 .. 12;
      BKP   at 0 range 13 .. 13;
      AOE   at 0 range 14 .. 14;
      MOE   at 0 range 15 .. 15;
   end record;
   
      
   type DCR_Register is record  --  DMA control register
      DBA       : Bits_5;    --  DMA base address
      Res1      : Reserved (5 .. 7) := (others => 0);
      DBL       : Bits_5;    --  DMA burst length
      Res2      : Reserved (13 .. 15) := (others => 0);
   end record;
   
   for DCR_Register use record
      DBA   at 0 range  0 .. 4;
      Res1  at 0 range  5 .. 7;
      DBL   at 0 range  8 .. 12;
      Res2  at 0 range 13 .. 15;
   end record;
   
   
   subtype DMAR_Register is Half_Word;  -- DMA address for full transfer
   
   
   -----------------------------------------------------------
   -- this is the top of the timer / counter definition     --
   -- usage example:                                        --
   --   T8 : STM32F4.O7xx.Timers.T1_8.Timer_Register;   --
   -----------------------------------------------------------
   	     
   type Timer_Register is record
      CR1         : Cr1_Register;   -- control register 1 
      CR2         : Cr2_Register;   -- control register 2 
      SMCR        : SMCR_Register;  -- slave mode control register
      DIER        : DIER_Register;  -- DMA/interrupt enable register
      SR          : SR_Register;    -- status register 
      EGR         : EGR_Register;   -- event generation register
      CCMR1       : CCMR1_Register; -- capture/compare mode register 1
      CCMR2       : CCMR2_Register; -- capture/compare mode register 2
      CCER        : CCER_Register;  -- capture/compare enable register 
      CNT         : CNT_Register;   -- counter
      PSC         : PSC_Register;   -- prescaler 
      ARR         : ARR_Register;   -- auto-reload register
      RCR         : RCR_Register;   -- repetition counter register
      CCR1        : CCR_Register;  -- capture/compare register 1
      CCR2        : CCR_Register;  -- capture/compare register 1
      CCR3        : CCR_Register;  -- capture/compare register 1
      CCR4        : CCR_Register;  -- capture/compare register 1
      BDTR        : BDTR_Register; -- break and dead-time register 
      DCR         : DCR_Register;  -- DMA control register
      DMAR        : DMAR_Register; -- DMA address for full transfer 
   end record;
   
   for Timer_Register use record
      CR1   at 0   range 0 .. 31;
      CR2   at 4   range 0 .. 31;
      SMCR  at 8   range 0 .. 31;
      DIER  at 12  range 0 .. 31;
      SR    at 16  range 0 .. 31;
      EGR   at 20  range 0 .. 31;
      CCMR1 at 24  range 0 .. 31;
      CCMR2 at 28  range 0 .. 31;
      CCER  at 32  range 0 .. 31;
      CNT   at 36  range 0 .. 31;
      PSC   at 40  range 0 .. 31;
      ARR   at 44  range 0 .. 31;
      RCR   at 48  range 0 .. 31;
      CCR1  at 52  range 0 .. 31;
      CCR2  at 56  range 0 .. 31;
      CCR3  at 60  range 0 .. 31;
      CCR4  at 64  range 0 .. 31;
      BDTR  at 68  range 0 .. 31;
      DCR   at 72  range 0 .. 31;
      DMAR  at 76  range 0 .. 31;
      --    at 80  range 0 .. 31;
   end record;
      
      
   
end STM32F4.O7xx.Timers.T1_8;
 

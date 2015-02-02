------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                        S T M 32 F 4 . O7xx . R c c                       --
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
--  chapter 7.3    RCC registers       RM0090 Reference manual
--

pragma Restrictions (No_Elaboration_Code);

package STM32F4.o7xx.Rcc is

   --------------------------------------------------
   -- constants for use with the rcc definitions   --
   --  in the child packages                       --
   --------------------------------------------------
   
   Locked        : constant Bits_1 := 1;
   Unlocked      : constant Bits_1 := 0;
   Enable        : constant Bits_1 := 1;
   Enable_Lp     : constant Bits_1 := 1;
   On            : constant Bits_1 := 1;
   Bypassed      : constant Bits_1 := 1;
   Ready         : constant Bits_1 := 1;
   Tripped       : constant Bits_1 := 1;
   Reset         : constant Bits_1 := 1;
   Off           : constant Bits_1 := 0;
   Hse_Sel       : constant Bits_1 := 1;
   Hsi_Sel       : constant Bits_1 := 0;
   Sysclk_Sel    : constant Bits_2 := 0;
   PLLI2Sclk_sel : constant Bits_2 := 1;
   HSEclk_Sel    : constant Bits_2 := 2;
   PLLclk_Sel    : constant Bits_2 := 3;
   Nodiv         : constant Bits_3 := 0;
   Div_2         : constant Bits_3 := 4;
   Div_3         : constant Bits_3 := 5;
   Div_4         : constant Bits_3 := 6;
   Div_5         : constant Bits_3 := 7;
   PLLI2Sclk_Use : constant Bits_1 := 0;
   Ext_Clk_Use   : constant Bits_1 := 1;
   Hsi_Clk_Out   : constant Bits_2 := 0;
   Lse_Osc_Out   : constant Bits_2 := 1;
   Hse_Osc_Out   : constant Bits_2 := 2;
   Pll_Clk_Out   : constant Bits_2 := 3;
   Div2          : constant Bits_3 := 4;
   Div4          : constant Bits_3 := 5;
   Div8          : constant Bits_3 := 6;
   Div16         : constant Bits_3 := 7;
   Nodivn        : constant Bits_4 := 0;
   Divn2         : constant Bits_4 := 8;
   Divn4         : constant Bits_4 := 9;
   Divn8         : constant Bits_4 := 10;
   Divn16        : constant Bits_4 := 11;
   Divn64        : constant Bits_4 := 12;
   Divn128       : constant Bits_4 := 13;
   Divn256       : constant Bits_4 := 14;
   Divn512       : constant Bits_4 := 15;
   Hsi_Osc_Used  : constant Bits_2 := 0;
   Lse_Osc_Used  : constant Bits_2 := 1;
   Pll_Used      : constant Bits_2 := 3;
   Clear_Int     : constant Bits_1 := 1;
   No_Clk        : constant Bits_2 := 0;
   Lsi_Osc_Used  : constant Bits_2 := 2;
   Hse_Osc_Used  : constant Bits_2 := 3;
   Down          : constant Bits_1 := 1;
   Center        : constant Bits_1 := 0;
   
   
   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------

   type CR_Register is record
      --  clock control register 
      Res0      : Reserved (28 .. 31) := (others => 0);
      PLLI2SRDY : Bits_1;
      -- PLLI2S clock ready flag
      --- Locked, Unlocked
      PLLI2SON  : Bits_1;
      --  PLLI2S enable
      --- On, Off
      PLLRDY    : Bits_1;
      -- Main PLL (PLL) clock ready flag
      --- Locked, Unlocked
      PLLON     : Bits_1;
      -- Main PLL (PLL) enable
      --- On, Off
      Res1      : Reserved (20 .. 23) := (others => 0);
      CSSON     : Bits_1;
      --  Clock security system enable
      --- On, Off
      HSEBYP    : Bits_1;
      --  HSE clock bypass
      --- Bypassed, Off
      HSERDY    : Bits_1;
      --  HSE clock ready flag
      --- Ready, Off
      HSEON     : Bits_1;
      --  HSE clock enable
      --- On, Off
      HSICAL    : Byte;
      -- Internal high-speed clock calibration
      --- not software controlled, read only
      HSITRIM   : Bits_5;
      --  Internal high-speed clock trimming
      --- ?
      Res2      : Reserved (2 .. 2)   := (others => 0);
      HSIRDY    : Bits_1;
      --  Internal high-speed clock ready flag
      --- Ready, Off
      HSION     : Bits_1;
      --  Internal high-speed clock enable
      --- On, Off
   end record;

   for CR_Register use record
      Res0      at 0 range 28 .. 31;
      PLLI2SRDY at 0 range 27 .. 27;
      PLLI2SON  at 0 range 26 .. 26;
      PLLRDY    at 0 range 25 .. 25;
      PLLON     at 0 range 24 .. 24;
      Res1      at 0 range 20 .. 23;
      CSSON     at 0 range 19 .. 19;
      HSEBYP    at 0 range 18 .. 18;
      HSERDY    at 0 range 17 .. 17;
      HSEON     at 0 range 16 .. 16;
      HSICAL    at 0 range  8 .. 15;
      HSITRIM   at 0 range  3 ..  7;
      Res2      at 0 range  2 ..  2;
      HSIRDY    at 0 range  1 ..  1;
      HSION     at 0 range  0 ..  0;
   end record;

   type PLLCFG_Register is record
      --  PLL configuration register 
      Res0   : Reserved (28 .. 31) := (others => 0);
      PLLQ   : Bits_4;
      -- Main PLL (PLL) division factor for USB OTG FS, SDIO and 
      -- random number generator clocks
      --- any number between 2 and 15
      Res1   : Reserved (23 .. 23) := (others => 0);
      PLLSRC : Bits_1;
      -- Main PLL(PLL) and audio PLL (PLLI2S) entry clock source
      --- Hse_Sel, Hsi_Sel
      Res2   : Reserved (18 .. 21) := (others => 0);
      PLLP   : Bits_2;
      --  Main PLL (PLL) division factor for main system clock
      --- 2, 4, 6, 8
      Res3   : Reserved (15 .. 15) := (others => 0);
      PLLN   : Bits_9;
      --  Main PLL (PLL) multiplication factor for VCO
      ---  between 192 and 432
      PLLM   : Bits_6;
      --  Division factor for the main PLL (PLL) and 
      -- audio PLL (PLLI2S) input clock
      ---  between 2 and 63
   end record;

   for PLLCFG_Register use record
      Res0   at 0 range 28 .. 31;
      PLLQ   at 0 range 24 .. 27;
      Res1   at 0 range 23 .. 23;
      PLLSRC at 0 range 22 .. 22;
      Res2   at 0 range 18 .. 21;
      PLLP   at 0 range 16 .. 17;
      Res3   at 0 range 15 .. 15;
      PLLN   at 0 range  6 .. 14;
      PLLM   at 0 range  0 ..  5;
   end record;

   type CCFG_Register is record
      --  clock configuration register 
      MCO2    : Bits_2;
      --  Microcontroller clock output 2
      --- Sysclk_Sel, PLLI2Sclk_sel, HSEclk_Sel, PLLclk_Sel
      MCO2PRE : Bits_3;
      -- MCO2 prescaler
      --- Nodiv, Div_2, Div_3, Div_4, Div_5
      MCO1PRE : Bits_3;
      --  MCO1 prescaler
      --- Nodiv, Div_2, Div_3, Div_4, Div_5
      I2SSRC  : Bits_1;
      --  I2S clock selection
      --- PLLI2Sclk_Use, Ext_Clk_Use
      MCO1    : Bits_2;
      --  Microcontroller clock output 1
      --- Hsi_Clk_Out, Lse_Osc_Out, Hse_Osc_Out, Pll_Clk_Out
      RTCPRE  : Bits_5;
      -- HSE division factor for RTC clock
      --- number between 2 and 31
      PPRE2   : Bits_3;
      --  APB high-speed prescaler (APB2)
      --- Nodiv, Div2, Div4, Div8, Div16
      PPRE1   : Bits_3;
      -- APB Low speed prescaler (APB1)
      --- Nodiv, Div2, Div4, Div8, Div16
      Res0    : Reserved (8 .. 9) := (others => 0);
      HPRE    : Bits_4;
      -- AHB prescaler
      --- Nodivn, Divn2, Divn4, Divn8, Divn16, Divn64, Divn128, Divn256, Divn512
      SWS     : Bits_2;
      --  System clock switch status
      --- Hsi_Osc_Used, Lse_Osc_Used, Pll_Used
      SW      : Bits_2;
      --  System clock switch
      --- Hsi_Osc_Used, Lse_Osc_Used, Pll_Used
   end record;

   for CCFG_Register use record
      MCO2    at 0 range 30 .. 31;
      MCO2PRE at 0 range 27 .. 29;
      MCO1PRE at 0 range 24 .. 26;
      I2SSRC  at 0 range 23 .. 23;
      MCO1    at 0 range 21 .. 22;
      RTCPRE  at 0 range 16 .. 20;
      PPRE2   at 0 range 13 .. 15;
      PPRE1   at 0 range 10 .. 12;
      Res0    at 0 range  8 ..  9;
      HPRE    at 0 range  4 ..  7;
      SWS     at 0 range  2 ..  3;
      SW      at 0 range  0 ..  1;
   end record;

   type CIR_Register is record
      --  clock interrupt register 
      Res0        : Reserved (24 .. 31) := (others => 0);
      CSSC        : Bits_1;
      --  Clock security system interrupt clear
      --- Clear_Int, Off
      Res1        : Reserved (22 .. 22) := (others => 0);
      PLLI2SRDYC  : Bits_1;
      --  PLLI2S ready interrupt clear
      --- Clear_Int, Off
      PLLRDYC     : Bits_1;
      --  Main PLL(PLL) ready interrupt clear
      --- Clear_Int, Off
      HSERDYC     : Bits_1;
      -- HSE ready interrupt clear
      --- Clear_Int, Off
      HSIRDYC     : Bits_1;
      -- HSI ready interrupt clear
      --- Clear_Int, Off
      LSERDYC     : Bits_1;
      -- LSE ready interrupt clear
      --- Clear_Int, Off
      LSIRDYC     : Bits_1;
      --  LSI ready interrupt clear
      --- Clear_Int, Off
      Res2        : Reserved (14 .. 15) := (others => 0);
      PLLI2SRDYIE : Bits_1;
      -- PLLI2S ready interrupt enable
      --- Enable, Off
      PLLRDYIE    : Bits_1;
      -- Main PLL (PLL) ready interrupt enable
      --- Enable, Off
      HSERDYIE    : Bits_1;
      --  HSE ready interrupt enable
      --- Enable, Off
      HSIRDYIE    : Bits_1;
      -- HSI ready interrupt enable
      --- Enable, Off
      LSERDYIE    : Bits_1;
      -- LSE ready interrupt enable
      --- Enable, Off
      LSIRDYIE    : Bits_1;
      --  LSI ready interrupt enable
      --- Enable, Off
      CSSF        : Bits_1;
      --  Clock security system interrupt flag
      --- Tripped, off
      Res3        : Reserved (6 .. 6)   := (others => 0);
      PLLI2SRDYF  : Bits_1;
      -- : PLLI2S ready interrupt flag
      --- Tripped, off
      PLLRDYF     : Bits_1;
      --  Main PLL (PLL) ready interrupt flag
      --- Tripped, off
      HSERDYF     : Bits_1;
      -- HSE ready interrupt flag
      --- Tripped, off
      HSIRDYF     : Bits_1;
      -- HSI ready interrupt flag
      --- Tripped, off
      LSERDYF     : Bits_1;
      --  LSE ready interrupt flag
      --- Tripped, off
      LSIRDYF     : Bits_1;
      --  LSI ready interrupt flag
      --- Tripped, off
   end record;

   for CIR_Register use record
      Res0        at 0 range 24 .. 31;
      CSSC        at 0 range 23 .. 23;
      Res1        at 0 range 22 .. 22;
      PLLI2SRDYC  at 0 range 21 .. 21;
      PLLRDYC     at 0 range 20 .. 20;
      HSERDYC     at 0 range 19 .. 19;
      HSIRDYC     at 0 range 18 .. 18;
      LSERDYC     at 0 range 17 .. 17;
      LSIRDYC     at 0 range 16 .. 16;
      Res2        at 0 range 14 .. 15;
      PLLI2SRDYIE at 0 range 13 .. 13;
      PLLRDYIE    at 0 range 12 .. 12;
      HSERDYIE    at 0 range 11 .. 11;
      HSIRDYIE    at 0 range 10 .. 10;
      LSERDYIE    at 0 range  9 ..  9;
      LSIRDYIE    at 0 range  8 ..  8;
      CSSF        at 0 range  7 ..  7;
      Res3        at 0 range  6 ..  6;
      PLLI2SRDYF  at 0 range  5 ..  5;
      PLLRDYF     at 0 range  4 ..  4;
      HSERDYF     at 0 range  3 ..  3;
      HSIRDYF     at 0 range  2 ..  2;
      LSERDYF     at 0 range  1 ..  1;
      LSIRDYF     at 0 range  0 ..  0;
   end record;

   type AHB1RST_Register is record
      --  AHB1 peripheral reset register 
      Res0   : Reserved (30 .. 31) := (others => 0);
      OTGHS  : Bits_1;
      --  USB OTG HS module reset
      --- Reset, Off
      Res1   : Reserved (26 .. 28) := (others => 0);
      ETHMAC : Bits_1;
      -- Ethernet MAC reset
      --- Reset, Off
      Res2   : Reserved (23 .. 24) := (others => 0);
      DMA2   : Bits_1;
      --  DMA2 reset
      --- Reset, Off
      DMA1   : Bits_1;
      --  DMA1 reset
      --- Reset, Off
      Res3   : Reserved (13 .. 20) := (others => 0);
      CRC    : Bits_1;
      --  CRC reset
      --- Reset, Off
      Res4   : Reserved (9 .. 11)  := (others => 0);
      GPIOI  : Bits_1;
      --  IO port I reset
      --- Reset, Off
      GPIOH  : Bits_1;
      --  IO port H reset
      --- Reset, Off
      GPIOG  : Bits_1;
      --  IO port G reset
      --- Reset, Off
      GPIOF  : Bits_1;
      --  IO port F reset
      --- Reset, Off
      GPIOE  : Bits_1;
      --  IO port E reset
      --- Reset, Off
      GPIOD  : Bits_1;
      --  IO port D reset
      --- Reset, Off
      GPIOC  : Bits_1;
      --  IO port C reset
      --- Reset, Off
      GPIOB  : Bits_1;
      --  IO port B reset
      --- Reset, Off
      GPIOA  : Bits_1;
      --  IO port A reset
      --- Reset, Off
   end record;

   for AHB1RST_Register use record
      Res0   at 0 range 30 .. 31;
      OTGHS  at 0 range 29 .. 29;
      Res1   at 0 range 26 .. 28;
      ETHMAC at 0 range 25 .. 25;
      Res2   at 0 range 23 .. 24;
      DMA2   at 0 range 22 .. 22;
      DMA1   at 0 range 21 .. 21;
      Res3   at 0 range 13 .. 20;
      CRC    at 0 range 12 .. 12;
      Res4   at 0 range  9 .. 11;
      GPIOI  at 0 range  8 ..  8;
      GPIOH  at 0 range  7 ..  7;
      GPIOG  at 0 range  6 ..  6;
      GPIOF  at 0 range  5 ..  5;
      GPIOE  at 0 range  4 ..  4;
      GPIOD  at 0 range  3 ..  3;
      GPIOC  at 0 range  2 ..  2;
      GPIOB  at 0 range  1 ..  1;
      GPIOA  at 0 range  0 ..  0;
   end record;

   type AHB2RST_Register is record
      --  AHB2 peripheral reset register
      Res0  : Reserved (8 .. 31) := (others => 0);
      OTGFS : Bits_1;
      -- USB OTG FS module reset
      --- Reset, Off
      RNG   : Bits_1;
      --  Random number generator module reset
      --- Reset, Off
      HASH  : Bits_1;
      -- Hash module reset
      --- Reset, Off
      CRYP  : Bits_1;
      --  Cryptographic module reset
      --- Reset, Off
      Res1  : Reserved (1 .. 3)  := (others => 0);
      DCMI  : Bits_1;
      --  Camera interface reset
      --- Reset, Off
   end record;

   for AHB2RST_Register use record
      Res0  at 0 range 8 .. 31;
      OTGFS at 0 range 7 ..  7;
      RNG   at 0 range 6 ..  6;
      HASH  at 0 range 5 ..  5;
      CRYP  at 0 range 4 ..  4;
      Res1  at 0 range 1 ..  3;
      DCMI  at 0 range 0 ..  0;
   end record;

   type AHB3RST_Register is record
      --  AHB3 peripheral reset register
      Res0 : Reserved (1 .. 31) := (others => 0);
      FSMC : Bits_1;
      -- Flexible static memory controller module reset
      --- Reset, Off
   end record;

   for AHB3RST_Register use record
      Res0 at 0 range 1 .. 31;
      FSMC at 0 range 0 ..  0;
   end record;

   type APB1RST_Register is record
      -- APB1 peripheral reset register
      Res0  : Reserved (30 .. 31) := (others => 0);
      DAC   : Bits_1;
      --  DAC reset
      --- Reset, Off
      PWR   : Bits_1;
      --  Power interface reset
      --- Reset, Off
      Res1  : Reserved (27 .. 27) := (others => 0);
      CAN2  : Bits_1;
      -- CAN2 reset
      --- Reset, Off
      CAN1  : Bits_1;
      -- CAN1 reset
      --- Reset, Off
      Res2  : Reserved (24 .. 24) := (others => 0);
      I2C3  : Bits_1;
      --  I2C3 reset
      --- Reset, Off
      I2C2  : Bits_1;
      --  I2C2 reset
      --- Reset, Off
      I2C1  : Bits_1;
      --  I2C1 reset
      --- Reset, Off
      UART5 : Bits_1;
      --  UART5 reset
      --- Reset, Off
      UART4 : Bits_1;
      --  USART4 reset
      --- Reset, Off
      UART3 : Bits_1;
      --  USART3 reset
      --- Reset, Off
      UART2 : Bits_1;
      --  USART2 reset
      --- Reset, Off
      Res3  : Reserved (16 .. 16) := (others => 0);
      SPI3  : Bits_1;
      --  SPI3 reset
      --- Reset, Off
      SPI2  : Bits_1;
      --  SPI2 reset
      --- Reset, Off
      Res4  : Reserved (12 .. 13) := (others => 0);
      WWDG  : Bits_1;
      -- Window watchdog reset
      --- Reset, Off
      Res5  : Reserved (9 .. 10)  := (others => 0);
      TIM14 : Bits_1;
      --  TIM14 reset
      --- Reset, Off
      TIM13 : Bits_1;
      --  TIM13 reset
      --- Reset, Off
      TIM12 : Bits_1;
      --  TIM12 reset
      --- Reset, Off
      TIM7  : Bits_1;
      --  TIM7 reset
      --- Reset, Off
      TIM6  : Bits_1;
      --  TIM6 reset
      --- Reset, Off
      TIM5  : Bits_1;
      --  TIM5 reset
      --- Reset, Off
      TIM4  : Bits_1;
      --  TIM4 reset
      --- Reset, Off
      TIM3  : Bits_1;
      --  TIM3 reset
      --- Reset, Off
      TIM2  : Bits_1;
      --  TIM2 reset
      --- Reset, Off
   end record;

   for APB1RST_Register use record
      Res0  at 0 range 30 .. 31;
      DAC   at 0 range 29 .. 29;
      PWR   at 0 range 28 .. 28;
      Res1  at 0 range 27 .. 27;
      CAN2  at 0 range 26 .. 26;
      CAN1  at 0 range 25 .. 25;
      Res2  at 0 range 24 .. 24;
      I2C3  at 0 range 23 .. 23;
      I2C2  at 0 range 22 .. 22;
      I2C1  at 0 range 21 .. 21;
      UART5 at 0 range 20 .. 20;
      UART4 at 0 range 19 .. 19;
      UART3 at 0 range 18 .. 18;
      UART2 at 0 range 17 .. 17;
      Res3  at 0 range 16 .. 16;
      SPI3  at 0 range 15 .. 15;
      SPI2  at 0 range 14 .. 14;
      Res4  at 0 range 12 .. 13;
      WWDG  at 0 range 11 .. 11;
      Res5  at 0 range  9 .. 10;
      TIM14 at 0 range  8 ..  8;
      TIM13 at 0 range  7 ..  7;
      TIM12 at 0 range  6 ..  6;
      TIM7  at 0 range  5 ..  5;
      TIM6  at 0 range  4 ..  4;
      TIM5  at 0 range  3 ..  3;
      TIM4  at 0 range  2 ..  2;
      TIM3  at 0 range  1 ..  1;
      TIM2  at 0 range  0 ..  0;
   end record;

   type APB2RST_Register is record
      -- 
      Res0   : Reserved (19 .. 31) := (others => 0);
      TIM11  : Bits_1;
      --  TIM11 reset
      --- Reset, Off
      TIM10  : Bits_1;
      --  TIM10 reset
      --- Reset, Off
      TIM9   : Bits_1;
      --  TIM 9 reset
      --- Reset, Off
      Res1   : Reserved (15 .. 15) := (others => 0);
      SYSCFG : Bits_1;
      -- System configuration controller reset
      --- Reset, Off
      Res2   : Reserved (13 .. 13) := (others => 0);
      SPI1   : Bits_1;
      --  SPI1 reset
      --- Reset, Off
      SDIO   : Bits_1;
      --  SDIO reset
      --- Reset, Off
      Res3   : Reserved (9 .. 10)  := (others => 0);
      ADC    : Bits_1;
      -- ADC interface reset (common to all ADCs)
      --- Reset, Off
      Res4   : Reserved (6 .. 7)   := (others => 0);
      UART6  : Bits_1;
      --  USART6 reset
      --- Reset, Off
      UART1  : Bits_1;
      --  USART1 reset
      --- Reset, Off
      Res5   : Reserved (2 .. 3)   := (others => 0);
      TIM8   : Bits_1;
      -- TIM8 reset
      --- Reset, Off
      TIM1   : Bits_1;
      -- TIM1 reset
      --- Reset, Off
   end record;

   for APB2RST_Register use record
      Res0   at 0 range 19 .. 31;
      TIM11  at 0 range 18 .. 18;
      TIM10  at 0 range 17 .. 17;
      TIM9   at 0 range 16 .. 16;
      Res1   at 0 range 15 .. 15;
      SYSCFG at 0 range 14 .. 14;
      Res2   at 0 range 13 .. 13;
      SPI1   at 0 range 12 .. 12;
      SDIO   at 0 range 11 .. 11;
      Res3   at 0 range  9 .. 10;
      ADC    at 0 range  8 ..  8;
      Res4   at 0 range  6 ..  7;
      UART6  at 0 range  5 ..  5;
      UART1  at 0 range  4 ..  4;
      Res5   at 0 range  2 ..  3;
      TIM8   at 0 range  1 ..  1;
      TIM1   at 0 range  0 ..  0;
   end record;

   type AHB1EN_Register is record
      --  AHB1 peripheral clock enable register
      Res0       : Reserved (31 .. 31) := (others => 0);
      OTGHSULPI  : Bits_1;
      -- USB OTG HSULPI clock enable
      --- Enable, Off
      OTGHS      : Bits_1;
      -- USB OTG HS clock enable
      --- Enable, Off
      ETHMACPTP  : Bits_1;
      -- Ethernet PTP clock enable
      --- Enable, Off
      ETHMACRX   : Bits_1;
      -- Ethernet Reception clock enable
      --- Enable, Off
      ETHMACTX   : Bits_1;
      -- Ethernet Transmission clock enable
      --- Enable, Off
      ETHMAC     : Bits_1;
      -- Ethernet MAC clock enable
      --- Enable, Off
      Res2       : Reserved (23 .. 24) := (others => 0);
      DMA2       : Bits_1;
      --  DMA2 clock enable
      --- Enable, Off
      DMA1       : Bits_1;
      --  DMA1 clock enable
      --- Enable, Off
      CCMDATARAM : Bits_1;
      --  CCM data RAM clock enable
      --- Enable, Off
      Res3       : Reserved (19 .. 19) := (others => 0);
      BKPSRAM    : Bits_1;
      --  Backup SRAM interface clock enable
      --- Enable, Off
      Res5       : Reserved (13 .. 17) := (others => 0);
      CRC        : Bits_1;
      -- CRC clock enable
      --- Enable, Off
      Res4       : Reserved (9 .. 11)  := (others => 0);
      GPIOI      : Bits_1;
      -- IO port I clock enable
      --- Enable, Off
      GPIOH      : Bits_1;
      -- IO port H clock enable
      --- Enable, Off
      GPIOG      : Bits_1;
      -- IO port G clock enable
      --- Enable, Off
      GPIOF      : Bits_1;
      -- IO port F clock enable
      --- Enable, Off
      GPIOE      : Bits_1;
      -- IO port E clock enable
      --- Enable, Off
      GPIOD      : Bits_1;
      -- IO port D clock enable
      --- Enable, Off
      GPIOC      : Bits_1;
      -- IO port C clock enable
      --- Enable, Off
      GPIOB      : Bits_1;
      -- IO port B clock enable
      --- Enable, Off
      GPIOA      : Bits_1;
      -- IO port A clock enable
      --- Enable, Off
   end record;

   for AHB1EN_Register use record
      Res0       at 0 range 31 .. 31;
      OTGHSULPI  at 0 range 30 .. 30;
      OTGHS      at 0 range 29 .. 29;
      ETHMACPTP  at 0 range 28 .. 28;
      ETHMACRX   at 0 range 27 .. 27;
      ETHMACTX   at 0 range 26 .. 26;
      ETHMAC     at 0 range 25 .. 25;
      Res2       at 0 range 23 .. 24;
      DMA2       at 0 range 22 .. 22;
      DMA1       at 0 range 21 .. 21;
      CCMDATARAM at 0 range 20 .. 20;
      Res3       at 0 range 19 .. 19;
      BKPSRAM    at 0 range 18 .. 18;
      Res5       at 0 range 13 .. 17;
      CRC        at 0 range 12 .. 12;
      Res4       at 0 range  9 .. 11;
      GPIOI      at 0 range  8 ..  8;
      GPIOH      at 0 range  7 ..  7;
      GPIOG      at 0 range  6 ..  6;
      GPIOF      at 0 range  5 ..  5;
      GPIOE      at 0 range  4 ..  4;
      GPIOD      at 0 range  3 ..  3;
      GPIOC      at 0 range  2 ..  2;
      GPIOB      at 0 range  1 ..  1;
      GPIOA      at 0 range  0 ..  0;
   end record;

   type AHB2EN_Register is record
      -- AHB2 peripheral clock enable register 
      Res0  : Reserved (8 .. 31) := (others => 0);
      OTGFS : Bits_1;
      --  USB OTG FS clock enable
      --- Enable, Off
      RNG   : Bits_1;
      --  Random number generator clock enable
      --- Enable, Off
      HASH  : Bits_1;
      --  Hash modules clock enable
      --- Enable, Off
      CRYP  : Bits_1;
      --  Cryptographic modules clock enable
      --- Enable, Off
      Res1  : Reserved (1 .. 3)  := (others => 0);
      DCMI  : Bits_1;
      -- Camera interface enable
      --- Enable, Off
   end record;

   for AHB2EN_Register use record
      Res0  at 0 range 8 .. 31;
      OTGFS at 0 range 7 ..  7;
      RNG   at 0 range 6 ..  6;
      HASH  at 0 range 5 ..  5;
      CRYP  at 0 range 4 ..  4;
      Res1  at 0 range 1 ..  3;
      DCMI  at 0 range 0 ..  0;
   end record;

   type AHB3EN_Register is record
      -- AHB3 peripheral clock enable register
      Res0 : Reserved (1 .. 31) := (others => 0);
      FSMC : Bits_1;
      --  Flexible static memory controller module clock enable
      --- Enable, Off
   end record;

   for AHB3EN_Register use record
      Res0 at 0 range 1 .. 31;
      FSMC at 0 range 0 ..  0;
   end record;

   type APB1EN_Register is record
      -- APB1 peripheral clock enable register
      Res0  : Reserved (30 .. 31) := (others => 0);
      DAC   : Bits_1;
      -- DAC interface clock enable
      --- Enable, Off
      PWR   : Bits_1;
      --  Power interface clock enable
      --- Enable, Off
      Res1  : Reserved (27 .. 27) := (others => 0);
      CAN2  : Bits_1;
      --  CAN 2 clock enable
      --- Enable, Off
      CAN1  : Bits_1;
      --  CAN 1 clock enable
      --- Enable, Off
      Res2  : Reserved (24 .. 24) := (others => 0);
      I2C3  : Bits_1;
      --  I2C3 clock enable
      --- Enable, Off
      I2C2  : Bits_1;
      --  I2C2 clock enable
      --- Enable, Off
      I2C1  : Bits_1;
      --  I2C1 clock enable
      --- Enable, Off
      UART5 : Bits_1;
      -- UART5 clock enable
      --- Enable, Off
      UART4 : Bits_1;
      -- UART4 clock enable
      --- Enable, Off
      UART3 : Bits_1;
      -- UsART3 clock enable
      --- Enable, Off
      UART2 : Bits_1;
      -- UsART2 clock enable
      --- Enable, Off
      Res3  : Reserved (16 .. 16) := (others => 0);
      SPI3  : Bits_1;
      --  SPI3 clock enable
      --- Enable, Off
      SPI2  : Bits_1;
      --  SPI23 clock enable
      --- Enable, Off
      Res4  : Reserved (12 .. 13) := (others => 0);
      WWDG  : Bits_1;
      -- Window watchdog clock enable
      --- Enable, Off
      Res5  : Reserved (9 .. 10)  := (others => 0);
      TIM14 : Bits_1;
      -- TIM14 clock enable
      --- Enable, Off
      TIM13 : Bits_1;
      -- TIM13 clock enable
      --- Enable, Off
      TIM12 : Bits_1;
      -- TIM12 clock enable
      --- Enable, Off
      TIM7  : Bits_1;
      -- TIM7 clock enable
      --- Enable, Off
      TIM6  : Bits_1;
      -- TIM6 clock enable
      --- Enable, Off
      TIM5  : Bits_1;
      -- TIM5 clock enable
      --- Enable, Off
      TIM4  : Bits_1;
      -- TIM4 clock enable
      --- Enable, Off
      TIM3  : Bits_1;
      -- TIM3 clock enable
      --- Enable, Off
      TIM2  : Bits_1;
      -- TIM2 clock enable
      --- Enable, Off
   end record;

   for APB1EN_Register use record
      Res0  at 0 range 30 .. 31;
      DAC   at 0 range 29 .. 29;
      PWR   at 0 range 28 .. 28;
      Res1  at 0 range 27 .. 27;
      CAN2  at 0 range 26 .. 26;
      CAN1  at 0 range 25 .. 25;
      Res2  at 0 range 24 .. 24;
      I2C3  at 0 range 23 .. 23;
      I2C2  at 0 range 22 .. 22;
      I2C1  at 0 range 21 .. 21;
      UART5 at 0 range 20 .. 20;
      UART4 at 0 range 19 .. 19;
      UART3 at 0 range 18 .. 18;
      UART2 at 0 range 17 .. 17;
      Res3  at 0 range 16 .. 16;
      SPI3  at 0 range 15 .. 15;
      SPI2  at 0 range 14 .. 14;
      Res4  at 0 range 12 .. 13;
      WWDG  at 0 range 11 .. 11;
      Res5  at 0 range  9 .. 10;
      TIM14 at 0 range  8 ..  8;
      TIM13 at 0 range  7 ..  7;
      TIM12 at 0 range  6 ..  6;
      TIM7  at 0 range  5 ..  5;
      TIM6  at 0 range  4 ..  4;
      TIM5  at 0 range  3 ..  3;
      TIM4  at 0 range  2 ..  2;
      TIM3  at 0 range  1 ..  1;
      TIM2  at 0 range  0 ..  0;
   end record;

   type APB2EN_Register is record
      --  APB2 peripheral clock enable register
      Res0   : Reserved (19 .. 31) := (others => 0);
      TIM11  : Bits_1;
      --  TIM11 clock enable
      --- Enable, Off
      TIM10  : Bits_1;
      --  TIM10 clock enable
      --- Enable, Off
      TIM9   : Bits_1;
      --  TIM 9 clock enable
      --- Enable, Off
      Res1   : Reserved (15 .. 15) := (others => 0);
      SYSCFG : Bits_1;
      -- System configuration controller clock enable
      --- Enable, Off
      Res2   : Reserved (13 .. 13) := (others => 0);
      SPI1   : Bits_1;
      --  SPI1 clock enable
      --- Enable, Off
      SDIO   : Bits_1;
      -- SDIO clock enable
      --- Enable, Off
      ADC3   : Bits_1;
      -- ADC3 clock enable
      --- Enable, Off
      ADC2   : Bits_1;
      -- ADC2 clock enable
      --- Enable, Off
      ADC1   : Bits_1;
      -- ADC1 clock enable
      --- Enable, Off
      Res4   : Reserved (6 .. 7)   := (others => 0);
      UART6  : Bits_1;
      --  USART6 clock enable
      --- Enable, Off
      UART1  : Bits_1;
      --  USART1 clock enable
      --- Enable, Off
      Res5   : Reserved (2 .. 3)   := (others => 0);
      TIM8   : Bits_1;
      -- TIM8 clock enable
      --- Enable, Off
      TIM1   : Bits_1;
      -- TIM1 clock enable
      --- Enable, Off
   end record;

   for APB2EN_Register use record
      Res0   at 0 range 19 .. 31;
      TIM11  at 0 range 18 .. 18;
      TIM10  at 0 range 17 .. 17;
      TIM9   at 0 range 16 .. 16;
      Res1   at 0 range 15 .. 15;
      SYSCFG at 0 range 14 .. 14;
      Res2   at 0 range 13 .. 13;
      SPI1   at 0 range 12 .. 12;
      SDIO   at 0 range 11 .. 11;
      ADC3   at 0 range 10 .. 10;
      ADC2   at 0 range  9 ..  9;
      ADC1   at 0 range  8 ..  8;
      Res4   at 0 range  6 ..  7;
      UART6  at 0 range  5 ..  5;
      UART1  at 0 range  4 ..  4;
      Res5   at 0 range  2 ..  3;
      TIM8   at 0 range  1 ..  1;
      TIM1   at 0 range  0 ..  0;
   end record;

   type AHB1LPEN_Register is record
      -- AHB1 peripheral clock enable in low power mode register
      Res0      : Reserved (31 .. 31) := (others => 0);
      OTGHSULPI : Bits_1;
      -- USB OTG HS ULPI clock enable during Sleep mode
      --- Enable_Lp, off
      OTGHS     : Bits_1;
      --  USB OTG HS clock enable during Sleep mode
      --- Enable_Lp, off
      ETHMACPTP : Bits_1;
      --  Ethernet PTP clock enable during Sleep mode
      --- Enable_Lp, off
      ETHMACRX  : Bits_1;
      --  Ethernet reception clock enable during Sleep mode
      --- Enable_Lp, off
      ETHMACTX  : Bits_1;
      --  Ethernet transmission clock enable during Sleep mode
      --- Enable_Lp, off
      ETHMAC    : Bits_1;
      -- Ethernet MAC clock enable during Sleep mode
      --- Enable_Lp, off
      Res2      : Reserved (23 .. 24) := (others => 0);
      DMA2      : Bits_1;
      --  DMA2 clock enable during Sleep mode
      --- Enable_Lp, off
      DMA1      : Bits_1;
      --  DMA1 clock enable during Sleep mode
      --- Enable_Lp, off
      Res3      : Reserved (19 .. 20) := (others => 0);
      BKPSRAM   : Bits_1;
      --  Backup SRAM interface clock enable during Sleep mode
      --- Enable_Lp, off
      SRAM2     : Bits_1;
      -- SRAM 2 interface clock enable during Sleep mode
      --- Enable_Lp, off
      SRAM1     : Bits_1;
      -- SRAM 1 interface clock enable during Sleep mode
      --- Enable_Lp, off
      FLITF     : Bits_1;
      --  Flash interface clock enable during Sleep mode
      --- Enable_Lp, off
      Res5      : Reserved (13 .. 14) := (others => 0);
      CRC       : Bits_1;
      -- CRC clock enable during Sleep mode
      --- Enable_Lp, off
      Res4      : Reserved (9 .. 11)  := (others => 0);
      GPIOI     : Bits_1;
      --  IO port I clock enable during Sleep mode
      --- Enable_Lp, off
      GPIOH     : Bits_1;
      --  IO port H clock enable during Sleep mode
      --- Enable_Lp, off
      GPIOG     : Bits_1;
      --  IO port G clock enable during Sleep mode
      --- Enable_Lp, off
      GPIOF     : Bits_1;
      --  IO port F clock enable during Sleep mode
      --- Enable_Lp, off
      GPIOE     : Bits_1;
      --  IO port E clock enable during Sleep mode
      --- Enable_Lp, off
      GPIOD     : Bits_1;
      --  IO port D clock enable during Sleep mode
      --- Enable_Lp, off
      GPIOC     : Bits_1;
      --  IO port C clock enable during Sleep mode
      --- Enable_Lp, off
      GPIOB     : Bits_1;
      --  IO port B clock enable during Sleep mode
      --- Enable_Lp, off
      GPIOA     : Bits_1;
      --  IO port A clock enable during Sleep mode
      --- Enable_Lp, off
   end record;

   for AHB1LPEN_Register use record
      Res0      at 0 range 31 .. 31;
      OTGHSULPI at 0 range 30 .. 30;
      OTGHS     at 0 range 29 .. 29;
      ETHMACPTP at 0 range 28 .. 28;
      ETHMACRX  at 0 range 27 .. 27;
      ETHMACTX  at 0 range 26 .. 26;
      ETHMAC    at 0 range 25 .. 25;
      Res2      at 0 range 23 .. 24;
      DMA2      at 0 range 22 .. 22;
      DMA1      at 0 range 21 .. 21;
      Res3      at 0 range 19 .. 20;
      BKPSRAM   at 0 range 18 .. 18;
      SRAM2     at 0 range 17 .. 17;
      SRAM1     at 0 range 16 .. 16;
      FLITF     at 0 range 15 .. 15;
      Res5      at 0 range 13 .. 14;
      CRC       at 0 range 12 .. 12;
      Res4      at 0 range  9 .. 11;
      GPIOI     at 0 range  8 ..  8;
      GPIOH     at 0 range  7 ..  7;
      GPIOG     at 0 range  6 ..  6;
      GPIOF     at 0 range  5 ..  5;
      GPIOE     at 0 range  4 ..  4;
      GPIOD     at 0 range  3 ..  3;
      GPIOC     at 0 range  2 ..  2;
      GPIOB     at 0 range  1 ..  1;
      GPIOA     at 0 range  0 ..  0;
   end record;

   type AHB2LPEN_Register is record
      --  AHB2 peripheral clock enable in low power mode register
      Res0  : Reserved (8 .. 31) := (others => 0);
      OTGFS : Bits_1;
      --  USB OTG FS clock enable during Sleep mode
      --- Enable_Lp, off
      RNG   : Bits_1;
      --  Random number generator clock enable during Sleep mode
      --- Enable_Lp, off
      HASH  : Bits_1;
      -- Hash modules clock enable during Sleep mode
      --- Enable_Lp, off
      CRYP  : Bits_1;
      --  Cryptography modules clock enable during Sleep mode
      --- Enable_Lp, off
      Res1  : Reserved (1 .. 3)  := (others => 0);
      DCMI  : Bits_1;
      --  Camera interface enable during Sleep mode
      --- Enable_Lp, off
   end record;

   for AHB2LPEN_Register use record
      Res0  at 0 range 8 .. 31;
      OTGFS at 0 range 7 ..  7;
      RNG   at 0 range 6 ..  6;
      HASH  at 0 range 5 ..  5;
      CRYP  at 0 range 4 ..  4;
      Res1  at 0 range 1 ..  3;
      DCMI  at 0 range 0 ..  0;
   end record;

   type AHB3LPEN_Register is record
      -- AHB3 peripheral clock enable in low power mode register
      Res0 : Reserved (1 .. 31) := (others => 0);
      FSMC : Bits_1;
      --  Flexible static memory controller module clock enable 
      --  during Sleep mode
      --- Enable_Lp, off
   end record;

   for AHB3LPEN_Register use record
      Res0 at 0 range 1 .. 31;
      FSMC at 0 range 0 ..  0;
   end record;

   type APB1LPEN_Register is record
      --  APB1 peripheral clock enable in low power mode register
      --- Enable_Lp, off
      Res0  : Reserved (30 .. 31) := (others => 0);
      DAC   : Bits_1;
      -- DAC interface clock enable during Sleep mode
      --- Enable_Lp, off
      PWR   : Bits_1;
      -- Power interface clock enable during Sleep mode
      --- Enable_Lp, off
      Res1  : Reserved (27 .. 27) := (others => 0);
      CAN2  : Bits_1;
      --  CAN 2 clock enable during Sleep mode
      --- Enable_Lp, off
      CAN1  : Bits_1;
      --  CAN 1 clock enable during Sleep mode
      --- Enable_Lp, off
      Res2  : Reserved (24 .. 24) := (others => 0);
      I2C3  : Bits_1;
      --  I2C3 clock enable during Sleep mode
      --- Enable_Lp, off
      I2C2  : Bits_1;
      --  I2C2 clock enable during Sleep mode
      --- Enable_Lp, off
      I2C1  : Bits_1;
      --  I2C1 clock enable during Sleep mode
      --- Enable_Lp, off
      UART5 : Bits_1;
      --  UART5 clock enable during Sleep mode
      --- Enable_Lp, off
      UART4 : Bits_1;
      --  UART4 clock enable during Sleep mode
      --- Enable_Lp, off
      UART3 : Bits_1;
      --  UsART3 clock enable during Sleep mode
      --- Enable_Lp, off
      UART2 : Bits_1;
      --  UsART2 clock enable during Sleep mode
      --- Enable_Lp, off
      Res3  : Reserved (16 .. 16) := (others => 0);
      SPI3  : Bits_1;
      -- SPI3 clock enable during Sleep mode
      --- Enable_Lp, off
      SPI2  : Bits_1;
      -- SPI2 clock enable during Sleep mode
      --- Enable_Lp, off
      Res4  : Reserved (12 .. 13) := (others => 0);
      WWDG  : Bits_1;
      --  Window watchdog clock enable during Sleep mode
      --- Enable_Lp, off
      Res5  : Reserved (9 .. 10)  := (others => 0);
      TIM14 : Bits_1;
      --  TIM14 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM13 : Bits_1;
      --  TIM13 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM12 : Bits_1;
      --  TIM12 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM7  : Bits_1;
      --  TIM7 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM6  : Bits_1;
      --  TIM6 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM5  : Bits_1;
      --  TIM5 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM4  : Bits_1;
      --  TIM4 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM3  : Bits_1;
      --  TIM3 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM2  : Bits_1;
      --  TIM2 clock enable during Sleep mode
      --- Enable_Lp, off
   end record;

   for APB1LPEN_Register use record
      Res0  at 0 range 30 .. 31;
      DAC   at 0 range 29 .. 29;
      PWR   at 0 range 28 .. 28;
      Res1  at 0 range 27 .. 27;
      CAN2  at 0 range 26 .. 26;
      CAN1  at 0 range 25 .. 25;
      Res2  at 0 range 24 .. 24;
      I2C3  at 0 range 23 .. 23;
      I2C2  at 0 range 22 .. 22;
      I2C1  at 0 range 21 .. 21;
      UART5 at 0 range 20 .. 20;
      UART4 at 0 range 19 .. 19;
      UART3 at 0 range 18 .. 18;
      UART2 at 0 range 17 .. 17;
      Res3  at 0 range 16 .. 16;
      SPI3  at 0 range 15 .. 15;
      SPI2  at 0 range 14 .. 14;
      Res4  at 0 range 12 .. 13;
      WWDG  at 0 range 11 .. 11;
      Res5  at 0 range  9 .. 10;
      TIM14 at 0 range  8 ..  8;
      TIM13 at 0 range  7 ..  7;
      TIM12 at 0 range  6 ..  6;
      TIM7  at 0 range  5 ..  5;
      TIM6  at 0 range  4 ..  4;
      TIM5  at 0 range  3 ..  3;
      TIM4  at 0 range  2 ..  2;
      TIM3  at 0 range  1 ..  1;
      TIM2  at 0 range  0 ..  0;
   end record;

   type APB2LPEN_Register is record
      -- APB2 peripheral clock enabled in low power mode
      Res0   : Reserved (19 .. 31) := (others => 0);
      TIM11  : Bits_1;
      -- TIM11 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM10  : Bits_1;
      -- TIM10 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM9   : Bits_1;
      -- TIM 9 clock enable during Sleep mode
      --- Enable_Lp, off
      Res1   : Reserved (15 .. 15) := (others => 0);
      SYSCFG : Bits_1;
      -- System configuration controller clock enable during Sleep mode
      --- Enable_Lp, off
      Res2   : Reserved (13 .. 13) := (others => 0);
      SPI1   : Bits_1;
      -- SPI1 clock enable during Sleep mode
      --- Enable_Lp, off
      SDIO   : Bits_1;
      --  SDIO clock enable during Sleep mode
      --- Enable_Lp, off
      ADC3   : Bits_1;
      -- ADC 3 clock enable during Sleep mode
      --- Enable_Lp, off
      ADC2   : Bits_1;
      -- ADC 2 clock enable during Sleep mode
      --- Enable_Lp, off
      ADC1   : Bits_1;
      -- ADC 1 clock enable during Sleep mode
      --- Enable_Lp, off
      Res4   : Reserved (6 .. 7)   := (others => 0);
      UART6  : Bits_1;
      -- USART6 clock enable during Sleep mode
      --- Enable_Lp, off
      UART1  : Bits_1;
      -- USART1 clock enable during Sleep mode
      --- Enable_Lp, off
      Res5   : Reserved (2 .. 3)   := (others => 0);
      TIM8   : Bits_1;
      -- TIM8 clock enable during Sleep mode
      --- Enable_Lp, off
      TIM1   : Bits_1;
      -- TIM1 clock enable during Sleep mode
      --- Enable_Lp, off
   end record;

   for APB2LPEN_Register use record
      Res0   at 0 range 19 .. 31;
      TIM11  at 0 range 18 .. 18;
      TIM10  at 0 range 17 .. 17;
      TIM9   at 0 range 16 .. 16;
      Res1   at 0 range 15 .. 15;
      SYSCFG at 0 range 14 .. 14;
      Res2   at 0 range 13 .. 13;
      SPI1   at 0 range 12 .. 12;
      SDIO   at 0 range 11 .. 11;
      ADC3   at 0 range 10 .. 10;
      ADC2   at 0 range  9 ..  9;
      ADC1   at 0 range  8 ..  8;
      Res4   at 0 range  6 ..  7;
      UART6  at 0 range  5 ..  5;
      UART1  at 0 range  4 ..  4;
      Res5   at 0 range  2 ..  3;
      TIM8   at 0 range  1 ..  1;
      TIM1   at 0 range  0 ..  0;
   end record;

   type BDC_Register is record
      -- Backup domain control register
      Res0   : Reserved (17 .. 31) := (others => 0);
      BDRST  : Bits_1;
      --  Backup domain software reset
      --- Reset, Off
      RTCEN  : Bits_1;
      -- RTC clock enable
      --- Enable, Off
      Res1   : Reserved (10 .. 14) := (others => 0);
      RTCSEL : Bits_2;
      -- RTC clock source selection
      --- No_Clk, Lse_Osc_Used, Lsi_Osc_Used, Hse_Osc_Used
      Res2   : Reserved (3 .. 7)   := (others => 0);
      LSEBYP : Bits_1;
      -- External low-speed oscillator bypass
      --- Enable, Off
      LSERDY : Bits_1;
      --  External low-speed oscillator ready
      --- Ready, Off
      LSEON  : Bits_1;
      -- External low-speed oscillator enable
      --- On, Off
   end record;

   for BDC_Register use record
      Res0   at 0 range 17 .. 31;
      BDRST  at 0 range 16 .. 16;
      RTCEN  at 0 range 15 .. 15;
      Res1   at 0 range 10 .. 14;
      RTCSEL at 0 range  8 ..  9;
      Res2   at 0 range  3 ..  7;
      LSEBYP at 0 range  2 ..  2;
      LSERDY at 0 range  1 ..  1;
      LSEON  at 0 range  0 ..  0;
   end record;

   type CCS_Register is record
      -- clock control & status register
      LPWRRSTF : Bits_1;
      -- Low-power reset flag
      --- Tripped, off
      WWDGRSTF : Bits_1;
      -- Window watchdog reset flag
      --- Tripped, off
      IWDGRSTF : Bits_1;
      -- Independent watchdog reset flag
      --- Tripped, off
      SFTRSTF  : Bits_1;
      --  Software reset flag
      --- Tripped, off
      PORRSTF  : Bits_1;
      -- POR/PDR reset flag
      --- Tripped, off
      PINRSTF  : Bits_1;
      -- PIN reset flag
      --- Tripped, off
      BORRSTF  : Bits_1;
      -- BOR reset flag
      --- Tripped, off
      RMVF     : Bits_1;
      --  Remove reset flag
      --- Clear_Int, off
      Res1     : Reserved (2 .. 23) := (others => 0);
      LSIRDY   : Bits_1;
      -- Internal low-speed oscillator ready
      --- Ready, Off
      LSION    : Bits_1;
      --  Internal low-speed oscillator enable
      --- On, Off
   end record;

   for CCS_Register use record
      LPWRRSTF at 0 range 31 .. 31;
      WWDGRSTF at 0 range 30 .. 30;
      IWDGRSTF at 0 range 29 .. 29;
      SFTRSTF  at 0 range 28 .. 28;
      PORRSTF  at 0 range 27 .. 27;
      PINRSTF  at 0 range 26 .. 26;
      BORRSTF  at 0 range 25 .. 25;
      RMVF     at 0 range 24 .. 24;
      Res1     at 0 range  2 .. 23;
      LSIRDY   at 0 range  1 ..  1;
      LSION    at 0 range  0 ..  0;
   end record;

   type SSCG_Register is record
      -- spread spectrum clock generation register 
      SSCGEN    : Bits_1;
      --  Spread spectrum modulation enable
      --- Enable, Off
      SPREADSEL : Bits_1;
      -- Spread Select method
      --- Down, Center
      Res0      : Reserved (28 .. 29) := (others => 0);
      INCSTEP   : Bits_15;
      --  Incrementation step
      --- ?
      MODPER    : Bits_13;
      -- Modulation period
      --- ?
   end record;

   for SSCG_Register use record
      SSCGEN    at 0 range 31 .. 31;
      SPREADSEL at 0 range 30 .. 30;
      Res0      at 0 range 28 .. 29;
      INCSTEP   at 0 range 13 .. 27;
      MODPER    at 0 range  0 .. 12;
   end record;

   type PLLI2SCFG_Register is record
      --  PLLI2S configuration register 
      Res0    : Reserved (31 .. 31) := (others => 0);
      PLLI2SR : Bits_3;
      --  PLLI2S division factor for I2S clocks
      --- between 2 and 7
      Res1    : Reserved (15 .. 27) := (others => 0);
      PLLI2SN : Bits_9;
      --  PLLI2S multiplication factor for VCO
      --- between 192 and 432
      Res2    : Reserved (0 .. 5)   := (others => 0);
   end record;

   for PLLI2SCFG_Register use record
      Res0    at 0 range 31 .. 31;
      PLLI2SR at 0 range 28 .. 30;
      Res1    at 0 range 15 .. 27;
      PLLI2SN at 0 range  6 .. 14;
      Res2    at 0 range  0 ..  5;
   end record;

   type RESERVED_Register is array (0 .. 1) of aliased Bits_32x1;

   type RCC_TypeDef is record
      CR         : CR_Register;
      PLLCFGR    : PLLCFG_Register;
      CFGR       : CCFG_Register;
      CIR        : CIR_Register;
      AHB1RSTR   : AHB1RST_Register;
      AHB2RSTR   : AHB2RST_Register;
      AHB3RSTR   : AHB3RST_Register;
      RESERVED0  : Bits_32;
      APB1RSTR   : APB1RST_Register;
      APB2RSTR   : APB2RST_Register;
      RESERVED1  : RESERVED_Register;
      AHB1ENR    : AHB1EN_Register;
      AHB2ENR    : AHB2EN_Register;
      AHB3ENR    : AHB3EN_Register;
      RESERVED2  : Bits_32;
      APB1ENR    : APB1EN_Register;
      APB2ENR    : APB2EN_Register;
      RESERVED3  : RESERVED_Register;
      AHB1LPENR  : AHB1LPEN_Register;
      AHB2LPENR  : AHB2LPEN_Register;
      AHB3LPENR  : AHB3LPEN_Register;
      RESERVED4  : Bits_32;
      APB1LPENR  : APB1LPEN_Register;
      APB2LPENR  : APB2LPEN_Register;
      RESERVED5  : RESERVED_Register;
      BDCR       : BDC_Register;
      CSR        : CCS_Register;
      RESERVED6  : RESERVED_Register;
      SSCGR      : SSCG_Register;
      PLLI2SCFGR : PLLI2SCFG_Register;
      ---PLLSAICFGR        : PLLSAICFG_Register;
      -- DCKCFGR           : DCKCFG_Register;
   end record;

   for RCC_TypeDef use record
      CR         at   0 range 0 .. 31;
      PLLCFGR    at   4 range 0 .. 31;
      CFGR       at   8 range 0 .. 31;
      CIR        at  12 range 0 .. 31;
      AHB1RSTR   at  16 range 0 .. 31;
      AHB2RSTR   at  20 range 0 .. 31;
      AHB3RSTR   at  24 range 0 .. 31;
      RESERVED0  at  28 range 0 .. 31;
      APB1RSTR   at  32 range 0 .. 31;
      APB2RSTR   at  36 range 0 .. 31;
      RESERVED1  at  40 range 0 .. 63;
      AHB1ENR    at  48 range 0 .. 31;
      AHB2ENR    at  52 range 0 .. 31;
      AHB3ENR    at  56 range 0 .. 31;
      RESERVED2  at  60 range 0 .. 31;
      APB1ENR    at  64 range 0 .. 31;
      APB2ENR    at  68 range 0 .. 31;
      RESERVED3  at  72 range 0 .. 63;
      AHB1LPENR  at  80 range 0 .. 31;
      AHB2LPENR  at  84 range 0 .. 31;
      AHB3LPENR  at  88 range 0 .. 31;
      RESERVED4  at  92 range 0 .. 31;
      APB1LPENR  at  96 range 0 .. 31;
      APB2LPENR  at 100 range 0 .. 31;
      RESERVED5  at 104 range 0 .. 63;
      BDCR       at 112 range 0 .. 31;
      CSR        at 116 range 0 .. 31;
      RESERVED6  at 120 range 0 .. 63;
      SSCGR      at 128 range 0 .. 31;
      PLLI2SCFGR at 132 range 0 .. 31;
      --PLLSAICFGR        at  136 range 0 ..    31;
      --DCKCFGR           at  140 range 0 ..    31;
   end record;

end STM32F4.o7xx.Rcc;

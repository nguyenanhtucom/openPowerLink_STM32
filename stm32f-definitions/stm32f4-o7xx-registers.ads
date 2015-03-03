------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                    S T M 32 F 4 . O7xx . R e g i s t e r s               --
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

--pragma Restrictions (No_Elaboration_Code);

--with Interfaces;

with System;
with STM32F4.O7xx.Eth;
with STM32F4.o7xx.Dma;
with STM32F4.o7xx.Usart;
with STM32F4.O7xx.Timers.T1_8;
with STM32F4.O7xx.Timers.T2_5;
with STM32F4.O7xx.Timers.T6_7;
with STM32F4.O7xx.Timers.T9_12;
with STM32F4.O7xx.Timers.T10_14;
with STM32F4.O7xx.Adc.Adc1_3;
with STM32F4.O7xx.Adc.Common;
with STM32F4.O7xx.Dac;
with STM32F4.o7xx.Wwdg;
with STM32F4.o7xx.Iwdg;
with STM32F4.o7xx.Rcc;
with STM32F4.O7xx.Syscfg;
with STM32F4.Gpio;


package STM32F4.O7xx.Registers is
   
   
   -----------------
   -- Rcc (ch 7)  --
   -----------------
   
   Rcc : STM32F4.o7xx.Rcc.RCC_TypeDef with
     Volatile,
     Address => System'To_Address (RCC_Base);
   pragma Import (Ada, Rcc);
   
   
   -------------------
   -- GPIO (ch 8)   --
   -------------------
   
   package Gpio renames STM32F4.Gpio;
   
   GPIOA : Gpio.GPIO_Register with
     Volatile,
     Address => System'To_Address (Gpioa_Base);
   pragma Import (Ada, GPIOA);
   
    GPIOB : Gpio.GPIO_Register with
     Volatile,
     Address => System'To_Address (GpioB_Base);
   pragma Import (Ada, GPIOB);
   
    GPIOC : Gpio.GPIO_Register with
     Volatile,
     Address => System'To_Address (GpioC_Base);
   pragma Import (Ada, GPIOC);
   
    GPIOD : Gpio.GPIO_Register with
     Volatile,
     Address => System'To_Address (GpioD_Base);
   pragma Import (Ada, GPIOD);
   
   GPIOE : Gpio.GPIO_Register with
     Volatile,
     Address => System'To_Address (GpioE_Base);
   pragma Import (Ada, GPIOE);
   
   GPIOF : Gpio.GPIO_Register with
     Volatile,
     Address => System'To_Address (GpioF_Base);
   pragma Import (Ada, GPIOF);
   
   GPIOG : Gpio.GPIO_Register with
     Volatile,
     Address => System'To_Address (GpioG_Base);
   pragma Import (Ada, GPIOG);
   
   --   GPIOH : Gpio.GPIO_Register with
   --    Volatile,
   --    Address => System'To_Address (GpioH_Base);
   --  pragma Import (Ada, GPIOH);
   
   --   GPIOI : Gpio.GPIO_Register with
   --    Volatile,
   --    Address => System'To_Address (GpioI_Base);
   --  pragma Import (Ada, GPIOI);
   
   
   
   
   -------------------
   -- SYSCFG (ch 9) --
   -------------------
   
   Syscfg : STM32F4.o7xx.Syscfg.SYSCFG_TypeDef with
     Volatile,
     Address => System'To_Address (SYSCFG_Base);
   pragma Import (Ada, Syscfg);
   
   
   -----------------
   -- DMA (ch 10) --
   -----------------
   
   package Dma renames STM32F4.o7xx.Dma;
   
   Dma1 : Dma.DMA_TypeDef with
     Volatile,
     Address => System'To_Address (DMA1_Base);
   pragma Import (Ada, Dma1);
   
   DMA2 : Dma.DMA_TypeDef with
     Volatile,
     Address => System'To_Address (DMA2_Base);
   pragma Import (Ada, DMA2);
   
   
   
   
   -------------------------------
   -- A to D converters (ch 13) --
   -------------------------------
   
   package Adc1_3 renames STM32F4.O7xx.Adc.Adc1_3;
   package Adc_C  renames STM32F4.O7xx.Adc.Common;
   
   ADCC : Adc_C.Adcc_Register with
     Volatile,
     Address => System'To_Address (ADC_Common_Base);
   pragma Import (Ada, ADCC);
   
   ADC1 : Adc1_3.Adc_Register with
     Volatile,
     Address => System'To_Address (ADC1_Base);
   pragma Import (Ada, Adc1);
   
   ADC2 : Adc1_3.Adc_Register with
     Volatile,
     Address => System'To_Address (ADC2_Base);
   pragma Import (Ada, Adc2);
   
   ADC3 : Adc1_3.Adc_Register with
     Volatile,
     Address => System'To_Address (ADC3_Base);
   pragma Import (Ada, Adc3);
   
   
   -------------------
   --  Dac (ch 14)  --
   -------------------
      
   DAC : STM32F4.O7xx.Dac.Dac_Register with
     Volatile,
     Address => System'To_Address (DAC_Base);
   pragma Import (Ada, DAC);
   
   
   ---------------------------------
   -- Timer / Counters (ch 17-20) --
   ---------------------------------
   
   package T1_8   renames STM32F4.O7xx.Timers.T1_8;
   package T2_5   renames STM32F4.O7xx.Timers.T2_5;
   package T6_7   renames STM32F4.O7xx.Timers.T6_7;
   package T9_12  renames STM32F4.O7xx.Timers.T9_12;
   package T10_14 renames STM32F4.O7xx.Timers.T10_14;
   
   TIM1 : T1_8.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM1_Base);
   pragma Import (Ada, TIM1);
   
   TIM2 : T2_5.Timer2_Register with
     Volatile,
     Address => System'To_Address (TIM2_Base);
   pragma Import (Ada, TIM2);
   
   TIM3 : T2_5.Timer34_Register with
     Volatile,
     Address => System'To_Address (TIM3_Base);
   pragma Import (Ada, TIM3);

   TIM4 : T2_5.Timer34_Register with
     Volatile,
     Address => System'To_Address (TIM4_Base);
   pragma Import (Ada, TIM4);
   
   TIM5 : T2_5.Timer5_Register with
     Volatile,
     Address => System'To_Address (TIM5_Base);
   pragma Import (Ada, TIM5);

   TIM6 : T6_7.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM6_Base);
   pragma Import (Ada, TIM6);
   
   TIM7 : T6_7.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM7_Base);
   pragma Import (Ada, TIM7);
   
   TIM8 : T1_8.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM8_Base);
   pragma Import (Ada, TIM8);
   
   TIM9 : T9_12.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM9_Base);
   pragma Import (Ada, TIM9);
   
   TIM10 : T10_14.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM10_Base);
   pragma Import (Ada, TIM10);
   
   TIM11 : T10_14.Timer11_Register with
     Volatile,
     Address => System'To_Address (TIM11_Base);
   pragma Import (Ada, TIM11);
   
   TIM12 : T9_12.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM12_Base);
   pragma Import (Ada, TIM12);
   
   TIM13 : T10_14.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM13_Base);
   pragma Import (Ada, TIM13);
   
   TIM14 : T10_14.Timer_Register with
     Volatile,
     Address => System'To_Address (TIM14_Base);
   pragma Import (Ada, TIM14);
   
   
   ---------------------
   -- watchdog (ch21) --
   ---------------------
   
   IWDG : STM32F4.o7xx.Iwdg.IWDG_TypeDef with
     Volatile,
     Address => System'To_Address (IWDG_Base);
   pragma Import (Ada, IWDG);
   
  
   ---------------------
   -- watchdog (ch22) --
   ---------------------
   
   WWDG : STM32F4.o7xx.Wwdg.WWDG_TypeDef with
     Volatile,
     Address => System'To_Address (WWDG_Base);
   pragma Import (Ada, WWDG);
   
   
   --------------------
   --  USARTS (ch30) --
   --------------------
   
   package Usart renames STM32F4.o7xx.Usart;
   
   USART1 : Usart.USART_TypeDef with
     Volatile,
     Address => System'To_Address (USART1_Base);
   pragma Import (Ada, USART1);
   
   USART2 : Usart.USART_TypeDef with
     Volatile,
     Address => System'To_Address (USART2_Base);
   pragma Import (Ada, USART2);
   
   USART3 : Usart.USART_TypeDef with
     Volatile,
     Address => System'To_Address (USART3_Base);
   pragma Import (Ada, USART3);
   
   UART4 : Usart.USART_TypeDef with
     Volatile,
     Address => System'To_Address (UART4_Base);
   pragma Import (Ada, UART4);
   
   UART5 : Usart.USART_TypeDef with
     Volatile,
     Address => System'To_Address (UART5_Base);
   pragma Import (Ada, UART5);
   
   USART6 : Usart.USART_TypeDef with
     Volatile,
     Address => System'To_Address (USART6_Base);
   pragma Import (Ada, USART6);
   
   
   ---------------------
   -- Ethernet (ch33) --
   ---------------------
   
   ETH_MAC : STM32F4.o7xx.Eth.Eth_Register with
     Volatile,
     Address => System'To_Address (ETHERNET_Base);
   pragma Import (Ada, ETH_MAC);
   
   
   
   
   
   
end STM32F4.O7xx.Registers;

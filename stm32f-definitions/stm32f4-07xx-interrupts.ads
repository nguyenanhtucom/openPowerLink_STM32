------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                 S T M 32 F 4 . O7xx . I n t e r r u p t s                --
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

--  This file provides interrupt definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

pragma Restrictions (No_Elaboration_Code);

with System.BB.Interrupts;

package Stm32f4.o7xx.Interrupts is

   subtype IRQn is System.BB.Interrupts.Interrupt_ID;
   -- subtype IRQn is Natural;
   
   NonMaskableInt_IRQn     : constant IRQn := -14;
   -- Non Maskable Interrupt
   MemoryManagement_IRQn   : constant IRQn := -12;
   -- Cortex-M4 Memory Management Interrupt
   BusFault_IRQn           : constant IRQn := -11;
   -- Cortex-M4 Bus Fault Interrupt
   UsageFault_IRQn         : constant IRQn := -10;
   -- Cortex-M4 Usage Fault Interrupt
   SVCall_IRQn             : constant IRQn := -5;
   -- Cortex-M4 SV Call Interrupt
   DebugMonitor_IRQn       : constant IRQn := -4;
   -- Cortex-M4 Debug Monitor Interrupt
   PendSV_IRQn             : constant IRQn := -2;
   -- Cortex-M4 Pend SV Interrupt
   SysTick_IRQn            : constant IRQn := -1;
   -- Cortex-M4 System Tick Interrupt
   
   -- STM32 specific Interrupt Numbers --
   WWDG_IRQn               : constant IRQn := 0;
   -- Window WatchDog Interrupt
   PVD_IRQn                : constant IRQn := 1;
   -- PVD through EXTI Line detection Interrupt
   TAMP_STAMP_IRQn         : constant IRQn := 2;
   -- Tamper and TimeStamp interrupts through the EXTI line
   RTC_WKUP_IRQn           : constant IRQn := 3;
   -- RTC Wakeup interrupt through the EXTI line
   FLASH_IRQn              : constant IRQn := 4;
   -- FLASH global Interrupt
   RCC_IRQn                : constant IRQn := 5;
   -- RCC global Interrupt
   EXTI0_IRQn              : constant IRQn := 6;
   -- EXTI Line0 Interrupt
   EXTI1_IRQn              : constant IRQn := 7;
   -- EXTI Line1 Interrupt
   EXTI2_IRQn              : constant IRQn := 8;
   -- EXTI Line2 Interrupt
   EXTI3_IRQn              : constant IRQn := 9;
   -- EXTI Line3 Interrupt
   EXTI4_IRQn              : constant IRQn := 10;
   -- EXTI Line4 Interrupt
   DMA1_Stream0_IRQn       : constant IRQn := 11;
   -- DMA1 Stream 0 global Interrupt
   DMA1_Stream1_IRQn       : constant IRQn := 12;
   -- DMA1 Stream 1 global Interrupt
   DMA1_Stream2_IRQn       : constant IRQn := 13;
   -- DMA1 Stream 2 global Interrupt
   DMA1_Stream3_IRQn       : constant IRQn := 14;
   -- DMA1 Stream 3 global Interrupt
   DMA1_Stream4_IRQn       : constant IRQn := 15;
   -- DMA1 Stream 4 global Interrupt
   DMA1_Stream5_IRQn       : constant IRQn := 16;
   -- DMA1 Stream 5 global Interrupt
   DMA1_Stream6_IRQn       : constant IRQn := 17;
   -- DMA1 Stream 6 global Interrupt
   ADC_IRQn                : constant IRQn := 18;
   -- ADC1, ADC2 and ADC3 global Interrupts
   CAN1_TX_IRQn            : constant IRQn := 19;
   -- CAN1 TX Interrupt
   CAN1_RX0_IRQn           : constant IRQn := 20;
   -- CAN1 RX0 Interrupt
   CAN1_RX1_IRQn           : constant IRQn := 21;
   -- CAN1 RX1 Interrupt
   CAN1_SCE_IRQn           : constant IRQn := 22;
   -- CAN1 SCE Interrupt
   EXTI9_5_IRQn            : constant IRQn := 23;
   -- External Line[9:5] Interrupts
   TIM1_BRK_TIM9_IRQn      : constant IRQn := 24;
   -- TIM1 Break interrupt and TIM9 global interrupt
   TIM1_UP_TIM10_IRQn      : constant IRQn := 25;
   -- TIM1 Update Interrupt and TIM10 global interrupt
   TIM1_TRG_COM_TIM11_IRQn : constant IRQn := 26;
   -- TIM1 Trigger and Commutation Interrupt and TIM11 global interrupt
   TIM1_CC_IRQn            : constant IRQn := 27;
   -- TIM1 Capture Compare Interrupt
   TIM2_IRQn               : constant IRQn := 28;
   -- TIM2 global Interrupt
   TIM3_IRQn               : constant IRQn := 29;
   -- TIM3 global Interrupt
   TIM4_IRQn               : constant IRQn := 30;
   -- TIM4 global Interrupt
   I2C1_EV_IRQn            : constant IRQn := 31;
   -- I2C1 Event Interrupt
   I2C1_ER_IRQn            : constant IRQn := 32;
   -- I2C1 Error Interrupt
   I2C2_EV_IRQn            : constant IRQn := 33;
   -- I2C2 Event Interrupt
   I2C2_ER_IRQn            : constant IRQn := 34;
   -- I2C2 Error Interrupt
   SPI1_IRQn               : constant IRQn := 35;
   -- SPI1 global Interrupt
   SPI2_IRQn               : constant IRQn := 36;
   -- SPI2 global Interrupt
   USART1_IRQn             : constant IRQn := 37;
   -- USART1 global Interrupt
   USART2_IRQn             : constant IRQn := 38;
   -- USART2 global Interrupt
   USART3_IRQn             : constant IRQn := 39;
   -- USART3 global Interrupt
   EXTI15_10_IRQn          : constant IRQn := 40;
   -- External Line[15:10] Interrupts
   RTC_Alarm_IRQn          : constant IRQn := 41;
   -- RTC Alarm (A and B) through EXTI Line Interrupt
   OTG_FS_WKUP_IRQn        : constant IRQn := 42;
   -- USB OTG FS Wakeup through EXTI line interrupt
   TIM8_BRK_TIM12_IRQn     : constant IRQn := 43;
   -- TIM8 Break Interrupt and TIM12 global interrupt
   TIM8_UP_TIM13_IRQn      : constant IRQn := 44;
   -- TIM8 Update Interrupt and TIM13 global interrupt
   TIM8_TRG_COM_TIM14_IRQn : constant IRQn := 45;
   -- TIM8 Trigger and Commutation Interrupt and TIM14 global interrupt
   TIM8_CC_IRQn            : constant IRQn := 46;
   -- TIM8 Capture Compare Interrupt
   DMA1_Stream7_IRQn       : constant IRQn := 47;
   -- DMA1 Stream7 Interrupt
   FSMC_IRQn               : constant IRQn := 48;
   -- FSMC global Interrupt
   SDIO_IRQn               : constant IRQn := 49;
   -- SDIO global Interrupt
   TIM5_IRQn               : constant IRQn := 50;
   -- TIM5 global Interrupt
   SPI3_IRQn               : constant IRQn := 51;
   -- SPI3 global Interrupt
   UART4_IRQn              : constant IRQn := 52;
   -- UART4 global Interrupt
   UART5_IRQn              : constant IRQn := 53;
   -- UART5 global Interrupt
   TIM6_DAC_IRQn           : constant IRQn := 54;
   -- TIM6 global and DAC1&2 underrun error  interrupts
   TIM7_IRQn               : constant IRQn := 55;
   -- TIM7 global interrupt
   DMA2_Stream0_IRQn       : constant IRQn := 56;
   -- DMA2 Stream 0 global Interrupt
   DMA2_Stream1_IRQn       : constant IRQn := 57;
   -- DMA2 Stream 1 global Interrupt
   DMA2_Stream2_IRQn       : constant IRQn := 58;
   -- DMA2 Stream 2 global Interrupt
   DMA2_Stream3_IRQn       : constant IRQn := 59;
   -- DMA2 Stream 3 global Interrupt
   DMA2_Stream4_IRQn       : constant IRQn := 60;
   -- DMA2 Stream 4 global Interrupt
   ETH_IRQn                : constant IRQn := 61;
   -- Ethernet global Interrupt
   ETH_WKUP_IRQn           : constant IRQn := 62;
   -- Ethernet Wakeup through EXTI line Interrupt
   CAN2_TX_IRQn            : constant IRQn := 63;
   -- CAN2 TX Interrupt
   CAN2_RX0_IRQn           : constant IRQn := 64;
   -- CAN2 RX0 Interrupt
   CAN2_RX1_IRQn           : constant IRQn := 65;
   -- CAN2 RX1 Interrupt
   CAN2_SCE_IRQn           : constant IRQn := 66;
   -- CAN2 SCE Interrupt
   OTG_FS_IRQn             : constant IRQn := 67;
   -- USB OTG FS global Interrupt
   DMA2_Stream5_IRQn       : constant IRQn := 68;
   -- DMA2 Stream 5 global interrupt
   DMA2_Stream6_IRQn       : constant IRQn := 69;
   -- DMA2 Stream 6 global interrupt
   DMA2_Stream7_IRQn       : constant IRQn := 70;
   -- DMA2 Stream 7 global interrupt
   USART6_IRQn             : constant IRQn := 71;
   -- USART6 global interrupt
   I2C3_EV_IRQn            : constant IRQn := 72;
   -- I2C3 event interrupt
   I2C3_ER_IRQn            : constant IRQn := 73;
   -- I2C3 error interrupt
   OTG_HS_EP1_OUT_IRQn     : constant IRQn := 74;
   -- USB OTG HS End Point 1 Out global interrupt
   OTG_HS_EP1_IN_IRQn      : constant IRQn := 75;
   -- USB OTG HS End Point 1 In global interrupt
   OTG_HS_WKUP_IRQn        : constant IRQn := 76;
   -- USB OTG HS Wakeup through EXTI interrupt
   OTG_HS_IRQn             : constant IRQn := 77;
   -- USB OTG HS global interrupt
   DCMI_IRQn               : constant IRQn := 78;
   -- DCMI global interrupt
   CRYP_IRQn               : constant IRQn := 79;
   -- CRYP crypto global interrupt
   HASH_RNG_IRQn           : constant IRQn := 80;
   -- Hash and Rng global interrupt
   FPU_IRQn                : constant IRQn := 81;
   -- FPU global interrupt
   
      subtype IRQn_Type is IRQn;

end Stm32f4.o7xx.Interrupts;

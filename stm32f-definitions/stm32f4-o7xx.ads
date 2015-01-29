------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                            S T M 32 F 4 . O7xx                           --
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

pragma Restrictions (No_Elaboration_Code);

--with Interfaces;

package STM32F4.o7xx is
   
   --  Define address bases for the various system components

   RNG_Base             : constant := AHB2_Peripheral_Base + 16#006_0800#;
   HASH_Base            : constant := AHB2_Peripheral_Base + 16#006_0400#;
   CRYP_Base            : constant := AHB2_Peripheral_Base + 16#006_0000#;
   DCMI_Base            : constant := AHB2_Peripheral_Base + 16#005_0000#;
   USB_OTG_FS_Base      : constant := AHB2_Peripheral_Base + 16#000_0000#;
   
   USB_OTG_HS_Base      : constant := AHB1_Peripheral_Base + 16#002_0000#;
   --DMA2D_Base           : constant := AHB1_Peripheral_Base + 16#B000#;
   ETHERNET_Base        : constant := AHB1_Peripheral_Base + 16#8000#;
   --ETHERNET MAC is not defined yet
   DMA2_Base            : constant := AHB1_Peripheral_Base + 16#6400#;
   DMA1_Base            : constant := AHB1_Peripheral_Base + 16#6000#;
   --BKPSRAM not defined yet
   FLASH_INTERFACE_Base : constant := AHB1_Peripheral_Base + 16#3C00#;
   --RCC_Base is defined in STM32F4
   CRC_Base             : constant := AHB1_Peripheral_Base + 16#3000#;
   -- gpios to be defined as needed it depends on the actual chip.
   
   --LCD-TFT_Base         : constant := APB2_Peripheral_Base + 16#6800#;
   --SPI6
   --SPI5
   
   TIM11_Base           : constant := APB2_Peripheral_Base + 16#4800#;
   TIM10_Base           : constant := APB2_Peripheral_Base + 16#4400#;
   TIM9_Base            : constant := APB2_Peripheral_Base + 16#4000#;
   --EXTI_Base is defined in STM32F4
   --SYSCFG_Base is defined in STM32F4
   SPI1_Base            : constant := APB2_Peripheral_Base + 16#3000#;
   SDIO_Base            : constant := APB2_Peripheral_Base + 16#2C00#;
   
   ADC_Peripheral_Base  : constant := APB2_Peripheral_Base + 16#2000#;
   
   ADC1_Base            : constant := ADC_Peripheral_Base  + 16#0000#;
   ADC2_Base            : constant := ADC_Peripheral_Base  + 16#0100#;
   ADC3_Base            : constant := ADC_Peripheral_Base  + 16#0200#;
   ADC_Common_Base      : constant := ADC_Peripheral_Base  + 16#0300#;
   
   USART6_Base          : constant := APB2_Peripheral_Base + 16#1400#;
   USART1_Base          : constant := APB2_Peripheral_Base + 16#1000#;
   TIM8_Base            : constant := APB2_Peripheral_Base + 16#0400#;
   TIM1_Base            : constant := APB2_Peripheral_Base + 16#0000#;
   
   DAC_Base             : constant := APB1_Peripheral_Base + 16#7400#;
   --PWR_Base is defined in STM32F4
   CAN2_Base            : constant := APB1_Peripheral_Base + 16#6800#;
   CAN1_Base            : constant := APB1_Peripheral_Base + 16#6400#;
   I2C3_Base            : constant := APB1_Peripheral_Base + 16#5C00#;
   I2C2_Base            : constant := APB1_Peripheral_Base + 16#5800#;
   I2C1_Base            : constant := APB1_Peripheral_Base + 16#5400#;
   UART5_Base           : constant := APB1_Peripheral_Base + 16#5000#;
   UART4_Base           : constant := APB1_Peripheral_Base + 16#4C00#;
   USART3_Base          : constant := APB1_Peripheral_Base + 16#4800#;
   USART2_Base          : constant := APB1_Peripheral_Base + 16#4400#;
   I2S3ext_Base         : constant := APB1_Peripheral_Base + 16#4000#;
   SPI3_I2S3_Base       : constant := APB1_Peripheral_Base + 16#3C00#;
   SPI2_I2S2_Base       : constant := APB1_Peripheral_Base + 16#3800#;
   I2S2ext_Base         : constant := APB1_Peripheral_Base + 16#3400#;
   IWDG_Base            : constant := APB1_Peripheral_Base + 16#3000#;
   WWDG_Base            : constant := APB1_Peripheral_Base + 16#2C00#;
   RTC_Base             : constant := APB1_Peripheral_Base + 16#2800#;
   TIM14_Base           : constant := APB1_Peripheral_Base + 16#2000#;
   TIM13_Base           : constant := APB1_Peripheral_Base + 16#1C00#;
   TIM12_Base           : constant := APB1_Peripheral_Base + 16#1800#;
   TIM7_Base            : constant := APB1_Peripheral_Base + 16#1400#;
   TIM6_Base            : constant := APB1_Peripheral_Base + 16#1000#;
   TIM5_Base            : constant := APB1_Peripheral_Base + 16#0C00#;
   TIM4_Base            : constant := APB1_Peripheral_Base + 16#0800#;
   TIM3_Base            : constant := APB1_Peripheral_Base + 16#0400#;
   TIM2_Base            : constant := APB1_Peripheral_Base + 16#0000#;
   
end STM32F4.O7xx;


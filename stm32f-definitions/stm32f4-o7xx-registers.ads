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
with STM32F4.O7xx.Timers.T1_8;
with STM32F4.O7xx.Timers.T2_5;
with STM32F4.O7xx.Timers.T6_7;
with STM32F4.O7xx.Timers.T9_12;
with STM32F4.O7xx.Timers.T10_14;
with STM32F4.O7xx.Adc.Adc1_3;
with STM32F4.O7xx.Adc.Common;

package STM32F4.O7xx.Registers is
   
   -----------------------
   -- A to D converters --
   -----------------------
   
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
   
   
   ----------------------
   -- Timer / Counters --
   ----------------------
   
   package T1_8   renames STM32F4.O7xx.Timers.T1_8;
   package T2_5   renames STM32F4.O7xx.Timers.T2_5;
   package T6_7   renames STM32F4.O7xx.Timers.T6_7;
   package T9_12  renames STM32F4.O7xx.Timers.T9_12;
   package T10_14 renames STM32F4.O7xx.Timers.T10_14;
   
   TIM1 : T1_8.Timer_Register (1) with
     Volatile,
     Address => System'To_Address (TIM1_Base);
   pragma Import (Ada, TIM1);
   
   TIM2 : T2_5.Timer_Register (2) with
     Volatile,
     Address => System'To_Address (TIM2_Base);
   pragma Import (Ada, TIM2);
   
   TIM3 : T2_5.Timer_Register (3) with
     Volatile,
     Address => System'To_Address (TIM3_Base);
   pragma Import (Ada, TIM3);

   TIM4 : T2_5.Timer_Register (4) with
     Volatile,
     Address => System'To_Address (TIM4_Base);
   pragma Import (Ada, TIM4);
   
   TIM5 : T2_5.Timer_Register (5) with
     Volatile,
     Address => System'To_Address (TIM5_Base);
   pragma Import (Ada, TIM5);

   TIM6 : T6_7.Timer_Register (6) with
     Volatile,
     Address => System'To_Address (TIM6_Base);
   pragma Import (Ada, TIM6);
   
   TIM7 : T6_7.Timer_Register (7) with
     Volatile,
     Address => System'To_Address (TIM7_Base);
   pragma Import (Ada, TIM7);
   
   TIM8 : T1_8.Timer_Register (8) with
     Volatile,
     Address => System'To_Address (TIM8_Base);
   pragma Import (Ada, TIM8);
   
   TIM9 : T9_12.Timer_Register (9) with
     Volatile,
     Address => System'To_Address (TIM9_Base);
   pragma Import (Ada, TIM9);
   
   TIM10 : T10_14.Timer_Register (10) with
     Volatile,
     Address => System'To_Address (TIM10_Base);
   pragma Import (Ada, TIM10);
   
   TIM11 : T10_14.Timer_Register (11) with
     Volatile,
     Address => System'To_Address (TIM11_Base);
   pragma Import (Ada, TIM11);
   
   TIM12 : T9_12.Timer_Register (12) with
     Volatile,
     Address => System'To_Address (TIM12_Base);
   pragma Import (Ada, TIM12);
   
   TIM13 : T10_14.Timer_Register (13) with
     Volatile,
     Address => System'To_Address (TIM13_Base);
   pragma Import (Ada, TIM13);
   
   TIM14 : T10_14.Timer_Register (14) with
     Volatile,
     Address => System'To_Address (TIM14_Base);
   pragma Import (Ada, TIM14);
   
   
end STM32F4.O7xx.Registers;

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
--with System.BB.Interrupts;
package interrupts_h is

   --subtype IRQn is System.BB.Interrupts.Interrupt_ID;
   subtype IRQn is Natural;
   NonMaskableInt_IRQn     : constant IRQn := -14;
   MemoryManagement_IRQn   : constant IRQn := -12;
   BusFault_IRQn           : constant IRQn := -11;
   UsageFault_IRQn         : constant IRQn := -10;
   SVCall_IRQn             : constant IRQn := -5;
   DebugMonitor_IRQn       : constant IRQn := -4;
   PendSV_IRQn             : constant IRQn := -2;
   SysTick_IRQn            : constant IRQn := -1;
   WWDG_IRQn               : constant IRQn := 0;
   PVD_IRQn                : constant IRQn := 1;
   TAMP_STAMP_IRQn         : constant IRQn := 2;
   RTC_WKUP_IRQn           : constant IRQn := 3;
   FLASH_IRQn              : constant IRQn := 4;
   RCC_IRQn                : constant IRQn := 5;
   EXTI0_IRQn              : constant IRQn := 6;
   EXTI1_IRQn              : constant IRQn := 7;
   EXTI2_IRQn              : constant IRQn := 8;
   EXTI3_IRQn              : constant IRQn := 9;
   EXTI4_IRQn              : constant IRQn := 10;
   DMA1_Stream0_IRQn       : constant IRQn := 11;
   DMA1_Stream1_IRQn       : constant IRQn := 12;
   DMA1_Stream2_IRQn       : constant IRQn := 13;
   DMA1_Stream3_IRQn       : constant IRQn := 14;
   DMA1_Stream4_IRQn       : constant IRQn := 15;
   DMA1_Stream5_IRQn       : constant IRQn := 16;
   DMA1_Stream6_IRQn       : constant IRQn := 17;
   ADC_IRQn                : constant IRQn := 18;
   CAN1_TX_IRQn            : constant IRQn := 19;
   CAN1_RX0_IRQn           : constant IRQn := 20;
   CAN1_RX1_IRQn           : constant IRQn := 21;
   CAN1_SCE_IRQn           : constant IRQn := 22;
   EXTI9_5_IRQn            : constant IRQn := 23;
   TIM1_BRK_TIM9_IRQn      : constant IRQn := 24;
   TIM1_UP_TIM10_IRQn      : constant IRQn := 25;
   TIM1_TRG_COM_TIM11_IRQn : constant IRQn := 26;
   TIM1_CC_IRQn            : constant IRQn := 27;
   TIM2_IRQn               : constant IRQn := 28;
   TIM3_IRQn               : constant IRQn := 29;
   TIM4_IRQn               : constant IRQn := 30;
   I2C1_EV_IRQn            : constant IRQn := 31;
   I2C1_ER_IRQn            : constant IRQn := 32;
   I2C2_EV_IRQn            : constant IRQn := 33;
   I2C2_ER_IRQn            : constant IRQn := 34;
   SPI1_IRQn               : constant IRQn := 35;
   SPI2_IRQn               : constant IRQn := 36;
   USART1_IRQn             : constant IRQn := 37;
   USART2_IRQn             : constant IRQn := 38;
   USART3_IRQn             : constant IRQn := 39;
   EXTI15_10_IRQn          : constant IRQn := 40;
   RTC_Alarm_IRQn          : constant IRQn := 41;
   OTG_FS_WKUP_IRQn        : constant IRQn := 42;
   TIM8_BRK_TIM12_IRQn     : constant IRQn := 43;
   TIM8_UP_TIM13_IRQn      : constant IRQn := 44;
   TIM8_TRG_COM_TIM14_IRQn : constant IRQn := 45;
   TIM8_CC_IRQn            : constant IRQn := 46;
   DMA1_Stream7_IRQn       : constant IRQn := 47;
   FSMC_IRQn               : constant IRQn := 48;
   SDIO_IRQn               : constant IRQn := 49;
   TIM5_IRQn               : constant IRQn := 50;
   SPI3_IRQn               : constant IRQn := 51;
   UART4_IRQn              : constant IRQn := 52;
   UART5_IRQn              : constant IRQn := 53;
   TIM6_DAC_IRQn           : constant IRQn := 54;
   TIM7_IRQn               : constant IRQn := 55;
   DMA2_Stream0_IRQn       : constant IRQn := 56;
   DMA2_Stream1_IRQn       : constant IRQn := 57;
   DMA2_Stream2_IRQn       : constant IRQn := 58;
   DMA2_Stream3_IRQn       : constant IRQn := 59;
   DMA2_Stream4_IRQn       : constant IRQn := 60;
   ETH_IRQn                : constant IRQn := 61;
   ETH_WKUP_IRQn           : constant IRQn := 62;
   CAN2_TX_IRQn            : constant IRQn := 63;
   CAN2_RX0_IRQn           : constant IRQn := 64;
   CAN2_RX1_IRQn           : constant IRQn := 65;
   CAN2_SCE_IRQn           : constant IRQn := 66;
   OTG_FS_IRQn             : constant IRQn := 67;
   DMA2_Stream5_IRQn       : constant IRQn := 68;
   DMA2_Stream6_IRQn       : constant IRQn := 69;
   DMA2_Stream7_IRQn       : constant IRQn := 70;
   USART6_IRQn             : constant IRQn := 71;
   I2C3_EV_IRQn            : constant IRQn := 72;
   I2C3_ER_IRQn            : constant IRQn := 73;
   OTG_HS_EP1_OUT_IRQn     : constant IRQn := 74;
   OTG_HS_EP1_IN_IRQn      : constant IRQn := 75;
   OTG_HS_WKUP_IRQn        : constant IRQn := 76;
   OTG_HS_IRQn             : constant IRQn := 77;
   DCMI_IRQn               : constant IRQn := 78;
   CRYP_IRQn               : constant IRQn := 79;
   HASH_RNG_IRQn           : constant IRQn := 80;
   FPU_IRQn                : constant IRQn := 81;  -- interrupts.h:1

  --*****  Cortex-M4 Processor Exceptions Numbers ***************************************************************
  --!< 2 Non Maskable Interrupt
  --!< 4 Cortex-M4 Memory Management Interrupt
  --!< 5 Cortex-M4 Bus Fault Interrupt
  --!< 6 Cortex-M4 Usage Fault Interrupt
  --!< 11 Cortex-M4 SV Call Interrupt
  --!< 12 Cortex-M4 Debug Monitor Interrupt
  --!< 14 Cortex-M4 Pend SV Interrupt
  --!< 15 Cortex-M4 System Tick Interrupt
  --*****  STM32 specific Interrupt Numbers *********************************************************************
  --!< Window WatchDog Interrupt
  --!< PVD through EXTI Line detection Interrupt
  --!< Tamper and TimeStamp interrupts through the EXTI line
  --!< RTC Wakeup interrupt through the EXTI line
  --!< FLASH global Interrupt
  --!< RCC global Interrupt
  --!< EXTI Line0 Interrupt
  --!< EXTI Line1 Interrupt
  --!< EXTI Line2 Interrupt
  --!< EXTI Line3 Interrupt
  --!< EXTI Line4 Interrupt
  --!< DMA1 Stream 0 global Interrupt
  --!< DMA1 Stream 1 global Interrupt
  --!< DMA1 Stream 2 global Interrupt
  --!< DMA1 Stream 3 global Interrupt
  --!< DMA1 Stream 4 global Interrupt
  --!< DMA1 Stream 5 global Interrupt
  --!< DMA1 Stream 6 global Interrupt
  --!< ADC1, ADC2 and ADC3 global Interrupts
  --!< CAN1 TX Interrupt
  --!< CAN1 RX0 Interrupt
  --!< CAN1 RX1 Interrupt
  --!< CAN1 SCE Interrupt
  --!< External Line[9:5] Interrupts
  --!< TIM1 Break interrupt and TIM9 global interrupt
  --!< TIM1 Update Interrupt and TIM10 global interrupt
  --!< TIM1 Trigger and Commutation Interrupt and TIM11 global interrupt
  --!< TIM1 Capture Compare Interrupt
  --!< TIM2 global Interrupt
  --!< TIM3 global Interrupt
  --!< TIM4 global Interrupt
  --!< I2C1 Event Interrupt
  --!< I2C1 Error Interrupt
  --!< I2C2 Event Interrupt
  --!< I2C2 Error Interrupt
  --!< SPI1 global Interrupt
  --!< SPI2 global Interrupt
  --!< USART1 global Interrupt
  --!< USART2 global Interrupt
  --!< USART3 global Interrupt
  --!< External Line[15:10] Interrupts
  --!< RTC Alarm (A and B) through EXTI Line Interrupt
  --!< USB OTG FS Wakeup through EXTI line interrupt
  --!< TIM8 Break Interrupt and TIM12 global interrupt
  --!< TIM8 Update Interrupt and TIM13 global interrupt
  --!< TIM8 Trigger and Commutation Interrupt and TIM14 global interrupt
  --!< TIM8 Capture Compare Interrupt
  --!< DMA1 Stream7 Interrupt
  --!< FSMC global Interrupt
  --!< SDIO global Interrupt
  --!< TIM5 global Interrupt
  --!< SPI3 global Interrupt
  --!< UART4 global Interrupt
  --!< UART5 global Interrupt
  --!< TIM6 global and DAC1&2 underrun error  interrupts
  --!< TIM7 global interrupt
  --!< DMA2 Stream 0 global Interrupt
  --!< DMA2 Stream 1 global Interrupt
  --!< DMA2 Stream 2 global Interrupt
  --!< DMA2 Stream 3 global Interrupt
  --!< DMA2 Stream 4 global Interrupt
  --!< Ethernet global Interrupt
  --!< Ethernet Wakeup through EXTI line Interrupt
  --!< CAN2 TX Interrupt
  --!< CAN2 RX0 Interrupt
  --!< CAN2 RX1 Interrupt
  --!< CAN2 SCE Interrupt
  --!< USB OTG FS global Interrupt
  --!< DMA2 Stream 5 global interrupt
  --!< DMA2 Stream 6 global interrupt
  --!< DMA2 Stream 7 global interrupt
  --!< USART6 global interrupt
  --!< I2C3 event interrupt
  --!< I2C3 error interrupt
  --!< USB OTG HS End Point 1 Out global interrupt
  --!< USB OTG HS End Point 1 In global interrupt
  --!< USB OTG HS Wakeup through EXTI interrupt
  --!< USB OTG HS global interrupt
  --!< DCMI global interrupt
  --!< CRYP crypto global interrupt
  --!< Hash and Rng global interrupt
  --!< FPU global interrupt
   subtype IRQn_Type is IRQn;

end interrupts_h;

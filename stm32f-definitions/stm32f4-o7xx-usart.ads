------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                      S T M 32 F 4 . O7xx . U s a r t                     --
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
--  chapter 30.6    USART registers       RM0090 Reference manual
--

pragma Restrictions (No_Elaboration_Code);

package STM32F4.o7xx.Usart is

   --------------------------------------------------
   -- constants for use with the dma definitions   --
   --  in the child packages                       --
   --------------------------------------------------

   Change       : constant Bits_1 := 1;
   On           : constant Bits_1 := 1;
   Off          : constant Bits_1 := 0;
   Detected     : constant Bits_1 := 1;
   Dr_Mt        : constant Bits_1 := 1;
   Complete     : constant Bits_1 := 1;
   Rd_Ready     : constant Bits_1 := 1;
   Tripped      : constant Bits_1 := 1;
   Enable       : constant Bits_1 := 1;
   D8_Bits      : constant Bits_1 := 0;
   D9_Bits      : constant Bits_1 := 1;
   Idle_line    : constant Bits_1 := 0;
   Addr_Mark    : constant Bits_1 := 1;
   Even         : constant Bits_1 := 0;
   Odd          : constant Bits_1 := 0;
   Active       : constant Bits_1 := 0;
   Mute         : constant Bits_1 := 1;
   Send         : constant Bits_1 := 1;
   S1_Bit       : constant Bits_2 := 0;
   S0_5bit      : constant Bits_2 := 1;
   S2_Bit       : constant Bits_2 := 2;
   S1_5bit      : constant Bits_2 := 3;
   Low          : constant Bits_1 := 0;
   High         : constant Bits_1 := 1;
   Ph0          : constant Bits_1 := 0;
   Ph180        : constant Bits_1 := 1;
   Irda_Ck_Div1 : constant Byte   := 1;
   Irda_Ck_Div2 : constant Byte   := 2;
   S_Ck_Div2    : constant Byte   := 1;
   S_Ck_Div4    : constant Byte   := 2;
   S_Ck_Div6    : constant Byte   := 3;
   S_Ck_Div8    : constant Byte   := 4;
   S_Ck_Div10   : constant Byte   := 5;
   S_Ck_Div12   : constant Byte   := 6;
   S_Ck_Div14   : constant Byte   := 7;
   S_Ck_Div16   : constant Byte   := 8;
   S_Ck_Div18   : constant Byte   := 9;
   S_Ck_Div20   : constant Byte   := 10;
   S_Ck_Div22   : constant Byte   := 11;
   S_Ck_Div24   : constant Byte   := 12;
   S_Ck_Div26   : constant Byte   := 13;
   S_Ck_Div28   : constant Byte   := 14;
   S_Ck_Div30   : constant Byte   := 15;
   S_Ck_Div32   : constant Byte   := 16;
   S_Ck_Div34   : constant Byte   := 17;
   S_Ck_Div36   : constant Byte   := 18;
   S_Ck_Div38   : constant Byte   := 19;
   S_Ck_Div40   : constant Byte   := 20;
   S_Ck_Div42   : constant Byte   := 21;
   S_Ck_Div44   : constant Byte   := 22;
   S_Ck_Div46   : constant Byte   := 23;
   S_Ck_Div48   : constant Byte   := 24;
   S_Ck_Div50   : constant Byte   := 25;
   S_Ck_Div52   : constant Byte   := 26;
   S_Ck_Div54   : constant Byte   := 27;
   S_Ck_Div56   : constant Byte   := 28;
   S_Ck_Div58   : constant Byte   := 29;
   S_Ck_Div60   : constant Byte   := 30;
   S_Ck_Div62   : constant Byte   := 31;

   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------

   type SR_Register is record
      -- USART Status register
      Res0	: Reserved (10 .. 31) := (others => 0);
      CTS	: Bits_1;
      -- CTS Flag
      --- Change, off
      --- Note: This bit is not available for UART4 & UART5.
      LBD	: Bits_1;
      -- LIN Break Detection Flag
      --- Detected, Off
      TXE	: Bits_1;
      -- Transmit Data Register Empty
      --- Dr_Mt, Off
      TC	: Bits_1;
      -- Transmission Complete
      --- Complete, Off
      RXNE	: Bits_1;
      -- Read Data Register Not Empty
      --- Rd_Ready, Off
      IDLE	: Bits_1;
      -- IDLE line detected
      --- Detected, Off
      ORE	: Bits_1;
      -- OverRun Error
      --- Tripped, Off
      NF	: Bits_1;
      -- Noise Error Flag
      --- Tripped, Off
      FE	: Bits_1;
      -- Framing Error
      --- Tripped, Off
      PE	: Bits_1;
      -- Parity Error
      --- Tripped, Off
   end record;

   for SR_Register use record
      Res0 at 0 range 10 .. 31;
      CTS  at 0 range  9 ..  9;
      LBD  at 0 range  8 ..  8;
      TXE  at 0 range  7 ..  7;
      TC   at 0 range  6 ..  6;
      RXNE at 0 range  5 ..  5;
      IDLE at 0 range  4 ..  4;
      ORE  at 0 range  3 ..  3;
      NF   at 0 range  2 ..  2;
      FE   at 0 range  1 ..  1;
      PE   at 0 range  0 ..  0;
   end record;

   type DR_Register is record
      -- USART Data register
      Res0	: Reserved (9 .. 31) := (others => 0);
      DR	: Bits_9;
      -- Data value
   end record;

   for DR_Register use record
      Res0 at 0 range 9 .. 31;
      DR   at 0 range 0 ..  8;
   end record;

   type BRR_Register is record
      -- USART Baud rate register
      Res0		: Reserved (16 .. 31) := (others => 0);
      DIV_Mantissa	: Bits_12;
      -- Mantissa of USARTDIV
      DIV_Fraction	: Bits_4;
      -- Fraction of USARTDIV
   end record;

   for BRR_Register use record
      Res0         at 0 range 16 .. 31;
      DIV_Mantissa at 0 range  4 .. 15;
      DIV_Fraction at 0 range  0 ..  3;
   end record;

   type CR1_Register is record
      -- USART Control register 1
      Res0	: Reserved (16 .. 31) := (others => 0);
      OVER8	: Bits_1;
      -- USART Oversampling by 8 enable
      --- Enable, Off
      Res1	: Reserved (14 .. 14) := (others => 0);
      UE	: Bits_1;
      -- USART Enable
      --- Enable, Off
      M		: Bits_1;
      -- Word length
      --- D8_Bits, D9_Bits
      WAKE	: Bits_1;
      -- Wakeup method
      --- Idle_line, Addr_Mark
      PCE	: Bits_1;
      -- Parity Control Enable
      --- Enable, Off
      PS	: Bits_1;
      -- Parity Selection
      --- Even, Odd
      PEIE	: Bits_1;
      -- PE Interrupt Enable
      --- Enable, Off
      TXEIE	: Bits_1;
      -- TXE interrupt enable
      --- Enable, Off
      TCIE	: Bits_1;
      -- Transmission Complete Interrupt Enable
      --- Enable, Off
      RXNEIE	: Bits_1;
      -- RXNE Interrupt Enable
      --- Enable, Off
      IDLEIE	: Bits_1;
      -- IDLE Interrupt Enable
      --- Enable, Off
      TE	: Bits_1;
      -- Transmitter Enable
      --- Enable, Off
      RE	: Bits_1;
      -- Receiver Enable
      --- Enable, Off
      RWU	: Bits_1;
      -- Receiver wakeup
      --- Active, Mute
      SBK	: Bits_1;
      -- Send Break
      --- Send, off
   end record;

   for CR1_Register use record
      Res0   at 0 range 16 .. 31;
      OVER8  at 0 range 15 .. 15;
      Res1   at 0 range 14 .. 14;
      UE     at 0 range 13 .. 13;
      M      at 0 range 12 .. 12;
      WAKE   at 0 range 11 .. 11;
      PCE    at 0 range 10 .. 10;
      PS     at 0 range  9 ..  9;
      PEIE   at 0 range  8 ..  8;
      TXEIE  at 0 range  7 ..  7;
      TCIE   at 0 range  6 ..  6;
      RXNEIE at 0 range  5 ..  5;
      IDLEIE at 0 range  4 ..  4;
      TE     at 0 range  3 ..  3;
      RE     at 0 range  2 ..  2;
      RWU    at 0 range  1 ..  1;
      SBK    at 0 range  0 ..  0;
   end record;

   type CR2_Register is record
      -- USART Control register 2
      Res0	: Reserved (15 .. 31) := (others => 0);
      LINEN	: Bits_1;
      -- LIN mode enable
      --- Enable, Off
      STOP	: Bits_2;
      -- STOP bits
      --- S1_Bit,  S0_5bit, S2_Bit, S1_5bit
      --- Note: The 0.5 Stop bit and 1.5 Stop bit are not available
      --- for UART4 & UART5.
      CLKEN	: Bits_1;
      -- Clock Enable
      --- Enable, Off
      CPOL	: Bits_1;
      -- Clock Polarity
      --- Low, High
      CPHA	: Bits_1;
      -- Clock Phase
      --- Ph0, Ph180
      --- Note: This bit is not available for UART4 & UART5.
      LBCL	: Bits_1;
      -- Last Bit Clock pulse
      --- Enable, Off
      ---  Note: 1: The last bit is the 8th or 9th data bit transmitted
      ---  depending on the 8 or 9 bit format selected by the M bit in
      ---  the USART_CR1 register.
      ---  2: This bit is not available for UART4 & UART5.
      Res1	: Reserved (7 .. 7) := (others => 0);
      LBDIE	: Bits_1;
      -- LIN Break Detection Interrupt Enable
      --- Enable, Off
      LBDL	: Bits_1;
      -- LIN Break Detection Length
      Res2	: Reserved (4 .. 4) := (others => 0);
      ADD	: Bits_4;
      -- Address of the USART node
   end record;

   for CR2_Register use record
      Res0  at 0 range 15 .. 31;
      LINEN at 0 range 14 .. 14;
      STOP  at 0 range 12 .. 13;
      CLKEN at 0 range 11 .. 11;
      CPOL  at 0 range 10 .. 10;
      CPHA  at 0 range  9 ..  9;
      LBCL  at 0 range  8 ..  8;
      Res1  at 0 range  7 ..  7;
      LBDIE at 0 range  6 ..  6;
      LBDL  at 0 range  5 ..  5;
      Res2  at 0 range  4 ..  4;
      ADD   at 0 range  0 ..  3;
   end record;

   type CR3_Register is record
      -- USART Control register 3
      Res0	: Reserved (12 .. 31) := (others => 0);
      ONEBIT	: Bits_1;
      -- USART One bit method enable
      --- Enable, Off
      CTSIE	: Bits_1;
      -- CTS Interrupt Enable
      --- Enable, Off
      ---  Note: This bit is not available for UART4 & UART5.
      CTSE	: Bits_1;
      -- CTS Enable
      --- Enable, Off
      ---  Note: This bit is not available for UART4 & UART5.
      RTSE	: Bits_1;
      -- RTS Enable
      --- Enable, Off
      --- Note: This bit is not available for UART4 & UART5.
      DMAT	: Bits_1;
      -- DMA Enable Transmitter
      --- Enable, Off
      DMAR	: Bits_1;
      -- DMA Enable Receiver
      --- Enable, Off
      SCEN	: Bits_1;
      -- Smartcard mode enable
      --- Enable, Off
      NACK	: Bits_1;
      -- Smartcard NACK enable
      --- Enable, Off
      ---  Note: This bit is not available for UART4 & UART5.
      HDSEL	: Bits_1;
      -- Half-Duplex Selection
      --- Enable, Off
      IRLP	: Bits_1;
      -- IrDA Low-Power
      --- Enable, Off
      IREN	: Bits_1;
      -- IrDA mode Enable
      --- Enable, Off
      EIE	: Bits_1;
      -- Error Interrupt Enable
      --- Enable, Off
   end record;

   for CR3_Register use record
      Res0   at 0 range 12 .. 31;
      ONEBIT at 0 range 11 .. 11;
      CTSIE  at 0 range 10 .. 10;
      CTSE   at 0 range  9 ..  9;
      RTSE   at 0 range  8 ..  8;
      DMAT   at 0 range  7 ..  7;
      DMAR   at 0 range  6 ..  6;
      SCEN   at 0 range  5 ..  5;
      NACK   at 0 range  4 ..  4;
      HDSEL  at 0 range  3 ..  3;
      IRLP   at 0 range  2 ..  2;
      IREN   at 0 range  1 ..  1;
      EIE    at 0 range  0 ..  0;
   end record;

   type GTPR_Register is record
      -- USART Guard time and prescaler register
      Res0	: Reserved (16 .. 31) := (others => 0);
      GT	: Byte;
      -- Guard time value
      PSC	: Byte;
      -- Prescaler value
      ---  Irda_Ck_Div1, Irda_Ck_Di0v2
      ---              S_Ck_Div2  , S_Ck_Div4  , S_Ck_Div6  , S_Ck_Div8
      --- S_Ck_Div10 , S_Ck_Div12 , S_Ck_Div14 , S_Ck_Div16 , S_Ck_Di2v18
      --- S_Ck_Div20 , S_Ck_Div22 , S_Ck_Div24 , S_Ck_Div26 , S_Ck_Div428
      --- S_Ck_Div30 , S_Ck_Div32 , S_Ck_Div34 , S_Ck_Div36 , S_Ck_Div538
      --- S_Ck_Div40 , S_Ck_Div42 , S_Ck_Div44 , S_Ck_Div46 , S_Ck_Div648
      --- S_Ck_Div50 , S_Ck_Div52 , S_Ck_Div54 , S_Ck_Div56 , S_Ck_Div758
      --- S_Ck_Div60 , S_Ck_Div62
   end record;

   for GTPR_Register use record
      Res0 at 0 range 16 .. 31;
      GT   at 0 range  8 .. 15;
      PSC  at 0 range  0 ..  7;
   end record;

   type USART_TypeDef is record
      SR	: SR_Register;
      DR	: DR_Register;
      BRR	: BRR_Register;
      CR1	: CR1_Register;
      CR2	: CR2_Register;
      CR3	: CR3_Register;
      GTPR	: GTPR_Register;
   end record;
   pragma Convention (C_Pass_By_Copy, USART_TypeDef);

   for USART_TypeDef use record
      SR   at  0 range 0 .. 31;
      DR   at  4 range 0 .. 31;
      BRR  at  8 range 0 .. 31;
      CR1  at 12 range 0 .. 31;
      CR2  at 16 range 0 .. 31;
      CR3  at 20 range 0 .. 31;
      GTPR at 24 range 0 .. 31;
   end record;

end STM32F4.o7xx.Usart;

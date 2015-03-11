------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                         S T M 32 F 4 . O7xx . E t h                      --
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
--  chapter 33.8    ETH registers       RM0090 Reference manual
--

pragma Restrictions (No_Elaboration_Code);

-- with System;

package STM32F4.o7xx.Eth is

   --------------------------------------------------
   -- constants for use with the eth definitions   --
   --  in the client packages                      --
   --------------------------------------------------

   Strip_Crc                   : constant Bits_1 := 1;
   Off                         : constant Bits_1 := 0;
   Disable_WD                  : constant Bits_1 := 1;
   WD_on                       : constant Bits_1 := 0;
   Disable_Jt                  : constant Bits_1 := 1;
   Jt_on                       : constant Bits_1 := 0;
   IFG_96Bit                   : constant Bits_3 := 0;
   IFG_88Bit                   : constant Bits_3 := 1;
   IFG_80Bit                   : constant Bits_3 := 2;
   IFG_72Bit                   : constant Bits_3 := 3;
   IFG_64Bit                   : constant Bits_3 := 4;
   IFG_56Bit                   : constant Bits_3 := 5;
   IFG_48Bit                   : constant Bits_3 := 6;
   IFG_40Bit                   : constant Bits_3 := 7;
   Cs_Disable                  : constant Bits_1 := 1;
   Cs_On                       : constant Bits_1 := 0;
   Mb_100                      : constant Bits_1 := 1;
   Mb_10                       : constant Bits_1 := 0;
   Ro_Disable                  : constant Bits_1 := 1;
   On                          : constant Bits_1 := 1;
   Enabled                     : constant Bits_1 := 1;
   Retr_Disabled               : constant Bits_1 := 1;
   BL_10                       : constant Bits_2 := 0;
   BL_8                        : constant Bits_2 := 1;
   BL_4                        : constant Bits_2 := 2;
   BL_1                        : constant Bits_2 := 3;
   Either                      : constant Bits_1 := 1;
   PCF_BlockAll                : constant Bits_2 := 0;
   PCF_ForwardAll_ExPause      : constant Bits_2 := 1;
   PCF_ForwardAll              : constant Bits_2 := 2;
   PCF_ForwardPassedAddrFilter : constant Bits_2 := 3;
   Filterb                     : constant Bits_1 := 1;
   Pass_Allb                   : constant Bits_1 := 0;
   Pass_allm                   : constant Bits_1 := 1;
   Filterm                     : constant Bits_1 := 0;
   Hash_Filter                 : constant Bits_1 := 1;
   Normal                      : constant Bits_1 := 0;
   CR_Div42                    : constant Bits_3 := 0;
   CR_Div62                    : constant Bits_3 := 1;
   CR_Div16                    : constant Bits_3 := 2;
   CR_Div26                    : constant Bits_3 := 3;
   CR_Div102                   : constant Bits_3 := 4;
   Write                       : constant Bits_1 := 1;
   Read                        : constant Bits_1 := 0;
   Busy                        : constant Bits_1 := 1;
   Clear                       : constant Bits_1 := 0;
   Pf_Disable                  : constant Bits_1 := 1;
   Pf_On                       : constant Bits_1 := 0;
   PLT_Minus4                  : constant Bits_2 := 0;
   PLT_Minus28                 : constant Bits_2 := 1;
   PLT_Minus144                : constant Bits_2 := 2;
   PLT_Minus256                : constant Bits_2 := 3;
   Reset                       : constant Bits_1 := 1;
   Wf_Recd                     : constant Bits_1 := 1;
   Mp_Recd                     : constant Bits_1 := 1;
   Pow_Down                    : constant Bits_1 := 1;
   Full                        : constant Bits_1 := 1;
   Nf                          : constant Bits_1 := 0;
   N_Mt                        : constant Bits_1 := 1;
   Mt                          : constant Bits_1 := 0;
   Yes                         : constant Bits_1 := 1;
   No                          : constant Bits_1 := 0;
   Tripped                     : constant Bits_1 := 1;
   Int_Disabled                : constant Bits_1 := 1;
   Int_Enabled                 : constant Bits_1 := 0;
   Maca_Sa                     : constant Bits_1 := 1;
   Maca_da                     : constant Bits_1 := 0;
   Update                      : constant Bits_1 := 1;
   Init                        : constant Bits_1 := 1;
   Fine                        : constant Bits_1 := 1;
   Coarse                      : constant Bits_1 := 0;
   RDP_1Beat                   : constant Bits_6 := 1;
   RDP_2Beat                   : constant Bits_6 := 2;
   RDP_4Beat                   : constant Bits_6 := 4;
   RDP_8Beat                   : constant Bits_6 := 8;
   RDP_16Beat                  : constant Bits_6 := 16;
   RDP_32Beat                  : constant Bits_6 := 32;
   RTPR_1_1                    : constant Bits_2 := 0;
   RTPR_2_1                    : constant Bits_2 := 1;
   RTPR_3_1                    : constant Bits_2 := 2;
   RTPR_4_1                    : constant Bits_2 := 3;
   PBL_1Beat                   : constant Bits_6 := 1;
   PBL_2Beat                   : constant Bits_6 := 2;
   PBL_4Beat                   : constant Bits_6 := 4;
   PBL_8Beat                   : constant Bits_6 := 8;
   PBL_16Beat                  : constant Bits_6 := 16;
   PBL_32Beat                  : constant Bits_6 := 32;
   Round_Robin                 : constant Bits_1 := 0;
   Rx_Tx                       : constant Bits_1 := 1;
   EBS_DescAccess              : constant Bits_3 := 4;
   EBS_ReadTransf              : constant Bits_3 := 2;
   EBS_DataTransfTx            : constant Bits_3 := 1;
   TPS_Stopped                 : constant Bits_3 := 0;
   TPS_Fetching                : constant Bits_3 := 1;
   TPS_Waiting                 : constant Bits_3 := 2;
   TPS_Reading                 : constant Bits_3 := 3;
   TPS_Suspended               : constant Bits_3 := 6;
   TPS_Closing                 : constant Bits_3 := 7;
   RPS_Stopped                 : constant Bits_3 := 0;
   RPS_Fetching                : constant Bits_3 := 1;
   RPS_Waiting                 : constant Bits_3 := 3;
   RPS_Suspended               : constant Bits_3 := 4;
   RPS_Closing                 : constant Bits_3 := 5;
   RPS_Queuing                 : constant Bits_3 := 7;
   F_Recd                      : constant Bits_1 := 1;
   F_Xmitted                   : constant Bits_1 := 1;
   Drop_Disabled               : constant Bits_1 := 1;
   Storen_Read                 : constant Bits_1 := 1;
   Cut_Through                 : constant Bits_1 := 0;
   Flush_Disable               : constant Bits_1 := 1;
   Storen_Xmit                 : constant Bits_1 := 1;
   Flush                       : constant Bits_1 := 1;
   TTC_64Bytes                 : constant Bits_3 := 0;
   TTC_128Bytes                : constant Bits_3 := 1;
   TTC_192Bytes                : constant Bits_3 := 2;
   TTC_256Bytes                : constant Bits_3 := 3;
   TTC_40Bytes                 : constant Bits_3 := 4;
   TTC_32Bytes                 : constant Bits_3 := 5;
   TTC_24Bytes                 : constant Bits_3 := 6;
   TTC_16Bytes                 : constant Bits_3 := 7;
   Start                       : constant Bits_1 := 1;
   Stop                        : constant Bits_1 := 0;
   RTC_64Bytes                 : constant Bits_2 := 0;
   RTC_32Bytes                 : constant Bits_2 := 1;
   RTC_96Bytes                 : constant Bits_2 := 2;
   RTC_128Bytes                : constant Bits_2 := 3;
   Msbyte                      : constant Bits_6 := 2#100000#;
   Byte5                       : constant Bits_6 := 2#010000#;
   Byte4                       : constant Bits_6 := 2#001000#;
   Byte3                       : constant Bits_6 := 2#000100#;
   Byte2                       : constant Bits_6 := 2#000010#;
   Lsbyte                      : constant Bits_6 := 2#000001#;
    
   --------------------------------------------------
   -- register definitions                         --
   --------------------------------------------------

   type MACCR_Register is record
      -- Bit definition for Ethernet MAC Control Register register
      Res0      : Reserved (26 .. 31) := (others => 0);
      CSTF      : Bits_1;
      --  CRC stripping for Type frames
      --: Strip_Crc, Off.
      Res5      : Reserved (24 .. 24) := (others => 0);
      WD        : Bits_1;
      -- Watchdog disable
      --: disable_WD, WD_on
      JD        : Bits_1;
      -- Jabber disable
      --: Disable_Jt, Jt_on
      Res1      : Reserved (20 .. 21) := (others => 0);
      IFG       : Bits_3;
      -- Inter-frame gap
      --: IFG_96Bit, IFG_88Bit, IFG_80Bit, IFG_72Bit,
      --: IFG_64Bit, IFG_56Bit, IFG_48Bit, IFG_40Bit
      CSD       : Bits_1;
      -- Carrier sense disable (during transmission)
      --: cs_disable, cs_on
      Res4      : Reserved (15 .. 15) := (others => 0);
      FES       : Bits_1;
      -- Fast ethernet speed
      --: Mb_100, Mb_10
      ROD       : Bits_1;
      -- Receive own disable
      --: Ro_Disable, off
      LM        : Bits_1;
      -- loopback mode
      --: On, Off
      DM        : Bits_1;
      -- Duplex mode
      --: On, Off
      IPCO      : Bits_1;
      -- IP Checksum offload
      --: Enabled, off
      RD        : Bits_1;
      -- Retry disable
      --: Retr_Disabled, off
      Res2      : Reserved (8 .. 8) := (others => 0);
      APCS      : Bits_1;
      -- Automatic Pad/CRC stripping
      --: On, Off
      BL        : Bits_2;
      -- Back-off limit: random integer number (r) of slot time delays before
      -- rescheduling a transmission attempt during retries after a
      -- collision: 0 =< r <2^k
      --: BL_10, BL_8, BL_4, BL_1
      DC        : Bits_1;
      -- Defferal check
      --: On, Off
      TE        : Bits_1;
      -- Transmitter enable
      --: On, Off
      RE        : Bits_1;
      -- Receiver enable
      --: On, Off
      Res3      : Reserved (0 .. 1) := (others => 0);
   end record;

   for MACCR_Register use record
      Res0 at 0 range 26 .. 31;
      CSTF at 0 range 25 .. 25;
      Res5 at 0 range 24 .. 24;
      WD   at 0 range 23 .. 23;
      JD   at 0 range 22 .. 22;
      Res1 at 0 range 20 .. 21;
      IFG  at 0 range 17 .. 19;
      CSD  at 0 range 16 .. 16;
      Res4 at 0 range 15 .. 15;
      FES  at 0 range 14 .. 14;
      ROD  at 0 range 13 .. 13;
      LM   at 0 range 12 .. 12;
      DM   at 0 range 11 .. 11;
      IPCO at 0 range 10 .. 10;
      RD   at 0 range  9 ..  9;
      Res2 at 0 range  8 ..  8;
      APCS at 0 range  7 ..  7;
      BL   at 0 range  5 ..  6;
      DC   at 0 range  4 ..  4;
      TE   at 0 range  3 ..  3;
      RE   at 0 range  2 ..  2;
      Res3 at 0 range  0 ..  1;
   end record;


   type MACFFR_Register is record
      -- Bit definition for Ethernet MAC Frame Filter Register
      RA        : Bits_1;
      -- Receive all
      --: On, Off
      Res0      : Reserved (11 .. 30) := (others => 0);
      HPF       : Bits_1;
      -- Hash or perfect filter
      --: Either, Off
      SAF       : Bits_1;
      -- Source address filter enable
      --: On, Off
      SAIF      : Bits_1;
      -- Source address inverse filtering
      --: On, Off
      PCF       : Bits_2;
      -- Pass control frames: 3 cases
      --: PCF_BlockAll, PCF_ForwardAll_ExPause,
      --: PCF_ForwardAll, PCF_ForwardPassedAddrFilter
      BFD       : Bits_1;
      -- Broadcast frame disable
      --: Filterb, Pass_Allb
      PAM       : Bits_1;
      -- Pass all mutlicast
      --: Pass_allm, Filterm
      DAIF      : Bits_1;
      -- DA Inverse filtering
      --: On, Off
      HM        : Bits_1;
      -- Hash multicast
      --: Hash_Filter, Normal
      HU        : Bits_1;
      -- Hash unicast
      --: Hash_Filter, Normal
      PM        : Bits_1;
      -- Promiscuous mode
      --: On, Off
   end record;

   for MACFFR_Register use record
      RA   at 0 range 31 .. 31;
      Res0 at 0 range 11 .. 30;
      HPF  at 0 range 10 .. 10;
      SAF  at 0 range  9 ..  9;
      SAIF at 0 range  8 ..  8;
      PCF  at 0 range  6 ..  7;
      BFD  at 0 range  5 ..  5;
      PAM  at 0 range  4 ..  4;
      DAIF at 0 range  3 ..  3;
      HM   at 0 range  2 ..  2;
      HU   at 0 range  1 ..  1;
      PM   at 0 range  0 ..  0;
   end record;


   subtype MACHTHR_Register is Bits_32;
   -- Bit definition for Ethernet MAC Hash Table High Register


   subtype MACHTLR_Register is Bits_32;
   -- Bit definition for Ethernet MAC Hash Table Low Register


   type MACMIIAR_Register is record
      -- Bit definition for Ethernet MAC MII Address Register
      Res0      : Reserved (16 .. 31) := (others => 0);
      PA        : Bits_5;
      -- Physical layer address
      MR        : Bits_5;
      -- MII register in the selected PHY
      Res1      : Reserved (5 .. 5) := (others => 0);
      CR        : Bits_3;
      -- CR clock range: 6 cases
      --: CR_Div42, CR_Div62, CR_Div16, CR_Div26, CR_Div102
      MW        : Bits_1;
      -- MII write
      --: Write, Read
      MB        : Bits_1;
      -- MII busy
      --: Busy, Clear;
   end record;

   for MACMIIAR_Register use record
      Res0 at 0 range 16 .. 31;
      PA   at 0 range 11 .. 15;
      MR   at 0 range  6 .. 10;
      Res1 at 0 range  5 ..  5;
      CR   at 0 range  2 ..  4;
      MW   at 0 range  1 ..  1;
      MB   at 0 range  0 ..  0;
   end record;


   type MACMIIDR_Register is record
      -- Bit definition for Ethernet MAC MII Data Register
      Res0      : Reserved (16 .. 31) := (others => 0);
      MD        : Bits_16;
      -- MII data: read/write data from/to PHY
   end record;

   for MACMIIDR_Register use record
      Res0 at 0 range 16 .. 31;
      MD   at 0 range  0 .. 15;
   end record;


   type MACFCR_Register is record
      -- Bit definition for Ethernet MAC Flow Control Register
      PT        : Bits_16;
      -- Pause time
      Res0      : Reserved (8 .. 15) := (others => 0);
      ZQPD      : Bits_1;
      -- Zero-quanta pause disable
      --: Pf_Disable, Pf_On
      Res1      : Reserved (6 .. 6) := (others => 0);
      PLT       : Bits_2;
      -- Pause low threshold: 4 cases
      --: PLT_Minus4, PLT_Minus28, PLT_Minus144, PLT_Minus256
      UPFD      : Bits_1;
      -- Unicast pause frame detect
      --: On, Off
      RFCE      : Bits_1;
      -- Receive flow control enable
      --: On, Off
      TFCE      : Bits_1;
      -- Transmit flow control enable
      --: On, Off
      FCBBPA    : Bits_1;
      -- Flow control busy/backpressure activate
      --: On, Off
   end record;

   for MACFCR_Register use record
      PT     at 0 range 16 .. 31;
      Res0   at 0 range  8 .. 15;
      ZQPD   at 0 range  7 ..  7;
      Res1   at 0 range  6 ..  6;
      PLT    at 0 range  4 ..  5;
      UPFD   at 0 range  3 ..  3;
      RFCE   at 0 range  2 ..  2;
      TFCE   at 0 range  1 ..  1;
      FCBBPA at 0 range  0 ..  0;
   end record;


   type MACVLANTR_Register is record
      -- Bit definition for Ethernet MAC VLAN Tag Register
      Res0      : Reserved (17 .. 31) := (others => 0);
      VLANTC    : Bits_1;
      -- 12-bit VLAN tag comparison
      --: On, Off
      VLANTI    : Bits_16;
      -- VLAN tag identifier (for receive frames)
   end record;

   for MACVLANTR_Register use record
      Res0   at 0 range 17 .. 31;
      VLANTC at 0 range 16 .. 16;
      VLANTI at 0 range  0 .. 15;
   end record;


   type MACRWUFFR_Register is record
      -- Bit definition for Ethernet MAC Remote Wake-UpFrame Filter Register
      D         : Bits_32;
      -- Eight sequential Writes to this address (offset 0x28) will write all
      -- Wake-UpFrame Filter Registers.
      -- Eight sequential Reads from this address (offset 0x28) will read all
      -- Wake-UpFrame Filter Registers.
      --
-- Wake-UpFrame Filter Reg0     : Filter 0 Byte Mask
-- Wake-UpFrame Filter Reg1     : Filter 1 Byte Mask
-- Wake-UpFrame Filter Reg2     : Filter 2 Byte Mask
-- Wake-UpFrame Filter Reg3     : Filter 3 Byte Mask
-- Wake-UpFrame Filter Reg4     : RSVD - Filter3 Command - RSVD - Filter2 Command
--                            RSVD - Filter1 Command - RSVD - Filter0 Command
      -- Wake-UpFrame Filter Re5        : Filter3 Offset - Filter2 Offset -
      --                           Filter1 Offset - Filter0 Offset
      -- Wake-UpFrame Filter Re6        : Filter1 CRC16 - Filter0 CRC16
      -- Wake-UpFrame Filter Re7        : Filter3 CRC16 - Filter2 CRC16
   end record;

   for MACRWUFFR_Register use record
      D at 0 range 0 .. 31;
   end record;


   type MACPMTCSR_Register is record
      -- Bit definition for Ethernet MAC PMT Control and Status Register
      WFFRPR    : Bits_1;
      -- Wake-Up Frame Filter Register Pointer Reset
      --: Reset, Off
      Res0      : Reserved (10 .. 30) := (others => 0);
      GU        : Bits_1;
      -- Global Unicast
      --: On, Off
      Res1      : Reserved (7 .. 8) := (others => 0);
      WFR       : Bits_1;
      -- Wake-Up Frame Received
      --: Wf_Recd, Off
      MPR       : Bits_1;
      -- Magic Packet Received
      --: Mp_Recd, Off
      Res2      : Reserved (3 .. 4) := (others => 0);
      WFE       : Bits_1;
      -- Wake-Up Frame Enable
      --: On, Off
      MPE       : Bits_1;
      -- Magic Packet Enable
      --: On, Off
      PD        : Bits_1;
      -- Power Down
      --: Pow_Down, Off
   end record;

   for MACPMTCSR_Register use record
      WFFRPR at 0 range 31 .. 31;
      Res0   at 0 range 10 .. 30;
      GU     at 0 range  9 ..  9;
      Res1   at 0 range  7 ..  8;
      WFR    at 0 range  6 ..  6;
      MPR    at 0 range  5 ..  5;
      Res2   at 0 range  3 ..  4;
      WFE    at 0 range  2 ..  2;
      MPE    at 0 range  1 ..  1;
      PD     at 0 range  0 ..  0;
   end record;


   type MACDBGR_Register is record
      -- This debug register gives the status of all the main modules of
      -- the transmit and receive data paths and the FIFOs.
      -- An all-zero status indicates that the MAC core is in Idle state
      -- (and FIFOs are empty) and no activity is going on in the data paths.
      Res0      : Reserved (26 .. 31) := (others => 0);
      TFF       : Bits_1;
      --  Tx FIFO full
      --: full, nf
      TFNE      : Bits_1;
      -- Tx FIFO not empty
      --: N_Mt, Mt
      Res1      : Reserved (23 .. 23) := (others => 0);
      TFWA      : Bits_1;
      -- Tx FIFO write active
      --: Busy, Clear
      TFRS      : Bits_2;
      -- Tx FIFO read status
      --: ?
      MTP       : Bits_1;
      --  MAC transmitter in pause
      --: Yes, No
      MTFCS     : Bits_2;
      -- MAC transmit frame controller status
      --: ?
      MMTEA     : Bits_1;
      -- MAC MII transmit engine active
      --: Yes, No
      Res2      : Reserved (10 .. 15) := (others => 0);
      RFFL      : Bits_2;
      --  Rx FIFO fill level
      ---
      Res3      : Reserved (7 .. 7) := (others => 0);
      RFRCS     : Bits_2;
      -- Rx FIFO read controller status
      RFWRA     : Bits_1;
      --  Rx FIFO write controller active
      --: Yes, No
      Res4      : Reserved (3 .. 3) := (others => 0);
      MSFRWCS   : Bits_2;
      -- MAC small FIFO read / write controllers status
      MMRPEA    : Bits_1;
      --MAC MII receive protocol engine active
      --: Yes, No
   end record;

   for MACDBGR_Register use record
      Res0    at 0 range 26 .. 31;
      TFF     at 0 range 25 .. 25;
      TFNE    at 0 range 24 .. 24;
      Res1    at 0 range 23 .. 23;
      TFWA    at 0 range 22 .. 22;
      TFRS    at 0 range 20 .. 21;
      MTP     at 0 range 19 .. 19;
      MTFCS   at 0 range 17 .. 18;
      MMTEA   at 0 range 16 .. 16;
      Res2    at 0 range 10 .. 15;
      RFFL    at 0 range  8 ..  9;
      Res3    at 0 range  7 ..  7;
      RFRCS   at 0 range  5 ..  6;
      RFWRA   at 0 range  4 ..  4;
      Res4    at 0 range  3 ..  3;
      MSFRWCS at 0 range  1 ..  2;
      MMRPEA  at 0 range  0 ..  0;
   end record;


   type MACSR_Register is record
      -- Bit definition for Ethernet MAC interrupt Status Register
      Res0      : Reserved (10 .. 15) := (others => 0);
      TSTS      : Bits_1;
      -- Time stamp trigger status
      --: Tripped, Off
      Res1      : Reserved (7 .. 8) := (others => 0);
      MMCTS     : Bits_1;
      -- MMC transmit status
      --: Tripped, Off
      MMMCRS    : Bits_1;
      -- MMC receive status
      --: Tripped, Off
      MMCS      : Bits_1;
      -- MMC status
      --: Tripped, Off
      PMTS      : Bits_1;
      -- PMT status
      --: Tripped, Off
      Res2      : Reserved (0 .. 2) := (others => 0);
   end record;

   for MACSR_Register use record
      Res0   at 0 range 10 .. 15;
      TSTS   at 0 range  9 ..  9;
      Res1   at 0 range  7 ..  8;
      MMCTS  at 0 range  6 ..  6;
      MMMCRS at 0 range  5 ..  5;
      MMCS   at 0 range  4 ..  4;
      PMTS   at 0 range  3 ..  3;
      Res2   at 0 range  0 ..  2;
   end record;


   type MACIMR_Register is record
      -- Bit definition for Ethernet MAC Interrupt Mask Register
      Res0      : Reserved (10 .. 15) := (others => 0);
      TSTIM     : Bits_1;
      -- Time stamp trigger interrupt mask
      --: Int_Disable, Int_Enable
      Res1      : Reserved (4 .. 8) := (others => 0);
      PMTIM     : Bits_1;
      -- PMT interrupt mask
      --: Int_Disable, Int_Enable
      Res2      : Reserved (0 .. 2) := (others => 0);
   end record;

   for MACIMR_Register use record
      Res0  at 0 range 10 .. 15;
      TSTIM at 0 range  9 ..  9;
      Res1  at 0 range  4 ..  8;
      PMTIM at 0 range  3 ..  3;
      Res2  at 0 range  0 ..  2;
   end record;


   type MACA0HR_Register is record
      -- Bit definition for Ethernet MAC Address0 High Register
      Mo        : Bits_1;
      -- always 1
      Res0      : Reserved (16 .. 30) := (others => 0);
      MACA0H    : Bits_16;
      -- MAC address0 high
   end record;

   for MACA0HR_Register use record
      Mo     at 0 range 31 .. 31;
      Res0   at 0 range 16 .. 30;
      MACA0H at 0 range  0 .. 15;
   end record;


   subtype MACA0LR_Register is Bits_32;
   -- Bit definition for Ethernet MAC Address0 Low Register


   type MACA1HR_Register is record
      -- Bit definition for Ethernet MAC Address1 High Register
      AE        : Bits_1;
      -- Address enable
      --: Enabled, off
      SA        : Bits_1;
      -- Source address
      -- Maca_Sa, Maca_da
      MBC       : Bits_6;
      -- Mask byte control: bits to mask for comparison of the MAC Address bytes
      -- msbyte, byte5, byte4, byte3, byte2, lsbyte;
      Res0      : Reserved (16 .. 23) := (others => 0);
      MACA1H    : Bits_16;
      -- MAC address1 high
   end record;

   for MACA1HR_Register use record
      AE     at 0 range 31 .. 31;
      SA     at 0 range 30 .. 30;
      MBC    at 0 range 24 .. 29;
      Res0   at 0 range 16 .. 23;
      MACA1H at 0 range  0 .. 15;
   end record;


   subtype MACA1LR_Register is Bits_32;
   -- Bit definition for Ethernet MAC Address1 Low Register


   type MACA2HR_Register is record
      -- Bit definition for Ethernet MAC Address2 High Register
      AE        : Bits_1;
      -- Address enable
      --: Enabled, off
      SA        : Bits_1;
      -- Source address
      -- Maca_Sa, Maca_da
      MBC       : Bits_6;
      -- Mask byte control
      Res0      : Reserved (16 .. 23) := (others => 0);
      MACA2H    : Bits_16;
      -- MAC address2 high
   end record;

   for MACA2HR_Register use record
      AE     at 0 range 31 .. 31;
      SA     at 0 range 30 .. 30;
      MBC    at 0 range 24 .. 29;
      Res0   at 0 range 16 .. 23;
      MACA2H at 0 range  0 .. 15;
   end record;


   subtype MACA2LR_Register is Bits_32;
   -- Bit definition for Ethernet MAC Address2 Low Register


   type MACA3HR_Register is record
      -- Bit definition for Ethernet MAC Address3 High Register
      AE        : Bits_1;
      -- Address enable
      --: Enabled, off
      SA        : Bits_1;
      -- Source address
      -- Maca_Sa, Maca_da
      MBC       : Bits_6;
      -- Mask byte control
      Res0      : Reserved (16 .. 23) := (others => 0);
      MACA3H    : Bits_16;
      -- MAC address3 high
   end record;

   for MACA3HR_Register use record
      AE     at 0 range 31 .. 31;
      SA     at 0 range 30 .. 30;
      MBC    at 0 range 24 .. 29;
      Res0   at 0 range 16 .. 23;
      MACA3H at 0 range  0 .. 15;
   end record;


   subtype MACA3LR_Register is Bits_32;
   -- Bit definition for Ethernet MAC Address3 Low Register
   
   
   --------------------------------------------
   -- Ethernet MMC Registers bits definition --
   --------------------------------------------

   type MMCCR_Register is record
      -- Bit definition for Ethernet MMC Contol Register
      Res0      : Reserved (6 .. 31) := (others => 0);
      MCFHP     : Bits_1;
      -- MMC counter Full-Half preset
      --: On, Off
      MCP       : Bits_1;
      -- MMC counter preset
      --: On, Off
      MCF       : Bits_1;
      -- MMC Counter Freeze
      --: On, Off
      ROR       : Bits_1;
      -- Reset on Read
      --: On, Off
      CSR       : Bits_1;
      -- Counter Stop Rollover
      --: On, Off
      CR        : Bits_1;
      -- Counters Reset
      --: reset, off
   end record;

   for MMCCR_Register use record
      Res0  at 0 range 6 .. 31;
      MCFHP at 0 range 5 ..  5;
      MCP   at 0 range 4 ..  4;
      MCF   at 0 range 3 ..  3;
      ROR   at 0 range 2 ..  2;
      CSR   at 0 range 1 ..  1;
      CR    at 0 range 0 ..  0;
   end record;


   type MMCRIR_Register is record
      -- Bit definition for Ethernet MMC Receive Interrupt Register
      Res0      : Reserved (18 .. 31) := (others => 0);
      RGUFS     : Bits_1;
      -- Set when Rx good unicast frames counter reaches half the maximum value
      --: Tripped, Off
      Res1      : Reserved (7 .. 16) := (others => 0);
      RFAES     : Bits_1;
      -- Set when Rx alignment error counter reaches half the maximum value
      --: Tripped, Off
      RFCES     : Bits_1;
      -- Set when Rx crc error counter reaches half the maximum value
      --: Tripped, Off
      Res2      : Reserved (0 .. 4) := (others => 0);
   end record;

   for MMCRIR_Register use record
      Res0  at 0 range 18 .. 31;
      RGUFS at 0 range 17 .. 17;
      Res1  at 0 range  7 .. 16;
      RFAES at 0 range  6 ..  6;
      RFCES at 0 range  5 ..  5;
      Res2  at 0 range  0 ..  4;
   end record;


   type MMCTIR_Register is record
      -- Bit definition for Ethernet MMC Transmit Interrupt Register
      Res0      : Reserved (22 .. 31) := (others => 0);
      TGFS      : Bits_1;
      -- Set when Tx good frame count counter reaches half the maximum value
      --: Tripped, Off
      Res1      : Reserved (16 .. 20) := (others => 0);
      TGFMSCS   : Bits_1;
      -- Set when Tx good multi col counter reaches half the maximum value
      --: Tripped, Off
      TGFSCS    : Bits_1;
      -- Set when Tx good single col counter reaches half the maximum value
      --: Tripped, Off
      Res2      : Reserved (0 .. 13) := (others => 0);
   end record;

   for MMCTIR_Register use record
      Res0    at 0 range 22 .. 31;
      TGFS    at 0 range 21 .. 21;
      Res1    at 0 range 16 .. 20;
      TGFMSCS at 0 range 15 .. 15;
      TGFSCS  at 0 range 14 .. 14;
      Res2    at 0 range  0 .. 13;
   end record;


   type MMCRIMR_Register is record
      -- Bit definition for Ethernet MMC Receive Interrupt Mask Register
      Res0      : Reserved (18 .. 31) := (others => 0);
      RGUFM     : Bits_1;
      -- Mask the interrupt when Rx good unicast frames counter reaches
      -- half the maximum value
      --: Int_Disabled, Int_Enabled
      Res1      : Reserved (7 .. 16) := (others => 0);
      RFAEM     : Bits_1;
      -- Mask the interrupt when when Rx alignment error counter reaches
      -- half the maximum value
      --: Int_Disabled, Int_Enabled
      RFCEM     : Bits_1;
      -- Mask the interrupt when Rx crc error counter reaches
      -- half the maximum value
      --: Int_Disabled, Int_Enabled
      Res2      : Reserved (0 .. 4) := (others => 0);
   end record;

   for MMCRIMR_Register use record
      Res0  at 0 range 18 .. 31;
      RGUFM at 0 range 17 .. 17;
      Res1  at 0 range  7 .. 16;
      RFAEM at 0 range  6 ..  6;
      RFCEM at 0 range  5 ..  5;
      Res2  at 0 range  0 ..  4;
   end record;


   type MMCTIMR_Register is record
      -- Bit definition for Ethernet MMC Transmit Interrupt Mask Register
      Res0      : Reserved (22 .. 31) := (others => 0);
      TGFM      : Bits_1;
      -- Mask the interrupt when Tx good frame count counter reaches
      -- half the maximum value
      --: Int_Disabled, Int_Enabled
      Res1      : Reserved (16 .. 20) := (others => 0);
      TGFMSCM   : Bits_1;
      -- Mask the interrupt when Tx good multi col counter reaches
      -- half the maximum value
      --: Int_Disabled, Int_Enabled
      TGFSCM    : Bits_1;
      -- Mask the interrupt when Tx good single col counter reaches
      -- half the maximum value
      --: Int_Disabled, Int_Enabled
      Res2      : Reserved (0 .. 13) := (others => 0);
   end record;

   for MMCTIMR_Register use record
      Res0    at 0 range 22 .. 31;
      TGFM    at 0 range 21 .. 21;
      Res1    at 0 range 16 .. 20;
      TGFMSCM at 0 range 15 .. 15;
      TGFSCM  at 0 range 14 .. 14;
      Res2    at 0 range  0 .. 13;
   end record;


   subtype MMCTGFSCCR_Register is Bits_32;
   -- Bit definition for Ethernet MMC Transmitted Good Frames after
   -- Single Collision Counter Register


   subtype MMCTGFMSCCR_Register is Bits_32;
   -- Bit definition for Ethernet MMC Transmitted Good Frames after
   -- More than a Single Collision Counter Register


   subtype MMCTGFCR_Register is Bits_32;
   -- Bit definition for Ethernet MMC Transmitted Good Frames Counter Register


   subtype MMCRFCECR_Register is Bits_32;
   -- Bit definition for Ethernet MMC Received Frames with
   -- CRC Error Counter Register


   subtype MMCRFAECR_Register is Bits_32;
   -- Bit definition for Ethernet MMC Received Frames with
   -- Alignement Error Counter Register


   subtype MMCRGUFCR_Register is Bits_32;
   -- Bit definition for Ethernet MMC Received Good Unicast Frames Counter Register
   
   
   --------------------------------------------
   -- Ethernet PTP Registers bits definition --
   --------------------------------------------

   type PTPTSCR_Register is record
      -- Bit definition for Ethernet PTP Time Stamp Contol Register
      Res0              : Reserved (19 .. 31) := (others => 0);
      TSPFFMAE          : Bits_1;
      -- Time stamp PTP frame filtering MAC address enable
      --: Enabled, Off
      TSCNT             : Bits_2;
      -- Time stamp clock node type
      --: ?
      TSSMRME           : Bits_1;
      -- Time stamp snapshot for message relevant to master enable
      --: Enabled, Off
      TSSEME            : Bits_1;
      -- Time stamp snapshot for event message enable
      --: Enabled, Off
      TSSIPV4FE         : Bits_1;
      -- Time stamp snapshot for IPv4 frames enable
      --: Enabled, Off
      TSSIPV6FE         : Bits_1;
      -- Time stamp snapshot for IPv6 frames enable
      --: Enabled, Off
      TSSPTPOEFE        : Bits_1;
      -- Time stamp snapshot for PTP over ethernet frames enable
      --: Enabled, Off
      TSPTPPSV2E        : Bits_1;
      -- Time stamp PTP packet snooping for version2 format enable
      --: Enabled, Off
      TSSSR             : Bits_1;
      -- Time stamp Sub-seconds rollover
      --: On, Off
      TSSARFE           : Bits_1;
      -- Time stamp snapshot for all received frames enable
      --: Enabled, Off
      Res2              : Reserved (6 .. 7) := (others => 0);
      TSARU             : Bits_1;
      -- Addend register update
      --: Update, off
      TSITE             : Bits_1;
      -- Time stamp interrupt trigger enable
      --: Enabled, Off
      TSSTU             : Bits_1;
      -- Time stamp update
      --: Update, off
      TSSTI             : Bits_1;
      -- Time stamp initialize
      --: Init, off
      TSFCU             : Bits_1;
      -- Time stamp fine or coarse update
      --: Fine, Coarse
      TSE               : Bits_1;
      -- Time stamp enable
      --: Enabled, Off
   end record;

   for PTPTSCR_Register use record
      Res0       at 0 range 19 .. 31;
      TSPFFMAE   at 0 range 18 .. 18;
      TSCNT      at 0 range 16 .. 17;
      TSSMRME    at 0 range 15 .. 15;
      TSSEME     at 0 range 14 .. 14;
      TSSIPV4FE  at 0 range 13 .. 13;
      TSSIPV6FE  at 0 range 12 .. 12;
      TSSPTPOEFE at 0 range 11 .. 11;
      TSPTPPSV2E at 0 range 10 .. 10;
      TSSSR      at 0 range  9 ..  9;
      TSSARFE    at 0 range  8 ..  8;
      Res2       at 0 range  6 ..  7;
      TSARU      at 0 range  5 ..  5;
      TSITE      at 0 range  4 ..  4;
      TSSTU      at 0 range  3 ..  3;
      TSSTI      at 0 range  2 ..  2;
      TSFCU      at 0 range  1 ..  1;
      TSE        at 0 range  0 ..  0;
   end record;


   type PTPSSIR_Register is record
      -- Bit definition for Ethernet PTP Sub-Second Increment Register
      Res0      : Reserved (8 .. 31) := (others => 0);
      STSSI     : Byte;
      -- System time Sub-second increment value
   end record;

   for PTPSSIR_Register use record
      Res0  at 0 range 8 .. 31;
      STSSI at 0 range 0 ..  7;
   end record;


   subtype PTPTSHR_Register is Bits_32;
   -- Bit definition for Ethernet PTP Time Stamp High Register


   type PTPTSLR_Register is record
      -- Bit definition for Ethernet PTP Time Stamp Low Register
      STPNS     : Bits_1;
      -- System Time Positive or negative time
      STSS      : Bits_31;
      -- System Time sub-seconds
   end record;

   for PTPTSLR_Register use record
      STPNS at 0 range 31 .. 31;
      STSS  at 0 range  0 .. 30;
   end record;


   subtype PTPTSHUR_Register is Bits_32;
   -- Bit definition for Ethernet PTP Time Stamp High Update Register


   type PTPTSLUR_Register is record
      -- Bit definition for Ethernet PTP Time Stamp Low Update Register
      TSUPNS    : Bits_1;
      -- Time stamp update Positive or negative time
      TSUSS     : Bits_31;
      -- Time stamp update sub-seconds
   end record;

   for PTPTSLUR_Register use record
      TSUPNS at 0 range 31 .. 31;
      TSUSS  at 0 range  0 .. 30;
   end record;


   subtype PTPTSAR_Register is Bits_32;
   -- Bit definition for Ethernet PTP Time Stamp Addend Register


   subtype PTPTTHR_Register is Bits_32;
   -- Bit definition for Ethernet PTP Target Time High Register


   subtype PTPTTLR_Register is Bits_32;
   -- Bit definition for Ethernet PTP Target Time Low Register


   type PTPTSSR_Register is record
      -- Bit definition for Ethernet PTP Time Stamp Status Register
      Res0      : Reserved (2 .. 31) := (others => 0);
      TSTTR     : Bits_1;
      -- Time stamp target time reached
      --: Tripped, Off
      TSSO      : Bits_1;
      -- Time stamp seconds overflow
      --: Tripped, Off
   end record;

   for PTPTSSR_Register use record
      Res0  at 0 range 2 .. 31;
      TSTTR at 0 range 1 ..  1;
      TSSO  at 0 range 0 ..  0;
   end record;


   type PTPPPSCR_Register is record
      -- Bit definition for Ethernet PTP Pulses Per Second Control Register
      Res0      : Reserved (4 .. 31) := (others => 0);
      PPSFREQ   : Bits_4;
      -- PPS frequency selection
      --: ? give it a number - 2 ** number is the frequency
   end record;

   for PTPPPSCR_Register use record
      Res0    at 0 range 4 .. 31;
      PPSFREQ at 0 range 0 ..  3;
   end record;


   type DMABMR_Register is record
      -- Bit definition for Ethernet DMA Bus Mode Register
      Res0      : Reserved (27 .. 31) := (others => 0);
      MB        : Bits_1;
      -- Mixed burst
      --: On, Off
      AAB       : Bits_1;
      -- Address-Aligned beats
      --: On, Off
      FPM       : Bits_1;
      -- 4xPBL mode
      --: On, Off
      USP       : Bits_1;
      -- Use separate PBL
      --: On, Off
      RDP       : Bits_6;
      -- maximum number of beats to be transferred in one RxDMA
      --: RDP_1Beat, RDP_2Beat, RDP_4Beat, RDP_8Beat, RDP_16Beat, RDP_32Beat
      FB        : Bits_1;
      -- Fixed Burst
      --: On, Off
      RTPR      : Bits_2;
      -- Rx Tx priority ratio
      --: RTPR_1_1, RTPR_2_1, RTPR_3_1, RTPR_4_1
      PBL       : Bits_6;
      -- maximum number of beats to be transferred in one TxDMA
      --: PBL_1Beat, PBL_2Beat;, PBL_4Beat, PBL_8Beat, PBL_16Beat, PBL_32Beat
      EDFE     : Bits_1;
      -- Enhanced Descriptor Enable
      --: Enabled, off
      DSL       : Bits_5;
      -- Descriptor Skip Length
      DA        : Bits_1;
      -- DMA arbitration scheme
      --: Rx_Tx, Round_Robin
      SR        : Bits_1;
      -- Software reset
      --: Reset, off
   end record;

   for DMABMR_Register use record
      Res0  at 0 range 27 .. 31;
      MB    at 0 range 26 .. 26;
      AAB   at 0 range 25 .. 25;
      FPM   at 0 range 24 .. 24;
      USP   at 0 range 23 .. 23;
      RDP   at 0 range 17 .. 22;
      FB    at 0 range 16 .. 16;
      RTPR  at 0 range 14 .. 15;
      PBL   at 0 range  8 .. 13;
      EDFE  at 0 range  7 ..  7;
      DSL   at 0 range  2 ..  6;
      DA    at 0 range  1 ..  1;
      SR    at 0 range  0 ..  0;
   end record;


   subtype DMATPDR_Register is Bits_32;
   -- Bit definition for Ethernet DMA Transmit Poll Demand Register


   subtype DMARPDR_Register is Bits_32;
   -- Bit definition for Ethernet DMA Receive Poll Demand Register


   subtype DMARDLAR_Register is Bits_32;
   -- Bit definition for Ethernet DMA Receive Descriptor List Address Register


   subtype DMATDLAR_Register is Bits_32;
   -- Bit definition for Ethernet DMA Transmit Descriptor List Address Register


   type DMASR_Register is record
      -- Bit definition for Ethernet DMA Status Register
      Res0      : Reserved (30 .. 31) := (others => 0);
      TSTS      : Bits_1;
      -- Time-stamp trigger status
      --: Tripped, off
      PMTS      : Bits_1;
      -- PMT status
      --: Tripped, off
      MMCS      : Bits_1;
      -- MMC status
      --: Tripped, off
      Res1      : Reserved (26 .. 26) := (others => 0);
      EBS       : Bits_3;
      -- Error bits status
      --: EBS_DataTransfTx, EBS_ReadTransf, EBS_DescAccess.
      --:   perhaps a wrong intepretation of the ref
      TPS       : Bits_3;
      -- Transmit process state
      --: TPS_Stopped, TPS_Fetching, TPS_Waiting,
      --: TPS_Reading, TPS_Suspended, TPS_Closing
      RPS       : Bits_3;
      -- Receive process state
      --: RPS_Stopped, RPS_Fetching, RPS_Waiting,
      --: RPS_Suspended, RPS_Closing, RPS_Queuing
      NIS       : Bits_1;
      -- Normal interrupt summary
      --: Tripped, Off
      AIS       : Bits_1;
      -- Abnormal interrupt summary
      --: Tripped, Off
      ERS       : Bits_1;
      -- Early receive status
      --: Tripped, Off
      FBES      : Bits_1;
      -- Fatal bus error status
      --: Tripped, Off
      Res2      : Reserved (11 .. 12) := (others => 0);
      ETS       : Bits_1;
      -- Early transmit status
      --: Tripped, Off
      RWTS      : Bits_1;
      -- Receive watchdog timeout status
      --: Tripped, Off
      RPSS      : Bits_1;
      -- Receive process stopped status
      --: On, Off
      RBUS      : Bits_1;
      -- Receive buffer unavailable status
      --: Busy, Off
      RS        : Bits_1;
      -- Receive status
      --: F_Recd, off
      TUS       : Bits_1;
      -- Transmit underflow status
      --: Tripped, Off
      ROS       : Bits_1;
      -- Receive overflow status
      --: Tripped, Off
      TJTS      : Bits_1;
      -- Transmit jabber timeout status
      --: Tripped, Off
      TBUS      : Bits_1;
      -- Transmit buffer unavailable status
      --: Busy, Off
      TPSS      : Bits_1;
      -- Transmit process stopped status
      --: Tripped, Off
      TS        : Bits_1;
      -- Transmit status
      --: F_Xmitted, Off
   end record;

   for DMASR_Register use record
      Res0 at 0 range 30 .. 31;
      TSTS at 0 range 29 .. 29;
      PMTS at 0 range 28 .. 28;
      MMCS at 0 range 27 .. 27;
      Res1 at 0 range 26 .. 26;
      EBS  at 0 range 23 .. 25;
      TPS  at 0 range 20 .. 22;
      RPS  at 0 range 17 .. 19;
      NIS  at 0 range 16 .. 16;
      AIS  at 0 range 15 .. 15;
      ERS  at 0 range 14 .. 14;
      FBES at 0 range 13 .. 13;
      Res2 at 0 range 11 .. 12;
      ETS  at 0 range 10 .. 10;
      RWTS at 0 range  9 ..  9;
      RPSS at 0 range  8 ..  8;
      RBUS at 0 range  7 ..  7;
      RS   at 0 range  6 ..  6;
      TUS  at 0 range  5 ..  5;
      ROS  at 0 range  4 ..  4;
      TJTS at 0 range  3 ..  3;
      TBUS at 0 range  2 ..  2;
      TPSS at 0 range  1 ..  1;
      TS   at 0 range  0 ..  0;
   end record;


   type DMAOMR_Register is record
      -- Bit definition for Ethernet DMA Operation Mode Register
      Res0      : Reserved (27 .. 31) := (others => 0);
      DTCEFD    : Bits_1;
      -- Disable Dropping of TCP/IP checksum error frames
      --: Drop_Disabled, Off
      RSF       : Bits_1;
      -- Receive store and forward
      --: Storen_Read, Cut_Through
      DFRF      : Bits_1;
      -- Disable flushing of received frames
      --: Flush_Disable, Off
      Res1      : Reserved (22 .. 23) := (others => 0);
      TSF       : Bits_1;
      -- Transmit store and forward
      --: Storen_Xmit, Cut_Through
      FTF       : Bits_1;
      -- Flush transmit FIFO
      --: Flush, Off
      Res2      : Reserved (17 .. 19) := (others => 0);
      TTC       : Bits_3;
      -- Transmit threshold control
      --: TTC_64Bytes, TTC_128Bytes, TTC_192Bytes, TTC_256Bytes,
      --: TTC_40Bytes, TTC_32Bytes, TTC_24Bytes, TTC_16Bytes
      ST        : Bits_1;
      -- Start/stop transmission command
      --: Start, Stop
      Res3      : Reserved (8 .. 12) := (others => 0);
      FEF       : Bits_1;
      -- Forward error frames
      --: On, Off
      FUGF      : Bits_1;
      -- Forward undersized good frames
      --: On, Off
      Res4      : Reserved (5 .. 5) := (others => 0);
      RTC       : Bits_2;
      -- receive threshold control
      --: RTC_64Bytes, RTC_32Bytes, RTC_96Bytes, RTC_128Bytes
      OSF       : Bits_1;
      -- operate on second frame
      --: On, Off
      SR        : Bits_1;
      -- Start/stop receive
      --: Start, Stop
      Res5      : Reserved (0 .. 0) := (others => 0);
   end record;

   for DMAOMR_Register use record
      Res0   at 0 range 27 .. 31;
      DTCEFD at 0 range 26 .. 26;
      RSF    at 0 range 25 .. 25;
      DFRF   at 0 range 24 .. 24;
      Res1   at 0 range 22 .. 23;
      TSF    at 0 range 21 .. 21;
      FTF    at 0 range 20 .. 20;
      Res2   at 0 range 17 .. 19;
      TTC    at 0 range 14 .. 16;
      ST     at 0 range 13 .. 13;
      Res3   at 0 range  8 .. 12;
      FEF    at 0 range  7 ..  7;
      FUGF   at 0 range  6 ..  6;
      Res4   at 0 range  5 ..  5;
      RTC    at 0 range  3 ..  4;
      OSF    at 0 range  2 ..  2;
      SR     at 0 range  1 ..  1;
      Res5   at 0 range  0 ..  0;
   end record;


   type DMAIER_Register is record
      -- Bit definition for Ethernet DMA Interrupt Enable Register
      Res0      : Reserved (17 .. 31) := (others => 0);
      NISE      : Bits_1;
      -- Normal interrupt summary enable
      --: Enabled, Off
      AISE      : Bits_1;
      -- Abnormal interrupt summary enable
      --: Enabled, Off
      ERIE      : Bits_1;
      -- Early receive interrupt enable
      --: Enabled, Off
      FBEIE     : Bits_1;
      -- Fatal bus error interrupt enable
      --: Enabled, Off
      Res1      : Reserved (11 .. 12) := (others => 0);
      ETIE      : Bits_1;
      -- Early transmit interrupt enable
      --: Enabled, Off
      RWTIE     : Bits_1;
      -- Receive watchdog timeout interrupt enable
      --: Enabled, Off
      RPSIE     : Bits_1;
      -- Receive process stopped interrupt enable
      --: Enabled, Off
      RBUIE     : Bits_1;
      -- Receive buffer unavailable interrupt enable
      --: Enabled, Off
      RIE       : Bits_1;
      -- Receive interrupt enable
      --: Enabled, Off
      TUIE      : Bits_1;
      -- Transmit Underflow interrupt enable
      --: Enabled, Off
      ROIE      : Bits_1;
      -- Receive Overflow interrupt enable
      --: Enabled, Off
      TJTIE     : Bits_1;
      -- Transmit jabber timeout interrupt enable
      --: Enabled, Off
      TBUIE     : Bits_1;
      -- Transmit buffer unavailable interrupt enable
      --: Enabled, Off
      TPSIE     : Bits_1;
      -- Transmit process stopped interrupt enable
      --: Enabled, Off
      TIE       : Bits_1;
      -- Transmit interrupt enable
      --: Enabled, Off
   end record;

   for DMAIER_Register use record
      Res0  at 0 range 17 .. 31;
      NISE  at 0 range 16 .. 16;
      AISE  at 0 range 15 .. 15;
      ERIE  at 0 range 14 .. 14;
      FBEIE at 0 range 13 .. 13;
      Res1  at 0 range 11 .. 12;
      ETIE  at 0 range 10 .. 10;
      RWTIE at 0 range  9 ..  9;
      RPSIE at 0 range  8 ..  8;
      RBUIE at 0 range  7 ..  7;
      RIE   at 0 range  6 ..  6;
      TUIE  at 0 range  5 ..  5;
      ROIE  at 0 range  4 ..  4;
      TJTIE at 0 range  3 ..  3;
      TBUIE at 0 range  2 ..  2;
      TPSIE at 0 range  1 ..  1;
      TIE   at 0 range  0 ..  0;
   end record;
   
   
   type DMAMFBOCR_Register is record
      -- Bit definition for Ethernet DMA
      -- Missed Frame and Buffer Overflow Counter Register
      Res0      : Reserved (29 .. 31) := (others => 0);
      OFOC      : Bits_1;
      -- Overflow bit for FIFO overflow counter
      --: Tripped, Off
      MFA       : Bits_11;
      -- Number of frames missed by the application
      OMFC      : Bits_1;
      -- Overflow bit for missed frame counter
      --: Tripped, Off
      MFC       : Bits_16;
      -- Number of frames missed by the controller
   end record;

   for DMAMFBOCR_Register use record
      Res0 at 0 range 29 .. 31;
      OFOC at 0 range 28 .. 28;
      MFA  at 0 range 17 .. 27;
      OMFC at 0 range 16 .. 16;
      MFC  at 0 range  0 .. 15;
   end record;
   
   
   type DMARSWTR_Register is record
      -- Bit definition for
      -- Ethernet DMA receive status watchdog timer register
      Res0      : Reserved (8 .. 31) := (others => 0);
      RSWTC     : Byte;
      -- Receive status (RS) watchdog timer count
   end record;

   for DMARSWTR_Register use record
      Res0  at 0 range 8 .. 31;
      RSWTC at 0 range 0 ..  7;
   end record;
   
   
   subtype DMACHTDR_Register is Bits_32;
   -- Ethernet DMA current host transmit descriptor register
   
   
   subtype DMACHRDR_Register is Bits_32;
   -- Ethernet DMA current host receive descriptor register
   
   
   subtype DMACHTBAR_Register is Bits_32;
   -- Ethernet DMA current host transmit buffer address register
   
   
   subtype DMACHRBAR_Register is Bits_32;
   -- Ethernet DMA current host receive buffer address register
   
   
   type RESERVED0_Register is array (0 .. 1) of aliased Bits_32x1;
   type RESERVED1_Register is array (0 .. 1) of aliased Bits_32x1;
   type RESERVED2_Register is array (0 .. 39) of aliased Bits_32x1;
   type RESERVED3_Register is array (0 .. 13) of aliased Bits_32x1;
   type RESERVED4_Register is array (0 .. 4) of aliased Bits_32x1;
   type RESERVED5_Register is array (0 .. 9) of aliased Bits_32x1;
   type RESERVED6_Register is array (0 .. 9) of aliased Bits_32x1;
   type RESERVED7_Register is array (0 .. 333) of aliased Bits_32x1;
   type RESERVED9_Register is array (0 .. 563) of aliased Bits_32x1;
   type RESERVED10_Register is array (0 .. 7) of aliased Bits_32x1;
   
   
   type Eth_Register is record
      MACCR             : MACCR_Register;
      MACFFR            : MACFFR_Register;
      MACHTHR           : MACHTHR_Register;
      MACHTLR           : MACHTLR_Register;
      MACMIIAR          : MACMIIAR_Register;
      MACMIIDR          : MACMIIDR_Register;
      MACFCR            : MACFCR_Register;
      MACVLANTR         : MACVLANTR_Register;
      RESERVED0         : RESERVED0_Register;
      MACRWUFFR         : MACRWUFFR_Register;
      MACPMTCSR         : MACPMTCSR_Register;
      RESERVED1         : Bits_32x1;
      MACDBGR           : MACDBGR_Register;
      MACSR             : MACSR_Register;
      Res0              : Bits_16;
      MACIMR            : MACIMR_Register;
      Res1              : Bits_16;
      MACA0HR           : MACA0HR_Register;
      MACA0LR           : MACA0LR_Register;
      MACA1HR           : MACA1HR_Register;
      MACA1LR           : MACA1LR_Register;
      MACA2HR           : MACA2HR_Register;
      MACA2LR           : MACA2LR_Register;
      MACA3HR           : MACA3HR_Register;
      MACA3LR           : MACA3LR_Register;
      RESERVED2         : RESERVED2_Register;
      MMCCR             : MMCCR_Register;
      MMCRIR            : MMCRIR_Register;
      MMCTIR            : MMCTIR_Register;
      MMCRIMR           : MMCRIMR_Register;
      MMCTIMR           : MMCTIMR_Register;
      RESERVED3         : RESERVED3_Register;
      MMCTGFSCCR        : MMCTGFSCCR_Register;
      MMCTGFMSCCR       : MMCTGFMSCCR_Register;
      RESERVED4         : RESERVED4_Register;
      MMCTGFCR          : MMCTGFCR_Register;
      RESERVED5         : RESERVED5_Register;
      MMCRFCECR         : MMCRFCECR_Register;
      MMCRFAECR         : MMCRFAECR_Register;
      RESERVED6         : RESERVED6_Register;
      MMCRGUFCR         : MMCRGUFCR_Register;
      RESERVED7         : RESERVED7_Register;
      PTPTSCR           : PTPTSCR_Register;
      PTPSSIR           : PTPSSIR_Register;
      PTPTSHR           : PTPTSHR_Register;
      PTPTSLR           : PTPTSLR_Register;
      PTPTSHUR          : PTPTSHUR_Register;
      PTPTSLUR          : PTPTSLUR_Register;
      PTPTSAR           : PTPTSAR_Register;
      PTPTTHR           : PTPTTHR_Register;
      PTPTTLR           : PTPTTLR_Register;
      RESERVED8         : Bits_32x1;
      PTPTSSR           : PTPTSSR_Register;
      PTPPPSCR          : PTPPPSCR_Register;
      RESERVED9         : RESERVED9_Register;
      DMABMR            : DMABMR_Register;
      DMATPDR           : DMATPDR_Register;
      DMARPDR           : DMARPDR_Register;
      DMARDLAR          : DMARDLAR_Register;
      DMATDLAR          : DMATDLAR_Register;
      DMASR             : DMASR_Register;
      DMAOMR            : DMAOMR_Register;
      DMAIER            : DMAIER_Register;
      DMAMFBOCR         : DMAMFBOCR_Register;
      DMARSWTR          : DMARSWTR_Register;
      RESERVED10        : RESERVED10_Register;
      DMACHTDR          : DMACHTDR_Register;
      DMACHRDR          : DMACHRDR_Register;
      DMACHTBAR         : DMACHTBAR_Register;
      DMACHRBAR         : DMACHRBAR_Register;
   end record;
   pragma Convention (C_Pass_By_Copy, Eth_Register);

   for Eth_Register use record
      MACCR       at    0 range 0 ..    31;
      MACFFR      at    4 range 0 ..    31;
      MACHTHR     at    8 range 0 ..    31;
      MACHTLR     at   12 range 0 ..    31;
      MACMIIAR    at   16 range 0 ..    31;
      MACMIIDR    at   20 range 0 ..    31;
      MACFCR      at   24 range 0 ..    31;
      MACVLANTR   at   28 range 0 ..    31;
      RESERVED0   at   32 range 0 ..    63;
      MACRWUFFR   at   40 range 0 ..    31;
      MACPMTCSR   at   44 range 0 ..    31;
      RESERVED1   at   48 range 0 ..    31;
      MACDBGR     at   52 range 0 ..    31;
      MACSR       at   56 range 0 ..    15;
      Res0        at   58 range 0 ..    15;
      MACIMR      at   60 range 0 ..    15;
      Res1        at   62 range 0 ..    15;
      MACA0HR     at   64 range 0 ..    31;
      MACA0LR     at   68 range 0 ..    31;
      MACA1HR     at   72 range 0 ..    31;
      MACA1LR     at   76 range 0 ..    31;
      MACA2HR     at   80 range 0 ..    31;
      MACA2LR     at   84 range 0 ..    31;
      MACA3HR     at   88 range 0 ..    31;
      MACA3LR     at   92 range 0 ..    31;
      RESERVED2   at   96 range 0 ..  1279;
      MMCCR       at  256 range 0 ..    31;
      MMCRIR      at  260 range 0 ..    31;
      MMCTIR      at  264 range 0 ..    31;
      MMCRIMR     at  268 range 0 ..    31;
      MMCTIMR     at  272 range 0 ..    31;
      RESERVED3   at  276 range 0 ..   447;
      MMCTGFSCCR  at  332 range 0 ..    31;
      MMCTGFMSCCR at  336 range 0 ..    31;
      RESERVED4   at  340 range 0 ..   159;
      MMCTGFCR    at  360 range 0 ..    31;
      RESERVED5   at  364 range 0 ..   319;
      MMCRFCECR   at  404 range 0 ..    31;
      MMCRFAECR   at  408 range 0 ..    31;
      RESERVED6   at  412 range 0 ..   319;
      MMCRGUFCR   at  452 range 0 ..    31;
      RESERVED7   at  456 range 0 .. 10687;
      PTPTSCR     at 1792 range 0 ..    31;
      PTPSSIR     at 1796 range 0 ..    31;
      PTPTSHR     at 1800 range 0 ..    31;
      PTPTSLR     at 1804 range 0 ..    31;
      PTPTSHUR    at 1808 range 0 ..    31;
      PTPTSLUR    at 1812 range 0 ..    31;
      PTPTSAR     at 1816 range 0 ..    31;
      PTPTTHR     at 1820 range 0 ..    31;
      PTPTTLR     at 1824 range 0 ..    31;
      RESERVED8   at 1828 range 0 ..    31;
      PTPTSSR     at 1832 range 0 ..    31;
      PTPPPSCR    at 1836 range 0 ..    31;
      RESERVED9   at 1840 range 0 .. 18047;
      DMABMR      at 4096 range 0 ..    31;
      DMATPDR     at 4100 range 0 ..    31;
      DMARPDR     at 4104 range 0 ..    31;
      DMARDLAR    at 4108 range 0 ..    31;
      DMATDLAR    at 4112 range 0 ..    31;
      DMASR       at 4116 range 0 ..    31;
      DMAOMR      at 4120 range 0 ..    31;
      DMAIER      at 4124 range 0 ..    31;
      DMAMFBOCR   at 4128 range 0 ..    31;
      DMARSWTR    at 4132 range 0 ..    31;
      RESERVED10  at 4136 range 0 ..   255;
      DMACHTDR    at 4168 range 0 ..    31;
      DMACHRDR    at 4172 range 0 ..    31;
      DMACHTBAR   at 4176 range 0 ..    31;
      DMACHRBAR   at 4180 range 0 ..    31;
   end record;

end STM32F4.o7xx.Eth;

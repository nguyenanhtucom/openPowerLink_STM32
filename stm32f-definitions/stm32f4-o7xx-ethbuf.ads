------------------------------------------------------------------------------
--                                                                          --
--                             STM32F4 COMPONENTS                           --
--                                                                          --
--                       S T M 32 F 4 . O7xx . E t h b u f                  --
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
--  chapter 33.6    ETH-DMA buffer registers       RM0090 Reference manual
--

pragma Restrictions (No_Elaboration_Code);

package STM32F4.o7xx.Ethbuf is

   --------------------------------------------------
   -- constants for use with the eth definitions   --
   --  in the client packages                      --
   --------------------------------------------------
   
   Dma                    : constant Bits_1 := 1;
   Me                     : constant Bits_1 := 0;
   Enable                 : constant Bits_1 := 1;
   Disable                : constant Bits_1 := 1;
   Off                    : constant Bits_1 := 0;
   Tru                    : constant Bits_1 := 1;
   None                   : constant Bits_2 := 0;
   Ck_head                : constant Bits_2 := 1;
   Ck_Head_Payload        : constant Bits_2 := 2;
   Ck_Head_Pseudo_Payload : constant Bits_2 := 3;
   Captured               : constant Bits_1 := 1;
   Tripped                : constant Bits_1 := 1;
   Vlan                   : constant Bits_1 := 1;
   Deferred               : constant Bits_1 := 1;
   Failed                 : constant Bits_1 := 1;
   Proto                  : constant Bits_1 := 1;
   V2                     : constant Bits_1 := 1;
   V1                     : constant Bits_1 := 0;
   Direct                 : constant Bits_1 := 1;
   Udp_T                  : constant Bits_1 := 0;
   Nix                    : constant Bits_4 := 0;
   Sync                   : constant Bits_4 := 1;
   Follow_Up              : constant Bits_4 := 2;
   Delay_Req              : constant Bits_4 := 3;
   Delay_Resp             : constant Bits_4 := 4;
   Pdelay_Req             : constant Bits_4 := 5;
   Announce               : constant Bits_4 := 5;
   Pdelay_Resp            : constant Bits_4 := 6;
   Management             : constant Bits_4 := 6;
   Pdelay_Resp_Follow_Up  : constant Bits_4 := 7;
   Signaling              : constant Bits_4 := 7;
   Unknown                : constant Bits_3 := 0;
   Udp                    : constant Bits_3 := 1;
   Tcp                    : constant Bits_3 := 2;
   Icmp                   : constant Bits_3 := 3;
   
   --------------------------------------------------
   -- descriptor definitions                       --
   --------------------------------------------------
   
   type Tdes0_Type is record
      -- Normal Tx DMA descriptor
      Own    : Bits_1;
      -- Own bit.
      -- When set, this bit indicates that the descriptor is owned by the DMA.
      --: Dma, me
      Ic     : Bits_1;
      -- Interrupt on completion
      --- Enable, Off 
      Ls     : Bits_1;
      --  Last segment
      --- Tru, Off
      Fs     : Bits_1;
      --  First segment
      --- Tru, Off
      Dc     : Bits_1;
      --  Disable CRC for 1st frame only
      --- Disable, Off
      Dp     : Bits_1;
      -- Disable pad, what on earth!!
      --- Disable, Off
      Ttse   : Bits_1;
      --  Transmit time stamp enable, Fs and Ls? must be set for this to work
      --- Enable, Off 
      Res0   : Reserved (24 .. 24) := (others => 0);
      Cic    : Bits_2;
      --  Checksum insertion control
      --- Ck_head, Ck_Head_Payload, Ck_Head_Pseudo_Payload
      Ter    : Bits_1;
      --  Transmit end of ring
      --- Tru, Off
      Thc    : Bits_1;
      --  Second address chained
      --- Tru, Off
      Res1   : Reserved (18 .. 19) := (others => 0);
      Ttss   : Bits_1;
      -- Transmit time stamp status
      --- Captured, Off
      Ihe    : Bits_1;
      -- IP header error
      --- Tripped, Off
      Es     : Bits_1;
      -- Error summary
      --- Tripped, Off
      Jt     : Bits_1;
      -- Jabber timeout
      --- Tripped, Off
      Ff     : Bits_1;
      --  Frame flushed
      --- Tripped, Off
      Ipe    : Bits_1;
      --  IP payload error
      --- Tripped, Off
      Lca    : Bits_1;
      -- Loss of carrier
      --- Tripped, Off
      Nc     : Bits_1;
      -- No carrier
      --- Tripped, Off
      Lco    : Bits_1;
      -- Late collision
      --- Tripped, Off
      Ec     : Bits_1;
      -- Excessive collision
      --- Tripped, Off
      Vf     : Bits_1;
      -- VLAN frame
      --- Vlan, off
      Cc     : Bits_4;
      -- Collision count
      --- number of collisions for this frame
      Ed     : Bits_1;
      --  Excessive deferral
      --- Tripped, Off
      Uf     : Bits_1;
      -- Underflow error
      --- Tripped, Off
      Db     : Bits_1;
      --  Deferred bit
      --- Deferred, Off
   end record;
   
   for Tdes0_Type use record
      Own    at 0 range 31 .. 31;
      Ic     at 0 range 30 .. 30;
      Ls     at 0 range 29 .. 29;
      Fs     at 0 range 28 .. 28;
      Dc     at 0 range 27 .. 27;
      Dp     at 0 range 26 .. 26;
      Ttse   at 0 range 25 .. 25;
      Res0   at 0 range 24 .. 24;
      Cic    at 0 range 22 .. 23;
      Ter    at 0 range 21 .. 21;
      Thc    at 0 range 20 .. 20;
      Res1   at 0 range 18 .. 19;
      Ttss   at 0 range 17 .. 17;
      Ihe    at 0 range 16 .. 16;
      Es     at 0 range 15 .. 15;
      Jt     at 0 range 14 .. 14;
      Ff     at 0 range 13 .. 13;
      Ipe    at 0 range 12 .. 12;
      Lca    at 0 range 11 .. 11;
      Nc     at 0 range 10 .. 10;
      Lco    at 0 range  9 ..  9;
      Ec     at 0 range  8 ..  8;
      Vf     at 0 range  7 ..  7;
      Cc     at 0 range  3 ..  6;
      Ed     at 0 range  2 ..  2;
      Uf     at 0 range  1 ..  1;
      Db     at 0 range  0 ..  0;
   end record;
   
   
   type Tdes1_Type is record
      Res0   : Reserved (29 .. 31) := (others => 0);
      Tbs2   : Bits_13;
      --  Transmit buffer 2 size
      --- size in bytes
      Res1   : Reserved (13 .. 15) := (others => 0);
      Tbs1   : Bits_13;
      -- Transmit buffer 1 size
      --- size in bytes, set to zero to ignore this buffer
   end record;
   
   for Tdes1_Type use record
      Res0   at 0 range 29 .. 31;
      Tbs2   at 0 range 16 .. 28;
      Res1   at 0 range 13 .. 15;
      Tbs1   at 0 range  0 .. 12;
   end record;
   
   
   type Tdes2_Type (Xl : Boolean := False) is record
      case Xl is
	 when False =>
	    Tbap1 : Bits_32;
	    --  Transmit buffer 1 address pointer
	 when True =>
	    Ttsl  : Bits_32;
	    -- 32 least significant bits of the transmitted time stamp 
      end case;
   end record with Unchecked_Union;
   
   for Tdes2_Type use record
      Tbap1 at 0 range  0 .. 31;
      Ttsl  at 0 range  0 .. 31;
   end record;

   
   type Tdes3_Type (Xl : Boolean := False) is record
      case Xl is
	 when False =>
	    Tbap2 : Bits_32;
	    -- Transmit buffer 2 address pointer or Next descriptor address. 
	 when True =>
	    Ttsh  : Bits_32;
	    -- 32 most significant bits of the time stamp
      end case;
   end record with Unchecked_Union;
   
   for Tdes3_Type use record
      Tbap2 at 0 range  0 .. 31;
      Ttsh  at 0 range  0 .. 31;
   end record;
   
   
   type Rdes0_Type is record
      -- Contains the received frame status, the frame length and 
      -- the descriptor ownership information.
      Own       : Bits_1;
      -- Own bit.
      -- When set, this bit indicates that the descriptor is owned by the DMA.
      --: Dma, me
      Afm       : Bits_1;
      --  Destination address filter fail
      --- Failed , Off
      Fl        : Bits_14;
      --  Frame length
      --- Bytes, transferred to memory
      Es        : Bits_1;
      -- Error summary
      --- Tripped, Off
      De        : Bits_1;
      -- Descriptor error
      --- Tripped, Off
      Saf       : Bits_1;
      --  Source address filter fail
      --- Failed , Off
      Le        : Bits_1;
      --  Length error
      --- Tripped, Off
      Oe        : Bits_1;
      -- Overflow error
      --- Tripped, Off
      Vf        : Bits_1;
      -- VLAN frame
      --- Vlan, off
      Fs        : Bits_1;
      -- First descriptor
      --- Tru, Off
      Ls        : Bits_1;
      -- Last descriptor
      --- Tru, Off
      Iphce_Tsv : Bits_1;
      --  IPv header checksum error / time stamp valid
      --- Tripped/Tru, Off
      Lco       : Bits_1;
      -- Late collision
      --- Tripped, Off
      Ft        : Bits_1;
      -- Frame type, Protocol > 0x0600
      --- Proto, Off
      Rwt       : Bits_1;
      -- Receive watchdog timeout
      --- Tripped, Off
      Re        : Bits_1;
      --  Receive error
      --- Tripped, Off
      Drbe      : Bits_1;
      -- Dribble bit error
      --- Tripped, Off
      Ce        : Bits_1;
      --  CRC error
      --- Tripped, Off
      Pce_Esa   : Bits_1;
      -- Tripped/Tru, Off
   end record;
   
   for Rdes0_Type use record
      Own       at 0 range 31 .. 31;
      Afm       at 0 range 30 .. 30;
      Fl        at 0 range 16 .. 29;
      Es        at 0 range 15 .. 15;
      De        at 0 range 14 .. 14;
      Saf       at 0 range 13 .. 13;
      Le        at 0 range 12 .. 12;
      Oe        at 0 range 11 .. 11;
      Vf        at 0 range 10 .. 10;
      Fs        at 0 range  9 ..  9;
      Ls        at 0 range  8 ..  8;
      Iphce_Tsv at 0 range  7 ..  7;
      Lco       at 0 range  6 ..  6;
      Ft        at 0 range  5 ..  5;
      Rwt       at 0 range  4 ..  4;
      Re        at 0 range  3 ..  3;
      Drbe      at 0 range  2 ..  2;
      Ce        at 0 range  1 ..  1;
      Pce_Esa   at 0 range  0 ..  0;
   end record;
   
   
   type Rdes1_Type is record
      -- Receive descriptor Word1
      Dic       : Bits_1;
      --  Disable interrupt on completion
      --- Disable, Off
      Res0      : Reserved (29 .. 30) := (others => 0);
      Rbs2      : Bits_13;
      -- Receive buffer 2 size, 
      --- size in bytes
      Rer       : Bits_1;
      -- Receive end of ring
      --- Tru, Off
      Rch       : Bits_1;
      -- Second address chained
      --- Tru, Off
      Res1      : Reserved (13 .. 13) := (others => 0);
      Rbs1      : Bits_13;
      --  Receive buffer 1 size
      --- size in bytes, set to zero to ignore this buffer
   end record;
   
   for Rdes1_Type use record
      Dic       at 0 range 31 .. 31;
      Res0      at 0 range 29 .. 30;
      Rbs2      at 0 range 16 .. 28;
      Rer       at 0 range 15 .. 15;
      Rch       at 0 range 14 .. 14;
      Res1      at 0 range 13 .. 13;
      Rbs1      at 0 range  0 .. 12;
   end record;
   
   
   type Rdes2_Type (Xl : Boolean := False) is record
      -- contains the address pointer to the first data buffer 
      -- in the descriptor, or it contains time stamp data.
      case Xl is
	 when False =>
	    Rbap1 : Bits_32;
	    -- Receive buffer 1 address pointer
	 when True =>
	    Rtsl  : Bits_32;
	    -- 32 least significant bits of the received time stamp 
      end case;
   end record with Unchecked_Union;
   
   for Rdes2_Type use record
      Rbap1 at 0 range  0 .. 31;
      Rtsl  at 0 range  0 .. 31;
   end record;
   
   
   type Rdes3_Type (Xl : Boolean := False) is record
      -- contains the address pointer either to the second data buffer in the 
      -- descriptor or to the next descriptor, or it contains time stamp data.
      case Xl is
	 when False =>
	    Rbap2 : Bits_32;
	    -- Receive buffer 2 address pointer or Next descriptor address. 
	 when True =>
	    Rtsh  : Bits_32;
	    -- 32 most significant bits of the received time stamp
      end case;
   end record with Unchecked_Union;
   
   for Rdes3_Type use record
      Rbap2 at 0 range  0 .. 31;
      Rtsh  at 0 range  0 .. 31;
   end record;
   
   
   type Rdes4_Type is record
      -- The extended status, shown below, is valid only when there is status 
      -- related to IPv4 checksum or 
      -- time stamp available as indicated by bit 0 in RDES0.
      Res0   : Reserved (14 .. 31) := (others => 0);
      Pv     : Bits_1;
      --  PTP version
      --- V2, V1
      Pft    : Bits_1;
      --  PTP frame type
      --- Direct, Udp_T
      Pmt    : Bits_4;
      --  PTP message type
      --- Nix,      Sync,        Follow_Up,  Delay_Req, Delay_Resp, Pdelay_Req, 
      --- Announce, Pdelay_Resp, Management, Pdelay_Resp_Follow_Up, Signaling
      Ipv6pr : Bits_1;
      --  IPv6 packet received
      --- Tru, Off
      Ipv4pr : Bits_1;
      --  IPv4 packet received
      --- Tru, Off
      Ipcb   : Bits_1;
      --  IP checksum bypassed
      --- Tru, Off
      Ippe   : Bits_1;
      --  IP payload error
      --- Tripped, Off
      Iphe   : Bits_1;
      --  IP header error
      --- Tripped, Off
      Ippt   : Bits_3;
      --  IP payload type
      --- Unknown, Udp, Tcp, Icmp
   end record;
   
   for Rdes4_Type use record
      Res0   at 0 range 14 .. 31;
      Pv     at 0 range 13 .. 13;
      Pft    at 0 range 12 .. 12;
      Pmt    at 0 range  8 .. 11;
      Ipv6pr at 0 range  7 ..  7;
      Ipv4pr at 0 range  6 ..  6;
      Ipcb   at 0 range  5 ..  5;
      Ippe   at 0 range  4 ..  4;
      Iphe   at 0 range  3 ..  3;
      Ippt   at 0 range  0 ..  2;
   end record;
   
   
   type Normal_Xmit_Desc_Type is record
      Tdes0 : Tdes0_Type;
      Tdes1 : Tdes1_Type;
      Tdes2 : Tdes2_Type (False);
      Tdes3 : Tdes3_Type (False);
   end record;
   
   for Normal_Xmit_Desc_Type use record
      Tdes0  at  0 range 0 .. 31;
      Tdes1  at  4 range 0 .. 31;
      Tdes2  at  8 range 0 .. 31;
      Tdes3  at 12 range 0 .. 31;
   end record;
   
   
   type Xl_Xmit_Desc_Type is record
      Tdes0     : Tdes0_Type;
      Tdes1     : Tdes1_Type;
      Tdes2     : Tdes2_Type (True); -- ?????
      Tdes3     : Tdes3_Type (True); -- ??????
      Reserved4 : Bits_32;
      Reserved5 : Bits_32;
      Ttsl      : Bits_32;
      -- 32 least significant bits of the time stamp.
      Ttsh       : Bits_32;
      -- 32 most significant bits of the time stamp.
   end record;
   
   for Xl_Xmit_Desc_Type use record
      Tdes0     at  0 range 0 .. 31;
      Tdes1     at  4 range 0 .. 31;
      Tdes2     at  8 range 0 .. 31;
      Tdes3     at 12 range 0 .. 31;
      Reserved4 at 16 range 0 .. 31;
      Reserved5 at 20 range 0 .. 31;
      Ttsl      at 24 range 0 .. 31;
      Ttsh      at 28 range 0 .. 31;
   end record;
   
   
   type Normal_Recv_Desc_Type is record
      Rdes0 : Rdes0_Type;
      Rdes1 : Rdes1_Type;
      Rdes2 : Rdes2_Type (False);
      Rdes3 : Rdes3_Type (False);
   end record;
     
   for Normal_Recv_Desc_Type use record
      Rdes0  at  0 range 0 .. 31;
      Rdes1  at  4 range 0 .. 31;
      Rdes2  at  8 range 0 .. 31;
      Rdes3  at 12 range 0 .. 31;
   end record;
   
   
   type Xl_Recv_Desc_Type is record
      Rdes0     : Rdes0_Type;
      Rdes1     : Rdes1_Type;
      Rdes2     : Rdes2_Type (False); -- ???????
      Rdes3     : Rdes3_Type (False); -- ???????
      Rdes4     : Rdes4_Type;
      Reserved5 : Bits_32;
      Rtsl      : Bits_32;
      -- 32 least significant bits of the time stamp.
      Rtsh      : Bits_32;
      -- 32 most significant bits of the time stamp.
   end record;
   
   for Xl_Recv_Desc_Type use record
      Rdes0     at  0 range 0 .. 31;
      Rdes1     at  4 range 0 .. 31;
      Rdes2     at  8 range 0 .. 31;
      Rdes3     at 12 range 0 .. 31;
      Rdes4     at 16 range 0 .. 31;
      Reserved5 at 20 range 0 .. 31;
      Rtsl      at 24 range 0 .. 31;
      Rtsh      at 28 range 0 .. 31;
   end record;
   
end STM32F4.o7xx.Ethbuf;

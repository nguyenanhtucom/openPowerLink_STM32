pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;

package eth_h is

   --  unsupported macro: ETH_MACCR_WD
   --  unsupported macro: ETH_MACCR_JD
   --  unsupported macro: ETH_MACCR_IFG
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_CSD
   --  unsupported macro: ETH_MACCR_FES
   --  unsupported macro: ETH_MACCR_ROD
   --  unsupported macro: ETH_MACCR_LM
   --  unsupported macro: ETH_MACCR_DM
   --  unsupported macro: ETH_MACCR_IPCO
   --  unsupported macro: ETH_MACCR_RD
   --  unsupported macro: ETH_MACCR_APCS
   --  unsupported macro: ETH_MACCR_BL
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_
   --  unsupported macro: ETH_MACCR_DC
   --  unsupported macro: ETH_MACCR_TE
   --  unsupported macro: ETH_MACCR_RE
   --  unsupported macro: ETH_MACFFR_RA
   --  unsupported macro: ETH_MACFFR_HPF
   --  unsupported macro: ETH_MACFFR_SAF
   --  unsupported macro: ETH_MACFFR_SAIF
   --  unsupported macro: ETH_MACFFR_PCF
   --  unsupported macro: ETH_MACFFR_
   --  unsupported macro: ETH_MACFFR_
   --  unsupported macro: ETH_MACFFR_
   --  unsupported macro: ETH_MACFFR_BFD
   --  unsupported macro: ETH_MACFFR_PAM
   --  unsupported macro: ETH_MACFFR_DAIF
   --  unsupported macro: ETH_MACFFR_HM
   --  unsupported macro: ETH_MACFFR_HU
   --  unsupported macro: ETH_MACFFR_PM
   --  unsupported macro: ETH_MACHTHR_HTH
   --  unsupported macro: ETH_MACHTLR_HTL
   --  unsupported macro: ETH_MACMIIAR_PA
   --  unsupported macro: ETH_MACMIIAR_MR
   --  unsupported macro: ETH_MACMIIAR_CR
   --  unsupported macro: ETH_MACMIIAR_
   --  unsupported macro: ETH_MACMIIAR_
   --  unsupported macro: ETH_MACMIIAR_
   --  unsupported macro: ETH_MACMIIAR_
   --  unsupported macro: ETH_MACMIIAR_
   --  unsupported macro: ETH_MACMIIAR_MW
   --  unsupported macro: ETH_MACMIIAR_MB
   --  unsupported macro: ETH_MACMIIDR_MD
   --  unsupported macro: ETH_MACFCR_PT
   --  unsupported macro: ETH_MACFCR_ZQPD
   --  unsupported macro: ETH_MACFCR_PLT
   --  unsupported macro: ETH_MACFCR_
   --  unsupported macro: ETH_MACFCR_
   --  unsupported macro: ETH_MACFCR_
   --  unsupported macro: ETH_MACFCR_
   --  unsupported macro: ETH_MACFCR_UPFD
   --  unsupported macro: ETH_MACFCR_RFCE
   --  unsupported macro: ETH_MACFCR_TFCE
   --  unsupported macro: ETH_MACFCR_FCBBPA
   --  unsupported macro: ETH_MACVLANTR_VLANTC
   --  unsupported macro: ETH_MACVLANTR_VLANTI
   --  unsupported macro: ETH_MACRWUFFR_D
   --  unsupported macro: ETH_MACPMTCSR_WFFRPR
   --  unsupported macro: ETH_MACPMTCSR_GU
   --  unsupported macro: ETH_MACPMTCSR_WFR
   --  unsupported macro: ETH_MACPMTCSR_MPR
   --  unsupported macro: ETH_MACPMTCSR_WFE
   --  unsupported macro: ETH_MACPMTCSR_MPE
   --  unsupported macro: ETH_MACPMTCSR_PD
   --  unsupported macro: ETH_MACSR_TSTS
   --  unsupported macro: ETH_MACSR_MMCTS
   --  unsupported macro: ETH_MACSR_MMMCRS
   --  unsupported macro: ETH_MACSR_MMCS
   --  unsupported macro: ETH_MACSR_PMTS
   --  unsupported macro: ETH_MACIMR_TSTIM
   --  unsupported macro: ETH_MACIMR_PMTIM
   --  unsupported macro: ETH_MACA0HR_MACA0H
   --  unsupported macro: ETH_MACA0LR_MACA0L
   --  unsupported macro: ETH_MACA1HR_AE
   --  unsupported macro: ETH_MACA1HR_SA
   --  unsupported macro: ETH_MACA1HR_MBC
   --  unsupported macro: ETH_MACA1HR_MBC_HBits15_8
   --  unsupported macro: ETH_MACA1HR_MBC_HBits7_0
   --  unsupported macro: ETH_MACA1HR_MBC_LBits31_24
   --  unsupported macro: ETH_MACA1HR_MBC_LBits23_16
   --  unsupported macro: ETH_MACA1HR_MBC_LBits15_8
   --  unsupported macro: ETH_MACA1HR_MBC_LBits7_0
   --  unsupported macro: ETH_MACA1HR_MACA1H
   --  unsupported macro: ETH_MACA1LR_MACA1L
   --  unsupported macro: ETH_MACA2HR_AE
   --  unsupported macro: ETH_MACA2HR_SA
   --  unsupported macro: ETH_MACA2HR_MBC
   --  unsupported macro: ETH_MACA2HR_MBC_HBits15_8
   --  unsupported macro: ETH_MACA2HR_MBC_HBits7_0
   --  unsupported macro: ETH_MACA2HR_MBC_LBits31_24
   --  unsupported macro: ETH_MACA2HR_MBC_LBits23_16
   --  unsupported macro: ETH_MACA2HR_MBC_LBits15_8
   --  unsupported macro: ETH_MACA2HR_MBC_LBits7_0
   --  unsupported macro: ETH_MACA2HR_MACA2H
   --  unsupported macro: ETH_MACA2LR_MACA2L
   --  unsupported macro: ETH_MACA3HR_AE
   --  unsupported macro: ETH_MACA3HR_SA
   --  unsupported macro: ETH_MACA3HR_MBC
   --  unsupported macro: ETH_MACA3HR_MBC_HBits15_8
   --  unsupported macro: ETH_MACA3HR_MBC_HBits7_0
   --  unsupported macro: ETH_MACA3HR_MBC_LBits31_24
   --  unsupported macro: ETH_MACA3HR_MBC_LBits23_16
   --  unsupported macro: ETH_MACA3HR_MBC_LBits15_8
   --  unsupported macro: ETH_MACA3HR_MBC_LBits7_0
   --  unsupported macro: ETH_MACA3HR_MACA3H
   --  unsupported macro: ETH_MACA3LR_MACA3L
   --  unsupported macro: ETH_MMCCR_MCFHP
   --  unsupported macro: ETH_MMCCR_MCP
   --  unsupported macro: ETH_MMCCR_MCF
   --  unsupported macro: ETH_MMCCR_ROR
   --  unsupported macro: ETH_MMCCR_CSR
   --  unsupported macro: ETH_MMCCR_CR
   --  unsupported macro: ETH_MMCRIR_RGUFS
   --  unsupported macro: ETH_MMCRIR_RFAES
   --  unsupported macro: ETH_MMCRIR_RFCES
   --  unsupported macro: ETH_MMCTIR_TGFS
   --  unsupported macro: ETH_MMCTIR_TGFMSCS
   --  unsupported macro: ETH_MMCTIR_TGFSCS
   --  unsupported macro: ETH_MMCRIMR_RGUFM
   --  unsupported macro: ETH_MMCRIMR_RFAEM
   --  unsupported macro: ETH_MMCRIMR_RFCEM
   --  unsupported macro: ETH_MMCTIMR_TGFM
   --  unsupported macro: ETH_MMCTIMR_TGFMSCM
   --  unsupported macro: ETH_MMCTIMR_TGFSCM
   --  unsupported macro: ETH_MMCTGFSCCR_TGFSCC
   --  unsupported macro: ETH_MMCTGFMSCCR_TGFMSCC
   --  unsupported macro: ETH_MMCTGFCR_TGFC
   --  unsupported macro: ETH_MMCRFCECR_RFCEC
   --  unsupported macro: ETH_MMCRFAECR_RFAEC
   --  unsupported macro: ETH_MMCRGUFCR_RGUFC
   --  unsupported macro: ETH_PTPTSCR_TSCNT
   --  unsupported macro: ETH_PTPTSSR_TSSMRME
   --  unsupported macro: ETH_PTPTSSR_TSSEME
   --  unsupported macro: ETH_PTPTSSR_TSSIPV4FE
   --  unsupported macro: ETH_PTPTSSR_TSSIPV6FE
   --  unsupported macro: ETH_PTPTSSR_TSSPTPOEFE
   --  unsupported macro: ETH_PTPTSSR_TSPTPPSV2E
   --  unsupported macro: ETH_PTPTSSR_TSSSR
   --  unsupported macro: ETH_PTPTSSR_TSSARFE
   --  unsupported macro: ETH_PTPTSCR_TSARU
   --  unsupported macro: ETH_PTPTSCR_TSITE
   --  unsupported macro: ETH_PTPTSCR_TSSTU
   --  unsupported macro: ETH_PTPTSCR_TSSTI
   --  unsupported macro: ETH_PTPTSCR_TSFCU
   --  unsupported macro: ETH_PTPTSCR_TSE
   --  unsupported macro: ETH_PTPSSIR_STSSI
   --  unsupported macro: ETH_PTPTSHR_STS
   --  unsupported macro: ETH_PTPTSLR_STPNS
   --  unsupported macro: ETH_PTPTSLR_STSS
   --  unsupported macro: ETH_PTPTSHUR_TSUS
   --  unsupported macro: ETH_PTPTSLUR_TSUPNS
   --  unsupported macro: ETH_PTPTSLUR_TSUSS
   --  unsupported macro: ETH_PTPTSAR_TSA
   --  unsupported macro: ETH_PTPTTHR_TTSH
   --  unsupported macro: ETH_PTPTTLR_TTSL
   --  unsupported macro: ETH_PTPTSSR_TSTTR
   --  unsupported macro: ETH_PTPTSSR_TSSO
   --  unsupported macro: ETH_DMABMR_AAB
   --  unsupported macro: ETH_DMABMR_FPM
   --  unsupported macro: ETH_DMABMR_USP
   --  unsupported macro: ETH_DMABMR_RDP
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_RDP_4xPBL_4Beat
   --  unsupported macro: ETH_DMABMR_RDP_4xPBL_8Beat
   --  unsupported macro: ETH_DMABMR_RDP_4xPBL_16Beat
   --  unsupported macro: ETH_DMABMR_RDP_4xPBL_32Beat
   --  unsupported macro: ETH_DMABMR_RDP_4xPBL_64Beat
   --  unsupported macro: ETH_DMABMR_RDP_4xPBL_128Beat
   --  unsupported macro: ETH_DMABMR_FB
   --  unsupported macro: ETH_DMABMR_RTPR
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_
   --  unsupported macro: ETH_DMABMR_PBL
   --  unsupported macro: ETH_DMABMR_PBL_1Beat
   --  unsupported macro: ETH_DMABMR_PBL_2Beat
   --  unsupported macro: ETH_DMABMR_PBL_4Beat
   --  unsupported macro: ETH_DMABMR_PBL_8Beat
   --  unsupported macro: ETH_DMABMR_PBL_16Beat
   --  unsupported macro: ETH_DMABMR_PBL_32Beat
   --  unsupported macro: ETH_DMABMR_PBL_4xPBL_4Beat
   --  unsupported macro: ETH_DMABMR_PBL_4xPBL_8Beat
   --  unsupported macro: ETH_DMABMR_PBL_4xPBL_16Beat
   --  unsupported macro: ETH_DMABMR_PBL_4xPBL_32Beat
   --  unsupported macro: ETH_DMABMR_PBL_4xPBL_64Beat
   --  unsupported macro: ETH_DMABMR_PBL_4xPBL_128Beat
   --  unsupported macro: ETH_DMABMR_EDE
   --  unsupported macro: ETH_DMABMR_DSL
   --  unsupported macro: ETH_DMABMR_DA
   --  unsupported macro: ETH_DMABMR_SR
   --  unsupported macro: ETH_DMATPDR_TPD
   --  unsupported macro: ETH_DMARPDR_RPD
   --  unsupported macro: ETH_DMARDLAR_SRL
   --  unsupported macro: ETH_DMATDLAR_STL
   --  unsupported macro: ETH_DMASR_TSTS
   --  unsupported macro: ETH_DMASR_PMTS
   --  unsupported macro: ETH_DMASR_MMCS
   --  unsupported macro: ETH_DMASR_EBS
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_TPS
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_RPS
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_
   --  unsupported macro: ETH_DMASR_NIS
   --  unsupported macro: ETH_DMASR_AIS
   --  unsupported macro: ETH_DMASR_ERS
   --  unsupported macro: ETH_DMASR_FBES
   --  unsupported macro: ETH_DMASR_ETS
   --  unsupported macro: ETH_DMASR_RWTS
   --  unsupported macro: ETH_DMASR_RPSS
   --  unsupported macro: ETH_DMASR_RBUS
   --  unsupported macro: ETH_DMASR_RS
   --  unsupported macro: ETH_DMASR_TUS
   --  unsupported macro: ETH_DMASR_ROS
   --  unsupported macro: ETH_DMASR_TJTS
   --  unsupported macro: ETH_DMASR_TBUS
   --  unsupported macro: ETH_DMASR_TPSS
   --  unsupported macro: ETH_DMASR_TS
   --  unsupported macro: ETH_DMAOMR_DTCEFD
   --  unsupported macro: ETH_DMAOMR_RSF
   --  unsupported macro: ETH_DMAOMR_DFRF
   --  unsupported macro: ETH_DMAOMR_TSF
   --  unsupported macro: ETH_DMAOMR_FTF
   --  unsupported macro: ETH_DMAOMR_TTC
   --  unsupported macro: ETH_DMAOMR_TTC_64Bytes
   --  unsupported macro: ETH_DMAOMR_TTC_128Bytes
   --  unsupported macro: ETH_DMAOMR_TTC_192Bytes
   --  unsupported macro: ETH_DMAOMR_TTC_256Bytes
   --  unsupported macro: ETH_DMAOMR_TTC_40Bytes
   --  unsupported macro: ETH_DMAOMR_TTC_32Bytes
   --  unsupported macro: ETH_DMAOMR_TTC_24Bytes
   --  unsupported macro: ETH_DMAOMR_TTC_16Bytes
   --  unsupported macro: ETH_DMAOMR_ST
   --  unsupported macro: ETH_DMAOMR_FEF
   --  unsupported macro: ETH_DMAOMR_FUGF
   --  unsupported macro: ETH_DMAOMR_RTC
   --  unsupported macro: ETH_DMAOMR_RTC_64Bytes
   --  unsupported macro: ETH_DMAOMR_RTC_32Bytes
   --  unsupported macro: ETH_DMAOMR_RTC_96Bytes
   --  unsupported macro: ETH_DMAOMR_RTC_128Bytes
   --  unsupported macro: ETH_DMAOMR_OSF
   --  unsupported macro: ETH_DMAOMR_SR
   --  unsupported macro: ETH_DMAIER_NISE
   --  unsupported macro: ETH_DMAIER_AISE
   --  unsupported macro: ETH_DMAIER_ERIE
   --  unsupported macro: ETH_DMAIER_FBEIE
   --  unsupported macro: ETH_DMAIER_ETIE
   --  unsupported macro: ETH_DMAIER_RWTIE
   --  unsupported macro: ETH_DMAIER_RPSIE
   --  unsupported macro: ETH_DMAIER_RBUIE
   --  unsupported macro: ETH_DMAIER_RIE
   --  unsupported macro: ETH_DMAIER_TUIE
   --  unsupported macro: ETH_DMAIER_ROIE
   --  unsupported macro: ETH_DMAIER_TJTIE
   --  unsupported macro: ETH_DMAIER_TBUIE
   --  unsupported macro: ETH_DMAIER_TPSIE
   --  unsupported macro: ETH_DMAIER_TIE
   --  unsupported macro: ETH_DMAMFBOCR_OFOC
   --  unsupported macro: ETH_DMAMFBOCR_MFA
   --  unsupported macro: ETH_DMAMFBOCR_OMFC
   --  unsupported macro: ETH_DMAMFBOCR_MFC
   --  unsupported macro: ETH_DMACHTDR_HTDAP
   --  unsupported macro: ETH_DMACHRDR_HRDAP
   --  unsupported macro: ETH_DMACHTBAR_HTBAP
   --  unsupported macro: ETH_DMACHRBAR_HRBAP
  --****************************************************************************
   --
   --                Ethernet MAC Registers bits definitions
   --
  --****************************************************************************
   -- Bit definition for Ethernet MAC Control Register register
   -- Bit definition for Ethernet MAC Frame Filter Register
   -- Bit definition for Ethernet MAC Hash Table High Register
   -- Bit definition for Ethernet MAC Hash Table Low Register
   -- Bit definition for Ethernet MAC MII Address Register
   -- Bit definition for Ethernet MAC MII Data Register
   -- Bit definition for Ethernet MAC Flow Control Register
   -- Bit definition for Ethernet MAC VLAN Tag Register
   -- Bit definition for Ethernet MAC Remote Wake-UpFrame Filter Register
   -- Eight sequential Writes to this address (offset 0x28) will write all Wake-UpFrame Filter Registers.
   --   Eight sequential Reads from this address (offset 0x28) will read all Wake-UpFrame Filter Registers.

   -- Wake-UpFrame Filter Reg0 : Filter 0 Byte Mask
   --   Wake-UpFrame Filter Reg1 : Filter 1 Byte Mask
   --   Wake-UpFrame Filter Reg2 : Filter 2 Byte Mask
   --   Wake-UpFrame Filter Reg3 : Filter 3 Byte Mask
   --   Wake-UpFrame Filter Reg4 : RSVD - Filter3 Command - RSVD - Filter2 Command -
--                              RSVD - Filter1 Command - RSVD - Filter0 Command
   --   Wake-UpFrame Filter Re5 : Filter3 Offset - Filter2 Offset - Filter1 Offset - Filter0 Offset
   --   Wake-UpFrame Filter Re6 : Filter1 CRC16 - Filter0 CRC16
   --   Wake-UpFrame Filter Re7 : Filter3 CRC16 - Filter2 CRC16

   -- Bit definition for Ethernet MAC PMT Control and Status Register
   -- Bit definition for Ethernet MAC Status Register
   -- Bit definition for Ethernet MAC Interrupt Mask Register
   -- Bit definition for Ethernet MAC Address0 High Register
   -- Bit definition for Ethernet MAC Address0 Low Register
   -- Bit definition for Ethernet MAC Address1 High Register
   -- Bit definition for Ethernet MAC Address1 Low Register
   -- Bit definition for Ethernet MAC Address2 High Register
   -- Bit definition for Ethernet MAC Address2 Low Register
   -- Bit definition for Ethernet MAC Address3 High Register
   -- Bit definition for Ethernet MAC Address3 Low Register
  --****************************************************************************
   --                Ethernet MMC Registers bits definition
  --****************************************************************************
   -- Bit definition for Ethernet MMC Contol Register
   -- Bit definition for Ethernet MMC Receive Interrupt Register
   -- Bit definition for Ethernet MMC Transmit Interrupt Register
   -- Bit definition for Ethernet MMC Receive Interrupt Mask Register
   -- Bit definition for Ethernet MMC Transmit Interrupt Mask Register
   -- Bit definition for Ethernet MMC Transmitted Good Frames after Single Collision Counter Register
   -- Bit definition for Ethernet MMC Transmitted Good Frames after More than a Single Collision Counter Register
   -- Bit definition for Ethernet MMC Transmitted Good Frames Counter Register
   -- Bit definition for Ethernet MMC Received Frames with CRC Error Counter Register
   -- Bit definition for Ethernet MMC Received Frames with Alignement Error Counter Register
   -- Bit definition for Ethernet MMC Received Good Unicast Frames Counter Register
  --****************************************************************************
   --               Ethernet PTP Registers bits definition
  --****************************************************************************
   -- Bit definition for Ethernet PTP Time Stamp Contol Register
   -- Bit definition for Ethernet PTP Sub-Second Increment Register
   -- Bit definition for Ethernet PTP Time Stamp High Register
   -- Bit definition for Ethernet PTP Time Stamp Low Register
   -- Bit definition for Ethernet PTP Time Stamp High Update Register
   -- Bit definition for Ethernet PTP Time Stamp Low Update Register
   -- Bit definition for Ethernet PTP Time Stamp Addend Register
   -- Bit definition for Ethernet PTP Target Time High Register
   -- Bit definition for Ethernet PTP Target Time Low Register
   -- Bit definition for Ethernet PTP Time Stamp Status Register
  --****************************************************************************
   --                 Ethernet DMA Registers bits definition
  --****************************************************************************
   -- Bit definition for Ethernet DMA Bus Mode Register
   -- Bit definition for Ethernet DMA Transmit Poll Demand Register
   -- Bit definition for Ethernet DMA Receive Poll Demand Register
   -- Bit definition for Ethernet DMA Receive Descriptor List Address Register
   -- Bit definition for Ethernet DMA Transmit Descriptor List Address Register
   -- Bit definition for Ethernet DMA Status Register
   -- combination with EBS[2:0] for GetFlagStatus function
   -- Bit definition for Ethernet DMA Operation Mode Register
   -- Bit definition for Ethernet DMA Interrupt Enable Register
   -- Bit definition for Ethernet DMA Missed Frame and Buffer Overflow Counter Register
   -- Bit definition for Ethernet DMA Current Host Transmit Descriptor Register
   -- Bit definition for Ethernet DMA Current Host Receive Descriptor Register
   -- Bit definition for Ethernet DMA Current Host Transmit Buffer Address Register
   -- Bit definition for Ethernet DMA Current Host Receive Buffer Address Register
  --*
   --  * @brief Ethernet MAC
   --

   type ETH_TypeDef_RESERVED0_array is array (0 .. 1) of aliased unsigned;
   type ETH_TypeDef_RESERVED1_array is array (0 .. 1) of aliased unsigned;
   type ETH_TypeDef_RESERVED2_array is array (0 .. 39) of aliased unsigned;
   type ETH_TypeDef_RESERVED3_array is array (0 .. 13) of aliased unsigned;
   type ETH_TypeDef_RESERVED4_array is array (0 .. 4) of aliased unsigned;
   type ETH_TypeDef_RESERVED5_array is array (0 .. 9) of aliased unsigned;
   type ETH_TypeDef_RESERVED6_array is array (0 .. 9) of aliased unsigned;
   type ETH_TypeDef_RESERVED7_array is array (0 .. 333) of aliased unsigned;
   type ETH_TypeDef_RESERVED9_array is array (0 .. 564) of aliased unsigned;
   type ETH_TypeDef_RESERVED10_array is array (0 .. 7) of aliased unsigned;
   type ETH_TypeDefr is record
      MACCR       : aliased unsigned;  -- eth.h:446
      MACFFR      : aliased unsigned;  -- eth.h:447
      MACHTHR     : aliased unsigned;  -- eth.h:448
      MACHTLR     : aliased unsigned;  -- eth.h:449
      MACMIIAR    : aliased unsigned;  -- eth.h:450
      MACMIIDR    : aliased unsigned;  -- eth.h:451
      MACFCR      : aliased unsigned;  -- eth.h:452
      MACVLANTR   : aliased unsigned;  -- eth.h:453
      RESERVED0   : aliased ETH_TypeDef_RESERVED0_array;  -- eth.h:454
      MACRWUFFR   : aliased unsigned;  -- eth.h:455
      MACPMTCSR   : aliased unsigned;  -- eth.h:456
      RESERVED1   : aliased ETH_TypeDef_RESERVED1_array;  -- eth.h:457
      MACSR       : aliased unsigned;  -- eth.h:458
      MACIMR      : aliased unsigned;  -- eth.h:459
      MACA0HR     : aliased unsigned;  -- eth.h:460
      MACA0LR     : aliased unsigned;  -- eth.h:461
      MACA1HR     : aliased unsigned;  -- eth.h:462
      MACA1LR     : aliased unsigned;  -- eth.h:463
      MACA2HR     : aliased unsigned;  -- eth.h:464
      MACA2LR     : aliased unsigned;  -- eth.h:465
      MACA3HR     : aliased unsigned;  -- eth.h:466
      MACA3LR     : aliased unsigned;  -- eth.h:467
      RESERVED2   : aliased ETH_TypeDef_RESERVED2_array;  -- eth.h:468
      MMCCR       : aliased unsigned;  -- eth.h:469
      MMCRIR      : aliased unsigned;  -- eth.h:470
      MMCTIR      : aliased unsigned;  -- eth.h:471
      MMCRIMR     : aliased unsigned;  -- eth.h:472
      MMCTIMR     : aliased unsigned;  -- eth.h:473
      RESERVED3   : aliased ETH_TypeDef_RESERVED3_array;  -- eth.h:474
      MMCTGFSCCR  : aliased unsigned;  -- eth.h:475
      MMCTGFMSCCR : aliased unsigned;  -- eth.h:476
      RESERVED4   : aliased ETH_TypeDef_RESERVED4_array;  -- eth.h:477
      MMCTGFCR    : aliased unsigned;  -- eth.h:478
      RESERVED5   : aliased ETH_TypeDef_RESERVED5_array;  -- eth.h:479
      MMCRFCECR   : aliased unsigned;  -- eth.h:480
      MMCRFAECR   : aliased unsigned;  -- eth.h:481
      RESERVED6   : aliased ETH_TypeDef_RESERVED6_array;  -- eth.h:482
      MMCRGUFCR   : aliased unsigned;  -- eth.h:483
      RESERVED7   : aliased ETH_TypeDef_RESERVED7_array;  -- eth.h:484
      PTPTSCR     : aliased unsigned;  -- eth.h:485
      PTPSSIR     : aliased unsigned;  -- eth.h:486
      PTPTSHR     : aliased unsigned;  -- eth.h:487
      PTPTSLR     : aliased unsigned;  -- eth.h:488
      PTPTSHUR    : aliased unsigned;  -- eth.h:489
      PTPTSLUR    : aliased unsigned;  -- eth.h:490
      PTPTSAR     : aliased unsigned;  -- eth.h:491
      PTPTTHR     : aliased unsigned;  -- eth.h:492
      PTPTTLR     : aliased unsigned;  -- eth.h:493
      RESERVED8   : aliased unsigned;  -- eth.h:494
      PTPTSSR     : aliased unsigned;  -- eth.h:495
      RESERVED9   : aliased ETH_TypeDef_RESERVED9_array;  -- eth.h:496
      DMABMR      : aliased unsigned;  -- eth.h:497
      DMATPDR     : aliased unsigned;  -- eth.h:498
      DMARPDR     : aliased unsigned;  -- eth.h:499
      DMARDLAR    : aliased unsigned;  -- eth.h:500
      DMATDLAR    : aliased unsigned;  -- eth.h:501
      DMASR       : aliased unsigned;  -- eth.h:502
      DMAOMR      : aliased unsigned;  -- eth.h:503
      DMAIER      : aliased unsigned;  -- eth.h:504
      DMAMFBOCR   : aliased unsigned;  -- eth.h:505
      DMARSWTR    : aliased unsigned;  -- eth.h:506
      RESERVED10  : aliased ETH_TypeDef_RESERVED10_array;  -- eth.h:507
      DMACHTDR    : aliased unsigned;  -- eth.h:508
      DMACHRDR    : aliased unsigned;  -- eth.h:509
      DMACHTBAR   : aliased unsigned;  -- eth.h:510
      DMACHRBAR   : aliased unsigned;  -- eth.h:511
   end record;
   pragma Convention (C_Pass_By_Copy, ETH_TypeDefr);  -- eth.h:512

   --  skipped anonymous struct anon_0

   --    8
   --   11
   --   15
   --   24
   --   65
   --   69
   --   84

   type ETH_TypeDef is record
      MACCR_Register       : aliased unsigned;  -- eth.h:446
      MACFFR_Register      : aliased unsigned;  -- eth.h:447
      MACHTHR_Register     : aliased unsigned;  -- eth.h:448
      MACHTLR_Register     : aliased unsigned;  -- eth.h:449
      MACMIIAR_Register    : aliased unsigned;  -- eth.h:450
      MACMIIDR_Register    : aliased unsigned;  -- eth.h:451
      MACFCR_Register      : aliased unsigned;  -- eth.h:452
      MACVLANTR_Register   : aliased unsigned;  -- eth.h:453
      RESERVED0_Register   : aliased ETH_TypeDef_RESERVED0_array;  -- eth.h:454
      MACRWUFFR_Register   : aliased unsigned;  -- eth.h:455
      MACPMTCSR_Register   : aliased unsigned;  -- eth.h:456
      RESERVED1_Register   : aliased ETH_TypeDef_RESERVED1_array;  -- eth.h:457
      MACSR_Register       : aliased unsigned;  -- eth.h:458
      MACIMR_Register      : aliased unsigned;  -- eth.h:459
      MACA0HR_Register     : aliased unsigned;  -- eth.h:460
      MACA0LR_Register     : aliased unsigned;  -- eth.h:461
      MACA1HR_Register     : aliased unsigned;  -- eth.h:462
      MACA1LR_Register     : aliased unsigned;  -- eth.h:463
      MACA2HR_Register     : aliased unsigned;  -- eth.h:464
      MACA2LR_Register     : aliased unsigned;  -- eth.h:465
      MACA3HR_Register     : aliased unsigned;  -- eth.h:466
      MACA3LR_Register     : aliased unsigned;  -- eth.h:467
      RESERVED2_Register   : aliased ETH_TypeDef_RESERVED2_array;  -- eth.h:468
      MMCCR_Register       : aliased unsigned;  -- eth.h:469
      MMCRIR_Register      : aliased unsigned;  -- eth.h:470
      MMCTIR_Register      : aliased unsigned;  -- eth.h:471
      MMCRIMR_Register     : aliased unsigned;  -- eth.h:472
      MMCTIMR_Register     : aliased unsigned;  -- eth.h:473
      RESERVED3_Register   : aliased ETH_TypeDef_RESERVED3_array;  -- eth.h:474
      MMCTGFSCCR_Register  : aliased unsigned;  -- eth.h:475
      MMCTGFMSCCR_Register : aliased unsigned;  -- eth.h:476
      RESERVED4_Register   : aliased ETH_TypeDef_RESERVED4_array;  -- eth.h:477
      MMCTGFCR_Register    : aliased unsigned;  -- eth.h:478
      RESERVED5_Register   : aliased ETH_TypeDef_RESERVED5_array;  -- eth.h:479
      MMCRFCECR_Register   : aliased unsigned;  -- eth.h:480
      MMCRFAECR_Register   : aliased unsigned;  -- eth.h:481
      RESERVED6_Register   : aliased ETH_TypeDef_RESERVED6_array;  -- eth.h:482
      MMCRGUFCR_Register   : aliased unsigned;  -- eth.h:483
      RESERVED7_Register   : aliased ETH_TypeDef_RESERVED7_array;  -- eth.h:484
      PTPTSCR_Register     : aliased unsigned;  -- eth.h:485
      PTPSSIR_Register     : aliased unsigned;  -- eth.h:486
      PTPTSHR_Register     : aliased unsigned;  -- eth.h:487
      PTPTSLR_Register     : aliased unsigned;  -- eth.h:488
      PTPTSHUR_Register    : aliased unsigned;  -- eth.h:489
      PTPTSLUR_Register    : aliased unsigned;  -- eth.h:490
      PTPTSAR_Register     : aliased unsigned;  -- eth.h:491
      PTPTTHR_Register     : aliased unsigned;  -- eth.h:492
      PTPTTLR_Register     : aliased unsigned;  -- eth.h:493
      RESERVED8_Register   : aliased unsigned;  -- eth.h:494
      PTPTSSR_Register     : aliased unsigned;  -- eth.h:495
      RESERVED9_Register   : aliased ETH_TypeDef_RESERVED9_array;  -- eth.h:496
      DMABMR_Register      : aliased unsigned;  -- eth.h:497
      DMATPDR_Register     : aliased unsigned;  -- eth.h:498
      DMARPDR_Register     : aliased unsigned;  -- eth.h:499
      DMARDLAR_Register    : aliased unsigned;  -- eth.h:500
      DMATDLAR_Register    : aliased unsigned;  -- eth.h:501
      DMASR_Register       : aliased unsigned;  -- eth.h:502
      DMAOMR_Register      : aliased unsigned;  -- eth.h:503
      DMAIER_Register      : aliased unsigned;  -- eth.h:504
      DMAMFBOCR_Register   : aliased unsigned;  -- eth.h:505
      DMARSWTR_Register    : aliased unsigned;  -- eth.h:506
      RESERVED10_Register  : aliased ETH_TypeDef_RESERVED10_array;  -- eth.h:507
      DMACHTDR_Register    : aliased unsigned;  -- eth.h:508
      DMACHRDR_Register    : aliased unsigned;  -- eth.h:509
      DMACHTBAR_Register   : aliased unsigned;  -- eth.h:510
      DMACHRBAR_Register   : aliased unsigned;  -- eth.h:511
   end record;
   pragma Convention (C_Pass_By_Copy, ETH_TypeDef);  -- eth.h:512

end eth_h;

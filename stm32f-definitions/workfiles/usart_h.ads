pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;

package usart_h is

   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_SR_
   --  unsupported macro: USART_DR_DR
   --  unsupported macro: USART_BRR_DIV_Fraction
   --  unsupported macro: USART_BRR_DIV_Mantissa
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR1_
   --  unsupported macro: USART_CR2_
   --  unsupported macro: USART_CR2_
   --  unsupported macro: USART_CR2_
   --  unsupported macro: USART_CR2_
   --  unsupported macro: USART_CR2_
   --  unsupported macro: USART_CR2_
   --  unsupported macro: USART_CR2_CLKEN
   --  unsupported macro: USART_CR2_STOP
   --  unsupported macro: USART_CR2_STOP_0
   --  unsupported macro: USART_CR2_STOP_1
   --  unsupported macro: USART_CR2_LINEN
   --  unsupported macro: USART_CR3_EIE
   --  unsupported macro: USART_CR3_IREN
   --  unsupported macro: USART_CR3_IRLP
   --  unsupported macro: USART_CR3_HDSEL
   --  unsupported macro: USART_CR3_NACK
   --  unsupported macro: USART_CR3_SCEN
   --  unsupported macro: USART_CR3_DMAR
   --  unsupported macro: USART_CR3_DMAT
   --  unsupported macro: USART_CR3_RTSE
   --  unsupported macro: USART_CR3_CTSE
   --  unsupported macro: USART_CR3_CTSIE
   --  unsupported macro: USART_CR3_ONEBIT
   --  unsupported macro: USART_GTPR_PSC
   --  unsupported macro: USART_GTPR_PSC_0
   --  unsupported macro: USART_GTPR_PSC_1
   --  unsupported macro: USART_GTPR_PSC_2
   --  unsupported macro: USART_GTPR_PSC_3
   --  unsupported macro: USART_GTPR_PSC_4
   --  unsupported macro: USART_GTPR_PSC_5
   --  unsupported macro: USART_GTPR_PSC_6
   --  unsupported macro: USART_GTPR_PSC_7 ((uint16_t)0x0080)
   --  unsupported macro: USART_GTPR_GT ((uint16_t)0xFF00)
  --* 
  --  * @brief Universal Synchronous Asynchronous Receiver Transmitter
  --   

  --!< USART Status register,                   Address offset: 0x00  
   type USART_TypeDef is record
      SR : SR_Register;
      RE : RESERVED0_Register;
      DR : DR_Register;
      RESERVED1 : RESERVED1_Register;
      BRR : BRR_Register;
      RESERVED2 : RESERVED2_Register;
      CR1 : CR1_Register;
      RESERVED3 : RESERVED3_Register;
      CR2 : CR2_Register;
      RESERVED4 : RESERVED4_Register;
      CR3 : CR3_Register;
      RESERVED5 : RESERVED5_Register;
      GTPR : GTPR_Register;
      RESERVED6 : RESERVED6_Register;
   end record;
   pragma Convention (C_Pass_By_Copy, USART_TypeDef);  -- usart.h:23

   --  skipped anonymous struct anon_0

  --!< Reserved, 0x02                                                 
  --!< USART Data register,                     Address offset: 0x04  
  --!< Reserved, 0x06                                                 
  --!< USART Baud rate register,                Address offset: 0x08  
  --!< Reserved, 0x0A                                                 
  --!< USART Control register 1,                Address offset: 0x0C  
  --!< Reserved, 0x0E                                                 
  --!< USART Control register 2,                Address offset: 0x10  
  --!< Reserved, 0x12                                                 
  --!< USART Control register 3,                Address offset: 0x14  
  --!< Reserved, 0x16                                                 
  --!< USART Guard time and prescaler register, Address offset: 0x18  
  --!< Reserved, 0x1A                                                 
  --**************************************************************************** 
  --                                                                             
  --         Universal Synchronous Asynchronous Receiver Transmitter             
  --                                                                             
  --**************************************************************************** 
  --******************  Bit definition for USART_SR register  ****************** 
  --******************  Bit definition for USART_DR register  ****************** 
  --*****************  Bit definition for USART_BRR register  ****************** 
  --*****************  Bit definition for USART_CR1 register  ****************** 
  --*****************  Bit definition for USART_CR2 register  ****************** 
  --*****************  Bit definition for USART_CR3 register  ****************** 
  --*****************  Bit definition for USART_GTPR register  ***************** 
end usart_h;

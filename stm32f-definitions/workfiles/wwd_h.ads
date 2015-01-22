pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;

package wwd_h is

   --  unsupported macro: WWDG_CR_T ((uint8_t)0x7F)
   --  unsupported macro: WWDG_CR_T0 ((uint8_t)0x01)
   --  unsupported macro: WWDG_CR_T1 ((uint8_t)0x02)
   --  unsupported macro: WWDG_CR_T2 ((uint8_t)0x04)
   --  unsupported macro: WWDG_CR_T3 ((uint8_t)0x08)
   --  unsupported macro: WWDG_CR_T4 ((uint8_t)0x10)
   --  unsupported macro: WWDG_CR_T5 ((uint8_t)0x20)
   --  unsupported macro: WWDG_CR_T6 ((uint8_t)0x40)
   --  unsupported macro: WWDG_CR_WDGA ((uint8_t)0x80)
   --  unsupported macro: WWDG_CFR_W ((uint16_t)0x007F)
   --  unsupported macro: WWDG_CFR_W0 ((uint16_t)0x0001)
   --  unsupported macro: WWDG_CFR_W1 ((uint16_t)0x0002)
   --  unsupported macro: WWDG_CFR_W2 ((uint16_t)0x0004)
   --  unsupported macro: WWDG_CFR_W3 ((uint16_t)0x0008)
   --  unsupported macro: WWDG_CFR_W4 ((uint16_t)0x0010)
   --  unsupported macro: WWDG_CFR_W5 ((uint16_t)0x0020)
   --  unsupported macro: WWDG_CFR_W6 ((uint16_t)0x0040)
   --  unsupported macro: WWDG_CFR_WDGTB ((uint16_t)0x0180)
   --  unsupported macro: WWDG_CFR_WDGTB0 ((uint16_t)0x0080)
   --  unsupported macro: WWDG_CFR_WDGTB1 ((uint16_t)0x0100)
   --  unsupported macro: WWDG_CFR_EWI ((uint16_t)0x0200)
   --  unsupported macro: WWDG_SR_EWIF ((uint8_t)0x01)
  --* 
  --  * @brief Window WATCHDOG
  --   

  --!< WWDG Control register,       Address offset: 0x00  
   type WWDG_TypeDef is record
      CR : aliased stdint_h.uint32_t;  -- wwd.h:9
      CFR : aliased stdint_h.uint32_t;  -- wwd.h:10
      SR : aliased stdint_h.uint32_t;  -- wwd.h:11
   end record;
   pragma Convention (C_Pass_By_Copy, WWDG_TypeDef);  -- wwd.h:12

   --  skipped anonymous struct anon_0

  --!< WWDG Configuration register, Address offset: 0x04  
  --!< WWDG Status register,        Address offset: 0x08  
  --**************************************************************************** 
  --                                                                             
  --                            Window WATCHDOG                                  
  --                                                                             
  --**************************************************************************** 
  --******************  Bit definition for WWDG_CR register  ******************* 
  --******************  Bit definition for WWDG_CFR register  ****************** 
  --******************  Bit definition for WWDG_SR register  ******************* 
end wwd_h;

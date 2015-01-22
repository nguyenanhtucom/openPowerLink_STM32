pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;

package wwd_h is

   --  unsupported macro: WWDG_CR_T
   --  unsupported macro: WWDG_CR_T0
   --  unsupported macro: WWDG_CR_T1
   --  unsupported macro: WWDG_CR_T2
   --  unsupported macro: WWDG_CR_T3
   --  unsupported macro: WWDG_CR_T4
   --  unsupported macro: WWDG_CR_T5
   --  unsupported macro: WWDG_CR_T6
   --  unsupported macro: WWDG_CR_WDGA
   --  unsupported macro: WWDG_CFR_W
   --  unsupported macro: WWDG_CFR_W0
   --  unsupported macro: WWDG_CFR_W1
   --  unsupported macro: WWDG_CFR_W2
   --  unsupported macro: WWDG_CFR_W3
   --  unsupported macro: WWDG_CFR_W4
   --  unsupported macro: WWDG_CFR_W5
   --  unsupported macro: WWDG_CFR_W6
   --  unsupported macro: WWDG_CFR_WDGTB
   --  unsupported macro: WWDG_CFR_WDGTB0
   --  unsupported macro: WWDG_CFR_WDGTB1
   --  unsupported macro: WWDG_CFR_EWI
   --  unsupported macro: WWDG_SR_EWIF
  --* 
  --  * @brief Window WATCHDOG
  --   

  --!< WWDG Control register,       Address offset: 0x00  
   type WWDG_TypeDef is record
      CR  : aliased stdint_h.uint32_t;  -- wwd.h:9
      CFR : aliased stdint_h.uint32_t;  -- wwd.h:10
      SR  : aliased stdint_h.uint32_t;  -- wwd.h:11
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

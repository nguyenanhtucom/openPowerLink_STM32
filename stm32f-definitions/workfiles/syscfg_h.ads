pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;

package syscfg_h is

   --  unsupported macro: SYSCFG_MEMRMP_MEM_MODE
   --  unsupported macro: SYSCFG_MEMRMP_MEM_MODE_0
   --  unsupported macro: SYSCFG_MEMRMP_MEM_MODE_1
   --  unsupported macro: SYSCFG_MEMRMP_MEM_MODE_2
   --  unsupported macro: SYSCFG_MEMRMP_FB_MODE
   --  unsupported macro: SYSCFG_MEMRMP_SWP_FMC
   --  unsupported macro: SYSCFG_MEMRMP_SWP_FMC_0
   --  unsupported macro: SYSCFG_MEMRMP_SWP_FMC_1
   --  unsupported macro: SYSCFG_PMC_ADCxDC2
   --  unsupported macro: SYSCFG_PMC_ADC1DC2
   --  unsupported macro: SYSCFG_PMC_ADC2DC2
   --  unsupported macro: SYSCFG_PMC_ADC3DC2
   --  unsupported macro: SYSCFG_PMC_MII_RMII_SEL
   --  unsupported macro: SYSCFG_PMC_MII_RMII SYSCFG_PMC_MII_RMII_SEL
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PA
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PB
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PC
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PD
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PE
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PF
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PG
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PH
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PI
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PJ
   --  unsupported macro: SYSCFG_EXTICR1_EXTI0_PK
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PA
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PB
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PC
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PD
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PE
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PF
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PG
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PH
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PI
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PJ
   --  unsupported macro: SYSCFG_EXTICR1_EXTI1_PK
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PA
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PB
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PC
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PD
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PE
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PF
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PG
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PH
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PI
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PJ
   --  unsupported macro: SYSCFG_EXTICR1_EXTI2_PK
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PA
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PB
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PC
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PD
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PE
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PF
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PG
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PH
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PI
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PJ
   --  unsupported macro: SYSCFG_EXTICR1_EXTI3_PK
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PA
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PB
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PC
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PD
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PE
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PF
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PG
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PH
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PI
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PJ
   --  unsupported macro: SYSCFG_EXTICR2_EXTI4_PK
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PA
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PB
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PC
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PD
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PE
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PF
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PG
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PH
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PI
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PJ
   --  unsupported macro: SYSCFG_EXTICR2_EXTI5_PK
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PA
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PB
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PC
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PD
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PE
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PF
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PG
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PH
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PI
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PJ
   --  unsupported macro: SYSCFG_EXTICR2_EXTI6_PK
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PA
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PB
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PC
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PD
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PE
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PF
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PG
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PH
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PI
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PJ
   --  unsupported macro: SYSCFG_EXTICR2_EXTI7_PK
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PA
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PB
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PC
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PD
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PE
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PF
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PG
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PH
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PI
   --  unsupported macro: SYSCFG_EXTICR3_EXTI8_PJ
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PA
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PB
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PC
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PD
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PE
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PF
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PG
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PH
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PI
   --  unsupported macro: SYSCFG_EXTICR3_EXTI9_PJ
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PA
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PB
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PC
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PD
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PE
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PF
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PG
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PH
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PI
   --  unsupported macro: SYSCFG_EXTICR3_EXTI10_PJ
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PA
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PB
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PC
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PD
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PE
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PF
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PG
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PH
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PI
   --  unsupported macro: SYSCFG_EXTICR3_EXTI11_PJ
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PA
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PB
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PC
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PD
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PE
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PF
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PG
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PH
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PI
   --  unsupported macro: SYSCFG_EXTICR4_EXTI12_PJ
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PA
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PB
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PC
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PD
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PE
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PF
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PG
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PH
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PI
   --  unsupported macro: SYSCFG_EXTICR4_EXTI13_PJ
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PA
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PB
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PC
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PD
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PE
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PF
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PG
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PH
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PI
   --  unsupported macro: SYSCFG_EXTICR4_EXTI14_PJ
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PA
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PB
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PC
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PD
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PE
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PF
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PG
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PH
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PI
   --  unsupported macro: SYSCFG_EXTICR4_EXTI15_PJ
   --  unsupported macro: SYSCFG_CMPCR_CMP_PD
   --  unsupported macro: SYSCFG_CMPCR_READY
  --**************************************************************************** 
  --                                                                             
  --                                 SYSCFG                                      
  --                                                                             
  --**************************************************************************** 
  --*****************  Bit definition for SYSCFG_MEMRMP register  ************** 
  --*****************  Bit definition for SYSCFG_PMC register  ***************** 
  -- Old MII_RMII_SEL bit definition, maintained for legacy purpose  
  --****************  Bit definition for SYSCFG_EXTICR1 register  ************** 
  --* 
  --  * @brief   EXTI0 configuration  
  --   

  --* 
  --  * @brief   EXTI1 configuration  
  --   

  --* 
  --  * @brief   EXTI2 configuration  
  --   

  --* 
  --  * @brief   EXTI3 configuration  
  --   

  --****************  Bit definition for SYSCFG_EXTICR2 register  ************** 
  --* 
  --  * @brief   EXTI4 configuration  
  --   

  --* 
  --  * @brief   EXTI5 configuration  
  --   

  --* 
  --  * @brief   EXTI6 configuration  
  --   

  --* 
  --  * @brief   EXTI7 configuration  
  --   

  --****************  Bit definition for SYSCFG_EXTICR3 register  ************** 
  --* 
  --  * @brief   EXTI8 configuration  
  --   

  --* 
  --  * @brief   EXTI9 configuration  
  --   

  --* 
  --  * @brief   EXTI10 configuration  
  --   

  --* 
  --  * @brief   EXTI11 configuration  
  --   

  --****************  Bit definition for SYSCFG_EXTICR4 register  ************** 
  --* 
  --  * @brief   EXTI12 configuration  
  --   

  --* 
  --  * @brief   EXTI13 configuration  
  --   

  --* 
  --  * @brief   EXTI14 configuration  
  --   

  --* 
  --  * @brief   EXTI15 configuration  
  --   

  --*****************  Bit definition for SYSCFG_CMPCR register  *************** 
  --* 
  --  * @brief System configuration controller
  --   

  --!< SYSCFG memory remap register,                      Address offset: 0x00       
   type SYSCFG_TypeDef_EXTICR_array is array (0 .. 3) of aliased stdint_h.uint32_t;
   type SYSCFG_TypeDef_RESERVED_array is array (0 .. 1) of aliased stdint_h.uint32_t;
   type SYSCFG_TypeDef is record
      MEMRMP : aliased stdint_h.uint32_t;  -- syscfg.h:299
      PMC : aliased stdint_h.uint32_t;  -- syscfg.h:300
      EXTICR : aliased SYSCFG_TypeDef_EXTICR_array;  -- syscfg.h:301
      RESERVED : aliased SYSCFG_TypeDef_RESERVED_array;  -- syscfg.h:302
      CMPCR : aliased stdint_h.uint32_t;  -- syscfg.h:303
   end record;
   pragma Convention (C_Pass_By_Copy, SYSCFG_TypeDef);  -- syscfg.h:304

   --  skipped anonymous struct anon_0

  --!< SYSCFG peripheral mode configuration register,     Address offset: 0x04       
  --!< SYSCFG external interrupt configuration registers, Address offset: 0x08-0x14  
  --!< Reserved, 0x18-0x1C                                                           
  --!< SYSCFG Compensation cell control register,         Address offset: 0x20       
end syscfg_h;

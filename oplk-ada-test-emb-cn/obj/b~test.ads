pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2014 (20140331)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_test" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#e398e447#;
   pragma Export (C, u00001, "testB");
   u00002 : constant Version_32 := 16#fbff4c67#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#93c52800#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#8539e8c9#;
   pragma Export (C, u00004, "oplkS");
   u00005 : constant Version_32 := 16#40052ea6#;
   pragma Export (C, u00005, "oplk__errordefsS");
   u00006 : constant Version_32 := 16#69adb1b9#;
   pragma Export (C, u00006, "interfacesS");
   u00007 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00007, "interfaces__cB");
   u00008 : constant Version_32 := 16#3b563890#;
   pragma Export (C, u00008, "interfaces__cS");
   u00009 : constant Version_32 := 16#56fc860c#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#a5ccaeb2#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00011, "adaS");
   u00012 : constant Version_32 := 16#032105bb#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerB");
   u00013 : constant Version_32 := 16#2b293877#;
   pragma Export (C, u00013, "ada__exceptions__last_chance_handlerS");
   u00014 : constant Version_32 := 16#90249111#;
   pragma Export (C, u00014, "systemS");
   u00015 : constant Version_32 := 16#daf76b33#;
   pragma Export (C, u00015, "system__soft_linksB");
   u00016 : constant Version_32 := 16#dafbf423#;
   pragma Export (C, u00016, "system__soft_linksS");
   u00017 : constant Version_32 := 16#c8ed38da#;
   pragma Export (C, u00017, "system__parametersB");
   u00018 : constant Version_32 := 16#96fe09a3#;
   pragma Export (C, u00018, "system__parametersS");
   u00019 : constant Version_32 := 16#c96bf39e#;
   pragma Export (C, u00019, "system__secondary_stackB");
   u00020 : constant Version_32 := 16#3b455e78#;
   pragma Export (C, u00020, "system__secondary_stackS");
   u00021 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00021, "system__storage_elementsB");
   u00022 : constant Version_32 := 16#bde7db15#;
   pragma Export (C, u00022, "system__storage_elementsS");
   u00023 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00023, "system__stack_checkingB");
   u00024 : constant Version_32 := 16#1e9bfaf9#;
   pragma Export (C, u00024, "system__stack_checkingS");
   u00025 : constant Version_32 := 16#393398c1#;
   pragma Export (C, u00025, "system__exception_tableB");
   u00026 : constant Version_32 := 16#3e3df704#;
   pragma Export (C, u00026, "system__exception_tableS");
   u00027 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00027, "system__exceptionsB");
   u00028 : constant Version_32 := 16#f847fce7#;
   pragma Export (C, u00028, "system__exceptionsS");
   u00029 : constant Version_32 := 16#2652ec14#;
   pragma Export (C, u00029, "system__exceptions__machineS");
   u00030 : constant Version_32 := 16#b895431d#;
   pragma Export (C, u00030, "system__exceptions_debugB");
   u00031 : constant Version_32 := 16#23c688af#;
   pragma Export (C, u00031, "system__exceptions_debugS");
   u00032 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00032, "system__img_intB");
   u00033 : constant Version_32 := 16#92ff71d3#;
   pragma Export (C, u00033, "system__img_intS");
   u00034 : constant Version_32 := 16#ff5c7695#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#b8200e4c#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00036, "system__wch_conB");
   u00037 : constant Version_32 := 16#8b59b3c3#;
   pragma Export (C, u00037, "system__wch_conS");
   u00038 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00038, "system__wch_stwB");
   u00039 : constant Version_32 := 16#a6489fc2#;
   pragma Export (C, u00039, "system__wch_stwS");
   u00040 : constant Version_32 := 16#9b29844d#;
   pragma Export (C, u00040, "system__wch_cnvB");
   u00041 : constant Version_32 := 16#84ee0930#;
   pragma Export (C, u00041, "system__wch_cnvS");
   u00042 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00042, "system__wch_jisB");
   u00043 : constant Version_32 := 16#049e1011#;
   pragma Export (C, u00043, "system__wch_jisS");
   u00044 : constant Version_32 := 16#8cb17bcd#;
   pragma Export (C, u00044, "system__traceback_entriesB");
   u00045 : constant Version_32 := 16#2535f183#;
   pragma Export (C, u00045, "system__traceback_entriesS");
   u00046 : constant Version_32 := 16#86d40249#;
   pragma Export (C, u00046, "oplk__oplkS");
   u00047 : constant Version_32 := 16#773b26c0#;
   pragma Export (C, u00047, "interfaces__c__extensionsS");
   u00048 : constant Version_32 := 16#48973b17#;
   pragma Export (C, u00048, "interfaces__c__stringsB");
   u00049 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00049, "interfaces__c__stringsS");
   u00050 : constant Version_32 := 16#8cad390f#;
   pragma Export (C, u00050, "oplk__cfmS");
   u00051 : constant Version_32 := 16#37eb657f#;
   pragma Export (C, u00051, "oplk__eventS");
   u00052 : constant Version_32 := 16#1839f009#;
   pragma Export (C, u00052, "oplk__nmtS");
   u00053 : constant Version_32 := 16#07963728#;
   pragma Export (C, u00053, "oplk__oplkincS");
   u00054 : constant Version_32 := 16#f7bf99d4#;
   pragma Export (C, u00054, "oplk__frameS");
   u00055 : constant Version_32 := 16#aef1321e#;
   pragma Export (C, u00055, "oplk__ledS");
   u00056 : constant Version_32 := 16#9b7d18f2#;
   pragma Export (C, u00056, "oplk__obdS");
   u00057 : constant Version_32 := 16#8fef49f9#;
   pragma Export (C, u00057, "oplk__sdoS");
   u00058 : constant Version_32 := 16#2d154371#;
   pragma Export (C, u00058, "test1S");
   u00059 : constant Version_32 := 16#233fba99#;
   pragma Export (C, u00059, "test1__eventB");
   u00060 : constant Version_32 := 16#d41fbb24#;
   pragma Export (C, u00060, "test1__eventS");
   u00061 : constant Version_32 := 16#2231f553#;
   pragma Export (C, u00061, "oplk__debugstrS");
   u00062 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00062, "system__concat_2B");
   u00063 : constant Version_32 := 16#928446c1#;
   pragma Export (C, u00063, "system__concat_2S");
   u00064 : constant Version_32 := 16#608e2cd1#;
   pragma Export (C, u00064, "system__concat_5B");
   u00065 : constant Version_32 := 16#177ad23f#;
   pragma Export (C, u00065, "system__concat_5S");
   u00066 : constant Version_32 := 16#932a4690#;
   pragma Export (C, u00066, "system__concat_4B");
   u00067 : constant Version_32 := 16#ee40ba31#;
   pragma Export (C, u00067, "system__concat_4S");
   u00068 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00068, "system__concat_3B");
   u00069 : constant Version_32 := 16#9b54cdb4#;
   pragma Export (C, u00069, "system__concat_3S");
   u00070 : constant Version_32 := 16#22ab03a2#;
   pragma Export (C, u00070, "system__img_unsB");
   u00071 : constant Version_32 := 16#5ed63f49#;
   pragma Export (C, u00071, "system__img_unsS");
   u00072 : constant Version_32 := 16#c12203be#;
   pragma Export (C, u00072, "system__unsigned_typesS");
   u00073 : constant Version_32 := 16#d6f619bb#;
   pragma Export (C, u00073, "system__memoryB");
   u00074 : constant Version_32 := 16#c959f725#;
   pragma Export (C, u00074, "system__memoryS");
   u00075 : constant Version_32 := 16#baff2c34#;
   pragma Export (C, u00075, "system__crtlS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.img_int%s
   --  system.img_int%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  interfaces.c%s
   --  interfaces.c.extensions%s
   --  interfaces.c.strings%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.secondary_stack%s
   --  interfaces.c.strings%b
   --  interfaces.c%b
   --  system.soft_links%b
   --  system.secondary_stack%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  oplk%s
   --  oplk.errordefs%s
   --  oplk.cfm%s
   --  oplk.led%s
   --  oplk.nmt%s
   --  oplk.oplkinc%s
   --  oplk.event%s
   --  oplk.frame%s
   --  oplk.obd%s
   --  oplk.sdo%s
   --  oplk.oplk%s
   --  oplk.debugstr%s
   --  test1%s
   --  test1.event%s
   --  test1.event%b
   --  test%b
   --  END ELABORATION ORDER


end ada_main;

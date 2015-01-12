pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~test.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~test.adb");

package body ada_main is
   pragma Warnings (Off);

   E16 : Short_Integer; pragma Import (Ada, E16, "system__soft_links_E");
   E26 : Short_Integer; pragma Import (Ada, E26, "system__exception_table_E");
   E08 : Short_Integer; pragma Import (Ada, E08, "interfaces__c_E");
   E49 : Short_Integer; pragma Import (Ada, E49, "interfaces__c__strings_E");
   E28 : Short_Integer; pragma Import (Ada, E28, "system__exceptions_E");
   E20 : Short_Integer; pragma Import (Ada, E20, "system__secondary_stack_E");
   E60 : Short_Integer; pragma Import (Ada, E60, "test1__event_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");
   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      Finalize_Library_Objects := null;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E26 := E26 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E28 := E28 + 1;
      E49 := E49 + 1;
      E08 := E08 + 1;
      System.Soft_Links'Elab_Body;
      E16 := E16 + 1;
      System.Secondary_Stack'Elab_Body;
      E20 := E20 + 1;
      Test1.Event'Elab_Body;
      E60 := E60 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-errordefs.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-cfm.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-led.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-nmt.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-oplkinc.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-event.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-frame.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-obd.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-sdo.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-oplk.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/oplk-debugstr.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/test1.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/test1-event.o
   --   /home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/test.o
   --   -L/home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/
   --   -L/home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/obj/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/4.7.4/rts-native/adalib/
   --   -L/home/jan/openPOWERLINK/Ada-Interface/oplk-ada-test-emb-cn/
   --   -L/usr/gnat-arm/lib/gnat/arm-eabi/ravenscar-sfp-stm32f4/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;


with System;
with Interfaces.C.Strings;
--with Oplk.Oplk;
with Oplk.Errordefs;
with Oplk.Nmt;
with Oplk.Event;
with Oplk.Led;
with Oplk.Debugstr;
--with Test1;

package body Test1.Event is
   
   package Ic   renames Interfaces.C;
   package Ics  renames Interfaces.C.Strings;
   package Plk  renames Oplk.Oplk;
   package Err  renames Oplk.Errordefs;
   package Nmt  renames Oplk.Nmt;
   package Event renames Oplk.Event;
   package Led  renames Oplk.Led;
   package Dstr renames Oplk.Debugstr;
   
   procedure Message (S : String)
   is
   begin
      null;
   end Message;
   
   
   -- The function processes state change events.
   -- EventType_p         Type of event
   -- pEventArg_p         Pointer to union which describes the event in detail
   -- pUserArg_p          User specific argument
   --
   --The function returns a tOplkError error code.
   function Process_State_Change_Event 
     (Event_Type_P : Plk.TOplkApiEventType;              -- not used
      PEvent_Arg_P : access Plk.TOplkApiEventArg;
      PUserArg_P   : System.Address)                      -- not used
     return Err.TOplkError
   is
      pragma Unreferenced (Event_Type_P, PUserArg_P);
      PNmtStateChange : constant Nmt.TEventNmtStateChange := 
	PEvent_Arg_P.NmtStateChange;
      New_Nmt_State   : constant Nmt.TNmtState            := 
	PNmtStateChange.NewNmtState;
      Nmt_Event       : constant Nmt.TNmtEvent            := 
	PNmtStateChange.NmtEvent;
   begin
      Message ("StateChangeEvent " & Ic.Unsigned'Image (New_Nmt_State) & 
		 " originating event = " & Ic.Unsigned'Image (Nmt_Event) &
		 Ics.Value (Dstr.Debugstr_GetNmtEventStr (Nmt_Event)));
      return Err.KErrorOk;
   end Process_State_Change_Event;
   
   
   -- The function processes error and warning events.
   -- EventType_p         Type of event
   -- pEventArg_p         Pointer to union which describes the event in detail
   -- pUserArg_p          User specific argument
   --
   -- The function returns a tOplkError error code.
   function Process_Error_Warning_Event
     (Event_Type_P : Plk.TOplkApiEventType;              -- not used
      PEvent_Arg_P : access Plk.TOplkApiEventArg;
      PUserArg_P   : System.Address)                      -- not used
     return Err.TOplkError
   is
      pragma Unreferenced (Event_Type_P, PUserArg_P);
      PInternalError : constant Event.TEventError  := PEvent_Arg_P.InternalError;
      EventSource    : constant Event.TEventSource := PInternalError.EventSource;
      OplkError      : constant Err.TOplkError     := PInternalError.OplkError;
   begin
      Message ("Err/Warn: Source = " &
		 Ics.Value (Dstr.Debugstr_GetEventSourceStr (EventSource)) &
		 " (" & Ic.Unsigned'Image (EventSource) & ") " &
		 "OplkError = " & 
		 Ics.Value (Dstr.Debugstr_GetRetValStr (OplkError)) &
		 " (" & Ic.Unsigned'Image (OplkError) & ")");
      case EventSource is
	 when Event.KEventSourceEventk | 
	   Event.KEventSourceEventu       => 
	    -- error occurred within event processing
	    -- either in kernel or in user part
	    Message 
	      (" OrgSource = " &
		 Ics.Value (Dstr.Debugstr_GetEventSourceStr 
			      (PInternalError.errorArg.EventSource)) & " (" &
		 Ic.Unsigned'Image (PInternalError.errorArg.EventSource) & ")");
	 when Event.KEventSourceDllk      =>
	    -- error occurred within the data link layer (e.g. interrupt processing)
	    -- the DWORD argument contains the DLL state and the NMT event
	    declare
	       PInternalError : constant Event.TEventError := 
		 PEvent_Arg_P.InternalError;
	    begin
	       Message 
		 (" val = " & Ic.Unsigned'Image (PInternalError.errorArg.uintArg));
	    end;
	 when others                      =>
	    Message ("");
      end case;
      return Err.KErrorOk;
   end Process_Error_Warning_Event;
   
   --------------------------------------------------
   -- Public functions                             --
   --------------------------------------------------
   
   -- The function implements the applications stack event handler.
   -- EventType_p         Type of event
   -- pEventArg_p         Pointer to union which describes the event in detail
   -- pUserArg_p          User specific argument
   --
   -- The function returns a tOplkError error code.
   function Process_Events 
     (Event_Type_P : Plk.TOplkApiEventType;
      PEvent_Arg_P : access Plk.TOplkApiEventArg;
      PUserArg_P   : System.Address)                      -- not used
     return Err.TOplkError;
   pragma Export (C, Process_Events, "Process_Events");
   
   function Process_Events 
     (Event_Type_P : Plk.TOplkApiEventType;
      PEvent_Arg_P : access Plk.TOplkApiEventArg;
      PUserArg_P   : System.Address)                      -- not used
     return Err.TOplkError
   is
      Ret : Err.TOplkError := Err.kErrorOk;
   begin
      case Event_Type_P is
	 when Plk.KOplkApiEventNmtStateChange  =>
	    Ret := Process_State_Change_Event 
	      (Event_Type_P, PEvent_Arg_P, PUserArg_P);
	 when Plk.KOplkApiEventCriticalError |
	   Plk.KOplkApiEventWarning            =>
	    Ret := Process_Error_Warning_Event 
	      (Event_Type_P, PEvent_Arg_P, PUserArg_P);
	 when Plk.KOplkApiEventLed                  =>
	    -- POWERLINK S/E LED needs to be changed
	    case PEvent_Arg_P.ledEvent.LedType is
	       when Led.KLedTypeStatus                    =>
		  --Gpio_SetStatusLed (PEvent_Arg_P.ledEvent.FOn);
		  null;
	       when Led.KLedTypeError                     =>
		  --Gpio_SetErrorLed (PEvent_Arg_P.ledEvent.FOn);
		  null;
	       --when others                            =>
		  --null;
	    end case;
	 when others                            =>
	  null;
      end case;
      
      return Ret;
   end Process_Events;
   
begin
   Process_Events_Cb := Process_Events'Access;

   null;
end Test1.Event;


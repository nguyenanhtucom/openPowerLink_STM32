
-- This file contains the definitions for the debug-string module.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Oplk.Nmt;
with Interfaces.C.Strings;
with Oplk.Event;
with Oplk.Oplk;
with Oplk.Sdo;
with Oplk.Errordefs;

------------------------------------------------------------------------------
--           Public Functions from debugstr.c                               --
------------------------------------------------------------------------------

package Oplk.Debugstr is
   
   
   function debugstr_getNmtEventStr 
     (nmtEvent_p : Nmt.tNmtEvent) 
     return Interfaces.C.Strings.chars_ptr;
   -- nmtEvent_p : Event to print
   -- The function returns a string describing the specified event.
   -- 
   -- The function returns the string describing the specified event.
   pragma Import 
     (C, debugstr_getNmtEventStr, "debugstr_getNmtEventStr");  -- oplk/debugstr.h:64
   
   
   function debugstr_getEventTypeStr 
     (eventType_p : Event.tEventType) 
     return Interfaces.C.Strings.chars_ptr;
   -- eventType_p : Event type to print
   -- The function returns a string describing the specified event type.
   -- 
   -- Return the string of the specified event type
   pragma Import 
     (C, debugstr_getEventTypeStr, "debugstr_getEventTypeStr");  -- oplk/debugstr.h:65
   
   
   function debugstr_getEventSourceStr 
     (eventSrc_p : Event.tEventSource) 
     return Interfaces.C.Strings.chars_ptr;
   -- eventType_p : Event source to print
   -- The function returns a string describing the specified event source.
   -- 
   -- Return the string of the specified event source
   pragma Import 
     (C, debugstr_getEventSourceStr, "debugstr_getEventSourceStr");  -- oplk/debugstr.h:66
   
   
   function debugstr_getEventSinkStr 
     (eventSink_p : Event.tEventSink) 
     return Interfaces.C.Strings.chars_ptr;
   -- eventSink_p : Event sink to print
   -- The function returns a string describing the specified event sink.
   -- 
   -- Return the string of the specified event sink
   pragma Import 
     (C, debugstr_getEventSinkStr, "debugstr_getEventSinkStr");  -- oplk/debugstr.h:67
   
   
   function debugstr_getNmtStateStr 
     (nmtState_p : Nmt.tNmtState) 
     return Interfaces.C.Strings.chars_ptr;
   -- nmtState_p : NMT state to print
   -- The function returns a string describing the specified NMT state.
   -- 
   -- Return the string of the specified NMT state
   pragma Import 
     (C, debugstr_getNmtStateStr, "debugstr_getNmtStateStr");  -- oplk/debugstr.h:68
   
   
   function debugstr_getApiEventStr 
     (ApiEvent_p : Oplk.tOplkApiEventType) 
     return Interfaces.C.Strings.chars_ptr;
   -- ApiEvent_p : API event to print
   -- The function returns a string describing the specified API event.
   -- 
   -- Return the string of the specified API event
   pragma Import 
     (C, debugstr_getApiEventStr, "debugstr_getApiEventStr");  -- oplk/debugstr.h:69
   
   
   function debugstr_getNmtNodeEventTypeStr 
     (NodeEventType_p : Nmt.tNmtNodeEvent) 
     return Interfaces.C.Strings.chars_ptr;
   -- NodeEventType_p : NMT node event to print
   -- The function returns a string describing the specified NMT node event.
   -- 
   -- Return the string of the specified NMT node event
   pragma Import 
     (C, debugstr_getNmtNodeEventTypeStr, "debugstr_getNmtNodeEventTypeStr");  -- oplk/debugstr.h:70
   
   
   function debugstr_getNmtBootEventTypeStr 
     (BootEventType_p : Nmt.tNmtBootEvent) 
     return Interfaces.C.Strings.chars_ptr;
   -- BootEventType_p : NMT boot event to print
   -- The function returns a string describing the specified NMT boot event.
   -- 
   -- Return the string of the specified NMT boot event
   pragma Import 
     (C, debugstr_getNmtBootEventTypeStr, "debugstr_getNmtBootEventTypeStr");  -- oplk/debugstr.h:71
   
   
   function debugstr_getSdoComConStateStr 
     (SdoComConState_p : Sdo.tSdoComConState) 
     return Interfaces.C.Strings.chars_ptr;
   -- SdoComConState_p : SDO command connection state to print
   -- The function returns a string describing the specified SDO command
   -- connection state.
   -- 
   -- Return the string of the specified SDO command connection state
   pragma Import 
     (C, debugstr_getSdoComConStateStr, "debugstr_getSdoComConStateStr");  -- oplk/debugstr.h:72
   
   
   function debugstr_getRetValStr 
     (OplkError_p : Errordefs.tOplkError) 
     return Interfaces.C.Strings.chars_ptr;
   -- OplkError_p : tOplkError value to print
   -- The function returns a string describing the specified tOplkError type.
   -- 
   -- Return the string of the specified SDO command connection state
   pragma Import 
     (C, debugstr_getRetValStr, "debugstr_getRetValStr");  -- oplk/debugstr.h:73
   
   
   function debugstr_getEmergErrCodeStr 
     (EmergErrCode_p : unsigned_short) 
     return Interfaces.C.Strings.chars_ptr;
   -- EmergErrCode_p : Emergency error code value to print
   -- The function returns a string describing the specified emergency error code.
   -- 
   -- Return the string describing the specified emergency error code
   pragma Import 
     (C, debugstr_getEmergErrCodeStr, "debugstr_getEmergErrCodeStr");  -- oplk/debugstr.h:74
   
   
   function debugstr_getAbortCodeStr 
     (abortCode_p : unsigned) 
     return Interfaces.C.Strings.chars_ptr;
   -- abortCode_p : Abort code value to print
   -- The function returns a string describing the specified abort code.
   -- 
   -- Return the string describing the specified abort code
   pragma Import 
     (C, debugstr_getAbortCodeStr, "debugstr_getAbortCodeStr");  -- oplk/debugstr.h:75

end Oplk.Debugstr;


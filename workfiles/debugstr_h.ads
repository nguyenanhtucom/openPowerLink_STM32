pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with nmt_h;
with Interfaces.C.Strings;
with event_h;
with oplk_h;
with sdo_h;
with errordefs_h;

package debugstr_h is

  --*
  --********************************************************************************
  --\file   oplk/debugstr.h
  --\brief  Definitions for debug-string module
  --This file contains the definitions for the debug-string module.
  --****************************************************************************** 

  --------------------------------------------------------------------------------
  --Copyright (c) 2014, Bernecker+Rainer Industrie-Elektronik Ges.m.b.H. (B&R)
  --All rights reserved.
  --Redistribution and use in source and binary forms, with or without
  --modification, are permitted provided that the following conditions are met:
  --    * Redistributions of source code must retain the above copyright
  --      notice, this list of conditions and the following disclaimer.
  --    * Redistributions in binary form must reproduce the above copyright
  --      notice, this list of conditions and the following disclaimer in the
  --      documentation and/or other materials provided with the distribution.
  --    * Neither the name of the copyright holders nor the
  --      names of its contributors may be used to endorse or promote products
  --      derived from this software without specific prior written permission.
  --THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  --ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  --WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  --DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDERS BE LIABLE FOR ANY
  --DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  --(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  --LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  --ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  --(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  --SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  -------------------------------------------------------------------------------- 

  --------------------------------------------------------------------------------
  -- includes
  --------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
  -- const defines
  --------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
  -- typedef
  --------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
  -- function prototypes
  --------------------------------------------------------------------------------
   function debugstr_getNmtEventStr (nmtEvent_p : nmt_h.tNmtEvent) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:64
   pragma Import (C, debugstr_getNmtEventStr, "debugstr_getNmtEventStr");

   function debugstr_getEventTypeStr (eventType_p : event_h.tEventType) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:65
   pragma Import (C, debugstr_getEventTypeStr, "debugstr_getEventTypeStr");

   function debugstr_getEventSourceStr (eventSrc_p : event_h.tEventSource) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:66
   pragma Import (C, debugstr_getEventSourceStr, "debugstr_getEventSourceStr");

   function debugstr_getEventSinkStr (eventSink_p : event_h.tEventSink) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:67
   pragma Import (C, debugstr_getEventSinkStr, "debugstr_getEventSinkStr");

   function debugstr_getNmtStateStr (nmtState_p : nmt_h.tNmtState) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:68
   pragma Import (C, debugstr_getNmtStateStr, "debugstr_getNmtStateStr");

   function debugstr_getApiEventStr (ApiEvent_p : oplk_h.tOplkApiEventType) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:69
   pragma Import (C, debugstr_getApiEventStr, "debugstr_getApiEventStr");

   function debugstr_getNmtNodeEventTypeStr (NodeEventType_p : nmt_h.tNmtNodeEvent) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:70
   pragma Import (C, debugstr_getNmtNodeEventTypeStr, "debugstr_getNmtNodeEventTypeStr");

   function debugstr_getNmtBootEventTypeStr (BootEventType_p : nmt_h.tNmtBootEvent) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:71
   pragma Import (C, debugstr_getNmtBootEventTypeStr, "debugstr_getNmtBootEventTypeStr");

   function debugstr_getSdoComConStateStr (SdoComConState_p : sdo_h.tSdoComConState) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:72
   pragma Import (C, debugstr_getSdoComConStateStr, "debugstr_getSdoComConStateStr");

   function debugstr_getRetValStr (OplkError_p : errordefs_h.tOplkError) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:73
   pragma Import (C, debugstr_getRetValStr, "debugstr_getRetValStr");

   function debugstr_getEmergErrCodeStr (EmergErrCode_p : unsigned_short) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:74
   pragma Import (C, debugstr_getEmergErrCodeStr, "debugstr_getEmergErrCodeStr");

   function debugstr_getAbortCodeStr (abortCode_p : unsigned) return Interfaces.C.Strings.chars_ptr;  -- oplk/debugstr.h:75
   pragma Import (C, debugstr_getAbortCodeStr, "debugstr_getAbortCodeStr");

end debugstr_h;

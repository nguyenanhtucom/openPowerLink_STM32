pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with oplkinc_h;
with System;
with errordefs_h;
with nmt_h;

package event_h is


   MAX_EVENT_ARG_SIZE : constant := 2048;  --  ./oplk/event.h:53

  --*
  --********************************************************************************
  --\file   oplk/event.h
  --\brief  Header file for event module
  --This file contains definitions for the event module.
  --****************************************************************************** 

  --------------------------------------------------------------------------------
  --Copyright (c) 2012, SYSTEC electronic GmbH
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
  -- max size of event argument
  --------------------------------------------------------------------------------
  -- typedef
  --------------------------------------------------------------------------------
  --*
  --\brief  Type of an event
  --This enumeration defines all valid event types.
  -- 

  --/< NMT event (arg is pointer to tNmtEvent)
  --/< PDO frame received event (PRes/PReq) (arg is pointer to tFrameInfo)
  --/< PDO frame transmitted event (PRes/PReq) (arg is pointer to tFrameInfo)
  --/< SoA frame received event (isochronous phase completed) (arg is pointer to nothing)
  --/< Sync event (e.g. SoC or anticipated SoC) (arg is pointer to nothing)
  --/< Timer event (arg is pointer to tTimerEventArg)
  --/< Heartbeat event (arg is pointer to tHeartbeatEvent)
  --/< Error history entry event (arg is pointer to the tErrHistoryEntry)
  --/< DLL kernel Flag 1 changed event (arg is pointer to nothing)
  --/< DLL kernel fill TxBuffer event (arg is pointer to tDllAsyncReqPriority)
  --/< DLL kernel PRes ready event (arg is pointer to nothing)
  --/< Error event for API layer (arg is pointer to tEventError)
  --/< Indicate change of NMT-State (arg is pointer to tEventNmtStateChange)
  --/< DLL error event for error handler (arg is pointer to tEventDllError)
  --/< received ASnd frame for DLL user module (arg is pointer to tPlkFrame)
  --/< configure ServiceIdFilter (arg is pointer to tDllAsndServiceId)
  --/< configure Identity (arg is pointer to tDllIdentParam)
  --/< configure ConfigParam (arg is pointer to tDllConfigParam)
  --/< issue Ident/Status request (arg is pointer to tDllCalIssueRequest)
  --/< add node to isochronous phase (arg is pointer to tDllNodeOpParam)
  --/< remove node from isochronous phase (arg is pointer to tDllNodeOpParam)
  --/< configures parameters of node (arg is pointer to tDllNodeInfo)
  --/< start reduced POWERLINK cycle on MN (arg is pointer to nothing)
  --/< NMT command was actually sent (arg is pointer to tPlkFrame)
  --/< user-defined event (arg is user-defined pointer)
  --/< SoA sent, cycle finished (arg is pointer to nothing)
  --/< alloc PDOs (arg is pointer to tPdoAllocationParam)
  --/< configure PDO channel (arg is pointer to tPdoChannelConf)
  --/< trigger NMT node command (arg is pointer to tNmtNodeCommand)
  --/< GW309ASCII request (arg is pointer to pointer of tGw309AsciiRequest)
  --/< node was added to isochronous phase by DLL (arg is pointer to unsigned int containing the node-ID)
  --/< dealloc PDOs
  --/< enable/disable the pdokcal sync trigger (arg is pointer to BOOL)
  --/< Free receive buffer (arg is pointer to the buffer to release)
  --/< Didn't receive ASnd frame for DLL user module (arg is pointer to tDllAsndNotRx)
  --/< Received ASnd frame for DLL user module (arg is pointer to tFrameInfo)
  --/< Received a PRes frame, which shall be forwarded to application (arg is pointer to tEventReceivedPres)
  --/< Request forwarding of a PRes frame to API layer (e.g. for conformance test)
  --/< SDO sequence layer event (for SDO command layer testing module)
   subtype tEventType is unsigned;
   kEventTypeNmtEvent : constant tEventType := 1;
   kEventTypePdoRx : constant tEventType := 2;
   kEventTypePdoTx : constant tEventType := 3;
   kEventTypePdoSoa : constant tEventType := 4;
   kEventTypeSync : constant tEventType := 5;
   kEventTypeTimer : constant tEventType := 6;
   kEventTypeHeartbeat : constant tEventType := 7;
   kEventTypeHistoryEntry : constant tEventType := 8;
   kEventTypeDllkFlag1 : constant tEventType := 9;
   kEventTypeDllkFillTx : constant tEventType := 10;
   kEventTypeDllkPresReady : constant tEventType := 11;
   kEventTypeError : constant tEventType := 12;
   kEventTypeNmtStateChange : constant tEventType := 13;
   kEventTypeDllError : constant tEventType := 14;
   kEventTypeAsndRx : constant tEventType := 15;
   kEventTypeDllkServFilter : constant tEventType := 16;
   kEventTypeDllkIdentity : constant tEventType := 17;
   kEventTypeDllkConfig : constant tEventType := 18;
   kEventTypeDllkIssueReq : constant tEventType := 19;
   kEventTypeDllkAddNode : constant tEventType := 20;
   kEventTypeDllkDelNode : constant tEventType := 21;
   kEventTypeDllkConfigNode : constant tEventType := 22;
   kEventTypeDllkStartReducedCycle : constant tEventType := 23;
   kEventTypeNmtMnuNmtCmdSent : constant tEventType := 24;
   kEventTypeApiUserDef : constant tEventType := 25;
   kEventTypeDllkCycleFinish : constant tEventType := 26;
   kEventTypePdokAlloc : constant tEventType := 32;
   kEventTypePdokConfig : constant tEventType := 33;
   kEventTypeNmtMnuNodeCmd : constant tEventType := 34;
   kEventTypeGw309AsciiReq : constant tEventType := 35;
   kEventTypeNmtMnuNodeAdded : constant tEventType := 36;
   kEventTypePdokSetupPdoBuf : constant tEventType := 37;
   kEventTypePdokControlSync : constant tEventType := 38;
   kEventTypeReleaseRxFrame : constant tEventType := 39;
   kEventTypeAsndNotRx : constant tEventType := 40;
   kEventTypeAsndRxInfo : constant tEventType := 41;
   kEventTypeReceivedPres : constant tEventType := 48;
   kEventTypeRequPresForward : constant tEventType := 49;
   kEventTypeSdoAsySend : constant tEventType := 50;  -- ./oplk/event.h:106

  --*
  --\brief  Valid sinks for an event
  --This enumeration defines all valid event sinks.
  -- 

  --/< Sync event for application or kernel POWERLINK module
  --/< events for Nmtk module
  --/< events for Dllk module
  --/< events for DlluCal module
  --/< events for DllkCal module
  --/< events for Pdok module
  --/< events for Nmtu module
  --/< events for Error handler module
  --/< events for Error signaling module
  --/< events for asyncronous SDO Sequence Layer module
  --/< events for NmtMnu module
  --/< events for Ledu module
  --/< events for PdokCal module
  --/< events for GW309ASCII module
  --/< events for API module
  --/< events for SDO testing module
  --/< Identifies an invalid sink
   subtype tEventSink is unsigned;
   kEventSinkSync : constant tEventSink := 0;
   kEventSinkNmtk : constant tEventSink := 1;
   kEventSinkDllk : constant tEventSink := 2;
   kEventSinkDlluCal : constant tEventSink := 3;
   kEventSinkDllkCal : constant tEventSink := 4;
   kEventSinkPdok : constant tEventSink := 5;
   kEventSinkNmtu : constant tEventSink := 6;
   kEventSinkErrk : constant tEventSink := 7;
   kEventSinkErru : constant tEventSink := 8;
   kEventSinkSdoAsySeq : constant tEventSink := 9;
   kEventSinkNmtMnu : constant tEventSink := 10;
   kEventSinkLedu : constant tEventSink := 11;
   kEventSinkPdokCal : constant tEventSink := 12;
   kEventSinkGw309Ascii : constant tEventSink := 14;
   kEventSinkApi : constant tEventSink := 15;
   kEventSinkSdoTest : constant tEventSink := 16;
   kEventSinkInvalid : constant tEventSink := 255;  -- ./oplk/event.h:133

  --*
  --\brief  Valid sources for an event
  --This enumeration defines all valid event sources.
  -- 

  --/< Events from Dllk module
  --/< Events from Nmtk module
  --/< Events from Obdk module
  --/< Events from Pdok module
  --/< Events from Timerk module
  --/< Events from Eventk module
  --/< Events from sync-Cb
  --/< Events from kernel error handler module
  --/< Events from Dllu module
  --/< Events from Nmtu module
  --/< Events from NmtCnu module
  --/< Events from NmtMnu module
  --/< Events from Obdu module
  --/< Events from Sdo/Udp module
  --/< Events from Sdo/Asnd module
  --/< Events from Sdo asynchronous Sequence Layer module
  --/< Events from Sdo command layer module
  --/< Events from Timeru module
  --/< Events from CfgMau module
  --/< Events from Eventu module
  --/< Events from Api module
  --/< Events from Ledu module
  --/< Events from GW309ASCII module
  --/< Events from User Error handler module
  --/< Events from SDO testing module
  --/< Identifies an invalid event source
   subtype tEventSource is unsigned;
   kEventSourceDllk : constant tEventSource := 1;
   kEventSourceNmtk : constant tEventSource := 2;
   kEventSourceObdk : constant tEventSource := 3;
   kEventSourcePdok : constant tEventSource := 4;
   kEventSourceTimerk : constant tEventSource := 5;
   kEventSourceEventk : constant tEventSource := 6;
   kEventSourceSyncCb : constant tEventSource := 7;
   kEventSourceErrk : constant tEventSource := 8;
   kEventSourceDllu : constant tEventSource := 16;
   kEventSourceNmtu : constant tEventSource := 17;
   kEventSourceNmtCnu : constant tEventSource := 18;
   kEventSourceNmtMnu : constant tEventSource := 19;
   kEventSourceObdu : constant tEventSource := 20;
   kEventSourceSdoUdp : constant tEventSource := 21;
   kEventSourceSdoAsnd : constant tEventSource := 22;
   kEventSourceSdoAsySeq : constant tEventSource := 23;
   kEventSourceSdoCom : constant tEventSource := 24;
   kEventSourceTimeru : constant tEventSource := 25;
   kEventSourceCfgMau : constant tEventSource := 26;
   kEventSourceEventu : constant tEventSource := 27;
   kEventSourceOplkApi : constant tEventSource := 28;
   kEventSourceLedu : constant tEventSource := 29;
   kEventSourceGw309Ascii : constant tEventSource := 30;
   kEventSourceErru : constant tEventSource := 31;
   kEventSourceSdoTest : constant tEventSource := 32;
   kEventSourceInvalid : constant tEventSource := 255;  -- ./oplk/event.h:171

  --*
  --\brief Enumerator for queue
  --This enumerator identifies an event queue instance in order to differ between
  --layer queues (kernel-to-user or user-to-kernel) and layer-internal queues
  --(kernel- or user-internal).
  -- 

  --/< Kernel-to-user queue
  --/< Kernel-internal queue
  --/< User-to-kernel queue
  --/< User-internal queue
  --/< Maximum number of queues
   type tEventQueue is 
     (kEventQueueK2U,
      kEventQueueKInt,
      kEventQueueU2K,
      kEventQueueUInt,
      kEventQueueNum);
   pragma Convention (C, tEventQueue);  -- ./oplk/event.h:187

  --*
  --\brief  Structure for events
  --The structure defines an openPOWERLINK event.
  --(element order must not be changed!)
  -- 

  --/< Type of this event
   type tEvent is record
      eventType : aliased tEventType;  -- ./oplk/event.h:197
      eventSink : aliased tEventSink;  -- ./oplk/event.h:198
      netTime : aliased oplkinc_h.tNetTime;  -- ./oplk/event.h:199
      eventArgSize : aliased unsigned;  -- ./oplk/event.h:200
      pEventArg : System.Address;  -- ./oplk/event.h:201
   end record;
   pragma Convention (C_Pass_By_Copy, tEvent);  -- ./oplk/event.h:202

   --  skipped anonymous struct anon_82

  --/< Sink of this event
  --/< Timestamp of the event
  --/< Size of the event argument
  --/< Pointer to event argument
  --*
  --\brief  Structure for OBD error information
  --The structure defines the error event information provided
  --by the obd module.
  -- 

  --/< Index of object
   type tEventObdError is record
      index : aliased unsigned;  -- ./oplk/event.h:212
      subIndex : aliased unsigned;  -- ./oplk/event.h:213
   end record;
   pragma Convention (C_Pass_By_Copy, tEventObdError);  -- ./oplk/event.h:214

   --  skipped anonymous struct anon_83

  --/< Sub index of object
  --*
  --\brief  Structure for error events
  --The structure defines an error event.
  -- 

  --/< Module which posted this error event
   type anon_85 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            byteArg : aliased unsigned_char;  -- ./oplk/event.h:227
         when 1 =>
            uintArg : aliased unsigned;  -- ./oplk/event.h:228
         when 2 =>
            eventSource : aliased tEventSource;  -- ./oplk/event.h:229
         when 3 =>
            obdError : aliased tEventObdError;  -- ./oplk/event.h:230
         when others =>
            eventSink : aliased tEventSink;  -- ./oplk/event.h:231
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_85);
   pragma Unchecked_Union (anon_85);
   type tEventError is record
      eventSource : aliased tEventSource;  -- ./oplk/event.h:223
      oplkError : aliased errordefs_h.tOplkError;  -- ./oplk/event.h:224
      errorArg : anon_85;  -- ./oplk/event.h:232
   end record;
   pragma Convention (C_Pass_By_Copy, tEventError);  -- ./oplk/event.h:233

   --  skipped anonymous struct anon_84

  --/< Error which occurred
  --/< BYTE argument
  --/< UINT32 argument
  --/< Argument from Eventk/u module (originating error source)
  --/< Argument from Obd module
  --/< Argument from Eventk/u module on oplkError == kErrorEventUnknownSink
  --*
  --\brief  Structure for DLL error events
  --The structure defines an DLL error event.
  -- 

  --/< Contains the DLL error (DLL_ERR_*)
   type tEventDllError is record
      dllErrorEvents : aliased unsigned_long;  -- ./oplk/event.h:242
      nodeId : aliased unsigned;  -- ./oplk/event.h:243
      nmtState : aliased nmt_h.tNmtState;  -- ./oplk/event.h:244
      oplkError : aliased errordefs_h.tOplkError;  -- ./oplk/event.h:245
   end record;
   pragma Convention (C_Pass_By_Copy, tEventDllError);  -- ./oplk/event.h:246

   --  skipped anonymous struct anon_86

  --/< Node ID
  --/< NMT state
  --/< openPOWERLINK error code
  --*
  --\brief  Callback function to get informed about sync event
  --\return The function returns a tOplkError error code.
  -- 

   type tSyncCb is access function return errordefs_h.tOplkError;
   pragma Convention (C, tSyncCb);  -- ./oplk/event.h:253

end event_h;

--\brief  Header file for event module
--This file contains definitions for the event module.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Oplk.oplkinc;
with System;
with Oplk.errordefs;
with Oplk.nmt;

package Oplk.event is

   MAX_EVENT_ARG_SIZE : constant := 2048;
   -- max size of event argument                    --  ./oplk/event.h:53
   
------------------------------------------------------------------------------
--\brief  Type of an event                                                  --
--                                                                          --
-- This enumeration defines all valid event types.                          --
------------------------------------------------------------------------------
   subtype tEventType is unsigned;
   kEventTypeNmtEvent              : constant tEventType := 1;
   -- NMT event (arg is pointer to tNmtEvent)
   kEventTypePdoRx                 : constant tEventType := 2;
   -- PDO frame received event (PRes/PReq) (arg is pointer to tFrameInfo)
   kEventTypePdoTx                 : constant tEventType := 3;
   -- PDO frame transmitted event (PRes/PReq) (arg is pointer to tFrameInfo)
   kEventTypePdoSoa                : constant tEventType := 4;
   -- SoA frame received event (isochronous phase completed) 
   -- (arg is pointer to nothing)
   kEventTypeSync                  : constant tEventType := 5;
   -- Sync event (e.g. SoC or anticipated SoC) (arg is pointer to nothing)
   kEventTypeTimer                 : constant tEventType := 6;
   -- Timer event (arg is pointer to tTimerEventArg)
   kEventTypeHeartbeat             : constant tEventType := 7;
   -- Heartbeat event (arg is pointer to tHeartbeatEvent)
   kEventTypeHistoryEntry          : constant tEventType := 8;
   -- Error history entry event (arg is pointer to the tErrHistoryEntry)
   kEventTypeDllkFlag1             : constant tEventType := 9;
   -- DLL kernel Flag 1 changed event (arg is pointer to nothing)
   kEventTypeDllkFillTx            : constant tEventType := 10;
   -- DLL kernel fill TxBuffer event (arg is pointer to tDllAsyncReqPriority)
   kEventTypeDllkPresReady         : constant tEventType := 11;
   -- DLL kernel PRes ready event (arg is pointer to nothing)
   kEventTypeError                 : constant tEventType := 12;
   -- Error event for API layer (arg is pointer to tEventError)
   kEventTypeNmtStateChange        : constant tEventType := 13;
   -- Indicate change of NMT-State (arg is pointer to tEventNmtStateChange)
   kEventTypeDllError              : constant tEventType := 14;
   -- DLL error event for error handler (arg is pointer to tEventDllError)
   kEventTypeAsndRx                : constant tEventType := 15;
   -- received ASnd frame for DLL user module (arg is pointer to tPlkFrame)
   kEventTypeDllkServFilter        : constant tEventType := 16;
   -- configure ServiceIdFilter (arg is pointer to tDllAsndServiceId)
   kEventTypeDllkIdentity          : constant tEventType := 17;
   -- configure Identity (arg is pointer to tDllIdentParam)
   kEventTypeDllkConfig            : constant tEventType := 18;
   -- configure ConfigParam (arg is pointer to tDllConfigParam)
   kEventTypeDllkIssueReq          : constant tEventType := 19;
   -- issue Ident/Status request (arg is pointer to tDllCalIssueRequest)
   kEventTypeDllkAddNode           : constant tEventType := 20;
   -- add node to isochronous phase (arg is pointer to tDllNodeOpParam)
   kEventTypeDllkDelNode           : constant tEventType := 21;
   -- remove node from isochronous phase (arg is pointer to tDllNodeOpParam)
   kEventTypeDllkConfigNode        : constant tEventType := 22;
   -- configures parameters of node (arg is pointer to tDllNodeInfo)
   kEventTypeDllkStartReducedCycle : constant tEventType := 23;
   -- start reduced POWERLINK cycle on MN (arg is pointer to nothing)
   kEventTypeNmtMnuNmtCmdSent      : constant tEventType := 24;
   -- NMT command was actually sent (arg is pointer to tPlkFrame)
   kEventTypeApiUserDef            : constant tEventType := 25;
   -- user-defined event (arg is user-defined pointer)
   kEventTypeDllkCycleFinish       : constant tEventType := 26;
   -- SoA sent, cycle finished (arg is pointer to nothing)
   kEventTypePdokAlloc             : constant tEventType := 32;
   -- alloc PDOs (arg is pointer to tPdoAllocationParam)
   kEventTypePdokConfig            : constant tEventType := 33;
   -- configure PDO channel (arg is pointer to tPdoChannelConf)
   kEventTypeNmtMnuNodeCmd         : constant tEventType := 34;
   -- trigger NMT node command (arg is pointer to tNmtNodeCommand)
   kEventTypeGw309AsciiReq         : constant tEventType := 35;
   -- GW309ASCII request (arg is pointer to pointer of tGw309AsciiRequest)
   kEventTypeNmtMnuNodeAdded       : constant tEventType := 36;
   -- node was added to isochronous phase by DLL 
   --  (arg is pointer to unsigned int containing the node-ID)
   kEventTypePdokSetupPdoBuf       : constant tEventType := 37;
   -- dealloc PDOs
   kEventTypePdokControlSync       : constant tEventType := 38;
   -- enable/disable the pdokcal sync trigger (arg is pointer to BOOL)
   kEventTypeReleaseRxFrame        : constant tEventType := 39;
   -- Free receive buffer (arg is pointer to the buffer to release)
   kEventTypeAsndNotRx             : constant tEventType := 40;
   -- Didn't receive ASnd frame for DLL user module 
   -- (arg is pointer to tDllAsndNotRx)
   kEventTypeAsndRxInfo            : constant tEventType := 41;
   -- Received ASnd frame for DLL user module (arg is pointer to tFrameInfo)
   kEventTypeReceivedPres          : constant tEventType := 48;
   -- Received a PRes frame, which shall be forwarded to application 
   -- (arg is pointer to tEventReceivedPres)
   kEventTypeRequPresForward       : constant tEventType := 49;
   -- Request forwarding of a PRes frame to API layer (e.g. for conformance test)
   kEventTypeSdoAsySend            : constant tEventType := 50;
   -- SDO sequence layer event (for SDO command layer testing module)  
                                                            -- ./oplk/event.h:106
   
   
------------------------------------------------------------------------------
--\brief  Valid sinks for an event                                          --
--                                                                          --
-- This enumeration defines all valid event sinks.                          --
------------------------------------------------------------------------------
   subtype tEventSink is unsigned;
   kEventSinkSync       : constant tEventSink := 0;
   -- Sync event for application or kernel POWERLINK module
   kEventSinkNmtk       : constant tEventSink := 1;
   -- events for Nmtk module
   kEventSinkDllk       : constant tEventSink := 2;
   -- events for Dllk module
   kEventSinkDlluCal    : constant tEventSink := 3;
   -- events for DlluCal module
   kEventSinkDllkCal    : constant tEventSink := 4;
   -- events for DllkCal module
   kEventSinkPdok       : constant tEventSink := 5;
   -- events for Pdok module
   kEventSinkNmtu       : constant tEventSink := 6;
   -- events for Nmtu module
   kEventSinkErrk       : constant tEventSink := 7;
   -- events for Error handler module
   kEventSinkErru       : constant tEventSink := 8;
   -- events for Error signaling module
   kEventSinkSdoAsySeq  : constant tEventSink := 9;
   -- events for asyncronous SDO Sequence Layer module
   kEventSinkNmtMnu     : constant tEventSink := 10;
   -- events for NmtMnu module
   kEventSinkLedu       : constant tEventSink := 11;
   -- events for Ledu module
   kEventSinkPdokCal    : constant tEventSink := 12;
   -- events for PdokCal module
   kEventSinkGw309Ascii : constant tEventSink := 14;
   -- events for GW309ASCII module
   kEventSinkApi        : constant tEventSink := 15;
   -- events for API module
   kEventSinkSdoTest    : constant tEventSink := 16;
   -- events for SDO testing module
   kEventSinkInvalid    : constant tEventSink := 255;
   -- Identifies an invalid sink                        -- ./oplk/event.h:133
   
   
------------------------------------------------------------------------------
--\brief  Valid sources for an event                                        --
--                                                                          --
-- This enumeration defines all valid event sources.                        --
------------------------------------------------------------------------------
   subtype tEventSource is unsigned;
   kEventSourceDllk       : constant tEventSource := 1;
   -- Events from Dllk module
   kEventSourceNmtk       : constant tEventSource := 2;
   -- Events from Nmtk module
   kEventSourceObdk       : constant tEventSource := 3;
   -- Events from Obdk module
   kEventSourcePdok       : constant tEventSource := 4;
   -- Events from Pdok module
   kEventSourceTimerk     : constant tEventSource := 5;
   -- Events from Timerk module
   kEventSourceEventk     : constant tEventSource := 6;
   -- Events from Eventk module
   kEventSourceSyncCb     : constant tEventSource := 7;
   -- Events from sync-Cb
   kEventSourceErrk       : constant tEventSource := 8;
   -- Events from kernel error handler module
   kEventSourceDllu       : constant tEventSource := 16;
   -- Events from Dllu module
   kEventSourceNmtu       : constant tEventSource := 17;
   -- Events from Nmtu module
   kEventSourceNmtCnu     : constant tEventSource := 18;
   -- Events from NmtCnu module
   kEventSourceNmtMnu     : constant tEventSource := 19;
   -- Events from NmtMnu module
   kEventSourceObdu       : constant tEventSource := 20;
   -- Events from Obdu module
   kEventSourceSdoUdp     : constant tEventSource := 21;
   -- Events from Sdo/Udp module
   kEventSourceSdoAsnd    : constant tEventSource := 22;
   -- Events from Sdo/Asnd module
   kEventSourceSdoAsySeq  : constant tEventSource := 23;
   -- Events from Sdo asynchronous Sequence Layer module
   kEventSourceSdoCom     : constant tEventSource := 24;
   -- Events from Sdo command layer module
   kEventSourceTimeru     : constant tEventSource := 25;
   -- Events from Timeru module
   kEventSourceCfgMau     : constant tEventSource := 26;
   -- Events from CfgMau module
   kEventSourceEventu     : constant tEventSource := 27;
   -- Events from Eventu module
   kEventSourceOplkApi    : constant tEventSource := 28;
   -- Events from Api module
   kEventSourceLedu       : constant tEventSource := 29;
   -- Events from Ledu module
   kEventSourceGw309Ascii : constant tEventSource := 30;
   -- Events from GW309ASCII module
   kEventSourceErru       : constant tEventSource := 31;
   -- Events from User Error handler module
   kEventSourceSdoTest    : constant tEventSource := 32;
   -- Events from SDO testing module
   kEventSourceInvalid    : constant tEventSource := 255;
   -- Identifies an invalid event source                 -- ./oplk/event.h:171
   
   
------------------------------------------------------------------------------
--\brief Enumerator for queue                                               --
--                                                                          --
-- This enumerator identifies an event queue instance in order to differ    --
-- between layer queues (kernel-to-user or user-to-kernel) and              --
-- layer-internal queues (kernel- or user-internal).                        --
------------------------------------------------------------------------------
   type tEventQueue is
     (kEventQueueK2U,
      -- Kernel-to-user queue
      kEventQueueKInt,
      -- Kernel-internal queue
      kEventQueueU2K,
      -- User-to-kernel queue
      kEventQueueUInt,
      -- User-internal queue
      KEventQueueNum
      -- Maximum number of queues
     );
   pragma Convention (C, tEventQueue);  -- ./oplk/event.h:187
   

------------------------------------------------------------------------------
--\brief  Structure for events                                              --
--                                                                          --
-- The structure defines an openPOWERLINK event.                            --
--  (element order must not be changed!)                                    --
------------------------------------------------------------------------------
   type tEvent is record
      eventType    : aliased tEventType;
      -- Type of this event
      eventSink    : aliased tEventSink;
      -- Sink of this event
      netTime      : aliased Oplk.oplkinc.tNetTime;
      -- Timestamp of the event
      eventArgSize : aliased unsigned;
      -- Size of the event argument
      pEventArg    : System.Address;
      -- Pointer to event argument
   end record;
   pragma Convention (C_Pass_By_Copy, tEvent);  -- ./oplk/event.h:202
   
   
------------------------------------------------------------------------------
--\brief  Structure for OBD error information                               --
--                                                                          --
-- The structure defines the error event information provided               --
-- by the obd module.                                                       --
------------------------------------------------------------------------------
   type tEventObdError is record
      index    : aliased unsigned;
      -- Index of object
      subIndex : aliased unsigned;
      -- Sub index of object
   end record;
   pragma Convention (C_Pass_By_Copy, tEventObdError);  -- ./oplk/event.h:214
   

------------------------------------------------------------------------------
--\brief  Structure for error events                                        --
--                                                                          --
-- The structure defines an error event.                                    --
------------------------------------------------------------------------------
   type anon_85 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            byteArg : aliased unsigned_char;
	    -- BYTE argument
         when 1 =>
            uintArg : aliased unsigned;
	    -- UINT32 argument
         when 2 =>
            eventSource : aliased tEventSource;
	    -- Argument from Eventk/u module (originating error source)
         when 3 =>
            obdError : aliased tEventObdError;
	    -- Argument from Obd module
         when others =>
            eventSink : aliased tEventSink;
	    -- Argument from Eventk/u module on oplkError == kErrorEventUnknownSink
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_85);
   pragma Unchecked_Union (anon_85);
   type tEventError is record
      eventSource : aliased tEventSource;
      -- Module which posted this error event
      oplkError   : aliased Oplk.errordefs.tOplkError;
      -- Error which occurred
      errorArg    : anon_85;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tEventError);  -- ./oplk/event.h:233
   

------------------------------------------------------------------------------
--\brief  Structure for DLL error events                                    --
--                                                                          --
-- The structure defines an DLL error event.                                --
------------------------------------------------------------------------------
   type tEventDllError is record
      dllErrorEvents : aliased unsigned_long;
      -- Contains the DLL error (DLL_ERR_*)
      nodeId         : aliased unsigned;
      -- Node ID
      nmtState       : aliased Oplk.nmt.tNmtState;
      -- NMT state
      oplkError : aliased Oplk.errordefs.tOplkError;
      -- openPOWERLINK error code
   end record;
   pragma Convention (C_Pass_By_Copy, tEventDllError);  -- ./oplk/event.h:246
   

------------------------------------------------------------------------------
--\brief  Callback function to get informed about sync event                --
--                                                                          --
-- The function returns a tOplkError error code.                            --
------------------------------------------------------------------------------
   type tSyncCb is access function 
			    return Oplk.errordefs.tOplkError;
   pragma Convention (C, tSyncCb);  -- ./oplk/event.h:253

end Oplk.event;

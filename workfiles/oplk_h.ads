pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with nmt_h;
with led_h;
with frame_h;
with stddef_h;
with System;
with event_h;
with sdo_h;
with obd_h;
with cfm_h;
with errordefs_h;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with oplkinc_h;

package oplk_h is

  --*
  --********************************************************************************
  --\file   oplk/oplk.h
  --\brief  Definitions for openPOWERLINK API
  --This file contains all definitions and declarations to use the openPOWERLINK
  --API.
  --****************************************************************************** 

  --------------------------------------------------------------------------------
  --Copyright (c) 2014, Bernecker+Rainer Industrie-Elektronik Ges.m.b.H. (B&R)
  --Copyright (c) 2013, SYSTEC electronic GmbH
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
  -- receive only ASnd frames with local or broadcast node ID
  -- receive any ASnd frame
   type tOplkApiAsndFilter is 
     (tOplkApiAsndFilterNone,
      tOplkApiAsndFilterLocal,
      tOplkApiAsndFilterAny);
   pragma Convention (C, tOplkApiAsndFilter);  -- ./oplk/oplk.h:67

  --*
  --\brief SDO stack
  --The following enum defines the different SDO stacks. The application can
  --switch between the SDO stacks.
  -- 

  -- Use the standard SDO stack (default)
  -- Use testing functions for SDO command layer
  -- Use testing functions for SDO sequence layer
   subtype tOplkApiSdoStack is unsigned;
   tOplkApiStdSdoStack : constant tOplkApiSdoStack := 0;
   tOplkApiTestSdoCom : constant tOplkApiSdoStack := 16;
   tOplkApiTestSdoSeq : constant tOplkApiSdoStack := 32;  -- ./oplk/oplk.h:80

  --*
  --\brief Node event
  --The following structure specifies a node event on an MN. The application will be
  --informed with this event if the state of the specified node has changed.
  -- 

  --/< Node ID of the node that changed the state
   type tOplkApiEventNode is record
      nodeId : aliased unsigned;  -- ./oplk/oplk.h:91
      nmtState : aliased nmt_h.tNmtState;  -- ./oplk/oplk.h:92
      nodeEvent : aliased nmt_h.tNmtNodeEvent;  -- ./oplk/oplk.h:93
      errorCode : aliased unsigned_short;  -- ./oplk/oplk.h:94
      fMandatory : aliased unsigned_char;  -- ./oplk/oplk.h:95
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventNode);  -- ./oplk/oplk.h:96

   --  skipped anonymous struct anon_89

  --/< The NMT state of the CN
  --/< The event that happens at the node
  --/< This variable contains an error code if nodeEvent == \ref kNmtNodeEventError
  --/< Determines if it is a mandatory node (TRUE) or an optional node (FALSE)
  --*
  --\brief Boot event
  --The following structure specifies a boot event. It is used to inform the application
  --about events concerning the entire boot-up process of the MN.
  -- 

  --/< NMT state of the local node
   type tOplkApiEventBoot is record
      nmtState : aliased nmt_h.tNmtState;  -- ./oplk/oplk.h:106
      bootEvent : aliased nmt_h.tNmtBootEvent;  -- ./oplk/oplk.h:107
      errorCode : aliased unsigned_short;  -- ./oplk/oplk.h:108
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventBoot);  -- ./oplk/oplk.h:109

   --  skipped anonymous struct anon_90

  --/< Boot event that occured
  --/< Contains an error code if bootEvent == \ref kNmtBootEventError
  --*
  --\brief LED event
  --This structure specifies a LED event. It contains change events for the POWERLINK
  --status and error LEDs. It allows the application to change the status and error
  --LEDs on the device according to the specification.
  -- 

  --/< Determines the type of the LED
   type tOplkApiEventLed is record
      ledType : aliased led_h.tLedType;  -- ./oplk/oplk.h:121
      fOn : aliased unsigned_char;  -- ./oplk/oplk.h:122
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventLed);  -- ./oplk/oplk.h:123

   --  skipped anonymous struct anon_91

  --/< The state of the LED
  --*
  --\brief CFM result event
  --The structure describes the CFM result event. This includes the normal progress
  --but also errors which occurred during the configuration process.
  --\note It is only valid for an MN.
  -- 

  --/< Node ID of the CN which generated the event
   type tOplkApiEventCfmResult is record
      nodeId : aliased unsigned;  -- ./oplk/oplk.h:135
      nodeCommand : aliased nmt_h.tNmtNodeCommand;  -- ./oplk/oplk.h:136
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventCfmResult);  -- ./oplk/oplk.h:137

   --  skipped anonymous struct anon_92

  --/< Node command which will be issued to the CN as a result of the configuration process. See \ref tNmtNodeCommand
  --*
  --\brief Received ASnd event
  --This structure specifies the event for received ASnd frames. It is used to inform
  --the application about received ASnd frames.
  -- 

  --/< Pointer to the received ASnd frame
   type tOplkApiEventRcvAsnd is record
      pFrame : access frame_h.tPlkFrame;  -- ./oplk/oplk.h:147
      frameSize : aliased stddef_h.size_t;  -- ./oplk/oplk.h:148
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventRcvAsnd);  -- ./oplk/oplk.h:149

   --  skipped anonymous struct anon_93

  --/< Size of the received ASnd frame
  --*
  --\brief PDO changed event
  --This structure specifies the event for PDO changes. It will be sent to the
  --application if the PDO mapping has changed
  -- 

  --/< Determines if mapping is activated.
   type tOplkApiEventPdoChange is record
      fActivated : aliased unsigned_char;  -- ./oplk/oplk.h:159
      fTx : aliased unsigned_char;  -- ./oplk/oplk.h:160
      nodeId : aliased unsigned;  -- ./oplk/oplk.h:161
      mappParamIndex : aliased unsigned;  -- ./oplk/oplk.h:162
      mappObjectCount : aliased unsigned;  -- ./oplk/oplk.h:163
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventPdoChange);  -- ./oplk/oplk.h:164

   --  skipped anonymous struct anon_94

  --/< Determines if it is a TXPDO or RXPDO.
  --/< The node ID the mapping is related to.
  --/< The object index of the mapping parameter object.
  --/< The number of mapped objects (channels).
  --*
  --\brief Received PRes event
  --This structure specifies the event for received PRes frames. It is used to
  --forward requested PRes frames to the application (e.g. for diagnosis).
  -- 

  --/< Node ID of the received PRes frame
   type tOplkApiEventReceivedPres is record
      nodeId : aliased unsigned_short;  -- ./oplk/oplk.h:174
      frameSize : aliased unsigned_short;  -- ./oplk/oplk.h:175
      pFrame : access frame_h.tPlkFrame;  -- ./oplk/oplk.h:176
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventReceivedPres);  -- ./oplk/oplk.h:177

   --  skipped anonymous struct anon_95

  --/< Size of the received PRes frame
  --/< Pointer to the received PRes frame
  --*
  --\brief Received non-POWERLINK Ethernet frame event
  --This structure specifies the event for received Ethernet frames. It is used to
  --inform the application about received Ethernet frames.
  -- 

  --/< Pointer to the received Ethernet frame
   type tOplkApiEventReceivedNonPlk is record
      pFrame : access frame_h.tPlkFrame;  -- ./oplk/oplk.h:187
      frameSize : aliased stddef_h.size_t;  -- ./oplk/oplk.h:188
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventReceivedNonPlk);  -- ./oplk/oplk.h:189

   --  skipped anonymous struct anon_96

  --/< Size of the received Ethernet frame
  --*
  --\brief Default gateway changed event
  --This structure specifies the event for default gateway changed. It is used to
  --inform the application about the changed default gateway address.
  -- 

  --/< Default gateway
   type tOplkApiEventDefaultGwChange is record
      defaultGateway : aliased unsigned;  -- ./oplk/oplk.h:199
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventDefaultGwChange);  -- ./oplk/oplk.h:200

   --  skipped anonymous struct anon_97

  --*
  --\brief SDO command layer receive event
  --This structure specifies the event for an received SDO command layer.
  --It is used to inform the application about the received SDO command layer.
  -- 

  --/< Pointer to the SDO command layer
   type tOplkApiEventReceivedSdoCom is record
      pAsySdoCom : access frame_h.tAsySdoCom;  -- ./oplk/oplk.h:210
      dataSize : aliased unsigned;  -- ./oplk/oplk.h:211
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventReceivedSdoCom);  -- ./oplk/oplk.h:212

   --  skipped anonymous struct anon_98

  --/< Size of the received SDO command layer
  --*
  --\brief SDO sequence layer receive event
  --This structure specifies the event for an received SDO sequence layer.
  --It is used to inform the application about the received SDO sequence layer.
  -- 

  --/< Pointer to the SDO sequence layer
   type tOplkApiEventReceivedSdoSeq is record
      pAsySdoSeq : access frame_h.tAsySdoSeq;  -- ./oplk/oplk.h:222
      dataSize : aliased unsigned;  -- ./oplk/oplk.h:223
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventReceivedSdoSeq);  -- ./oplk/oplk.h:224

   --  skipped anonymous struct anon_99

  --/< Size of the received SDO sequence layer
  --*
  --\brief Application event types
  --This enumeration specifies the valid application events which can be
  --sent by the openPOWERLINK stack.
  -- 

  --* User defined event. It is issued for sending user-defined events. It
  --    can be used for e.g. synchronization purposes. The event argument contains
  --    a pointer to the user specific argument.  

  --* NMT state change event. If \ref kErrorReject is returned, the subsequent
  --    NMT state will not be entered. In this case the application is in charge of
  --    executing the appropriate NMT commands. The event argument contains an NMT
  --    state change event \ref tEventNmtStateChange . 

  --* Critical error event. If this event occurs, the NMT state machine will
  --    be switched off with NMT event \ref kNmtEventCriticalError. The application
  --    may restart the NMT state machine afterwards, but it is unlikely that the
  --    openPOWERLINK stack will run stable, because this critical error or the
  --    source of it often is a configuration error and not a run-time error. The
  --    event argument contains an error event (\ref tEventError).  

  --* Warning event. The warning may be a run-time error, which should be
  --    logged into an error log for further diagnostics. In any case the openPOWERLINK
  --    stack proceeds. The event argument contains an error event. (\ref tEventError)  

  --* New error history event. The event argument contains an error history
  --    entry (\ref tErrHistoryEntry).  

  --* Node event on MN. The state of the specified CN has changed. The event
  --    argument contains the node event information(\ref tOplkApiEventNode).  

  --* Boot event on MN. The MN reached the specified state in the boot-up
  --    process. The event argument contains the boot event information
  --    (\ref tOplkApiEventBoot). 

  --* SDO transfer finished. This event informs about a finished SDO transfer.
  --    The event argument contains the SDO command layer information
  --    (\ref tSdoComFinished).  

  --* Object dictionary access. This event informs about an access of the
  --    object dictionary. The event argument contains an OBD callback parameter
  --    (\ref tObdCbParam).  

  --* Status and error LED event. The event allows the application to perform
  --    the signaling of the POWERLINK LEDs according to the specification. The event
  --    argument contains a LED event (\ref kOplkApiEventLed).  

  --* CFM progress event. This event informs the application about the progress
  --    of the configuration of a specific CN. The event argument contains the CN
  --    progress information (\ref tCfmEventCnProgress).  

  --* CFM result event. This event informs the application about the result
  --    of the configuration of a specific CN. The event argument contains the
  --    CFM result information (\ref tOplkApiEventCfmResult).  

  --* Received ASnd event. This event informs the application about a received
  --    ASnd frame. This event is forwarded only if the application has enabled
  --    the forwarding of ASnd frames by oplk_setAsndForward(). The event argument
  --    contains information on the received ASnd frame (\ref tOplkApiEventRcvAsnd).  

  --* PDO changed event. This event informs the application about a changed
  --    PDO mapping.  

  --* Received PRes event. This event informs the application that a requested
  --    PRes frame was received. It can be used for diagnosis purpose.  

  --* Received Ethernet frame event. This event informs the application about
  --    a received Ethernet frame. The event argument contains information on the
  --    received Ethernet frame (\ref tOplkApiEventReceivedNonPlk).  

  --* Default gateway changed event. This event informs the application about
  --    a changed default gateway. The event argument gives the default gateway
  --    (\ref tOplkApiEventDefaultGwChange). 

  --* Received SDO command layer. This event informs the application about
  --    a received SDO command layer. This event argument contains information on the
  --    received SDO command layer. (\ref tOplkApiEventReceivedSdoCom).  

  --* Received SDO sequence layer. This event informs the application about
  --    a received SDO sequence layer. This event argument contains information on the
  --    received SDO sequence layer. (\ref tOplkApiEventReceivedSdoSeq).  

   subtype tOplkApiEventType is unsigned;
   kOplkApiEventUserDef : constant tOplkApiEventType := 0;
   kOplkApiEventNmtStateChange : constant tOplkApiEventType := 16;
   kOplkApiEventCriticalError : constant tOplkApiEventType := 18;
   kOplkApiEventWarning : constant tOplkApiEventType := 19;
   kOplkApiEventHistoryEntry : constant tOplkApiEventType := 20;
   kOplkApiEventNode : constant tOplkApiEventType := 32;
   kOplkApiEventBoot : constant tOplkApiEventType := 33;
   kOplkApiEventSdo : constant tOplkApiEventType := 98;
   kOplkApiEventObdAccess : constant tOplkApiEventType := 105;
   kOplkApiEventLed : constant tOplkApiEventType := 112;
   kOplkApiEventCfmProgress : constant tOplkApiEventType := 113;
   kOplkApiEventCfmResult : constant tOplkApiEventType := 114;
   kOplkApiEventReceivedAsnd : constant tOplkApiEventType := 115;
   kOplkApiEventPdoChange : constant tOplkApiEventType := 116;
   kOplkApiEventReceivedPres : constant tOplkApiEventType := 128;
   kOplkApiEventReceivedNonPlk : constant tOplkApiEventType := 129;
   kOplkApiEventDefaultGwChange : constant tOplkApiEventType := 130;
   kOplkApiEventReceivedSdoCom : constant tOplkApiEventType := 131;
   kOplkApiEventReceivedSdoSeq : constant tOplkApiEventType := 132;  -- ./oplk/oplk.h:330

  --*
  --\brief Event argument
  --This union specifies all data that can be specified as an event argument.
  --Depending on the event type (\ref tOplkApiEventType) the according member of
  --this union is used.
  -- 

  --/< User argument (\ref kOplkApiEventUserDef)
   type tOplkApiEventArg (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            pUserArg : System.Address;  -- ./oplk/oplk.h:342
         when 1 =>
            nmtStateChange : aliased nmt_h.tEventNmtStateChange;  -- ./oplk/oplk.h:343
         when 2 =>
            internalError : aliased event_h.tEventError;  -- ./oplk/oplk.h:344
         when 3 =>
            sdoInfo : aliased sdo_h.tSdoComFinished;  -- ./oplk/oplk.h:345
         when 4 =>
            obdCbParam : aliased obd_h.tObdCbParam;  -- ./oplk/oplk.h:346
         when 5 =>
            nodeEvent : aliased tOplkApiEventNode;  -- ./oplk/oplk.h:347
         when 6 =>
            bootEvent : aliased tOplkApiEventBoot;  -- ./oplk/oplk.h:348
         when 7 =>
            ledEvent : aliased tOplkApiEventLed;  -- ./oplk/oplk.h:349
         when 8 =>
            cfmProgress : aliased cfm_h.tCfmEventCnProgress;  -- ./oplk/oplk.h:350
         when 9 =>
            cfmResult : aliased tOplkApiEventCfmResult;  -- ./oplk/oplk.h:351
         when 10 =>
            errorHistoryEntry : aliased frame_h.tErrHistoryEntry;  -- ./oplk/oplk.h:352
         when 11 =>
            receivedAsnd : aliased tOplkApiEventRcvAsnd;  -- ./oplk/oplk.h:353
         when 12 =>
            pdoChange : aliased tOplkApiEventPdoChange;  -- ./oplk/oplk.h:354
         when 13 =>
            receivedPres : aliased tOplkApiEventReceivedPres;  -- ./oplk/oplk.h:355
         when 14 =>
            receivedEth : aliased tOplkApiEventReceivedNonPlk;  -- ./oplk/oplk.h:356
         when 15 =>
            defaultGwChange : aliased tOplkApiEventDefaultGwChange;  -- ./oplk/oplk.h:357
         when 16 =>
            receivedSdoCom : aliased tOplkApiEventReceivedSdoCom;  -- ./oplk/oplk.h:358
         when others =>
            receivedSdoSeq : aliased tOplkApiEventReceivedSdoSeq;  -- ./oplk/oplk.h:359
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventArg);
   pragma Unchecked_Union (tOplkApiEventArg);  -- ./oplk/oplk.h:360

   --  skipped anonymous struct anon_101

  --/< NMT state change information (\ref kOplkApiEventNmtStateChange)
  --/< Internal stack error (\ref kOplkApiEventCriticalError, \ref kOplkApiEventWarning)
  --/< SDO information (\ref kOplkApiEventSdo)
  --/< OBD callback parameter (\ref kOplkApiEventObdAccess)
  --/< Node event information (\ref kOplkApiEventNode)
  --/< Boot event information (\ref kOplkApiEventBoot)
  --/< LED event information (\ref kOplkApiEventLed)
  --/< CFM progress information (\ref kOplkApiEventCfmProgress)
  --/< CFM result information (\ref kOplkApiEventCfmResult)
  --/< Error history entry (\ref kOplkApiEventHistoryEntry)
  --/< Received ASnd frame information (\ref kOplkApiEventReceivedAsnd)
  --/< PDO change event (\ref kOplkApiEventPdoChange)
  --/< Received PRes frame (\ref kOplkApiEventReceivedPres)
  --/< Received Ethernet frame (\ref kOplkApiEventReceivedNonPlk)
  --/< Default gateway change event (\ref kOplkApiEventDefaultGwChange)
  --/< Received SDO command layer (\ref kOplkApiEventReceivedSdoCom)
  --/< Received SDO sequence layer (\ref kOplkApiEventReceivedSdoSeq)
  --*
  --\brief Type for API event callback function pointer
  --This type defines a function pointer to an API event callback function.
  --\param eventType_p  The type of the event
  --\param pEventArg_p  Pointer to the event argument
  --\param pUserArg_p   Pointer to the user defined argument
  --\return The function returns a tOplkError error code
  -- 

   type tOplkApiCbEvent is access function
        (arg1 : tOplkApiEventType;
         arg2 : access tOplkApiEventArg;
         arg3 : System.Address) return errordefs_h.tOplkError;
   pragma Convention (C, tOplkApiCbEvent);  -- ./oplk/oplk.h:373

  --*
  --\brief openPOWERLINK initialization parameters
  --The structure defines the openPOWERLINK initialization parameters. The openPOWERLINK
  --stack will be initialized with these parameters when oplk_init() is called. Most
  --of the parameters will be stored in the object dictionary. Some of these objects
  --are constant (read-only) objects and the initialization parameters are the only way of
  --setting their values. Writable objects could be overwritten later at the boot-up
  --process. This could be done by reading a CDC file for an MN or by configuration
  --of a CN from an MN via SDO transfers.
  --\note The elements of the parameter structure must be specified in platform
  --byte order!
  -- 

  --/< This field contains the size of the initialization parameter structure.
   type tOplkApiInitParam_aMacAddress_array is array (0 .. 5) of aliased unsigned_char;
   type tOplkApiInitParam_sHostname_array is array (0 .. 31) of aliased unsigned_char;
   type tOplkApiInitParam_aVendorSpecificExt2_array is array (0 .. 47) of aliased unsigned_char;
   type tOplkApiInitParam is record
      sizeOfInitParam : aliased unsigned;  -- ./oplk/oplk.h:391
      fAsyncOnly : aliased unsigned_char;  -- ./oplk/oplk.h:392
      nodeId : aliased unsigned;  -- ./oplk/oplk.h:393
      aMacAddress : aliased tOplkApiInitParam_aMacAddress_array;  -- ./oplk/oplk.h:394
      featureFlags : aliased unsigned;  -- ./oplk/oplk.h:395
      cycleLen : aliased unsigned;  -- ./oplk/oplk.h:396
      isochrTxMaxPayload : aliased unsigned;  -- ./oplk/oplk.h:397
      isochrRxMaxPayload : aliased unsigned;  -- ./oplk/oplk.h:398
      presMaxLatency : aliased unsigned;  -- ./oplk/oplk.h:399
      preqActPayloadLimit : aliased unsigned;  -- ./oplk/oplk.h:400
      presActPayloadLimit : aliased unsigned;  -- ./oplk/oplk.h:401
      asndMaxLatency : aliased unsigned;  -- ./oplk/oplk.h:402
      multiplCylceCnt : aliased unsigned;  -- ./oplk/oplk.h:403
      asyncMtu : aliased unsigned;  -- ./oplk/oplk.h:404
      prescaler : aliased unsigned;  -- ./oplk/oplk.h:405
      lossOfFrameTolerance : aliased unsigned;  -- ./oplk/oplk.h:406
      waitSocPreq : aliased unsigned;  -- ./oplk/oplk.h:407
      asyncSlotTimeout : aliased unsigned;  -- ./oplk/oplk.h:408
      deviceType : aliased unsigned;  -- ./oplk/oplk.h:409
      vendorId : aliased unsigned;  -- ./oplk/oplk.h:410
      productCode : aliased unsigned;  -- ./oplk/oplk.h:411
      revisionNumber : aliased unsigned;  -- ./oplk/oplk.h:412
      serialNumber : aliased unsigned;  -- ./oplk/oplk.h:413
      vendorSpecificExt1 : aliased Extensions.unsigned_long_long;  -- ./oplk/oplk.h:414
      verifyConfigurationDate : aliased unsigned;  -- ./oplk/oplk.h:415
      verifyConfigurationTime : aliased unsigned;  -- ./oplk/oplk.h:416
      applicationSwDate : aliased unsigned;  -- ./oplk/oplk.h:417
      applicationSwTime : aliased unsigned;  -- ./oplk/oplk.h:418
      ipAddress : aliased unsigned;  -- ./oplk/oplk.h:419
      subnetMask : aliased unsigned;  -- ./oplk/oplk.h:420
      defaultGateway : aliased unsigned;  -- ./oplk/oplk.h:421
      sHostname : aliased tOplkApiInitParam_sHostname_array;  -- ./oplk/oplk.h:422
      aVendorSpecificExt2 : aliased tOplkApiInitParam_aVendorSpecificExt2_array;  -- ./oplk/oplk.h:423
      pDevName : Interfaces.C.Strings.chars_ptr;  -- ./oplk/oplk.h:424
      pHwVersion : Interfaces.C.Strings.chars_ptr;  -- ./oplk/oplk.h:425
      pSwVersion : Interfaces.C.Strings.chars_ptr;  -- ./oplk/oplk.h:426
      pfnCbEvent : tOplkApiCbEvent;  -- ./oplk/oplk.h:427
      pEventUserArg : System.Address;  -- ./oplk/oplk.h:428
      pfnCbSync : event_h.tSyncCb;  -- ./oplk/oplk.h:429
      hwParam : aliased oplkinc_h.tHwParam;  -- ./oplk/oplk.h:434
      syncResLatency : aliased unsigned;  -- ./oplk/oplk.h:435
      syncNodeId : aliased unsigned;  -- ./oplk/oplk.h:436
      fSyncOnPrcNode : aliased unsigned_char;  -- ./oplk/oplk.h:437
      sdoStackType : aliased tOplkApiSdoStack;  -- ./oplk/oplk.h:438
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiInitParam);  -- ./oplk/oplk.h:441

   --  skipped anonymous struct anon_102

  --/< Determines if this node is an async-only node. If TRUE the node communicates only asynchronously.
  --/< The node ID of this node.
  --/< The MAC address of this node.
  --/< The POWERLINK feature flags of this node (0x1F82: NMT_FeatureFlags_U32)
  --/< The cycle Length (0x1006: NMT_CycleLen_U32) in [us]
  --/< Maximum isochronous transmit payload (0x1F98.1: IsochrTxMaxPayload_U16) Const!
  --/< Maximum isochronous receive payload (0x1F98.2: IsochrRxMaxPayload_U16) Const!
  --/< Maximum PRes latency in ns (0x1F98.3: PResMaxLatency_U32) Read-only!
  --/< Actual PReq payload limit (0x1F98.4: PReqActPayloadLimit_U16)
  --/< Actual PRes payload limit (0x1F98.5: PResActPayloadLimit_U16)
  --/< Maximum ASnd latency in ns (0x1F98.6: ASndMaxLatency_U32) Const!
  --/< Multiplexed cycle count (0x1F98.7: MultiplCycleCnt_U8)
  --/< Asynchronous MTU (0x1F98.8: AsyncMTU_U16)
  --/< SoC prescaler (0x1F98.9: Prescaler_U16)
  --/< Loss of frame tolerance in ns (0x1C14: DLL_LossOfFrameTolerance_U32)
  --/< Wait time for first PReq in ns (0x1F8A.1: WaitSoCPReq_U32) Only for MN!
  --/< Asynchronous slot timeout in ns (0x1F8A.2: AsyncSlotTimeout_U32) Only for MN!
  --/< The device type of this node (0x1000.0: NMT_DeviceType_U32) Const!
  --/< The vendor ID of this node (0x1018.1: NMT_IdentityObject_REC.VendorId_U32) Const!
  --/< The product code of this node (0x1018.2: NMT_IdentityObject_REC.ProductCode_U32) Const!
  --/< The revision number of this node (0x1018.3: NMT_IdentityObject_REC.RevisionNo_U32) Const!
  --/< The serial number of this node (0x1018.4: NMT_IdentityObject_REC.SerialNo_U32) Const!
  --/< Vendor specific extensions 1 listed in the IdentResponse frame
  --/< Local configuration date (0x1020.1 CFM_VerifyConfiguration_REC.ConfDate_U32)
  --/< Local configuration time (0x1020.2 CFM_VerifyConfiguration_REC.ConfTime_U32)
  --/< Local program Date (0x1F52.1 PDL_LocVerApplSw_REC.ApplSwDate_U32)
  --/< Local program Time (0x1F52.2 PDL_LocVerApplSw_REC.ApplSwTime_U32)
  --/< IP address of the node
  --/< Subnet mask of the node
  --/< Default gateway used by this node
  --/< DNS host name of the node (maximum length: 32 characters!)
  --/< Vendor specific extensions 2 listed in the IdentResponse frame
  --/< Pointer to manufacturer device name (0x1008.0: NMT_ManufactDevName_VS) Const!
  --/< Pointer to manufacturer hardware version (0x1009.0: NMT_ManufactHwVers_VS) Const!
  --/< Pointer to manufacturer software version (0x100A.0: NMT_ManufactSwVers_VS) Const!
  --/< Pointer to the applications event handling function.
  --/< Pointer to a user argument that is supplied to the event callback function (\ref tOplkApiCbEvent)
  --/< Pointer to the application sync callback function.
  --*< It is used for single process systems where the whole openPOWERLINK stack is linked to the application.
  --                                                         In this case the stack calls the provided application callback function when synchronous data can be
  --                                                         exchanged. If a split stack is used (e.g. Linux user/kernel) it must be initialized with NULL. In
  --                                                         this case the application must use oplk_waitSyncEvent() for waiting on synchronous data.  

  --/< The hardware parameters of the node
  --/< Constant response latency for SyncRes in ns
  --/< Specifies the synchronization point for the MN. The synchronization take place after a PRes from a CN with this node-ID (0 = SoC, 255 = SoA)
  --/< If it is TRUE, Sync on PRes chained CN; FALSE: conventional CN (PReq/PRes)
  --/< Specifies the SDO stack that should be used.
  --*< It is used for switching between the standard SDO stack and alternative SDO stacks. The available SDO stacks are defined by the \ref tOplkApiSdoStack enumeration.
  --                                                         If the standard SDO stack is used it must be initialized with 0x00. 

  --*
  --\brief  Process image information structure
  --This structure provides information about a process image.
  -- 

  --/< Pointer to the process image
   type tOplkApiProcessImage is record
      pImage : System.Address;  -- ./oplk/oplk.h:450
      imageSize : aliased unsigned;  -- ./oplk/oplk.h:451
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiProcessImage);  -- ./oplk/oplk.h:452

   --  skipped anonymous struct anon_103

  --/< Size of the process image
  --------------------------------------------------------------------------------
  -- function prototypes
  --------------------------------------------------------------------------------
  -- Generic API functions
   function oplk_init (pInitParam_p : access tOplkApiInitParam) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:462
   pragma Import (C, oplk_init, "oplk_init");

   function oplk_shutdown return errordefs_h.tOplkError;  -- ./oplk/oplk.h:463
   pragma Import (C, oplk_shutdown, "oplk_shutdown");

   function oplk_execNmtCommand (NmtEvent_p : nmt_h.tNmtEvent) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:464
   pragma Import (C, oplk_execNmtCommand, "oplk_execNmtCommand");

   function oplk_linkObject
     (objIndex_p : unsigned;
      pVar_p : System.Address;
      pVarEntries_p : access unsigned;
      pEntrySize_p : access obd_h.tObdSize;
      firstSubindex_p : unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:465
   pragma Import (C, oplk_linkObject, "oplk_linkObject");

   function oplk_readObject
     (pSdoComConHdl_p : access sdo_h.tSdoComConHdl;
      nodeId_p : unsigned;
      index_p : unsigned;
      subindex_p : unsigned;
      pDstData_le_p : System.Address;
      pSize_p : access unsigned;
      sdoType_p : sdo_h.tSdoType;
      pUserArg_p : System.Address) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:467
   pragma Import (C, oplk_readObject, "oplk_readObject");

   function oplk_writeObject
     (pSdoComConHdl_p : access sdo_h.tSdoComConHdl;
      nodeId_p : unsigned;
      index_p : unsigned;
      subindex_p : unsigned;
      pSrcData_le_p : System.Address;
      size_p : unsigned;
      sdoType_p : sdo_h.tSdoType;
      pUserArg_p : System.Address) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:470
   pragma Import (C, oplk_writeObject, "oplk_writeObject");

   function oplk_freeSdoChannel (sdoComConHdl_p : sdo_h.tSdoComConHdl) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:473
   pragma Import (C, oplk_freeSdoChannel, "oplk_freeSdoChannel");

   function oplk_abortSdo (sdoComConHdl_p : sdo_h.tSdoComConHdl; abortCode_p : unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:474
   pragma Import (C, oplk_abortSdo, "oplk_abortSdo");

   function oplk_readLocalObject
     (index_p : unsigned;
      subindex_p : unsigned;
      pDstData_p : System.Address;
      pSize_p : access unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:475
   pragma Import (C, oplk_readLocalObject, "oplk_readLocalObject");

   function oplk_writeLocalObject
     (index_p : unsigned;
      subindex_p : unsigned;
      pSrcData_p : System.Address;
      size_p : unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:476
   pragma Import (C, oplk_writeLocalObject, "oplk_writeLocalObject");

   function oplk_sendAsndFrame
     (dstNodeId_p : unsigned_char;
      pAsndFrame_p : access frame_h.tAsndFrame;
      asndSize_p : stddef_h.size_t) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:477
   pragma Import (C, oplk_sendAsndFrame, "oplk_sendAsndFrame");

   function oplk_sendEthFrame (pFrame_p : access frame_h.tPlkFrame; frameSize_p : unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:478
   pragma Import (C, oplk_sendEthFrame, "oplk_sendEthFrame");

   function oplk_setAsndForward (serviceId_p : unsigned_char; FilterType_p : tOplkApiAsndFilter) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:479
   pragma Import (C, oplk_setAsndForward, "oplk_setAsndForward");

   function oplk_setNonPlkForward (fEnable_p : unsigned_char) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:480
   pragma Import (C, oplk_setNonPlkForward, "oplk_setNonPlkForward");

   function oplk_postUserEvent (pUserArg_p : System.Address) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:481
   pragma Import (C, oplk_postUserEvent, "oplk_postUserEvent");

   function oplk_triggerMnStateChange (nodeId_p : unsigned; nodeCommand_p : nmt_h.tNmtNodeCommand) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:482
   pragma Import (C, oplk_triggerMnStateChange, "oplk_triggerMnStateChange");

   function oplk_setCdcBuffer (pbCdc_p : access unsigned_char; cdcSize_p : unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:483
   pragma Import (C, oplk_setCdcBuffer, "oplk_setCdcBuffer");

   function oplk_setCdcFilename (pszCdcFilename_p : Interfaces.C.Strings.chars_ptr) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:484
   pragma Import (C, oplk_setCdcFilename, "oplk_setCdcFilename");

   function oplk_process return errordefs_h.tOplkError;  -- ./oplk/oplk.h:485
   pragma Import (C, oplk_process, "oplk_process");

   function oplk_getIdentResponse (nodeId_p : unsigned; ppIdentResponse_p : System.Address) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:486
   pragma Import (C, oplk_getIdentResponse, "oplk_getIdentResponse");

   function oplk_getEthMacAddr (pMacAddr_p : access unsigned_char) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:487
   pragma Import (C, oplk_getEthMacAddr, "oplk_getEthMacAddr");

   function oplk_checkKernelStack return unsigned_char;  -- ./oplk/oplk.h:488
   pragma Import (C, oplk_checkKernelStack, "oplk_checkKernelStack");

   function oplk_waitSyncEvent (timeout_p : unsigned_long) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:489
   pragma Import (C, oplk_waitSyncEvent, "oplk_waitSyncEvent");

  -- Process image API functions
   function oplk_allocProcessImage (sizeProcessImageIn_p : unsigned; sizeProcessImageOut_p : unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:492
   pragma Import (C, oplk_allocProcessImage, "oplk_allocProcessImage");

   function oplk_freeProcessImage return errordefs_h.tOplkError;  -- ./oplk/oplk.h:493
   pragma Import (C, oplk_freeProcessImage, "oplk_freeProcessImage");

   function oplk_linkProcessImageObject
     (objIndex_p : unsigned;
      firstSubindex_p : unsigned;
      offsetPI_p : unsigned;
      fOutputPI_p : unsigned_char;
      entrySize_p : obd_h.tObdSize;
      pVarEntries_p : access unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:494
   pragma Import (C, oplk_linkProcessImageObject, "oplk_linkProcessImageObject");

   function oplk_exchangeProcessImageIn return errordefs_h.tOplkError;  -- ./oplk/oplk.h:496
   pragma Import (C, oplk_exchangeProcessImageIn, "oplk_exchangeProcessImageIn");

   function oplk_exchangeProcessImageOut return errordefs_h.tOplkError;  -- ./oplk/oplk.h:497
   pragma Import (C, oplk_exchangeProcessImageOut, "oplk_exchangeProcessImageOut");

   function oplk_getProcessImageIn return System.Address;  -- ./oplk/oplk.h:498
   pragma Import (C, oplk_getProcessImageIn, "oplk_getProcessImageIn");

   function oplk_getProcessImageOut return System.Address;  -- ./oplk/oplk.h:499
   pragma Import (C, oplk_getProcessImageOut, "oplk_getProcessImageOut");

  -- objdict specific process image functions
   function oplk_setupProcessImage return errordefs_h.tOplkError;  -- ./oplk/oplk.h:502
   pragma Import (C, oplk_setupProcessImage, "oplk_setupProcessImage");

  -- Request forwarding of Pres frame from DLL -> API
   function oplk_triggerPresForward (nodeId_p : unsigned) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:505
   pragma Import (C, oplk_triggerPresForward, "oplk_triggerPresForward");

  -- SDO Test Api functions
   procedure oplk_testSdoSetVal (pInitParam_p : access tOplkApiInitParam);  -- ./oplk/oplk.h:508
   pragma Import (C, oplk_testSdoSetVal, "oplk_testSdoSetVal");

   function oplk_testSdoComInit return errordefs_h.tOplkError;  -- ./oplk/oplk.h:509
   pragma Import (C, oplk_testSdoComInit, "oplk_testSdoComInit");

   function oplk_testSdoSeqInit return errordefs_h.tOplkError;  -- ./oplk/oplk.h:510
   pragma Import (C, oplk_testSdoSeqInit, "oplk_testSdoSeqInit");

  -- Testing functions for SDO command layer
   function oplk_testSdoComSend
     (uiNodeId_p : unsigned;
      SdoType_p : sdo_h.tSdoType;
      pSdoCom_p : access frame_h.tAsySdoCom;
      SdoSize_p : stddef_h.size_t) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:512
   pragma Import (C, oplk_testSdoComSend, "oplk_testSdoComSend");

   function oplk_testSdoComDelCon return errordefs_h.tOplkError;  -- ./oplk/oplk.h:514
   pragma Import (C, oplk_testSdoComDelCon, "oplk_testSdoComDelCon");

  -- Testing functions for SDO sequence layer
   function oplk_testSdoSeqSend
     (uiNodeId_p : unsigned;
      SdoType_p : sdo_h.tSdoType;
      pSdoCom_p : access frame_h.tAsySdoSeq;
      SdoSize_p : stddef_h.size_t) return errordefs_h.tOplkError;  -- ./oplk/oplk.h:516
   pragma Import (C, oplk_testSdoSeqSend, "oplk_testSdoSeqSend");

   function oplk_testSdoSeqDelCon return errordefs_h.tOplkError;  -- ./oplk/oplk.h:518
   pragma Import (C, oplk_testSdoSeqDelCon, "oplk_testSdoSeqDelCon");

end oplk_h;

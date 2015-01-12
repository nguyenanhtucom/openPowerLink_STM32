
--  Definitions for openPOWERLINK API
-- This file contains all definitions and declarations to use the openPOWERLINK API.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Oplk.nmt;
with Oplk.led;
with Oplk.frame;
with System;
with Oplk.event;
with Oplk.sdo;
with Oplk.obd;
with Oplk.cfm;
with Oplk.errordefs;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with Oplk.oplkinc;

package Oplk.Oplk is

   type tOplkApiAsndFilter is
     (tOplkApiAsndFilterNone, 
      -- 
      tOplkApiAsndFilterLocal,
      -- receive only ASnd frames with local or broadcast node ID
      tOplkApiAsndFilterAny
      -- receive any ASnd frame
   );
   pragma Convention (C, tOplkApiAsndFilter);  -- oplk/oplk.h:67
   
   
------------------------------------------------------------------------------
--  SDO stack                                                               --
-- The following enum defines the different SDO stacks. The application can --
-- switch between the SDO stacks.                                           --
------------------------------------------------------------------------------
   subtype tOplkApiSdoStack is unsigned;
   tOplkApiStdSdoStack : constant tOplkApiSdoStack := 0;
   -- Use the standard SDO stack (default)
   tOplkApiTestSdoCom  : constant tOplkApiSdoStack := 16;
   -- Use testing functions for SDO command layer
   tOplkApiTestSdoSeq  : constant tOplkApiSdoStack := 32;
   -- Use testing functions for SDO sequence layer  -- oplk/oplk.h:80
   
   
------------------------------------------------------------------------------
--\brief Node event                                                         --
-- The following structure specifies a node event on an MN.                 --
-- The application will be                                                  --
-- informed with this event if the state of the specified node has changed. --
------------------------------------------------------------------------------
   type tOplkApiEventNode is record
      NodeId     : aliased unsigned;
      -- Node ID of the node that changed the state
      NmtState   : aliased Nmt.tNmtState;
      -- The NMT state of the CN
      NodeEvent  : aliased nmt.tNmtNodeEvent;
      -- The event that happens at the node
      ErrorCode  : aliased unsigned_short;
      -- This variable contains an error code if nodeEvent = kNmtNodeEventError
      fMandatory : aliased unsigned_char;
      -- Determines if it is a mandatory node (TRUE) or an optional node (FALSE)
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventNode);  -- oplk/oplk.h:96
   
   
------------------------------------------------------------------------------
--\brief Boot event                                                         --
-- The following structure specifies a boot event.                          --
-- It is used to inform the application                                     --
-- about events concerning the entire boot-up process of the MN.            --
------------------------------------------------------------------------------
   type tOplkApiEventBoot is record
      nmtState  : aliased nmt.tNmtState;
      -- NMT state of the local node
      bootEvent : aliased nmt.tNmtBootEvent;
      -- Boot event that occured
      errorCode : aliased unsigned_short;
      -- Contains an error code if bootEvent = kNmtBootEventError
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventBoot);  -- oplk/oplk.h:109
   
   
------------------------------------------------------------------------------
--\brief LED event                                                          --
-- This structure specifies a LED event.                                    --
-- It contains change events for the POWERLINK status and error LEDs.       --
-- It allows the application to change the status and error LEDs            --
-- on the device according to the specification.                            --
------------------------------------------------------------------------------
   type tOplkApiEventLed is record
      ledType : aliased led.tLedType;
      -- Determines the type of the LED
      fOn     : aliased unsigned_char;
      -- The state of the LED
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventLed);  -- oplk/oplk.h:123
   
   
------------------------------------------------------------------------------
--\brief CFM result event                                                   --
-- The structure describes the CFM result event.                            --
-- This includes the normal progress but also errors which occurred during  --
-- the configuration process.                                               --
-- It is only valid for an MN.                                              --
------------------------------------------------------------------------------
   type tOplkApiEventCfmResult is record
      nodeId      : aliased unsigned;
      -- Node ID of the CN which generated the event
      nodeCommand : aliased nmt.tNmtNodeCommand;
      -- Node command which will be issued to the CN as a result of
      -- the configuration process. See \ref tNmtNodeCommand
   end record;
   pragma Convention
     (C_Pass_By_Copy, tOplkApiEventCfmResult);  -- oplk/oplk.h:137
   
   
------------------------------------------------------------------------------
--\brief Received ASnd event                                                --
-- This structure specifies the event for received ASnd frames.             --
-- It is used to inform the application about received ASnd frames.         --
------------------------------------------------------------------------------
   type tOplkApiEventRcvAsnd is record
      PFrame    : access frame.tPlkFrame;
      -- Pointer to the received ASnd frame
      frameSize : aliased size_t;
      -- Size of the received ASnd frame
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      tOplkApiEventRcvAsnd);  -- oplk/oplk.h:149
   
   
------------------------------------------------------------------------------
--\brief PDO changed event                                                  --
-- This structure specifies the event for PDO changes.                      --
-- It will be sent to the application if the PDO mapping has changed        --
------------------------------------------------------------------------------
   type tOplkApiEventPdoChange is record
      fActivated      : aliased unsigned_char;
      -- Determines if mapping is activated.
      fTx             : aliased unsigned_char;
      -- Determines if it is a TXPDO or RXPDO.
      nodeId          : aliased unsigned;
      -- The node ID the mapping is related to.
      mappParamIndex  : aliased unsigned;
      -- The object index of the mapping parameter object.
      mappObjectCount : aliased unsigned;
      -- The number of mapped objects (channels).
   end record;
   pragma Convention
     (C_Pass_By_Copy, tOplkApiEventPdoChange);  -- oplk/oplk.h:164
   
   
------------------------------------------------------------------------------
--\brief Received PRes event                                                --
-- This structure specifies the event for received PRes frames.             --
-- It is used to forward requested PRes frames to the application           --
-- (e.g. for diagnosis).                                                    --
------------------------------------------------------------------------------
   type tOplkApiEventReceivedPres is record
      nodeId    : aliased unsigned_short;
      -- Node ID of the received PRes frame
      frameSize : aliased unsigned_short;
      -- Size of the received PRes frame
      PFrame    : access frame.tPlkFrame;
      -- Pointer to the received PRes frame
   end record;
   pragma Convention
     (C_Pass_By_Copy, tOplkApiEventReceivedPres);  -- oplk/oplk.h:177
   
   
------------------------------------------------------------------------------
--\brief Received non-POWERLINK Ethernet frame event                        --
-- This structure specifies the event for received Ethernet frames.         --
-- It is used to inform the application about received Ethernet frames.     --
------------------------------------------------------------------------------
   type tOplkApiEventReceivedNonPlk is record
      pFrame    : access frame.tPlkFrame;
      -- Pointer to the received Ethernet frame
      frameSize : aliased size_t;
      -- Size of the received Ethernet frame
   end record;
   pragma Convention
     (C_Pass_By_Copy, tOplkApiEventReceivedNonPlk); --oplk/oplk.h:189
   
   
------------------------------------------------------------------------------
--\brief Default gateway changed event                                      --
-- This structure specifies the event for default gateway changed.          --
-- It is used to inform the application about the changed default           --
-- gateway address.                                                         --
------------------------------------------------------------------------------
   type tOplkApiEventDefaultGwChange is record
      defaultGateway : aliased unsigned;
      -- Default gateway
   end record;
   pragma Convention
     (C_Pass_By_Copy, tOplkApiEventDefaultGwChange);--oplk/oplk.h:200
   
   
------------------------------------------------------------------------------
--\brief SDO command layer receive event                                    --
-- This structure specifies the event for an received SDO command layer.    --
-- It is used to inform the application about the received                  --
-- SDO command layer.                                                       --
------------------------------------------------------------------------------
   type tOplkApiEventReceivedSdoCom is record
      pAsySdoCom : access frame.tAsySdoCom;
      -- Pointer to the SDO command layer
      dataSize   : aliased unsigned;
      -- Size of the received SDO command layer
   end record;
   pragma Convention
     (C_Pass_By_Copy, tOplkApiEventReceivedSdoCom); --oplk/oplk.h:212
   
   
------------------------------------------------------------------------------
--\brief SDO sequence layer receive event                                   --
-- This structure specifies the event for an received SDO sequence layer.   --
-- It is used to inform the application about the received                  --
-- SDO sequence layer.                                                      --
------------------------------------------------------------------------------
   type tOplkApiEventReceivedSdoSeq is record
      pAsySdoSeq : access frame.tAsySdoSeq;
      -- Pointer to the SDO sequence layer
      DataSize   : aliased unsigned;
      -- Size of the received SDO sequence layer
   end record;
   pragma Convention
     (C_Pass_By_Copy, tOplkApiEventReceivedSdoSeq); --oplk/oplk.h:224
   
   
------------------------------------------------------------------------------
--\brief Application event types                                            --
-- This enumeration specifies the valid application events which can be     --
-- sent by the openPOWERLINK stack.                                         --
------------------------------------------------------------------------------
   subtype tOplkApiEventType is unsigned;
   KOplkApiEventUserDef         : constant tOplkApiEventType := 0;
   -- User defined event. It is issued for sending user-defined events. It
   -- can be used for e.g. synchronization purposes. The event argument contains
   -- a pointer to the user specific argument.
   KOplkApiEventNmtStateChange  : constant tOplkApiEventType := 16;
   -- NMT state change event. If kErrorReject is returned, the subsequent
   -- NMT state will not be entered. In this case the application is in charge of
   -- executing the appropriate NMT commands. The event argument contains an NMT
   -- state change event tEventNmtStateChange .
   KOplkApiEventCriticalError   : constant tOplkApiEventType := 18;
   -- Critical error event. If this event occurs, the NMT state machine will
   -- be switched off with NMT event \ref kNmtEventCriticalError. The application
   -- may restart the NMT state machine afterwards, but it is unlikely that the
   -- openPOWERLINK stack will run stable, because this critical error or the
   -- source of it often is a configuration error and not a run-time error. The
   -- event argument contains an error event (\ref tEventError).
   KOplkApiEventWarning         : constant tOplkApiEventType := 19;
   -- Warning event. The warning may be a run-time error, which should be
   -- logged into an error log for further diagnostics. In any case the openPOWERLINK
   -- stack proceeds. The event argument contains an error event. (\ref tEventError)
   KOplkApiEventHistoryEntry    : constant tOplkApiEventType := 20;
   -- New error history event. The event argument contains an error history
   -- entry (\ref tErrHistoryEntry).
   kOplkApiEventNode            : constant tOplkApiEventType := 32;
   -- Node event on MN. The state of the specified CN has changed. The event
   -- argument contains the node event information(\ref tOplkApiEventNode).
   kOplkApiEventBoot            : constant tOplkApiEventType := 33;
   -- Boot event on MN. The MN reached the specified state in the boot-up
   -- process. The event argument contains the boot event information
   -- (\ref tOplkApiEventBoot).
   kOplkApiEventSdo             : constant tOplkApiEventType := 98;
   -- SDO transfer finished. This event informs about a finished SDO transfer.
   -- The event argument contains the SDO command layer information
   -- (\ref tSdoComFinished).
   kOplkApiEventObdAccess       : constant tOplkApiEventType := 105;
   -- Object dictionary access. This event informs about an access of the
   -- object dictionary. The event argument contains an OBD callback parameter
   -- (\ref tObdCbParam).
   kOplkApiEventLed             : constant tOplkApiEventType := 112;
   -- Status and error LED event. The event allows the application to perform
   -- the signaling of the POWERLINK LEDs according to the specification. The event
   -- argument contains a LED event (\ref kOplkApiEventLed).
   kOplkApiEventCfmProgress     : constant tOplkApiEventType := 113;
   -- CFM progress event. This event informs the application about the progress
   -- of the configuration of a specific CN. The event argument contains the CN
   -- progress information (\ref tCfmEventCnProgress).
   kOplkApiEventCfmResult       : constant tOplkApiEventType := 114;
   -- CFM result event. This event informs the application about the result
   -- of the configuration of a specific CN. The event argument contains the
   -- CFM result information (\ref tOplkApiEventCfmResult).
   kOplkApiEventReceivedAsnd    : constant tOplkApiEventType := 115;
   -- Received ASnd event. This event informs the application about a received
   -- ASnd frame. This event is forwarded only if the application has enabled
   -- the forwarding of ASnd frames by oplk_setAsndForward(). The event argument
   -- contains information on the received ASnd frame (\ref tOplkApiEventRcvAsnd).
   kOplkApiEventPdoChange       : constant tOplkApiEventType := 116;
   -- PDO changed event. This event informs the application about a changed
   -- PDO mapping.
   KOplkApiEventReceivedPres    : constant tOplkApiEventType := 128;
   -- Received PRes event. This event informs the application that a requested
   -- PRes frame was received. It can be used for diagnosis purpose.
   kOplkApiEventReceivedNonPlk  : constant tOplkApiEventType := 129;
   -- Received Ethernet frame event. This event informs the application about
   -- a received Ethernet frame. The event argument contains information on the
   -- received Ethernet frame (\ref tOplkApiEventReceivedNonPlk).
   kOplkApiEventDefaultGwChange : constant tOplkApiEventType := 130;
   -- Default gateway changed event. This event informs the application about
   -- a changed default gateway. The event argument gives the default gateway
   -- (\ref tOplkApiEventDefaultGwChange).
   KOplkApiEventReceivedSdoCom  : constant tOplkApiEventType := 131;
   -- Received SDO command layer. This event informs the application about
   -- a received SDO command layer. This event argument contains information on the
   -- received SDO command layer. (\ref tOplkApiEventReceivedSdoCom).
   kOplkApiEventReceivedSdoSeq  : constant tOplkApiEventType := 132;
   -- Received SDO sequence layer. This event informs the application about
   -- a received SDO sequence layer. This event argument contains information on the
   -- received SDO sequence layer. (\ref tOplkApiEventReceivedSdoSeq).
                                                          --   oplk/oplk.h:330
   
   
------------------------------------------------------------------------------
--\brief Event argument                                                     --
-- This union specifies all data that can be specified as an event argument.--
-- Depending on the event type (\ref tOplkApiEventType) the according member--
-- of this union is used.                                                   --
------------------------------------------------------------------------------
   type tOplkApiEventArg (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            pUserArg : System.Address;
	    -- User argument (\ref kOplkApiEventUserDef)
         when 1 =>
            nmtStateChange : aliased nmt.tEventNmtStateChange;
	    -- NMT state change information (\ref kOplkApiEventNmtStateChange)
         when 2 =>
            internalError     : aliased event.tEventError;
	    -- Internal stack error
	    -- (\ref kOplkApiEventCriticalError, \ref kOplkApiEventWarning)
         when 3 =>
            sdoInfo           : aliased sdo.tSdoComFinished;
	    -- SDO information (\ref kOplkApiEventSdo)
         when 4 =>
            obdCbParam        : aliased obd.tObdCbParam;
	    -- OBD callback parameter (\ref kOplkApiEventObdAccess)
         when 5 =>
            nodeEvent         : aliased tOplkApiEventNode;
	    -- Node event information (\ref kOplkApiEventNode)
         when 6 =>
            bootEvent         : aliased tOplkApiEventBoot;
	    -- Boot event information (\ref kOplkApiEventBoot)
         when 7 =>
            ledEvent          : aliased tOplkApiEventLed;
	    -- LED event information (\ref kOplkApiEventLed)
         when 8 =>
            cfmProgress       : aliased cfm.tCfmEventCnProgress;
	    -- CFM progress information (\ref kOplkApiEventCfmProgress)
         when 9 =>
            cfmResult         : aliased tOplkApiEventCfmResult;
	    -- CFM result information (\ref kOplkApiEventCfmResult)
         when 10 =>
            errorHistoryEntry : aliased frame.tErrHistoryEntry;
	    -- Error history entry (\ref kOplkApiEventHistoryEntry)
         when 11 =>
            receivedAsnd      : aliased tOplkApiEventRcvAsnd;
	    -- Received ASnd frame information (\ref kOplkApiEventReceivedAsnd)
         when 12 =>
            pdoChange         : aliased tOplkApiEventPdoChange;
	    -- PDO change event (\ref kOplkApiEventPdoChange)
         when 13 =>
            receivedPres      : aliased tOplkApiEventReceivedPres;
	    -- Received PRes frame (\ref kOplkApiEventReceivedPres)
         when 14 =>
            receivedEth       : aliased tOplkApiEventReceivedNonPlk;
	    -- Received Ethernet frame (\ref kOplkApiEventReceivedNonPlk)
         when 15 =>
            defaultGwChange   : aliased tOplkApiEventDefaultGwChange;
	    -- Default gateway change event (\ref kOplkApiEventDefaultGwChange)
         when 16 =>
            receivedSdoCom    : aliased tOplkApiEventReceivedSdoCom;
	    -- Received SDO command layer (\ref kOplkApiEventReceivedSdoCom)
         when others =>
            receivedSdoSeq    : aliased tOplkApiEventReceivedSdoSeq;
            -- Received SDO sequence layer (\ref kOplkApiEventReceivedSdoSeq)
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiEventArg);
   pragma Unchecked_Union (tOplkApiEventArg);               -- oplk/oplk.h:360
   
   
------------------------------------------------------------------------------
--\brief Type for API event callback function pointer                       --
-- This type defines a function pointer to an API event callback function.  --
------------------------------------------------------------------------------
   type tOplkApiCbEvent is access function
     (arg1 : tOplkApiEventType;
      -- The type of the event
      arg2 : access tOplkApiEventArg;
      -- Pointer to the event argument
      arg3 : System.Address
      -- Pointer to the user defined argument
     ) return errordefs.tOplkError;
   -- The function returns a tOplkError error code
   pragma Convention (C, tOplkApiCbEvent);                  -- oplk/oplk.h:373
   
   
------------------------------------------------------------------------------
--\brief openPOWERLINK initialization parameters                            --
-- The structure defines the openPOWERLINK initialization parameters.       --
-- The openPOWERLINK stack will be initialized with these parameters when   --
-- oplk_init() is called. Most of the parameters will be stored in the      --
-- object dictionary.                                                       --
-- Some of these objects are constant (read-only) objects and the           --
-- initialization parameters are the only way of setting their values.      --
-- Writable objects could be overwritten later at the boot-up process.      --
-- This could be done by reading a CDC file for an MN or by configuration   --
-- of a CN from an MN via SDO transfers.                                    --
-- \note The elements of the parameter structure must be specified in       --
-- platform byte order!                                                     --
------------------------------------------------------------------------------
   type tOplkApiInitParam_aMacAddress_array is
     array (0 .. 5) of aliased unsigned_char;
   type tOplkApiInitParam_sHostname_array is
     array (0 .. 31) of aliased unsigned_char;
   type tOplkApiInitParam_aVendorSpecificExt2_array is
     array (0 .. 47) of aliased unsigned_char;

   type tOplkApiInitParam is record
      sizeOfInitParam         : aliased unsigned;
      -- This field contains the size of the initialization parameter structure.
      fAsyncOnly              : aliased unsigned_char;
      -- Determines if this node is an async-only node.
      -- If TRUE the node communicates only asynchronously.
      nodeId                  : aliased unsigned;
      -- The node ID of this node.
      aMacAddress             : aliased tOplkApiInitParam_aMacAddress_array;
      -- The MAC address of this node.
      featureFlags            : aliased unsigned;
      -- The POWERLINK feature flags of this node (0x1F82: NMT_FeatureFlags_U32)
      cycleLen                : aliased unsigned;
      -- The cycle Length (0x1006: NMT_CycleLen_U32) in [us]
      isochrTxMaxPayload      : aliased unsigned;
      -- Maximum isochronous transmit payload
      -- (0x1F98.1: IsochrTxMaxPayload_U16) Const!
      isochrRxMaxPayload      : aliased unsigned;
      -- Maximum isochronous receive payload
      -- (0x1F98.2: IsochrRxMaxPayload_U16) Const!
      presMaxLatency          : aliased unsigned;
      -- Maximum PRes latency in ns (0x1F98.3: PResMaxLatency_U32) Read-only!
      preqActPayloadLimit     : aliased unsigned;
      -- Actual PReq payload limit (0x1F98.4: PReqActPayloadLimit_U16)
      presActPayloadLimit     : aliased unsigned;
      -- Actual PRes payload limit (0x1F98.5: PResActPayloadLimit_U16)
      asndMaxLatency          : aliased unsigned;
      -- Maximum ASnd latency in ns (0x1F98.6: ASndMaxLatency_U32) Const!
      multiplCylceCnt         : aliased unsigned;
      -- Multiplexed cycle count (0x1F98.7: MultiplCycleCnt_U8)
      asyncMtu                : aliased unsigned;
      -- Asynchronous MTU (0x1F98.8: AsyncMTU_U16)
      prescaler               : aliased unsigned;
      -- SoC prescaler (0x1F98.9: Prescaler_U16)
      lossOfFrameTolerance    : aliased unsigned;
      -- Loss of frame tolerance in ns (0x1C14: DLL_LossOfFrameTolerance_U32)
      waitSocPreq             : aliased unsigned;
      -- Wait time for first PReq in ns (0x1F8A.1: WaitSoCPReq_U32) Only for MN!
      asyncSlotTimeout        : aliased unsigned;
      -- Asynchronous slot timeout in ns (0x1F8A.2: AsyncSlotTimeout_U32)
      -- Only for MN!
      deviceType              : aliased unsigned;
      -- The device type of this node (0x1000.0: NMT_DeviceType_U32) Const!
      vendorId                : aliased unsigned;
      -- The vendor ID of this node
      -- (0x1018.1: NMT_IdentityObject_REC.VendorId_U32) Const!
      productCode             : aliased unsigned;
      -- The product code of this node
      -- (0x1018.2: NMT_IdentityObject_REC.ProductCode_U32) Const!
      revisionNumber          : aliased unsigned;
      -- The revision number of this node
      -- (0x1018.3: NMT_IdentityObject_REC.RevisionNo_U32) Const!
      serialNumber : aliased unsigned;
      -- The serial number of this node
      -- (0x1018.4: NMT_IdentityObject_REC.SerialNo_U32) Const!
      vendorSpecificExt1      : aliased Extensions.unsigned_long_long;
      -- Vendor specific extensions 1 listed in the IdentResponse frame
      verifyConfigurationDate : aliased unsigned;
      -- Local configuration date (0x1020.1 CFM_VerifyConfiguration_REC.ConfDate_U32)
      verifyConfigurationTime : aliased unsigned;
      -- Local configuration time (0x1020.2 CFM_VerifyConfiguration_REC.ConfTime_U32)
      applicationSwDate       : aliased unsigned;
      -- Local program Date (0x1F52.1 PDL_LocVerApplSw_REC.ApplSwDate_U32)
      applicationSwTime       : aliased unsigned;
      -- Local program Time (0x1F52.2 PDL_LocVerApplSw_REC.ApplSwTime_U32)
      ipAddress               : aliased unsigned;
      -- IP address of the node
      subnetMask              : aliased unsigned;
      -- Subnet mask of the node
      defaultGateway          : aliased unsigned;
      -- Default gateway used by this node
      sHostname               : aliased tOplkApiInitParam_sHostname_array;
      -- DNS host name of the node (maximum length: 32 characters!)
      aVendorSpecificExt2     : aliased tOplkApiInitParam_aVendorSpecificExt2_array;
      -- Vendor specific extensions 2 listed in the IdentResponse frame
      pDevName                : Interfaces.C.Strings.chars_ptr;
      -- Pointer to manufacturer device name
      -- (0x1008.0: NMT_ManufactDevName_VS) Const!
      pHwVersion              : Interfaces.C.Strings.chars_ptr;
      -- Pointer to manufacturer hardware version
      -- (0x1009.0: NMT_ManufactHwVers_VS) Const!
      pSwVersion              : Interfaces.C.Strings.chars_ptr;
      -- Pointer to manufacturer software version
      -- (0x100A.0: NMT_ManufactSwVers_VS) Const!
      pfnCbEvent              : tOplkApiCbEvent;
      -- Pointer to the applications event handling function.
      pEventUserArg           : System.Address;
      -- Pointer to a user argument that is supplied to the event callback function
      -- (\ref tOplkApiCbEvent)
      pfnCbSync               : event.tSyncCb;
      -- Pointer to the application sync callback function.
      -- It is used for single process systems where the whole openPOWERLINK stack
      -- is linked to the application. In this case the stack calls the provided
      -- application callback function when synchronous data can be exchanged.
      -- If a split stack is used (e.g. Linux user/kernel) it must be initialized
      -- with NULL. In this case the application must use oplk_waitSyncEvent()
      -- for waiting on synchronous data.
      hwParam                 : aliased oplkinc.tHwParam;
      -- The hardware parameters of the node
      syncResLatency          : aliased unsigned;
      -- Constant response latency for SyncRes in ns
      syncNodeId              : aliased unsigned;
      -- Specifies the synchronization point for the MN.
      -- The synchronization take place after a PRes from a CN with this node-ID
      -- (0 = SoC, 255 = SoA)
      fSyncOnPrcNode          : aliased unsigned_char;
      -- If it is TRUE, Sync on PRes chained CN; FALSE: conventional CN (PReq/PRes)
      sdoStackType            : aliased tOplkApiSdoStack;
      -- Specifies the SDO stack that should be used.
      -- It is used for switching between the standard SDO stack and
      -- alternative SDO stacks.
      -- The available SDO stacks are defined by the tOplkApiSdoStack enumeration.
      -- If the standard SDO stack is used it must be initialized with 0x00.
   end record;
   pragma Convention (C_Pass_By_Copy, tOplkApiInitParam);  -- oplk/oplk.h:441
   
   
------------------------------------------------------------------------------
--\brief  Process image information structure                               --
-- This structure provides information about a process image.               --
------------------------------------------------------------------------------
   type tOplkApiProcessImage is record
      pImage : System.Address;
      -- Pointer to the process image
      imageSize : aliased unsigned;
      -- Size of the process image
   end record;
   pragma Convention
     (C_Pass_By_Copy,
      tOplkApiProcessImage);                                -- oplk/oplk.h:452
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
--    Generic API functions                                                 --
------------------------------------------------------------------------------

   function Oplk_Init
     (pInitParam_p : access tOplkApiInitParam)
     return Errordefs.tOplkError;
   -- pInitParam_p : Pointer to the init parameters. The init
   --                parameters must be set by the application.
   -- tOplkError   :
   --   kErrorOk     Stack was successfully initialized.
   --   Other        Error occurred while initializing the openPOWERLINK stack.
   --
   -- The function initializes the openPOWERLINK stack. After the stack is
   -- initialized the application must start it by performing a software reset.
   -- This is done by sending the NMT event \ref kNmtEventSwReset. The event
   -- can be sent by calling oplk_execNmtCommand(kNmtEventSwReset).
   pragma Import (C, oplk_init, "oplk_init");
   
   
   function oplk_shutdown 
     return Errordefs.tOplkError;
   -- tOplkError   :
   --   kErrorOk     Stack was successfully shut down.
   --   Other        Error occurred while shutting down the openPOWERLINK stack.
   --
   -- The function shuts down the openPOWERLINK stack. Before shutting down the stack
   -- it should be stopped by sending the NMT command kNmtEventSwitchOff. The command
   -- can be sent by calling oplk_execNmtCommand(kNmtEventSwitchOff);
   pragma Import (C, oplk_shutdown, "oplk_shutdown");
   
   
   function oplk_execNmtCommand
     (NmtEvent_p : Nmt.tNmtEvent)
     return Errordefs.tOplkError;
   -- NmtEvent_p : NMT command to send.
   -- The function returns a tOplkError error code.
   --
   -- The function executes an NMT command, i.e. post the NMT event to the NMT module.
   -- NMT commands which are not appropriate in the current NMT state are silently
   -- ignored. Please keep in mind that the NMT state may change until the NMT command
   -- is actually executed.
   pragma Import (C, oplk_execNmtCommand, "oplk_execNmtCommand");
   
   
   function oplk_linkObject
     (objIndex_p      : unsigned;
      pVar_p          : System.Address;
      pVarEntries_p   : access unsigned;
      pEntrySize_p    : access Obd.tObdSize;
      firstSubindex_p : unsigned)
     return Errordefs.tOplkError;
   -- objIndex_p      : Index of the object to link the variable to.
   -- pVar_p          : Pointer to the application variable that should be
   --                   linked.
   -- pVarEntries_p   : Pointer to the number of entries to link. The function
   --                   stores the number of actually used entries at this
   --                   location.
   -- pEntrySize_p    : Pointer to the size of one entry. If the size is
   --                   zero, the actual size will be read from the object
   --                   dictionary. The function stores the entire size of
   --                   all linked entries at this location.
   -- firstSubindex_p : Specifies the first subindex to be linked.
   -- tOplkError      :
   --   kErrorOk        The variables are successfully linked to the
   --                   object dictionary.
   --   kErrorObdIndexNotExist    The object index does not exist in the object
   --                             dictionary.
   --   kErrorObdSubindexNotExist The subindex does not exist in the object
   --                             dictionary.
   --
   -- The function links an array of application variables onto the specified object
   -- in the object dictionary (OD).
   pragma Import (C, oplk_linkObject, "oplk_linkObject");         -- oplk/oplk.h:465
   
   
   function oplk_readObject
     (pSdoComConHdl_p : access Sdo.tSdoComConHdl;
      nodeId_p        : unsigned;
      index_p         : unsigned;
      subindex_p      : unsigned;
      pDstData_le_p   : System.Address;
      pSize_p         : access unsigned;
      sdoType_p       : Sdo.tSdoType;
      pUserArg_p      : System.Address)
     return Errordefs.tOplkError;
   -- pSdoComConHdl_p : A pointer to the SDO connection handle. It may be
   --                   NULL in case of local OD access.
   -- nodeId_p        : Node ID of the node to read. If node ID is 0, the
   --                   local OD will be read.
   -- index_p         : The index of the object to read.
   -- subindex_p      : The subindex of the object to read.
   -- pDstData_le_p   : Pointer where to store the read data. The data is in
   --                   little endian byte order.
   -- pSize_p         : Pointer to the size of the buffer. For local reads
   --                   the function stores the size of the object at this
   --                   location.
   -- sdoType_p       : The type of the SDO transfer (SDO over ASnd, SDO over
   --                   UDP or SDO over PDO)
   -- pUserArg_p      : User defined argument which will be passed to the
   --                   event callback function.
   -- tOplkError      :
   --   kErrorOk          Entry was successfully read from OD.
   --   Other             Error occurred while reading the OD.
   --
   -- The function reads the specified entry from the object dictionary of the specified
   -- node. If this node is a remote node, it performs an SDO transfer. In such case this
   -- function returns kErrorApiTaskDeferred and the application is informed via the
   -- event callback function when the task is completed.
   pragma Import (C, oplk_readObject, "oplk_readObject");           -- oplk/oplk.h:467
   
   
   function oplk_writeObject
     (pSdoComConHdl_p : access Sdo.tSdoComConHdl;
      nodeId_p        : unsigned;
      index_p         : unsigned;
      subindex_p      : unsigned;
      pSrcData_le_p   : System.Address;
      size_p          : unsigned;
      sdoType_p       : Sdo.tSdoType;
      pUserArg_p      : System.Address)
     return Errordefs.tOplkError;
   -- pSdoComConHdl_p : A pointer to the SDO connection handle. It may be
   --                   NULL in case of local OD access.
   -- nodeId_p        : Node ID of the node to write. If node ID is 0, the
   --                   local OD will be read.
   -- index_p         : The index of the object to write.
   -- subindex_p      : The subindex of the object to write.
   -- pSrcData_le_p   : Pointer to data. The data must be in little endian
   --                   byte order.
   -- size_p          : Size of the data to write.
   -- sdoType_p       : The type of the SDO transfer (SDO over ASnd, SDO over
   --                   UDP or SDO over PDO)
   -- pUserArg_p      : User defined argument which will be passed to the
   --                   event callback function.
   -- tOplkError      :
   --   kErrorOk          Entry was successfully written to the OD.
   --   Other             Error occurred while writing to the OD.
   -- 
   -- The function writes the specified entry to the object dictionary of the specified
   -- node. If this node is a remote node, it performs an SDO transfer. In such case this
   -- function returns kErrorApiTaskDeferred and the application is informed via the
   -- event callback function when the task is completed.
   pragma Import (C, oplk_writeObject, "oplk_writeObject");           -- oplk/oplk.h:470
   
   
   function oplk_freeSdoChannel
     (sdoComConHdl_p : sdo.tSdoComConHdl)
     return errordefs.tOplkError;
   -- sdoComConHdl_p : The SDO connection handle.
   -- tOplkError     :
   --   kErrorOk          SDO channel was successfully freed.
   --   Other             Error occurred while freeing the SDO channel.
   -- 
   -- The function frees the specified SDO channel. It must be called when the SDO
   -- channel to a remote node is not needed anymore. This may be done in the event
   -- callback function when the last SDO transfer to a remote node has completed.
   pragma Import (C, oplk_freeSdoChannel, "oplk_freeSdoChannel");  -- oplk/oplk.h:473
   
   
   function oplk_abortSdo
     (sdoComConHdl_p : Sdo.tSdoComConHdl;
      abortCode_p    : unsigned)
     return Errordefs.tOplkError;
   -- sdoComConHdl_p : The SDO connection handle.
   -- abortCode_p    : The abort code which shall be sent to the remote
   --                  node.
   -- tOplkError     :
   --   kErrorOk          SDO transfer was successfully freed.
   --   Other             Error occurred while aborting the SDO transfer.
   -- 
   -- The function aborts the running SDO transfer on the specified SDO channel.
   pragma Import (C, oplk_abortSdo, "oplk_abortSdo");          -- oplk/oplk.h:474
   
   
   function oplk_readLocalObject
     (index_p    : unsigned;
      subindex_p : unsigned;
      pDstData_p : System.Address;
      pSize_p    : access unsigned)
     return Errordefs.tOplkError;
   -- index_p    : The index of the object to read.
   -- subindex_p : The subindex of the object to read.
   -- pDstData_p : Pointer where to store the read data. The data is in
   --              platform byte order.
   -- pSize_p    : Pointer to the size of the buffer. The function
   --              stores the size of the object at this location.
   -- tOplkError     :
   -- kErrorOk          Entry was successfully read from local OD.
   -- Other             Error occurred while reading the OD.
   -- 
   -- The function reads the specified entry from the local object dictionary.
   pragma Import (C, oplk_readLocalObject, "oplk_readLocalObject");  -- oplk/oplk.h:475
   
   
   function oplk_writeLocalObject
     (index_p    : unsigned;
      subindex_p : unsigned;
      pSrcData_p : System.Address;
      size_p     : unsigned)
     return Errordefs.tOplkError;
   -- index_p    : The index of the object to write.
   -- subindex_p : The subindex of the object to write.
   -- pSrcData_p : Pointer to data. The data must be in platform byte
   --              order.
   -- size_p     : Size of the data to write.
   -- tOplkError     :
   -- kErrorOk          Entry was successfully written to local OD.
   -- Other             Error occurred while writing to the OD.
   -- 
   -- The function writes the specified entry to the local object dictionary.
   pragma Import (C, oplk_writeLocalObject, "oplk_writeLocalObject");  -- oplk/oplk.h:476
   
   
   function oplk_sendAsndFrame
     (dstNodeId_p  : unsigned_char;
      pAsndFrame_p : access Frame.tAsndFrame;
      asndSize_p   : size_t)
     return Errordefs.tOplkError;
   -- dstNodeId_p  : Destination Node ID to send the ASnd frame to.
   -- pAsndFrame_p : Pointer to ASnd frame which should be sent.
   -- asndSize_p   : Size of ASnd frame to send. The size contains the
   --                service ID and the payload. The size cannot
   --                exceed the maximum asynchronous size configured
   --                in AsyncMTU.
   -- tOplkError     :
   --   kErrorOk          The ASnd frame was successfully queued into the
   --                     generic ASnd buffer.
   --   Other             Error occurred while adding the ASnd frame into
   --                     the generic ASnd buffer.
   -- 
   -- The function sends a generic ASnd frame to the specified node. The function
   -- queues the frame into the generic ASnd queue and immediately returns. The
   -- sending of the frame is then controlled by the asynchronous scheduler.
   pragma Import (C, oplk_sendAsndFrame, "oplk_sendAsndFrame");
   
   
   function oplk_sendEthFrame
     (pFrame_p    : access Frame.tPlkFrame;
      frameSize_p : unsigned)
     return Errordefs.tOplkError;
   -- pFrame_p    : Pointer to frame which should be sent.
   -- frameSize_p : Size of frame which should be sent.
   --               The size shall include Ethernet header and payload
   --               (e.g. min. Ethernet frame 14 byte + 46 byte = 60 byte).
   -- tOplkError     :
   --   kErrorOk                Ethernet frame was successfully sent.
   --   kErrorInvalidOperation  EtherType set in frame is invalid.
   --   Other                   Error occurred while sending the Ethernet frame.
   -- 
   -- The function sends an Ethernet frame with generic priority. The given frame's
   -- EtherType must be set to a valid pattern unequal 0x0000 and 0x88AB. The lower
   -- layer inserts the node's MAC address if the source MAC address is set to 0.
   pragma Import (C, oplk_sendEthFrame, "oplk_sendEthFrame");  -- oplk/oplk.h:478
   
   
   function oplk_setAsndForward
     (serviceId_p  : unsigned_char;
      FilterType_p : tOplkApiAsndFilter)
     return Errordefs.tOplkError;
   -- serviceId_p  :The ASnd service ID for which the forwarding will
   --               be set.
   -- FilterType_p : Specifies which types of ASnd frames should be
   --                received. Could be none, unicast or all frames.
   -- tOplkError     :
   --   kErrorOk          Forwarding was successfully set.
   --   Other             Error occurred while setting ASnd forwarding.
   -- 
   -- The function enables or disables the forwarding of received ASnd frames
   -- to the application.
   pragma Import (C, oplk_setAsndForward, "oplk_setAsndForward");  -- oplk/oplk.h:479
   
   
   function oplk_setNonPlkForward
     (fEnable_p : unsigned_char)
     return Errordefs.tOplkError;
   -- fEnable_p : Enable received Ethernet frame forwarding with TRUE.
   --             Disable received Ethernet frame forwarding with FALSE.
   -- tOplkError     :
   --   kErrorOk                Forwarding was successfully set.
   --   kErrorIllegalInstance   Virtual Ethernet is not enabled.
   --   Other                   Error occurred while setting Ethernet forwarding.
   -- 
   -- The function enables or disables the forwarding of received non-POWERLINK
   -- Ethernet frames to the application.
   pragma Import (C, oplk_setNonPlkForward, "oplk_setNonPlkForward");  -- oplk/oplk.h:480
   
   
   function oplk_postUserEvent
     (pUserArg_p : System.Address)
     return Errordefs.tOplkError;
   -- pUserArg_p : User defined pointer.
   -- tOplkError     :
   --   kErrorOk          Event was successfully posted.
   --   Other             Error while posting the event.
   -- 
   -- The function posts user-defined events to event processing thread, i.e. calls
   -- user event callback function with event \ref kOplkApiEventUserDef. This function
   -- is thread safe and is meant for synchronization.
   pragma Import (C, oplk_postUserEvent, "oplk_postUserEvent");  -- oplk/oplk.h:481
   
   
   function oplk_triggerMnStateChange
     (nodeId_p      : unsigned;
      nodeCommand_p : Nmt.tNmtNodeCommand)
     return Errordefs.tOplkError;
   -- nodeId_p      : The Node ID for which the node command will be executed.
   -- nodeCommand_p : The Node command to execute.
   -- tOplkError     :
   --   kErrorOk          NMT node command was successfully sent.
   --   Other             Error occurred while sending NMT node command.
   -- 
   -- The function triggers a NMT state change by sending the specified node command
   -- for the specified node.
   pragma Import (C, oplk_triggerMnStateChange, "oplk_triggerMnStateChange");  -- oplk/oplk.h:482
   
   
   function oplk_setCdcBuffer
     (pbCdc_p   : access unsigned_char;
      cdcSize_p : unsigned)
     return Errordefs.tOplkError;
   -- pbCdc_p   : Pointer to the concise device description.
   -- cdcSize_p : Size of the concise device description
   -- tOplkError     :
   --   kErrorOk          The buffer has successfully been set.
   --   kErrorApiInvalidParam       The function is not available due to missing
   --                               CDC module.
   -- 
   -- The function sets the concise device description (CDC) buffer to be used by
   -- the stack to read the configuration. It can be used instead of
   -- oplk_setCdcFilename() when no file system is available (e.g. on an
   -- embedded system).
   -- \note   The function is only used if the CDC functionality is included in the
   -- openPOWERLINK stack.
   -- \see oplk_setCdcFilename()
   pragma Import (C, oplk_setCdcBuffer, "oplk_setCdcBuffer");  -- oplk/oplk.h:483
   
   
   function oplk_setCdcFilename
     (pszCdcFilename_p : Interfaces.C.Strings.chars_ptr)
     return Errordefs.tOplkError;
   -- pszCdcFilename_p : Filename to be used for reading the concise device
   --                    description.
   -- tOplkError     :
   --   kErrorOk                    The filename has successfully been set.
   --   kErrorApiInvalidParam       The function is not available due to missing
   --                               CDC module.
   -- 
   -- The function sets the concise device description (CDC) file to be used by
   -- the stack to read the configuration.
   -- \see oplk_setCdcBuffer()
   pragma Import (C, oplk_setCdcFilename, "oplk_setCdcFilename");  -- oplk/oplk.h:484
   
   
   function oplk_process
     return Errordefs.tOplkError;
   -- tOplkError     :
   --   kErrorOk
   --   Other
   -- 
   -- The process function is used in single threaded environments e.g. without any OS.
   -- It gives processing time to several tasks in the openPOWERLINK stack.
   pragma Import (C, oplk_process, "oplk_process");  -- oplk/oplk.h:485
   
   
   function oplk_getIdentResponse
     (nodeId_p          : unsigned;
      ppIdentResponse_p : System.Address)
     return Errordefs.tOplkError;
   -- nodeId_p          : Node ID of which to get the Ident Response frame.
   -- ppIdentResponse_p : Pointer to store the address of the IdentResponse
   --                     frame.
   -- tOplkError     :
   --   kErrorOk
   --   Other           On a CN it returns always \ref kErrorApiInvalidParam. 
   --
   -- The function returns the stored IdentResponse frame of the specified node.
   -- \note   The function is only used on an MN. On a CN it returns always
   --          \ref kErrorApiInvalidParam.
   pragma Import (C, oplk_getIdentResponse, "oplk_getIdentResponse");  -- oplk/oplk.h:486
   
   
   function oplk_getEthMacAddr
     (pMacAddr_p : access unsigned_char)
     return Errordefs.tOplkError;
   -- pMacAddr_p : Pointer to memory buffer which is used to copy the MAC
   --              address into. The memory buffer must have a size of
   --              6 bytes!
   -- tOplkError     :
   --   kErrorOk
   --   Other           
   -- 
   -- The function provides the Ethernet Interface MAC address used by the
   -- Ethernet controller.
   pragma Import (C, oplk_getEthMacAddr, "oplk_getEthMacAddr");  -- oplk/oplk.h:487
   
   
   function oplk_checkKernelStack return unsigned_char;
   -- Returns the status of the kernel stack.
   --   TRUE        The kernel stack is alive.
   --   FALSE       The kernel stack is dead.
   -- 
   -- The function checks if the kernel part of the stack is alive.
   pragma Import (C, oplk_checkKernelStack, "oplk_checkKernelStack");  -- oplk/oplk.h:488
   
   
   function oplk_waitSyncEvent
     (timeout_p : unsigned_long)
     return Errordefs.tOplkError;
   -- timeout_p : Specifies a timeout in microseconds. If 0 it waits
   --             forever.
   -- tOplkError     :
   --   kErrorOk            The sync event occurred.
   --   kErrorGeneralError  An error or timeout occurred while waiting for the
   --                       sync event.
   -- 
   -- The function waits for a sync event. It blocks until the sync event occurred or
   -- the specified timeout elapsed.
   -- \note In a single process solution where the whole stack is linked to the
   --       application, the function immediately returns! In this case, the
   --       application must register its sync function as callback function so that
   --       it is directly called from the stack (see pfnCbSync in
   --       \ref tOplkApiInitParam).
   pragma Import (C, oplk_waitSyncEvent, "oplk_waitSyncEvent");  -- oplk/oplk.h:489
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
--   Process image API functions  (from   processimage.c)                   --
------------------------------------------------------------------------------
   function oplk_allocProcessImage
     (sizeProcessImageIn_p  : unsigned;
      sizeProcessImageOut_p : unsigned)
     return Errordefs.tOplkError;
   -- sizeProcessImageIn_p  : Size for input process image.
   -- sizeProcessImageOut_p : Size for output process image.
   -- tOplkError     :
   --   kErrorOk                        Process images are successfully allocated.
   --   kErrorApiPIAlreadyAllocated     Process images were already allocated.
   --   kErrorApiPIOutOfMemory          Process images could not be allocated.
   --   kErrorApiNotInitialized         openPOWERLINK stack is not initialized.
   -- 
   -- The function allocates memory for the input and output process images.
   pragma Import (C, oplk_allocProcessImage, "oplk_allocProcessImage");  -- oplk/oplk.h:492
   
   
   function oplk_freeProcessImage
     return Errordefs.tOplkError;
   -- tOplkError     :
   --   kErrorOk                        Process images are successfully freed.
   --   kErrorApiNotInitialized         openPOWERLINK stack is not initialized.
   -- 
   -- The function frees the allocated process images
   pragma Import (C, oplk_freeProcessImage, "oplk_freeProcessImage");  -- oplk/oplk.h:493
   
   
   function oplk_linkProcessImageObject
     (objIndex_p      : unsigned;
      firstSubindex_p : unsigned;
      offsetPI_p      : unsigned;
      fOutputPI_p     : unsigned_char;
      entrySize_p     : Obd.tObdSize;
      pVarEntries_p   : access unsigned)
     return Errordefs.tOplkError;
   -- objIndex_p      : The object index of the object to link.
   -- firstSubindex_p : The sub-index of the object where the first
   --                   variable should be linked to.
   -- offsetPI_p      : The offset of the first process variable in the
   --                   process image.
   -- fOutputPI_p     : Determines if input image or output image should
   --                   be used: TRUE = output image, FALSE = imput image
   -- entrySize_p     : The size of one process variable.
   -- pVarEntries_p   : The number of process variables, which shall be
   --                   linked to the object dictionary. It returns the
   --                   actual number of process variables which were
   --                   linked to the object dictionary.
   -- tOplkError        :
   --   kErrorOk                    Object is successfully linked.
   --   kErrorApiInvalidParam       Invalid parameters specified.
   --   kErrorApiPISizeExceeded     Size of process image is exceeded.
   --   kErrorApiNotInitialized     openPOWERLINK stack is not initialized.
   -- 
   -- The function links an object in the OD into a location in the process image.
   pragma Import
     (C, oplk_linkProcessImageObject, "oplk_linkProcessImageObject");  -- oplk/oplk.h:494
   
   
   function oplk_exchangeProcessImageIn
     return Errordefs.tOplkError;
   -- tOplkError        :
   --   kErrorOk                    Input process image is successfully exchanged.
   --   kErrorApiPINotAllocated     Memory for process images is not allocated.
   --   kErrorApiNotInitialized     openPOWERLINK stack is not initialized.
   -- 
   -- The function exchanges the input process image.
   pragma Import
     (C, oplk_exchangeProcessImageIn, "oplk_exchangeProcessImageIn");  -- oplk/oplk.h:496
   
   
   function oplk_exchangeProcessImageOut
     return Errordefs.tOplkError;
   -- tOplkError        :
   --   kErrorOk                    Output process image is successfully exchanged.
   --   kErrorApiPINotAllocated     Memory for process images is not allocated.
   --   kErrorApiNotInitialized     openPOWERLINK stack is not initialized.
   -- 
   -- The function exchanges the output process image.
   pragma Import
     (C, oplk_exchangeProcessImageOut, "oplk_exchangeProcessImageOut");  -- oplk/oplk.h:497
   
   
   function oplk_getProcessImageIn 
     return System.Address;
   -- The function returns a pointer to the input process image or NULL if the
   -- stack is not initialized.
   -- 
   -- The function returns the pointer to the input process image.
   pragma Import (C, oplk_getProcessImageIn, "oplk_getProcessImageIn");  -- oplk/oplk.h:498
   
   
   function oplk_getProcessImageOut 
     return System.Address;
   --  The function returns a pointer to the output process image or NULL if
   --  the stack is not initialized.
   -- 
   -- The function returns the pointer to the output process image.
   pragma Import (C, oplk_getProcessImageOut, "oplk_getProcessImageOut");  -- oplk/oplk.h:499
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
--    objdict specific process image functions                              --
--  (from  processimage-cia302.c)                                           --
------------------------------------------------------------------------------
   function oplk_setupProcessImage
     return Errordefs.tOplkError;
   -- tOplkError        :
   --   kErrorOk
   --   Other
   -- 
   -- The function sets up a process image according to the CiA profile 302_4.
   pragma Import (C, oplk_setupProcessImage, "oplk_setupProcessImage");  -- oplk/oplk.h:502
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
--    Request forwarding of Pres frame from DLL -> API (from  generic.c)    --
------------------------------------------------------------------------------
   function oplk_triggerPresForward
     (nodeId_p : unsigned)
     return Errordefs.tOplkError;
   -- nodeId_p : Node ID of which to get the PRes frame.
   -- tOplkError        :
   --   kErrorOk
   --   Other
   -- 
   -- The function triggers the forwarding of a PRes frame from Node \p nodeId_p
   -- to the application. It can be used by the application for diagnosis purpose
   -- (e.g. conformance test). After request "one" PRes frame form the specified
   -- node will be forwarded to the application. The PRes frame is forwarded by
   -- a \ref kOplkApiEventReceivedPres event. The application has to handle this event
   -- to get the frame.
   pragma Import (C, oplk_triggerPresForward, "oplk_triggerPresForward");  -- oplk/oplk.h:505
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
--    SDO Test Api functions    (from    sdotest.c)                         --
------------------------------------------------------------------------------
   procedure oplk_testSdoSetVal
     (pInitParam_p : access tOplkApiInitParam);
   -- pInitParam_p : Pointer to the init parameters. The init
   --                parameters must be set by the application.
   -- 
   -- The function stores the init paramter struct for further use.
   pragma Import (C, oplk_testSdoSetVal, "oplk_testSdoSetVal");  -- oplk/oplk.h:508
   
   
   function oplk_testSdoComInit
     return Errordefs.tOplkError;
   -- tOplkError        :
   --   kErrorOk                Stack was successfully initialized.
   --   other                   Error occurred while initializing stack.
   -- 
   -- The function initializes the SDO test command layer stack.
   pragma Import (C, oplk_testSdoComInit, "oplk_testSdoComInit");  -- oplk/oplk.h:509
   
   
   function oplk_testSdoSeqInit
     return Errordefs.tOplkError;
   -- tOplkError        :
   --   kErrorOk                Stack was successfully initialized.
   --   Other                   Error occurred while initializing stack.
   -- 
   -- The function initializes the SDO test sequence layer stack.
   pragma Import (C, oplk_testSdoSeqInit, "oplk_testSdoSeqInit");  -- oplk/oplk.h:510
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
--    Testing functions for SDO command layer                               --
------------------------------------------------------------------------------
   function oplk_testSdoComSend
     (uiNodeId_p : unsigned;
      SdoType_p  : Sdo.tSdoType;
      pSdoCom_p  : access Frame.tAsySdoCom;
      SdoSize_p  : size_t)
     return Errordefs.tOplkError;
   -- uiNodeId_p : Node ID of target node
   -- SdoType_p  : Type of SDO lower layer (Asnd/Udp)
   -- pSdoCom_p  : Pointer to SDO command layer frame
   -- SdoSize_p  : Size of SDO command layer frame
   -- tOplkError        :
   --   kErrorOk                The function returns sucessfully if 
   --                            the request was forwarded.
   --   Other                   Error occurred while sending the frame.
   -- 
   -- The function forwards the Send request to the SDO command testing layer.
   pragma Import (C, oplk_testSdoComSend, "oplk_testSdoComSend");  -- oplk/oplk.h:512
   
   
   function oplk_testSdoComDelCon
     return Errordefs.tOplkError;
   -- tOplkError        :
   --   kErrorOk                The function returns sucessfully if 
   --                            the connection was deleted.
   --   kErrorInvalidOperation  Error occurred while deleting the 
   --                            command layer connection.
   -- 
   -- The functions deletes the SDO command testing layer.
   pragma Import (C, oplk_testSdoComDelCon, "oplk_testSdoComDelCon");  -- oplk/oplk.h:514
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
--    Testing functions for SDO sequence layer                              --
------------------------------------------------------------------------------
   function oplk_testSdoSeqSend
     (uiNodeId_p : unsigned;
      SdoType_p  : Sdo.tSdoType;
      pSdoCom_p  : access Frame.tAsySdoSeq;
      SdoSize_p  : size_t)
     return errordefs.tOplkError;
   -- uiNodeId_p : Node ID of target node
   -- SdoType_p  : Type of SDO lower layer (Asnd/Udp)
   -- pSdoCom_p  : Pointer to SDO sequence layer frame
   -- SdoSize_p  : Size of SDO sequence layer frame
   -- tOplkError        :
   --   kErrorOk                The function returns sucessfully if 
   --                             the request was forwarded.
   --   Other                   Error occurred while sending the frame.
   -- 
   -- The functions forwards the send request to the SDO sequence testing layer.
   pragma Import (C, oplk_testSdoSeqSend, "oplk_testSdoSeqSend");  -- oplk/oplk.h:516
   
   
   function oplk_testSdoSeqDelCon
     return Errordefs.tOplkError;
   -- tOplkError        :
   --   kErrorOk                The function returns sucessfully if 
   --                            the connection was deleted.
   --   kErrorInvalidOperation  Error occurred while deleting the 
   --                            sequence layer connection.
   -- 
   -- The function deletes the SDO sequence testing layer.
   pragma Import (C, oplk_testSdoSeqDelCon, "oplk_testSdoSeqDelCon");  -- oplk/oplk.h:518

end Oplk.Oplk;

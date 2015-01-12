--  \brief  openPOWERLINK error definitions
--    Definitions of return values and errors.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package Oplk.errordefs is
   
   --   Emergency error codes   --
   E_NO_ERROR          : constant := 16#0000#;
   -- 
   
   --   0xFxxx manufacturer specific error codes   ---
   E_NMT_NO_IDENT_RES  : constant := 16#F001#;
   -- 
   E_NMT_NO_STATUS_RES : constant := 16#F002#;
   -- 
   
   --   0x816x HW errors   --
   E_DLL_BAD_PHYS_MODE     : constant := 16#8161#;
   -- 
   E_DLL_COLLISION         : constant := 16#8162#;
   -- 
   E_DLL_COLLISION_TH      : constant := 16#8163#;
   -- 
   E_DLL_CRC_TH            : constant := 16#8164#;
   -- 
   E_DLL_LOSS_OF_LINK      : constant := 16#8165#;
   -- 
   E_DLL_MAC_BUFFER        : constant := 16#8166#;
   -- 
   
   --   0x82xx Protocol errors   --
   E_DLL_ADDRESS_CONFLICT  : constant := 16#8201#;
   -- 
   E_DLL_MULTIPLE_MN       : constant := 16#8202#;
   -- 
   
   --   0x821x Frame size errors   --
   E_PDO_SHORT_RX          : constant := 16#8210#;
   -- 
   E_PDO_MAP_VERS          : constant := 16#8211#;
   -- 
   E_NMT_ASND_MTU_DIF      : constant := 16#8212#;
   -- 
   E_NMT_ASND_MTU_LIM      : constant := 16#8213#;
   -- 
   E_NMT_ASND_TX_LIM       : constant := 16#8214#;
   -- 
   
   --   0x823x Timing errors   --
   E_NMT_CYCLE_LEN         : constant := 16#8231#;
   -- 
   E_DLL_CYCLE_EXCEED      : constant := 16#8232#;
   -- 
   E_DLL_CYCLE_EXCEED_TH   : constant := 16#8233#;
   -- 
   E_NMT_IDLE_LIM          : constant := 16#8234#;
   -- 
   E_DLL_JITTER_TH         : constant := 16#8235#;
   -- 
   E_DLL_LATE_PRES_TH      : constant := 16#8236#;
   -- 
   E_NMT_PREQ_CN           : constant := 16#8237#;
   -- 
   E_NMT_PREQ_LIM          : constant := 16#8238#;
   -- 
   E_NMT_PRES_CN           : constant := 16#8239#;
   -- 
   E_NMT_PRES_RX_LIM       : constant := 16#823A#;
   -- 
   E_NMT_PRES_TX_LIM       : constant := 16#823B#;
   -- 
   
   --   0x824x Frame errors   --
   E_DLL_INVALID_FORMAT    : constant := 16#8241#;
   -- 
   E_DLL_LOSS_PREQ_TH      : constant := 16#8242#;
   -- 
   E_DLL_LOSS_PRES_TH      : constant := 16#8243#;
   -- 
   E_DLL_LOSS_SOA_TH       : constant := 16#8244#;
   -- 
   E_DLL_LOSS_SOC_TH       : constant := 16#8245#;
   -- 
   
   --   0x84xx BootUp Errors   --
   E_NMT_BA1               : constant := 16#8410#;
   -- other MN in MsNotActive active
   E_NMT_BA1_NO_MN_SUPPORT : constant := 16#8411#;
   -- MN is not supported
   E_NMT_BPO1              : constant := 16#8420#;
   -- mandatory CN was not found or failed in BootStep1
   E_NMT_BPO1_GET_IDENT    : constant := 16#8421#;
   -- IdentRes was not received
   E_NMT_BPO1_DEVICE_TYPE  : constant := 16#8422#;
   -- wrong device type
   E_NMT_BPO1_VENDOR_ID    : constant := 16#8423#;
   -- wrong vendor ID
   E_NMT_BPO1_PRODUCT_CODE : constant := 16#8424#;
   -- wrong product code
   E_NMT_BPO1_REVISION_NO  : constant := 16#8425#;
   -- wrong revision number
   E_NMT_BPO1_SERIAL_NO    : constant := 16#8426#;
   -- wrong serial number
   E_NMT_BPO1_CF_VERIFY    : constant := 16#8428#;
   -- verification of configuration failed
   E_NMT_BPO2              : constant := 16#8430#;
   -- mandatory CN failed in BootStep2
   E_NMT_BRO               : constant := 16#8440#;
   -- CheckCommunication failed for mandatory CN
   E_NMT_WRONG_STATE       : constant := 16#8480#;
   -- mandatory CN has wrong NMT state
   
   
------------------------------------------------------------------------------
--\brief openPOWERLINK function error codes                                 --
--                                                                          --
-- The following enumeration specifies the error codes for return values of --
-- openPOWERLINK functions.                                                 --
------------------------------------------------------------------------------
   subtype tOplkError is unsigned;
   
   --   area for generic errors 0x0000 - 0x000F   --
   kErrorOk                       : constant tOplkError := 0;
   -- No error/successful run
   kErrorIllegalInstance          : constant tOplkError := 1;
   -- The called instance does not exist
   kErrorInvalidInstanceParam     : constant tOplkError := 2;
   -- There was an invalid instance parameter
   kErrorNoFreeInstance           : constant tOplkError := 3;
   -- XxxAddInstance was called but no free instance is available
   kErrorWrongSignature           : constant tOplkError := 4;
   -- Wrong signature while writing to object 0x1010 or 0x1011
   kErrorInvalidOperation         : constant tOplkError := 5;
   -- Operation is not allowed in this situation
   kErrorInvalidNodeId            : constant tOplkError := 7;
   -- An invalid NodeId was specified
   kErrorNoResource               : constant tOplkError := 8;
   -- The resource could not be created
   kErrorShutdown                 : constant tOplkError := 9;
   -- Stack is shutting down
   kErrorReject                   : constant tOplkError := 10;
   -- Reject the subsequent command
   kErrorRetry                    : constant tOplkError := 11;
   -- Retry this command
   kErrorInvalidEvent             : constant tOplkError := 12;
   -- Invalid event was posted
   kErrorGeneralError             : constant tOplkError := 13;
   -- General error
   kErrorFeatureMismatch          : constant tOplkError := 14;
   -- Features of user and kernel stack are mismatched
   
   --   area for EDRV module 0x0010 - 0x001F   --
   kErrorEdrvNoFreeTxDesc         : constant tOplkError := 17;
   -- No free TX descriptor available
   kErrorEdrvInvalidCycleLen      : constant tOplkError := 18;
   -- Invalid cycle length (e.g. 0)
   kErrorEdrvInit                 : constant tOplkError := 19;
   -- Edrv initialization error
   kErrorEdrvNoFreeBufEntry       : constant tOplkError := 20;
   -- No free entry in internal buffer table for TX frames
   kErrorEdrvBufNotExisting       : constant tOplkError := 21;
   -- The specified TX buffer does not exist
   kErrorEdrvInvalidRxBuf         : constant tOplkError := 22;
   -- The specified RX buffer is invalid
   kErrorEdrvInvalidParam         : constant tOplkError := 28;
   -- Invalid parameter in function call
   kErrorEdrvNextTxListNotEmpty   : constant tOplkError := 29;
   -- Next TX buffer list is not empty, i.e. still in use
   kErrorEdrvCurTxListEmpty       : constant tOplkError := 30;
   -- Current TX buffer list is empty, i.e. DLL didn't provide one
   kErrorEdrvTxListNotFinishedYet : constant tOplkError := 31;
   -- Current TX buffer list has not been finished yet, but new cycle has started
   
   --   area for DLL module 0x0020 - 0x002F   --
   kErrorDllOutOfMemory           : constant tOplkError := 33;
   -- DLL is out of memory
   kErrorDllIllegalHdl            : constant tOplkError := 34;
   -- Illegal handle for a TxFrame was passed
   kErrorDllCbAsyncRegistered     : constant tOplkError := 35;
   -- Handler for non-POWERLINK frames was already registered before
   kErrorDllAsyncSyncReqFull      : constant tOplkError := 36;
   -- The buffer for SyncRequests is full
   kErrorDllAsyncTxBufferEmpty    : constant tOplkError := 37;
   -- The transmit buffer for asynchronous frames is empty
   kErrorDllAsyncTxBufferFull     : constant tOplkError := 38;
   -- Transmit buffer for asynchronous frames is full
   kErrorDllNoNodeInfo            : constant tOplkError := 39;
   -- MN: too less space in the internal node info structure
   kErrorDllInvalidParam          : constant tOplkError := 40;
   -- Invalid parameters passed to DLL function
   kErrorDllInvalidAsndServiceId  : constant tOplkError := 41;
   -- Invalid AsndServiceId specified
   kErrorDllTxBufNotReady         : constant tOplkError := 46;
   -- TxBuffer (e.g. for PReq) is not ready yet
   kErrorDllTxFrameInvalid        : constant tOplkError := 47;
   -- TxFrame (e.g. for PReq) is invalid or does not exist
   
   --   area for OBD module 0x0030 - 0x003F   --
   kErrorObdIllegalPart           : constant tOplkError := 48;
   -- Unknown OD part
   kErrorObdIndexNotExist         : constant tOplkError := 49;
   -- Object index does not exist in OD
   kErrorObdSubindexNotExist      : constant tOplkError := 50;
   -- Sub-index does not exist in object index
   kErrorObdReadViolation         : constant tOplkError := 51;
   -- Read access to a write-only object
   kErrorObdWriteViolation        : constant tOplkError := 52;
   -- Write access to a read-only object
   kErrorObdAccessViolation       : constant tOplkError := 53;
   -- Access not allowed
   kErrorObdUnknownObjectType     : constant tOplkError := 54;
   -- The object type is not defined/known
   kErrorObdVarEntryNotExist      : constant tOplkError := 55;
   -- The object does not contain a VarEntry structure
   kErrorObdValueTooLow           : constant tOplkError := 56;
   -- The value to write to an object is too low
   kErrorObdValueTooHigh          : constant tOplkError := 57;
   -- The value to write to an object is too high
   kErrorObdValueLengthError      : constant tOplkError := 58;
   -- The value to write is to long or to short
   kErrorObdErrnoSet              : constant tOplkError := 59;
   -- File I/O error occurred and errno is set
   kErrorObdInvalidDcf            : constant tOplkError := 60;
   -- The device configuration file (CDC) is not valid
   kErrorObdOutOfMemory           : constant tOplkError := 61;
   -- Out of memory
   kErrorObdNoConfigData          : constant tOplkError := 62;
   -- No configuration data present (CDC is empty)
   
   --   area for NMT module 0x0040 - 0x004F   --
   kErrorNmtUnknownCommand        : constant tOplkError := 64;
   -- Unknown NMT command
   kErrorNmtInvalidFramePointer   : constant tOplkError := 65;
   -- Pointer to the frame is not valid
   kErrorNmtInvalidEvent          : constant tOplkError := 66;
   -- Invalid event send to NMT-module
   kErrorNmtInvalidState          : constant tOplkError := 67;
   -- Unknown state in NMT state machine
   kErrorNmtInvalidParam          : constant tOplkError := 68;
   -- Invalid parameters specified
   kErrorNmtSyncReqRejected       : constant tOplkError := 69;
   -- SyncReq could not be issued
   
   --   area for SDO/UDP module 0x0050 - 0x005F   --
   kErrorSdoUdpMissCb             : constant tOplkError := 80;
   -- Missing callback-function pointer during init of module
   kErrorSdoUdpNoSocket           : constant tOplkError := 81;
   -- Error during init of socket
   kErrorSdoUdpSocketError        : constant tOplkError := 82;
   -- Error during usage of socket
   kErrorSdoUdpThreadError        : constant tOplkError := 83;
   -- Error during start of listen thread
   kErrorSdoUdpNoFreeHandle       : constant tOplkError := 84;
   -- No free connection handle for Udp
   kErrorSdoUdpSendError          : constant tOplkError := 85;
   -- Error during sending of frame
   kErrorSdoUdpInvalidHdl         : constant tOplkError := 86;
   -- The connection handle is invalid
   
   --   area for SDO Sequence layer module 0x0060 - 0x006F   --
   kErrorSdoSeqMissCb             : constant tOplkError := 96;
   -- No SDO callback function is assigned
   kErrorSdoSeqNoFreeHandle       : constant tOplkError := 97;
   -- No free handle for connection
   kErrorSdoSeqInvalidHdl         : constant tOplkError := 98;
   -- Invalid handle in SDO sequence layer
   kErrorSdoSeqUnsupportedProt    : constant tOplkError := 99;
   -- Unsupported Protocol selected
   kErrorSdoSeqNoFreeHistory      : constant tOplkError := 100;
   -- No free entry in history
   kErrorSdoSeqFrameSizeError     : constant tOplkError := 101;
   -- The size of the frames is not correct
   kErrorSdoSeqRequestAckNeeded   : constant tOplkError := 102;
   -- Indicates that the history buffer is full and a ack request is needed
   kErrorSdoSeqInvalidFrame       : constant tOplkError := 103;
   -- The frame is not valid
   kErrorSdoSeqConnectionBusy     : constant tOplkError := 104;
   -- Connection is busy -> retry later
   kErrorSdoSeqInvalidEvent       : constant tOplkError := 105;
   -- Invalid event received
   
   --   area for SDO Command Layer module 0x0070 - 0x007F   --
   kErrorSdoComUnsupportedProt    : constant tOplkError := 112;
   -- Unsupported Protocol selected
   kErrorSdoComNoFreeHandle       : constant tOplkError := 113;
   -- No free handle for connection
   kErrorSdoComInvalidServiceType : constant tOplkError := 114;
   -- Invalid SDO service type specified
   kErrorSdoComInvalidHandle      : constant tOplkError := 115;
   -- Handle is invalid
   kErrorSdoComInvalidSendType    : constant tOplkError := 116;
   -- The state of frame to send is not possible
   kErrorSdoComNotResponsible     : constant tOplkError := 117;
   -- Internal error: command layer handle is not responsible for 
   -- this event from sequence layer
   kErrorSdoComHandleExists       : constant tOplkError := 118;
   -- Handle to same node already exists
   kErrorSdoComHandleBusy         : constant tOplkError := 119;
   -- Transfer via this handle is already running
   kErrorSdoComInvalidParam       : constant tOplkError := 120;
   -- Invalid parameters passed to function
   
   --   area for openPOWERLINK event module 0x0080 - 0x008F   --
   kErrorEventUnknownSink         : constant tOplkError := 128;
   -- Unknown sink for event
   kErrorEventPostError           : constant tOplkError := 129;
   -- Error during post of event
   kErrorEventReadError           : constant tOplkError := 130;
   -- Error during reading of event from queue
   kErrorEventWrongSize           : constant tOplkError := 131;
   -- Event arg has wrong size
   
   --   area for openPOWERLINK timer module 0x0090 - 0x009F   --
   kErrorTimerInvalidHandle       : constant tOplkError := 144;
   -- Invalid handle for timer
   kErrorTimerNoTimerCreated      : constant tOplkError := 145;
   -- No timer was created caused by an error
   kErrorTimerThreadError         : constant tOplkError := 146;
   -- Process thread could not be created
   
   --   area for openPOWERLINK SDO/Asnd module 0x00A0 - 0x0AF   --
   kErrorSdoAsndInvalidNodeId     : constant tOplkError := 160;
   -- Node-ID is invalid
   kErrorSdoAsndNoFreeHandle      : constant tOplkError := 161;
   -- No free handle for connection
   kErrorSdoAsndInvalidHandle     : constant tOplkError := 162;
   -- Handle for connection is invalid
   
   --   area for PDO module 0x00B0 - 0x00BF   --
   kErrorPdoNotExist              : constant tOplkError := 176;
   -- Selected PDO does not exist
   kErrorPdoLengthExceeded        : constant tOplkError := 177;
   -- Length of PDO mapping exceeds the current payload limit
   kErrorPdoGranularityMismatch   : constant tOplkError := 178;
   -- Configured PDO granularity is not equal to supported granularity
   kErrorPdoInitError             : constant tOplkError := 179;
   -- Error during initialization of PDO module
   kErrorPdoConfWhileEnabled      : constant tOplkError := 183;
   -- PDO configuration cannot be changed while it is enabled
   kErrorPdoErrorMapp             : constant tOplkError := 184;
   -- Invalid PDO mapping
   kErrorPdoVarNotFound           : constant tOplkError := 185;
   -- The referenced object in a PDO mapping does not exist
   kErrorPdoVarNotMappable        : constant tOplkError := 186;
   -- The referenced object in a PDO mapping is not mappable
   kErrorPdoSizeMismatch          : constant tOplkError := 188;
   -- Bit size of object mapping is larger than the object size
   kErrorPdoTooManyTxPdos         : constant tOplkError := 189;
   -- There exits more than one TPDO on CN
   kErrorPdoInvalidObjIndex       : constant tOplkError := 190;
   -- Invalid object index used for PDO mapping or communication parameter
   kErrorPdoTooManyPdos           : constant tOplkError := 191;
   -- Too many PDOs do exist
   
   --   Configuration manager module 0x00C0 - 0x00CF   --
   kErrorCfmConfigError           : constant tOplkError := 192;
   -- Error in configuration manager
   kErrorCfmSdocTimeOutError      : constant tOplkError := 193;
   -- Error in configuration manager, SDO timeout
   kErrorCfmInvalidDcf            : constant tOplkError := 194;
   -- Device configuration file (CDC) is not valid
   kErrorCfmUnsupportedDcf        : constant tOplkError := 195;
   -- Unsupported Dcf format
   kErrorCfmConfigWithErrors      : constant tOplkError := 196;
   -- Configuration finished with errors
   kErrorCfmNoFreeConfig          : constant tOplkError := 197;
   -- No free configuration entry
   kErrorCfmNoConfigData          : constant tOplkError := 198;
   -- No configuration data present
   kErrorCfmUnsuppDatatypeDcf     : constant tOplkError := 199;
   -- Unsupported datatype found in dcf -> this entry was not configured
   kErrorApiTaskDeferred          : constant tOplkError := 320;
   -- openPOWERLINK performs task in background and informs the application 
   -- (or vice-versa), when it is finished
   kErrorApiInvalidParam          : constant tOplkError := 322;
   -- Passed invalid parameters to a function (e.g. invalid node id)
   kErrorApiNoObdInitRam          : constant tOplkError := 323;
   -- No function pointer for ObdInitRam supplied
   kErrorApiSdoBusyIntern         : constant tOplkError := 324;
   -- The SDO channel to this node is internally used by the stack (e.g. the CFM) 
   -- and currently not available for the application.
   kErrorApiPIAlreadyAllocated    : constant tOplkError := 325;
   -- Process image is already allocated
   kErrorApiPIOutOfMemory         : constant tOplkError := 326;
   -- Process image: out of memory
   kErrorApiPISizeExceeded        : constant tOplkError := 327;
   -- Process image: variable linking or copy job exceeds the size of the PI
   kErrorApiPINotAllocated        : constant tOplkError := 328;
   -- Process image is not allocated
   kErrorApiPIJobQueueFull        : constant tOplkError := 329;
   -- Process image: job queue is full
   kErrorApiPIJobQueueEmpty       : constant tOplkError := 330;
   -- Process image: job queue is empty
   kErrorApiPIInvalidJobSize      : constant tOplkError := 331;
   -- Process image: invalid job size
   kErrorApiPIInvalidPIPointer    : constant tOplkError := 332;
   -- Process image: pointer to application's process image is invalid
   kErrorApiPINonBlockingNotSupp  : constant tOplkError := 333;
   -- Process image: non-blocking copy jobs are not supported on this target
   kErrorApiNotInitialized        : constant tOplkError := 334;
   -- PI called but stack is not initialized/running       -- ./oplk/errordefs.h:273
   
   --   area until 0x07FF is reserved                     --
   --   area for user application from 0x0800 to 0x7FFF   --
   
end Oplk.errordefs;

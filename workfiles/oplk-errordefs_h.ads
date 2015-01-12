pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package oplk.errordefs_h is


   E_NO_ERROR : constant := 16#0000#;  --  ./oplk/errordefs.h:51

   E_NMT_NO_IDENT_RES : constant := 16#F001#;  --  ./oplk/errordefs.h:53
   E_NMT_NO_STATUS_RES : constant := 16#F002#;  --  ./oplk/errordefs.h:54

   E_DLL_BAD_PHYS_MODE : constant := 16#8161#;  --  ./oplk/errordefs.h:57
   E_DLL_COLLISION : constant := 16#8162#;  --  ./oplk/errordefs.h:58
   E_DLL_COLLISION_TH : constant := 16#8163#;  --  ./oplk/errordefs.h:59
   E_DLL_CRC_TH : constant := 16#8164#;  --  ./oplk/errordefs.h:60
   E_DLL_LOSS_OF_LINK : constant := 16#8165#;  --  ./oplk/errordefs.h:61
   E_DLL_MAC_BUFFER : constant := 16#8166#;  --  ./oplk/errordefs.h:62

   E_DLL_ADDRESS_CONFLICT : constant := 16#8201#;  --  ./oplk/errordefs.h:64
   E_DLL_MULTIPLE_MN : constant := 16#8202#;  --  ./oplk/errordefs.h:65

   E_PDO_SHORT_RX : constant := 16#8210#;  --  ./oplk/errordefs.h:67
   E_PDO_MAP_VERS : constant := 16#8211#;  --  ./oplk/errordefs.h:68
   E_NMT_ASND_MTU_DIF : constant := 16#8212#;  --  ./oplk/errordefs.h:69
   E_NMT_ASND_MTU_LIM : constant := 16#8213#;  --  ./oplk/errordefs.h:70
   E_NMT_ASND_TX_LIM : constant := 16#8214#;  --  ./oplk/errordefs.h:71

   E_NMT_CYCLE_LEN : constant := 16#8231#;  --  ./oplk/errordefs.h:73
   E_DLL_CYCLE_EXCEED : constant := 16#8232#;  --  ./oplk/errordefs.h:74
   E_DLL_CYCLE_EXCEED_TH : constant := 16#8233#;  --  ./oplk/errordefs.h:75
   E_NMT_IDLE_LIM : constant := 16#8234#;  --  ./oplk/errordefs.h:76
   E_DLL_JITTER_TH : constant := 16#8235#;  --  ./oplk/errordefs.h:77
   E_DLL_LATE_PRES_TH : constant := 16#8236#;  --  ./oplk/errordefs.h:78
   E_NMT_PREQ_CN : constant := 16#8237#;  --  ./oplk/errordefs.h:79
   E_NMT_PREQ_LIM : constant := 16#8238#;  --  ./oplk/errordefs.h:80
   E_NMT_PRES_CN : constant := 16#8239#;  --  ./oplk/errordefs.h:81
   E_NMT_PRES_RX_LIM : constant := 16#823A#;  --  ./oplk/errordefs.h:82
   E_NMT_PRES_TX_LIM : constant := 16#823B#;  --  ./oplk/errordefs.h:83

   E_DLL_INVALID_FORMAT : constant := 16#8241#;  --  ./oplk/errordefs.h:85
   E_DLL_LOSS_PREQ_TH : constant := 16#8242#;  --  ./oplk/errordefs.h:86
   E_DLL_LOSS_PRES_TH : constant := 16#8243#;  --  ./oplk/errordefs.h:87
   E_DLL_LOSS_SOA_TH : constant := 16#8244#;  --  ./oplk/errordefs.h:88
   E_DLL_LOSS_SOC_TH : constant := 16#8245#;  --  ./oplk/errordefs.h:89

   E_NMT_BA1 : constant := 16#8410#;  --  ./oplk/errordefs.h:91
   E_NMT_BA1_NO_MN_SUPPORT : constant := 16#8411#;  --  ./oplk/errordefs.h:92
   E_NMT_BPO1 : constant := 16#8420#;  --  ./oplk/errordefs.h:93
   E_NMT_BPO1_GET_IDENT : constant := 16#8421#;  --  ./oplk/errordefs.h:94
   E_NMT_BPO1_DEVICE_TYPE : constant := 16#8422#;  --  ./oplk/errordefs.h:95
   E_NMT_BPO1_VENDOR_ID : constant := 16#8423#;  --  ./oplk/errordefs.h:96
   E_NMT_BPO1_PRODUCT_CODE : constant := 16#8424#;  --  ./oplk/errordefs.h:97
   E_NMT_BPO1_REVISION_NO : constant := 16#8425#;  --  ./oplk/errordefs.h:98
   E_NMT_BPO1_SERIAL_NO : constant := 16#8426#;  --  ./oplk/errordefs.h:99
   E_NMT_BPO1_CF_VERIFY : constant := 16#8428#;  --  ./oplk/errordefs.h:100
   E_NMT_BPO2 : constant := 16#8430#;  --  ./oplk/errordefs.h:101
   E_NMT_BRO : constant := 16#8440#;  --  ./oplk/errordefs.h:102
   E_NMT_WRONG_STATE : constant := 16#8480#;  --  ./oplk/errordefs.h:103

  --*
  --********************************************************************************
  --\file   oplk/errordefs.h
  --\brief  openPOWERLINK error definitions
  --Definitions of return values and errors.
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
  -- Emergency error codes
  -- 0xFxxx manufacturer specific error codes
  -- 0x816x HW errors
  -- 0x82xx Protocol errors
  -- 0x821x Frame size errors
  -- 0x823x Timing errors
  -- 0x824x Frame errors
  -- 0x84xx BootUp Errors
  --------------------------------------------------------------------------------
  -- typedef
  --------------------------------------------------------------------------------
  --*
  --\brief openPOWERLINK funciton error codes
  --The following enumeration specifies the error codes for return values of
  --openPOWERLINK functions.
  -- 

  -- area for generic errors 0x0000 - 0x000F
  --/< No error/successful run
  --/< The called instance does not exist
  --/< There was an invalid instance parameter
  --/< XxxAddInstance was called but no free instance is available
  --/< Wrong signature while writing to object 0x1010 or 0x1011
  --/< Operation is not allowed in this situation
  --/< An invalid NodeId was specified
  --/< The resource could not be created
  --/< Stack is shutting down
  --/< Reject the subsequent command
  --/< Retry this command
  --/< Invalid event was posted
  --/< General error
  --/< Features of user and kernel stack are mismatched
  -- area for EDRV module 0x0010 - 0x001F
  --/< No free TX descriptor available
  --/< Invalid cycle length (e.g. 0)
  --/< Edrv initialization error
  --/< No free entry in internal buffer table for TX frames
  --/< The specified TX buffer does not exist
  --/< The specified RX buffer is invalid
  --/< Invalid parameter in function call
  --/< Next TX buffer list is not empty, i.e. still in use
  --/< Current TX buffer list is empty, i.e. DLL didn't provide one
  --/< Current TX buffer list has not been finished yet, but new cycle has started
  -- area for DLL module 0x0020 - 0x002F
  --/< DLL is out of memory
  --/< Illegal handle for a TxFrame was passed
  --/< Handler for non-POWERLINK frames was already registered before
  --/< The buffer for SyncRequests is full
  --/< The transmit buffer for asynchronous frames is empty
  --/< Transmit buffer for asynchronous frames is full
  --/< MN: too less space in the internal node info structure
  --/< Invalid parameters passed to DLL function
  --/< Invalid AsndServiceId specified
  --/< TxBuffer (e.g. for PReq) is not ready yet
  --/< TxFrame (e.g. for PReq) is invalid or does not exist
  -- area for OBD module 0x0030 - 0x003F
  --/< Unknown OD part
  --/< Object index does not exist in OD
  --/< Sub-index does not exist in object index
  --/< Read access to a write-only object
  --/< Write access to a read-only object
  --/< Access not allowed
  --/< The object type is not defined/known
  --/< The object does not contain a VarEntry structure
  --/< The value to write to an object is too low
  --/< The value to write to an object is too high
  --/< The value to write is to long or to short
  --/< File I/O error occurred and errno is set
  --/< The device configuration file (CDC) is not valid
  --/< Out of memory
  --/< No configuration data present (CDC is empty)
  -- area for NMT module 0x0040 - 0x004F
  --/< Unknown NMT command
  --/< Pointer to the frame is not valid
  --/< Invalid event send to NMT-module
  --/< Unknown state in NMT state machine
  --/< Invalid parameters specified
  --/< SyncReq could not be issued
  -- area for SDO/UDP module 0x0050 - 0x005F
  --/< Missing callback-function pointer during init of module
  --/< Error during init of socket
  --/< Error during usage of socket
  --/< Error during start of listen thread
  --/< No free connection handle for Udp
  --/< Error during sending of frame
  --/< The connection handle is invalid
  -- area for SDO Sequence layer module 0x0060 - 0x006F
  --/< No SDO callback function is assigned
  --/< No free handle for connection
  --/< Invalid handle in SDO sequence layer
  --/< Unsupported Protocol selected
  --/< No free entry in history
  --/< The size of the frames is not correct
  --/< Indicates that the history buffer is full and a ack request is needed
  --/< The frame is not valid
  --/< Connection is busy -> retry later
  --/< Invalid event received
  -- area for SDO Command Layer module 0x0070 - 0x007F
  --/< Unsupported Protocol selected
  --/< No free handle for connection
  --/< Invalid SDO service type specified
  --/< Handle is invalid
  --/< The state of frame to send is not possible
  --/< Internal error: command layer handle is not responsible for this event from sequence layer
  --/< Handle to same node already exists
  --/< Transfer via this handle is already running
  --/< Invalid parameters passed to function
  -- area for openPOWERLINK event module 0x0080 - 0x008F
  --/< Unknown sink for event
  --/< Error during post of event
  --/< Error during reading of event from queue
  --/< Event arg has wrong size
  -- area for openPOWERLINK timer module 0x0090 - 0x009F
  --/< Invalid handle for timer
  --/< No timer was created caused by an error
  --/< Process thread could not be created
  -- area for openPOWERLINK SDO/Asnd module 0x00A0 - 0x0AF
  --/< Node-ID is invalid
  --/< No free handle for connection
  --/< Handle for connection is invalid
  -- area for PDO module 0x00B0 - 0x00BF
  --/< Selected PDO does not exist
  --/< Length of PDO mapping exceeds the current payload limit
  --/< Configured PDO granularity is not equal to supported granularity
  --/< Error during initialization of PDO module
  --/< PDO configuration cannot be changed while it is enabled
  --/< Invalid PDO mapping
  --/< The referenced object in a PDO mapping does not exist
  --/< The referenced object in a PDO mapping is not mappable
  --/< Bit size of object mapping is larger than the object size
  --/< There exits more than one TPDO on CN
  --/< Invalid object index used for PDO mapping or communication parameter
  --/< Too many PDOs do exist
  -- Configuration manager module 0x00C0 - 0x00CF
  --/< Error in configuration manager
  --/< Error in configuration manager, SDO timeout
  --/< Device configuration file (CDC) is not valid
  --/< Unsupported Dcf format
  --/< Configuration finished with errors
  --/< No free configuration entry
  --/< No configuration data present
  --/< Unsupported datatype found in dcf -> this entry was not configured
  --/< openPOWERLINK performs task in background and informs the application (or vice-versa), when it is finished
  --/< Passed invalid parameters to a function (e.g. invalid node id)
  --/< No function pointer for ObdInitRam supplied
  --/< The SDO channel to this node is internally used by the stack (e.g. the CFM) and currently not available for the application.
  --/< Process image is already allocated
  --/< Process image: out of memory
  --/< Process image: variable linking or copy job exceeds the size of the PI
  --/< Process image is not allocated
  --/< Process image: job queue is full
  --/< Process image: job queue is empty
  --/< Process image: invalid job size
  --/< Process image: pointer to application's process image is invalid
  --/< Process image: non-blocking copy jobs are not supported on this target
  --/< API called but stack is not initialized/running
  -- area until 0x07FF is reserved
  -- area for user application from 0x0800 to 0x7FFF
   subtype tOplkError is unsigned;
   kErrorOk : constant tOplkError := 0;
   kErrorIllegalInstance : constant tOplkError := 1;
   kErrorInvalidInstanceParam : constant tOplkError := 2;
   kErrorNoFreeInstance : constant tOplkError := 3;
   kErrorWrongSignature : constant tOplkError := 4;
   kErrorInvalidOperation : constant tOplkError := 5;
   kErrorInvalidNodeId : constant tOplkError := 7;
   kErrorNoResource : constant tOplkError := 8;
   kErrorShutdown : constant tOplkError := 9;
   kErrorReject : constant tOplkError := 10;
   kErrorRetry : constant tOplkError := 11;
   kErrorInvalidEvent : constant tOplkError := 12;
   kErrorGeneralError : constant tOplkError := 13;
   kErrorFeatureMismatch : constant tOplkError := 14;
   kErrorEdrvNoFreeTxDesc : constant tOplkError := 17;
   kErrorEdrvInvalidCycleLen : constant tOplkError := 18;
   kErrorEdrvInit : constant tOplkError := 19;
   kErrorEdrvNoFreeBufEntry : constant tOplkError := 20;
   kErrorEdrvBufNotExisting : constant tOplkError := 21;
   kErrorEdrvInvalidRxBuf : constant tOplkError := 22;
   kErrorEdrvInvalidParam : constant tOplkError := 28;
   kErrorEdrvNextTxListNotEmpty : constant tOplkError := 29;
   kErrorEdrvCurTxListEmpty : constant tOplkError := 30;
   kErrorEdrvTxListNotFinishedYet : constant tOplkError := 31;
   kErrorDllOutOfMemory : constant tOplkError := 33;
   kErrorDllIllegalHdl : constant tOplkError := 34;
   kErrorDllCbAsyncRegistered : constant tOplkError := 35;
   kErrorDllAsyncSyncReqFull : constant tOplkError := 36;
   kErrorDllAsyncTxBufferEmpty : constant tOplkError := 37;
   kErrorDllAsyncTxBufferFull : constant tOplkError := 38;
   kErrorDllNoNodeInfo : constant tOplkError := 39;
   kErrorDllInvalidParam : constant tOplkError := 40;
   kErrorDllInvalidAsndServiceId : constant tOplkError := 41;
   kErrorDllTxBufNotReady : constant tOplkError := 46;
   kErrorDllTxFrameInvalid : constant tOplkError := 47;
   kErrorObdIllegalPart : constant tOplkError := 48;
   kErrorObdIndexNotExist : constant tOplkError := 49;
   kErrorObdSubindexNotExist : constant tOplkError := 50;
   kErrorObdReadViolation : constant tOplkError := 51;
   kErrorObdWriteViolation : constant tOplkError := 52;
   kErrorObdAccessViolation : constant tOplkError := 53;
   kErrorObdUnknownObjectType : constant tOplkError := 54;
   kErrorObdVarEntryNotExist : constant tOplkError := 55;
   kErrorObdValueTooLow : constant tOplkError := 56;
   kErrorObdValueTooHigh : constant tOplkError := 57;
   kErrorObdValueLengthError : constant tOplkError := 58;
   kErrorObdErrnoSet : constant tOplkError := 59;
   kErrorObdInvalidDcf : constant tOplkError := 60;
   kErrorObdOutOfMemory : constant tOplkError := 61;
   kErrorObdNoConfigData : constant tOplkError := 62;
   kErrorNmtUnknownCommand : constant tOplkError := 64;
   kErrorNmtInvalidFramePointer : constant tOplkError := 65;
   kErrorNmtInvalidEvent : constant tOplkError := 66;
   kErrorNmtInvalidState : constant tOplkError := 67;
   kErrorNmtInvalidParam : constant tOplkError := 68;
   kErrorNmtSyncReqRejected : constant tOplkError := 69;
   kErrorSdoUdpMissCb : constant tOplkError := 80;
   kErrorSdoUdpNoSocket : constant tOplkError := 81;
   kErrorSdoUdpSocketError : constant tOplkError := 82;
   kErrorSdoUdpThreadError : constant tOplkError := 83;
   kErrorSdoUdpNoFreeHandle : constant tOplkError := 84;
   kErrorSdoUdpSendError : constant tOplkError := 85;
   kErrorSdoUdpInvalidHdl : constant tOplkError := 86;
   kErrorSdoSeqMissCb : constant tOplkError := 96;
   kErrorSdoSeqNoFreeHandle : constant tOplkError := 97;
   kErrorSdoSeqInvalidHdl : constant tOplkError := 98;
   kErrorSdoSeqUnsupportedProt : constant tOplkError := 99;
   kErrorSdoSeqNoFreeHistory : constant tOplkError := 100;
   kErrorSdoSeqFrameSizeError : constant tOplkError := 101;
   kErrorSdoSeqRequestAckNeeded : constant tOplkError := 102;
   kErrorSdoSeqInvalidFrame : constant tOplkError := 103;
   kErrorSdoSeqConnectionBusy : constant tOplkError := 104;
   kErrorSdoSeqInvalidEvent : constant tOplkError := 105;
   kErrorSdoComUnsupportedProt : constant tOplkError := 112;
   kErrorSdoComNoFreeHandle : constant tOplkError := 113;
   kErrorSdoComInvalidServiceType : constant tOplkError := 114;
   kErrorSdoComInvalidHandle : constant tOplkError := 115;
   kErrorSdoComInvalidSendType : constant tOplkError := 116;
   kErrorSdoComNotResponsible : constant tOplkError := 117;
   kErrorSdoComHandleExists : constant tOplkError := 118;
   kErrorSdoComHandleBusy : constant tOplkError := 119;
   kErrorSdoComInvalidParam : constant tOplkError := 120;
   kErrorEventUnknownSink : constant tOplkError := 128;
   kErrorEventPostError : constant tOplkError := 129;
   kErrorEventReadError : constant tOplkError := 130;
   kErrorEventWrongSize : constant tOplkError := 131;
   kErrorTimerInvalidHandle : constant tOplkError := 144;
   kErrorTimerNoTimerCreated : constant tOplkError := 145;
   kErrorTimerThreadError : constant tOplkError := 146;
   kErrorSdoAsndInvalidNodeId : constant tOplkError := 160;
   kErrorSdoAsndNoFreeHandle : constant tOplkError := 161;
   kErrorSdoAsndInvalidHandle : constant tOplkError := 162;
   kErrorPdoNotExist : constant tOplkError := 176;
   kErrorPdoLengthExceeded : constant tOplkError := 177;
   kErrorPdoGranularityMismatch : constant tOplkError := 178;
   kErrorPdoInitError : constant tOplkError := 179;
   kErrorPdoConfWhileEnabled : constant tOplkError := 183;
   kErrorPdoErrorMapp : constant tOplkError := 184;
   kErrorPdoVarNotFound : constant tOplkError := 185;
   kErrorPdoVarNotMappable : constant tOplkError := 186;
   kErrorPdoSizeMismatch : constant tOplkError := 188;
   kErrorPdoTooManyTxPdos : constant tOplkError := 189;
   kErrorPdoInvalidObjIndex : constant tOplkError := 190;
   kErrorPdoTooManyPdos : constant tOplkError := 191;
   kErrorCfmConfigError : constant tOplkError := 192;
   kErrorCfmSdocTimeOutError : constant tOplkError := 193;
   kErrorCfmInvalidDcf : constant tOplkError := 194;
   kErrorCfmUnsupportedDcf : constant tOplkError := 195;
   kErrorCfmConfigWithErrors : constant tOplkError := 196;
   kErrorCfmNoFreeConfig : constant tOplkError := 197;
   kErrorCfmNoConfigData : constant tOplkError := 198;
   kErrorCfmUnsuppDatatypeDcf : constant tOplkError := 199;
   kErrorApiTaskDeferred : constant tOplkError := 320;
   kErrorApiInvalidParam : constant tOplkError := 322;
   kErrorApiNoObdInitRam : constant tOplkError := 323;
   kErrorApiSdoBusyIntern : constant tOplkError := 324;
   kErrorApiPIAlreadyAllocated : constant tOplkError := 325;
   kErrorApiPIOutOfMemory : constant tOplkError := 326;
   kErrorApiPISizeExceeded : constant tOplkError := 327;
   kErrorApiPINotAllocated : constant tOplkError := 328;
   kErrorApiPIJobQueueFull : constant tOplkError := 329;
   kErrorApiPIJobQueueEmpty : constant tOplkError := 330;
   kErrorApiPIInvalidJobSize : constant tOplkError := 331;
   kErrorApiPIInvalidPIPointer : constant tOplkError := 332;
   kErrorApiPINonBlockingNotSupp : constant tOplkError := 333;
   kErrorApiNotInitialized : constant tOplkError := 334;  -- ./oplk/errordefs.h:273

end oplk.errordefs_h;

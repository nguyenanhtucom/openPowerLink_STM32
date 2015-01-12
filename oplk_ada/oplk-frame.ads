--\brief  Definitions for POWERLINK frames
--This header file contains definitions describing POWERLINK frames.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Oplk.oplkinc;
with Interfaces.C.Extensions;

package Oplk.frame is
   
   --   Defines for tFrameData.flag1 and tFrameData.flag2   --
   PLK_FRAME_FLAG1_RD                    : constant := 16#01#;
   -- ready                                    (PReq, PRes)
   PLK_FRAME_FLAG1_ER                    : constant := 16#02#;
   -- exception reset (error signalling)       (SoA)
   PLK_FRAME_FLAG1_EA                    : constant := 16#04#;
   -- exception acknowledge (error signalling) (PReq, SoA)
   PLK_FRAME_FLAG1_EC                    : constant := 16#08#;
   -- exception clear (error signalling)       (StatusRes)
   PLK_FRAME_FLAG1_EN                    : constant := 16#10#;
   -- exception new (error signalling)         (PRes, StatusRes)
   PLK_FRAME_FLAG1_MS                    : constant := 16#20#;
   -- multiplexed slot                         (PReq)
   PLK_FRAME_FLAG1_PS                    : constant := 16#40#;
   -- prescaled slot                           (SoC)
   PLK_FRAME_FLAG1_MC                    : constant := 16#80#;
   -- multiplexed cycle completed              (SoC)
   PLK_FRAME_FLAG2_RS                    : constant := 16#07#;
   -- number of pending requests to send       (PRes, StatusRes, IdentRes)
   PLK_FRAME_FLAG2_PR                    : constant := 16#38#;
   -- priority of requested asynch. frame      (PRes, StatusRes, IdentRes)
   PLK_FRAME_FLAG2_PR_SHIFT              : constant := 3;
   -- shift of priority of requested asynch. frame
   
   --   error history/status entry types   --
   ERR_ENTRYTYPE_STATUS                  : constant := 16#8000#;
   ERR_ENTRYTYPE_HISTORY                 : constant := 16#0000#;
   ERR_ENTRYTYPE_EMCY                    : constant := 16#4000#;
   ERR_ENTRYTYPE_MODE_ACTIVE             : constant := 16#1000#;
   ERR_ENTRYTYPE_MODE_CLEARED            : constant := 16#2000#;
   ERR_ENTRYTYPE_MODE_OCCURRED           : constant := 16#3000#;
   ERR_ENTRYTYPE_MODE_MASK               : constant := 16#3000#;
   ERR_ENTRYTYPE_PROF_VENDOR             : constant := 16#0001#;
   ERR_ENTRYTYPE_PROF_PLK                : constant := 16#0002#;
   ERR_ENTRYTYPE_PROF_MASK               : constant := 16#0FFF#;
   
   --   defines for POWERLINK version / PDO version   --
   PLK_VERSION_SUB                       : constant := 16#0F#;
   -- sub version
   PLK_VERSION_MAIN                      : constant := 16#F0#;
   -- main version
   
   PLK_FRAME_OFFSET_DST_MAC              : constant := 0;
   PLK_FRAME_OFFSET_SRC_MAC              : constant := 6;
   PLK_FRAME_OFFSET_ETHER_TYPE           : constant := 12;
   PLK_FRAME_OFFSET_MSG_TYPE             : constant := 14;
   PLK_FRAME_OFFSET_DST_NODEID           : constant := 15;
   PLK_FRAME_OFFSET_SRC_NODEID           : constant := 16;
   PLK_FRAME_OFFSET_SDO_SEQU             : constant := 18;
   PLK_FRAME_OFFSET_SDO_COMU             : constant := 22;
   PLK_FRAME_OFFSET_PDO_PAYLOAD          : constant := 24;
   
   --   defines for bit fields SyncControl and SyncStatus   --
   PLK_SYNC_PRES_TIME_FIRST_VALID        : constant := 16#00000001#;
   PLK_SYNC_PRES_TIME_SECOND_VALID       : constant := 16#00000002#;
   PLK_SYNC_SYNC_MN_DELAY_FIRST_VALID    : constant := 16#00000004#;
   PLK_SYNC_SYNC_MN_DELAY_SECOND_VALID   : constant := 16#00000008#;
   PLK_SYNC_PRES_FALL_BACK_TIMEOUT_VALID : constant := 16#00000010#;
   PLK_SYNC_DEST_MAC_ADDRESS_VALID       : constant := 16#00000020#;
   PLK_SYNC_PRES_MODE_RESET              : constant := 16#40000000#;
   PLK_SYNC_PRES_MODE_SET                : constant := 16#80000000#;
   
   --   defines for SDO command layer   --
   SDO_CMDL_HDR_FIXED_SIZE               : constant := 8;
   -- size of fixed header part
   SDO_CMDL_HDR_VAR_SIZE                 : constant := 4;
   -- size of variable header part
   SDO_CMDL_HDR_WRITEBYINDEX_SIZE        : constant := 4;
   -- size of write by index header (index + subindex + reserved)
   SDO_CMDL_HDR_READBYINDEX_SIZE         : constant := 4;
   -- size of read by index header (index + subindex + reserved)
   
   --   defines for SDO command layer flags   --
   SDO_CMDL_FLAG_RESPONSE                : constant := 16#80#;
   SDO_CMDL_FLAG_ABORT                   : constant := 16#40#;
   SDO_CMDL_FLAG_EXPEDITED               : constant := 16#00#;
   SDO_CMDL_FLAG_SEGMINIT                : constant := 16#10#;
   SDO_CMDL_FLAG_SEGMENTED               : constant := 16#20#;
   SDO_CMDL_FLAG_SEGMCOMPL               : constant := 16#30#;
   SDO_CMDL_FLAG_SEGM_MASK               : constant := 16#30#;

------------------------------------------------------------------------------
-- typedef                                                                  --
------------------------------------------------------------------------------

   type tSocFrame is record
      reserved1      : aliased unsigned_char;
      -- Offset 17: reserved
      flag1          : aliased unsigned_char;
      -- Offset 18: Flags: MC, PS
      flag2          : aliased unsigned_char;
      -- Offset 19: Flags: res
      netTimeLe      : aliased Oplk.oplkinc.tNetTime;
      -- Offset 20: supported if D_NMT_NetTimeIsRealTime_BOOL is set
      relativeTimeLe : aliased Extensions.unsigned_long_long;
      -- Offset 28: in us (supported if D_NMT_RelativeTime_BOOL is set)
   end record;
   pragma Convention (C_Pass_By_Copy, tSocFrame);  -- ./oplk/frame.h:136
   
   
   type tPreqFrame_aPayload_array is array (0 .. 255) of aliased unsigned_char;
   type tPreqFrame is record
      reserved1  : aliased unsigned_char;
      -- Offset 17: reserved
      flag1      : aliased unsigned_char;
      -- Offset 18: Flags: MS, EA, RD
      flag2      : aliased unsigned_char;
      -- Offset 19: Flags: res
      pdoVersion : aliased unsigned_char;
      -- Offset 20: PDO Version
      reserved2  : aliased unsigned_char;
      -- Offset 21: reserved
      sizeLe     : aliased unsigned_short;
      -- Offset 22:
      aPayload   : aliased tPreqFrame_aPayload_array;
      -- Offset 24: Payload
   end record;
   pragma Convention (C_Pass_By_Copy, tPreqFrame);  -- ./oplk/frame.h:147
   
   
   type tPresFrame_aPayload_array is array (0 .. 255) of aliased unsigned_char;
   type tPresFrame is record
      nmtStatus  : aliased unsigned_char;
      -- Offset 17: NMT state
      flag1      : aliased unsigned_char;
      -- Offset 18: Flags: MS, EN, RD
      flag2      : aliased unsigned_char;
      -- Offset 19: Flags: PR, RS
      pdoVersion : aliased unsigned_char;
      -- Offset 20:
      reserved2  : aliased unsigned_char;
      -- Offset 21: reserved
      sizeLe     : aliased unsigned_short;
      -- Offset 22:
      aPayload   : aliased tPresFrame_aPayload_array;
      -- Offset 24: Payload
   end record;
   pragma Convention (C_Pass_By_Copy, tPresFrame);  -- ./oplk/frame.h:158
   
   
   type tSyncRequest_aDestMacAddress_array is
     array (0 .. 5) of aliased unsigned_char;
   type tSyncRequest is record
      reserved              : aliased unsigned_char;
      -- Offset 23:
      syncControlLe         : aliased unsigned;
      -- 
      presTimeFirstLe       : aliased unsigned;
      -- 
      presTimeSecondLe      : aliased unsigned;
      -- 
      syncMnDelayFirstLe    : aliased unsigned;
      -- 
      syncMnDelaySecondLe   : aliased unsigned;
      -- 
      presFallBackTimeoutLe : aliased unsigned;
      -- 
      aDestMacAddress : aliased tSyncRequest_aDestMacAddress_array;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tSyncRequest);  -- ./oplk/frame.h:170
   
   
   type tSoaPayload (discr : unsigned := 0) is record
      case discr is
         when others =>
            syncRequest : aliased tSyncRequest;
	    -- Offset 23
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tSoaPayload);
   pragma Unchecked_Union (tSoaPayload);  -- ./oplk/frame.h:175
   
   
   type tSoaFrame is record
      nmtStatus        : aliased unsigned_char;
      -- Offset 17:NMT state
      flag1            : aliased unsigned_char;
      -- Offset 18: Flags: EA, ER
      flag2            : aliased unsigned_char;
      -- Offset 19: Flags: res
      reqServiceId     : aliased unsigned_char;
      -- Offset 20:
      reqServiceTarget : aliased unsigned_char;
      -- Offset 21:
      powerlinkVersion : aliased unsigned_char;
      -- Offset 22:
      payload          : tSoaPayload;
      -- Offset 23:
   end record;
   pragma Convention (C_Pass_By_Copy, tSoaFrame);  -- ./oplk/frame.h:186
   

------------------------------------------------------------------------------
--\brief Structure for error history entries                                --
--                                                                          --
-- The following structure defines an error history entry.                  --
------------------------------------------------------------------------------
   type tErrHistoryEntry_aAddInfo_array is
     array (0 .. 7) of aliased unsigned_char;
   type tErrHistoryEntry is record
      entryType : aliased unsigned_short;
      -- The type of the entry
      errorCode : aliased unsigned_short;
      -- The error code of the entry
      timeStamp : aliased Oplk.oplkinc.tNetTime;
      -- The timestamp when the error was added
      aAddInfo : aliased tErrHistoryEntry_aAddInfo_array;
      -- Additional error information
   end record;
   pragma Convention (C_Pass_By_Copy, tErrHistoryEntry);  -- ./oplk/frame.h:199
   
   
   type tStatusResponse_reserved1_array is
     array (0 .. 2) of aliased unsigned_char;
   type tStatusResponse_aErrorHistoryEntry_array is
     array (0 .. 13) of aliased tErrHistoryEntry;
   type tStatusResponse is record
      flag1         : aliased unsigned_char;
      -- Offset 18: Flags: EN, EC
      flag2         : aliased unsigned_char;
      -- Offset 19: Flags: PR, RS
      nmtStatus     : aliased unsigned_char;
      -- Offset 20: NMT state
      reserved1     : aliased tStatusResponse_reserved1_array;
      -- 
      staticErrorLe : aliased Extensions.unsigned_long_long;
      -- static error bit field
      aErrorHistoryEntry : aliased tStatusResponse_aErrorHistoryEntry_array;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tStatusResponse);  -- ./oplk/frame.h:209
   
   
   type tIdentResponse_sHostName_array is
     array (0 .. 31) of aliased unsigned_char;
   type tIdentResponse_aVendorSpecificExt2_array is
     array (0 .. 47) of aliased unsigned_char;
   type tIdentResponse is record
      flag1                   : aliased unsigned_char;
      -- Offset 18: Flags: res
      flag2                   : aliased unsigned_char;
      -- Flags: PR, RS
      nmtStatus               : aliased unsigned_char;
      -- NMT state
      identResponseFlags      : aliased unsigned_char;
      -- Flags: FW
      powerlinkProfileVersion : aliased unsigned_char;
      -- 
      reserved1               : aliased unsigned_char;
      -- 
      featureFlagsLe          : aliased unsigned;
      -- NMT_FeatureFlags_U32
      mtuLe                   : aliased unsigned_short;
      -- NMT_CycleTiming_REC.AsyncMTU_U16: C_IP_MIN_MTU - C_IP_MAX_MTU
      pollInSizeLe            : aliased unsigned_short;
      -- NMT_CycleTiming_REC.PReqActPayload_U16
      pollOutSizeLe           : aliased unsigned_short;
      -- NMT_CycleTiming_REC.PResActPayload_U16
      responseTimeLe          : aliased unsigned;
      -- NMT_CycleTiming_REC.PResMaxLatency_U32
      reserved2               : aliased unsigned_short;
      -- 
      deviceTypeLe            : aliased unsigned;
      -- NMT_DeviceType_U32
      vendorIdLe              : aliased unsigned;
      -- NMT_IdentityObject_REC.VendorId_U32
      productCodeLe           : aliased unsigned;
      -- NMT_IdentityObject_REC.ProductCode_U32
      revisionNumberLe        : aliased unsigned;
      -- NMT_IdentityObject_REC.RevisionNo_U32
      serialNumberLe          : aliased unsigned;
      -- NMT_IdentityObject_REC.SerialNo_U32
      vendorSpecificExt1Le    : aliased Extensions.unsigned_long_long;
      -- 
      verifyConfigurationDateLe : aliased unsigned;
      -- CFM_VerifyConfiguration_REC.ConfDate_U32
      verifyConfigurationTimeLe : aliased unsigned;
      -- CFM_VerifyConfiguration_REC.ConfTime_U32
      applicationSwDateLe       : aliased unsigned;
      -- PDL_LocVerApplSw_REC.ApplSwDate_U32 on programmable device or 
      -- date portion of NMT_ManufactSwVers_VS on non-programmable device
      applicationSwTimeLe       : aliased unsigned;
      -- PDL_LocVerApplSw_REC.ApplSwTime_U32 on programmable device or 
      -- time portion of NMT_ManufactSwVers_VS on non-programmable device
      ipAddressLe               : aliased unsigned;
      -- 
      subnetMaskLe              : aliased unsigned;
      -- 
      defaultGatewayLe          : aliased unsigned;
      -- 
      sHostName                 : aliased tIdentResponse_sHostName_array;
      -- 
      aVendorSpecificExt2       : aliased tIdentResponse_aVendorSpecificExt2_array;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tIdentResponse);  -- ./oplk/frame.h:240
   
   
   type tNmtCommandService_aNmtCommandData_array is
     array (0 .. 31) of aliased unsigned_char;
   type tNmtCommandService is record
      nmtCommandId    : aliased unsigned_char;
      -- Offset 18:
      reserved1       : aliased unsigned_char;
      -- 
      aNmtCommandData : aliased tNmtCommandService_aNmtCommandData_array;
      -- 
   end record;
   pragma Convention
     (C_Pass_By_Copy, tNmtCommandService);  -- ./oplk/frame.h:247
   
   
   type tSyncResponse is record
      reserved         : aliased unsigned_short;
      -- Offset 18:
      syncStatusLe     : aliased unsigned;
      -- 
      latencyLe        : aliased unsigned;
      -- 
      syncNodeNumberLe : aliased unsigned;
      -- 
      syncDelayLe      : aliased unsigned;
      -- 
      presTimeFirstLe  : aliased unsigned;
      -- 
      presTimeSecondLe : aliased unsigned;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tSyncResponse);  -- ./oplk/frame.h:258
   
   
   type tAsySdoCom_aCommandData_array is
     array (0 .. 7) of aliased unsigned_char;
   type tAsySdoCom is record
      reserved1     : aliased unsigned_char;
      -- 
      transactionId : aliased unsigned_char;
      -- 
      flags         : aliased unsigned_char;
      -- 
      commandId     : aliased unsigned_char;
      -- 
      segmentSizeLe : aliased unsigned_short;
      -- 
      reserved2     : aliased unsigned_short;
      -- 
      aCommandData : aliased tAsySdoCom_aCommandData_array;
      -- just reserve a minimum number of bytes as a placeholder
   end record;
   pragma Convention (C_Pass_By_Copy, tAsySdoCom);  -- ./oplk/frame.h:269
   
   
   --  asynchronous SDO Sequence Header   --
   type tAsySdoSeq_aReserved_array is array (0 .. 1) of aliased unsigned_char;
   type tAsySdoSeq is record
      recvSeqNumCon : aliased unsigned_char;
      -- 
      sendSeqNumCon : aliased unsigned_char;
      -- 
      aReserved     : aliased tAsySdoSeq_aReserved_array;
      -- 
      sdoSeqPayload : aliased tAsySdoCom;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tAsySdoSeq);  -- ./oplk/frame.h:279
   
   
   type tNmtRequestService_aNmtCommandData_array is
     array (0 .. 31) of aliased unsigned_char;
   type tNmtRequestService is record
      nmtCommandId    : aliased unsigned_char;
      -- Offset 18
      targetNodeId    : aliased unsigned_char;
      -- 
      aNmtCommandData : aliased tNmtRequestService_aNmtCommandData_array;
      -- 
   end record;
   pragma Convention
     (C_Pass_By_Copy, tNmtRequestService);  -- ./oplk/frame.h:287
   
   
   type tAsndPayload_aPayload_array is
     array (0 .. 255) of aliased unsigned_char;
   type tAsndPayload (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            statusResponse : aliased tStatusResponse;
	    -- Offset 18:
         when 1 =>
            identResponse : aliased tIdentResponse;
	    -- 
         when 2 =>
            nmtCommandService : aliased tNmtCommandService;
	    -- 
         when 3 =>
            nmtRequestService : aliased tNmtRequestService;
	    -- 
         when 4 =>
            sdoSequenceFrame : aliased tAsySdoSeq;
	    -- 
         when 5 =>
            syncResponse : aliased tSyncResponse;
	    -- 
         when others =>
            aPayload : aliased tAsndPayload_aPayload_array;
	    -- 
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tAsndPayload);
   pragma Unchecked_Union (tAsndPayload);  -- ./oplk/frame.h:299
   
   
   type tAsndFrame is record
      serviceId : aliased unsigned_char;
      -- Offset 17
      payload   : tAsndPayload;
      -- Offset 18
   end record;
   pragma Convention (C_Pass_By_Copy, tAsndFrame);  -- ./oplk/frame.h:305
   
   
   type tFrameData (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            soc : aliased tSocFrame;
	    -- Offset 17
         when 1 =>
            preq : aliased tPreqFrame;
	    -- 
         when 2 =>
            pres : aliased tPresFrame;
	    -- 
         when 3 =>
            soa : aliased tSoaFrame;
	    -- 
         when others =>
            asnd : aliased tAsndFrame;
	    -- 
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tFrameData);
   pragma Unchecked_Union (tFrameData);  -- ./oplk/frame.h:314
   
   
   type tPlkFrame_aDstMac_array is array (0 .. 5) of aliased unsigned_char;
   type tPlkFrame_aSrcMac_array is array (0 .. 5) of aliased unsigned_char;
   type tPlkFrame is record
      aDstMac     : aliased tPlkFrame_aDstMac_array;
      -- Offset 0: MAC address of the addressed nodes
      aSrcMac     : aliased tPlkFrame_aSrcMac_array;
      -- Offset 6: MAC address of the transmitting node
      etherType   : aliased unsigned_short;
      -- Offset 12: Ethernet message type (big endian)
      messageType : aliased unsigned_char;
      -- Offset 14: POWERLINK message type
      dstNodeId   : aliased unsigned_char;
      -- Offset 15: POWERLINK node ID of the addressed nodes
      srcNodeId   : aliased unsigned_char;
      -- Offset 16: POWERLINK node ID of the transmitting node
      data        : tFrameData;
      -- Offset 17:
   end record;
   pragma Convention (C_Pass_By_Copy, tPlkFrame);  -- ./oplk/frame.h:325
   
   
   subtype tMsgType is unsigned;
   kMsgTypeNonPowerlink : constant tMsgType := 0;
   kMsgTypeSoc          : constant tMsgType := 1;
   kMsgTypePreq         : constant tMsgType := 3;
   kMsgTypePres         : constant tMsgType := 4;
   kMsgTypeSoa          : constant tMsgType := 5;
   kMsgTypeAsnd         : constant tMsgType := 6;
   kMsgTypeAInv         : constant tMsgType := 13;  -- ./oplk/frame.h:342

end Oplk.frame;

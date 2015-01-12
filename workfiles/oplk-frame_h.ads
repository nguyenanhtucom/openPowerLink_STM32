pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with oplk.oplkinc_h;
with Interfaces.C.Extensions;

package oplk.frame_h is


   PLK_FRAME_FLAG1_RD : constant := 16#01#;  --  ./oplk/frame.h:50
   PLK_FRAME_FLAG1_ER : constant := 16#02#;  --  ./oplk/frame.h:51
   PLK_FRAME_FLAG1_EA : constant := 16#04#;  --  ./oplk/frame.h:52
   PLK_FRAME_FLAG1_EC : constant := 16#08#;  --  ./oplk/frame.h:53
   PLK_FRAME_FLAG1_EN : constant := 16#10#;  --  ./oplk/frame.h:54
   PLK_FRAME_FLAG1_MS : constant := 16#20#;  --  ./oplk/frame.h:55
   PLK_FRAME_FLAG1_PS : constant := 16#40#;  --  ./oplk/frame.h:56
   PLK_FRAME_FLAG1_MC : constant := 16#80#;  --  ./oplk/frame.h:57
   PLK_FRAME_FLAG2_RS : constant := 16#07#;  --  ./oplk/frame.h:58
   PLK_FRAME_FLAG2_PR : constant := 16#38#;  --  ./oplk/frame.h:59
   PLK_FRAME_FLAG2_PR_SHIFT : constant := 3;  --  ./oplk/frame.h:60

   ERR_ENTRYTYPE_STATUS : constant := 16#8000#;  --  ./oplk/frame.h:63
   ERR_ENTRYTYPE_HISTORY : constant := 16#0000#;  --  ./oplk/frame.h:64
   ERR_ENTRYTYPE_EMCY : constant := 16#4000#;  --  ./oplk/frame.h:65
   ERR_ENTRYTYPE_MODE_ACTIVE : constant := 16#1000#;  --  ./oplk/frame.h:66
   ERR_ENTRYTYPE_MODE_CLEARED : constant := 16#2000#;  --  ./oplk/frame.h:67
   ERR_ENTRYTYPE_MODE_OCCURRED : constant := 16#3000#;  --  ./oplk/frame.h:68
   ERR_ENTRYTYPE_MODE_MASK : constant := 16#3000#;  --  ./oplk/frame.h:69
   ERR_ENTRYTYPE_PROF_VENDOR : constant := 16#0001#;  --  ./oplk/frame.h:70
   ERR_ENTRYTYPE_PROF_PLK : constant := 16#0002#;  --  ./oplk/frame.h:71
   ERR_ENTRYTYPE_PROF_MASK : constant := 16#0FFF#;  --  ./oplk/frame.h:72

   PLK_VERSION_SUB : constant := 16#0F#;  --  ./oplk/frame.h:75
   PLK_VERSION_MAIN : constant := 16#F0#;  --  ./oplk/frame.h:76

   PLK_FRAME_OFFSET_DST_MAC : constant := 0;  --  ./oplk/frame.h:79
   PLK_FRAME_OFFSET_SRC_MAC : constant := 6;  --  ./oplk/frame.h:80
   PLK_FRAME_OFFSET_ETHER_TYPE : constant := 12;  --  ./oplk/frame.h:81
   PLK_FRAME_OFFSET_MSG_TYPE : constant := 14;  --  ./oplk/frame.h:82
   PLK_FRAME_OFFSET_DST_NODEID : constant := 15;  --  ./oplk/frame.h:83
   PLK_FRAME_OFFSET_SRC_NODEID : constant := 16;  --  ./oplk/frame.h:84
   PLK_FRAME_OFFSET_SDO_SEQU : constant := 18;  --  ./oplk/frame.h:85
   PLK_FRAME_OFFSET_SDO_COMU : constant := 22;  --  ./oplk/frame.h:86
   PLK_FRAME_OFFSET_PDO_PAYLOAD : constant := 24;  --  ./oplk/frame.h:87

   PLK_SYNC_PRES_TIME_FIRST_VALID : constant := 16#00000001#;  --  ./oplk/frame.h:90
   PLK_SYNC_PRES_TIME_SECOND_VALID : constant := 16#00000002#;  --  ./oplk/frame.h:91
   PLK_SYNC_SYNC_MN_DELAY_FIRST_VALID : constant := 16#00000004#;  --  ./oplk/frame.h:92
   PLK_SYNC_SYNC_MN_DELAY_SECOND_VALID : constant := 16#00000008#;  --  ./oplk/frame.h:93
   PLK_SYNC_PRES_FALL_BACK_TIMEOUT_VALID : constant := 16#00000010#;  --  ./oplk/frame.h:94
   PLK_SYNC_DEST_MAC_ADDRESS_VALID : constant := 16#00000020#;  --  ./oplk/frame.h:95
   PLK_SYNC_PRES_MODE_RESET : constant := 16#40000000#;  --  ./oplk/frame.h:96
   PLK_SYNC_PRES_MODE_SET : constant := 16#80000000#;  --  ./oplk/frame.h:97

   SDO_CMDL_HDR_FIXED_SIZE : constant := 8;  --  ./oplk/frame.h:100
   SDO_CMDL_HDR_VAR_SIZE : constant := 4;  --  ./oplk/frame.h:101
   SDO_CMDL_HDR_WRITEBYINDEX_SIZE : constant := 4;  --  ./oplk/frame.h:102
   SDO_CMDL_HDR_READBYINDEX_SIZE : constant := 4;  --  ./oplk/frame.h:103

   SDO_CMDL_FLAG_RESPONSE : constant := 16#80#;  --  ./oplk/frame.h:106
   SDO_CMDL_FLAG_ABORT : constant := 16#40#;  --  ./oplk/frame.h:107
   SDO_CMDL_FLAG_EXPEDITED : constant := 16#00#;  --  ./oplk/frame.h:108
   SDO_CMDL_FLAG_SEGMINIT : constant := 16#10#;  --  ./oplk/frame.h:109
   SDO_CMDL_FLAG_SEGMENTED : constant := 16#20#;  --  ./oplk/frame.h:110
   SDO_CMDL_FLAG_SEGMCOMPL : constant := 16#30#;  --  ./oplk/frame.h:111
   SDO_CMDL_FLAG_SEGM_MASK : constant := 16#30#;  --  ./oplk/frame.h:112
   --  unsupported macro: PACK_STRUCT __attribute__((packed))

  --*
  --********************************************************************************
  --\file   oplk/frame.h
  --\brief  Definitions for POWERLINK frames
  --This header file contains definitions describing POWERLINK frames.
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
  -- Defines for tFrameData.flag1 and tFrameData.flag2
  -- error history/status entry types
  -- defines for POWERLINK version / PDO version
  -- defines for bit fields SyncControl and SyncStatus
  -- defines for SDO command layer
  -- defines for SDO command layer flags
  --------------------------------------------------------------------------------
  -- typedef
  --------------------------------------------------------------------------------
  -- byte-align structures
  --/< Offset 17: reserved
   type tSocFrame is record
      reserved1 : aliased unsigned_char;  -- ./oplk/frame.h:131
      flag1 : aliased unsigned_char;  -- ./oplk/frame.h:132
      flag2 : aliased unsigned_char;  -- ./oplk/frame.h:133
      netTimeLe : aliased oplk.oplkinc_h.tNetTime;  -- ./oplk/frame.h:134
      relativeTimeLe : aliased Extensions.unsigned_long_long;  -- ./oplk/frame.h:135
   end record;
   pragma Convention (C_Pass_By_Copy, tSocFrame);  -- ./oplk/frame.h:136

   --  skipped anonymous struct anon_29

  --/< Offset 18: Flags: MC, PS
  --/< Offset 19: Flags: res
  --/< Offset 20: supported if D_NMT_NetTimeIsRealTime_BOOL is set
  --/< Offset 28: in us (supported if D_NMT_RelativeTime_BOOL is set)
  --/< Offset 17: reserved
   type tPreqFrame_aPayload_array is array (0 .. 255) of aliased unsigned_char;
   type tPreqFrame is record
      reserved1 : aliased unsigned_char;  -- ./oplk/frame.h:140
      flag1 : aliased unsigned_char;  -- ./oplk/frame.h:141
      flag2 : aliased unsigned_char;  -- ./oplk/frame.h:142
      pdoVersion : aliased unsigned_char;  -- ./oplk/frame.h:143
      reserved2 : aliased unsigned_char;  -- ./oplk/frame.h:144
      sizeLe : aliased unsigned_short;  -- ./oplk/frame.h:145
      aPayload : aliased tPreqFrame_aPayload_array;  -- ./oplk/frame.h:146
   end record;
   pragma Convention (C_Pass_By_Copy, tPreqFrame);  -- ./oplk/frame.h:147

   --  skipped anonymous struct anon_30

  --/< Offset 18: Flags: MS, EA, RD
  --/< Offset 19: Flags: res
  --/< Offset 20: PDO Version
  --/< Offset 21: reserved
  --/< Offset 22:
  --/< Offset 24: Payload
  --/< Offset 17: NMT state
   type tPresFrame_aPayload_array is array (0 .. 255) of aliased unsigned_char;
   type tPresFrame is record
      nmtStatus : aliased unsigned_char;  -- ./oplk/frame.h:151
      flag1 : aliased unsigned_char;  -- ./oplk/frame.h:152
      flag2 : aliased unsigned_char;  -- ./oplk/frame.h:153
      pdoVersion : aliased unsigned_char;  -- ./oplk/frame.h:154
      reserved2 : aliased unsigned_char;  -- ./oplk/frame.h:155
      sizeLe : aliased unsigned_short;  -- ./oplk/frame.h:156
      aPayload : aliased tPresFrame_aPayload_array;  -- ./oplk/frame.h:157
   end record;
   pragma Convention (C_Pass_By_Copy, tPresFrame);  -- ./oplk/frame.h:158

   --  skipped anonymous struct anon_31

  --/< Offset 18: Flags: MS, EN, RD
  --/< Offset 19: Flags: PR, RS
  --/< Offset 20:
  --/< Offset 21: reserved
  --/< Offset 22:
  --/< Offset 24: Payload
  --/< Offset 23:
   type tSyncRequest_aDestMacAddress_array is array (0 .. 5) of aliased unsigned_char;
   type tSyncRequest is record
      reserved : aliased unsigned_char;  -- ./oplk/frame.h:162
      syncControlLe : aliased unsigned;  -- ./oplk/frame.h:163
      presTimeFirstLe : aliased unsigned;  -- ./oplk/frame.h:164
      presTimeSecondLe : aliased unsigned;  -- ./oplk/frame.h:165
      syncMnDelayFirstLe : aliased unsigned;  -- ./oplk/frame.h:166
      syncMnDelaySecondLe : aliased unsigned;  -- ./oplk/frame.h:167
      presFallBackTimeoutLe : aliased unsigned;  -- ./oplk/frame.h:168
      aDestMacAddress : aliased tSyncRequest_aDestMacAddress_array;  -- ./oplk/frame.h:169
   end record;
   pragma Convention (C_Pass_By_Copy, tSyncRequest);  -- ./oplk/frame.h:170

   --  skipped anonymous struct anon_32

  --/< Offset 23
   type tSoaPayload (discr : unsigned := 0) is record
      case discr is
         when others =>
            syncRequest : aliased tSyncRequest;  -- ./oplk/frame.h:174
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tSoaPayload);
   pragma Unchecked_Union (tSoaPayload);  -- ./oplk/frame.h:175

   --  skipped anonymous struct anon_33

  --/< Offset 17:NMT state
   type tSoaFrame is record
      nmtStatus : aliased unsigned_char;  -- ./oplk/frame.h:179
      flag1 : aliased unsigned_char;  -- ./oplk/frame.h:180
      flag2 : aliased unsigned_char;  -- ./oplk/frame.h:181
      reqServiceId : aliased unsigned_char;  -- ./oplk/frame.h:182
      reqServiceTarget : aliased unsigned_char;  -- ./oplk/frame.h:183
      powerlinkVersion : aliased unsigned_char;  -- ./oplk/frame.h:184
      payload : tSoaPayload;  -- ./oplk/frame.h:185
   end record;
   pragma Convention (C_Pass_By_Copy, tSoaFrame);  -- ./oplk/frame.h:186

   --  skipped anonymous struct anon_34

  --/< Offset 18: Flags: EA, ER
  --/< Offset 19: Flags: res
  --/< Offset 20:
  --/< Offset 21:
  --/< Offset 22:
  --/< Offset 23:
  --*
  --\brief Structure for error history entries
  --The following structure defines an error history entry.
  -- 

  --/< The type of the entry
   type tErrHistoryEntry_aAddInfo_array is array (0 .. 7) of aliased unsigned_char;
   type tErrHistoryEntry is record
      entryType : aliased unsigned_short;  -- ./oplk/frame.h:195
      errorCode : aliased unsigned_short;  -- ./oplk/frame.h:196
      timeStamp : aliased oplk.oplkinc_h.tNetTime;  -- ./oplk/frame.h:197
      aAddInfo : aliased tErrHistoryEntry_aAddInfo_array;  -- ./oplk/frame.h:198
   end record;
   pragma Convention (C_Pass_By_Copy, tErrHistoryEntry);  -- ./oplk/frame.h:199

   --  skipped anonymous struct anon_35

  --/< The error code of the entry
  --/< The timestamp when the error was added
  --/< Additional error information
  --/< Offset 18: Flags: EN, EC
   type tStatusResponse_reserved1_array is array (0 .. 2) of aliased unsigned_char;
   type tStatusResponse_aErrorHistoryEntry_array is array (0 .. 13) of aliased tErrHistoryEntry;
   type tStatusResponse is record
      flag1 : aliased unsigned_char;  -- ./oplk/frame.h:203
      flag2 : aliased unsigned_char;  -- ./oplk/frame.h:204
      nmtStatus : aliased unsigned_char;  -- ./oplk/frame.h:205
      reserved1 : aliased tStatusResponse_reserved1_array;  -- ./oplk/frame.h:206
      staticErrorLe : aliased Extensions.unsigned_long_long;  -- ./oplk/frame.h:207
      aErrorHistoryEntry : aliased tStatusResponse_aErrorHistoryEntry_array;  -- ./oplk/frame.h:208
   end record;
   pragma Convention (C_Pass_By_Copy, tStatusResponse);  -- ./oplk/frame.h:209

   --  skipped anonymous struct anon_36

  --/< Offset 19: Flags: PR, RS
  --/< Offset 20: NMT state
  --/< static error bit field
  --/< Offset 18: Flags: res
   type tIdentResponse_sHostName_array is array (0 .. 31) of aliased unsigned_char;
   type tIdentResponse_aVendorSpecificExt2_array is array (0 .. 47) of aliased unsigned_char;
   type tIdentResponse is record
      flag1 : aliased unsigned_char;  -- ./oplk/frame.h:213
      flag2 : aliased unsigned_char;  -- ./oplk/frame.h:214
      nmtStatus : aliased unsigned_char;  -- ./oplk/frame.h:215
      identResponseFlags : aliased unsigned_char;  -- ./oplk/frame.h:216
      powerlinkProfileVersion : aliased unsigned_char;  -- ./oplk/frame.h:217
      reserved1 : aliased unsigned_char;  -- ./oplk/frame.h:218
      featureFlagsLe : aliased unsigned;  -- ./oplk/frame.h:219
      mtuLe : aliased unsigned_short;  -- ./oplk/frame.h:220
      pollInSizeLe : aliased unsigned_short;  -- ./oplk/frame.h:221
      pollOutSizeLe : aliased unsigned_short;  -- ./oplk/frame.h:222
      responseTimeLe : aliased unsigned;  -- ./oplk/frame.h:223
      reserved2 : aliased unsigned_short;  -- ./oplk/frame.h:224
      deviceTypeLe : aliased unsigned;  -- ./oplk/frame.h:225
      vendorIdLe : aliased unsigned;  -- ./oplk/frame.h:226
      productCodeLe : aliased unsigned;  -- ./oplk/frame.h:227
      revisionNumberLe : aliased unsigned;  -- ./oplk/frame.h:228
      serialNumberLe : aliased unsigned;  -- ./oplk/frame.h:229
      vendorSpecificExt1Le : aliased Extensions.unsigned_long_long;  -- ./oplk/frame.h:230
      verifyConfigurationDateLe : aliased unsigned;  -- ./oplk/frame.h:231
      verifyConfigurationTimeLe : aliased unsigned;  -- ./oplk/frame.h:232
      applicationSwDateLe : aliased unsigned;  -- ./oplk/frame.h:233
      applicationSwTimeLe : aliased unsigned;  -- ./oplk/frame.h:234
      ipAddressLe : aliased unsigned;  -- ./oplk/frame.h:235
      subnetMaskLe : aliased unsigned;  -- ./oplk/frame.h:236
      defaultGatewayLe : aliased unsigned;  -- ./oplk/frame.h:237
      sHostName : aliased tIdentResponse_sHostName_array;  -- ./oplk/frame.h:238
      aVendorSpecificExt2 : aliased tIdentResponse_aVendorSpecificExt2_array;  -- ./oplk/frame.h:239
   end record;
   pragma Convention (C_Pass_By_Copy, tIdentResponse);  -- ./oplk/frame.h:240

   --  skipped anonymous struct anon_37

  --/< Flags: PR, RS
  --/< NMT state
  --/< Flags: FW
  --/< NMT_FeatureFlags_U32
  --/< NMT_CycleTiming_REC.AsyncMTU_U16: C_IP_MIN_MTU - C_IP_MAX_MTU
  --/< NMT_CycleTiming_REC.PReqActPayload_U16
  --/< NMT_CycleTiming_REC.PResActPayload_U16
  --/< NMT_CycleTiming_REC.PResMaxLatency_U32
  --/< NMT_DeviceType_U32
  --/< NMT_IdentityObject_REC.VendorId_U32
  --/< NMT_IdentityObject_REC.ProductCode_U32
  --/< NMT_IdentityObject_REC.RevisionNo_U32
  --/< NMT_IdentityObject_REC.SerialNo_U32
  --/< CFM_VerifyConfiguration_REC.ConfDate_U32
  --/< CFM_VerifyConfiguration_REC.ConfTime_U32
  --/< PDL_LocVerApplSw_REC.ApplSwDate_U32 on programmable device or date portion of NMT_ManufactSwVers_VS on non-programmable device
  --/< PDL_LocVerApplSw_REC.ApplSwTime_U32 on programmable device or time portion of NMT_ManufactSwVers_VS on non-programmable device
  --/< Offset 18:
   type tNmtCommandService_aNmtCommandData_array is array (0 .. 31) of aliased unsigned_char;
   type tNmtCommandService is record
      nmtCommandId : aliased unsigned_char;  -- ./oplk/frame.h:244
      reserved1 : aliased unsigned_char;  -- ./oplk/frame.h:245
      aNmtCommandData : aliased tNmtCommandService_aNmtCommandData_array;  -- ./oplk/frame.h:246
   end record;
   pragma Convention (C_Pass_By_Copy, tNmtCommandService);  -- ./oplk/frame.h:247

   --  skipped anonymous struct anon_38

  --/< Offset 18:
   type tSyncResponse is record
      reserved : aliased unsigned_short;  -- ./oplk/frame.h:251
      syncStatusLe : aliased unsigned;  -- ./oplk/frame.h:252
      latencyLe : aliased unsigned;  -- ./oplk/frame.h:253
      syncNodeNumberLe : aliased unsigned;  -- ./oplk/frame.h:254
      syncDelayLe : aliased unsigned;  -- ./oplk/frame.h:255
      presTimeFirstLe : aliased unsigned;  -- ./oplk/frame.h:256
      presTimeSecondLe : aliased unsigned;  -- ./oplk/frame.h:257
   end record;
   pragma Convention (C_Pass_By_Copy, tSyncResponse);  -- ./oplk/frame.h:258

   --  skipped anonymous struct anon_39

   type tAsySdoCom_aCommandData_array is array (0 .. 7) of aliased unsigned_char;
   type tAsySdoCom is record
      reserved1 : aliased unsigned_char;  -- ./oplk/frame.h:262
      transactionId : aliased unsigned_char;  -- ./oplk/frame.h:263
      flags : aliased unsigned_char;  -- ./oplk/frame.h:264
      commandId : aliased unsigned_char;  -- ./oplk/frame.h:265
      segmentSizeLe : aliased unsigned_short;  -- ./oplk/frame.h:266
      reserved2 : aliased unsigned_short;  -- ./oplk/frame.h:267
      aCommandData : aliased tAsySdoCom_aCommandData_array;  -- ./oplk/frame.h:268
   end record;
   pragma Convention (C_Pass_By_Copy, tAsySdoCom);  -- ./oplk/frame.h:269

   --  skipped anonymous struct anon_40

  -- just reserve a minimum number of bytes as a placeholder
  -- asynchronous SDO Sequence Header
   type tAsySdoSeq_aReserved_array is array (0 .. 1) of aliased unsigned_char;
   type tAsySdoSeq is record
      recvSeqNumCon : aliased unsigned_char;  -- ./oplk/frame.h:275
      sendSeqNumCon : aliased unsigned_char;  -- ./oplk/frame.h:276
      aReserved : aliased tAsySdoSeq_aReserved_array;  -- ./oplk/frame.h:277
      sdoSeqPayload : aliased tAsySdoCom;  -- ./oplk/frame.h:278
   end record;
   pragma Convention (C_Pass_By_Copy, tAsySdoSeq);  -- ./oplk/frame.h:279

   --  skipped anonymous struct anon_41

  -- Offset 18
   type tNmtRequestService_aNmtCommandData_array is array (0 .. 31) of aliased unsigned_char;
   type tNmtRequestService is record
      nmtCommandId : aliased unsigned_char;  -- ./oplk/frame.h:284
      targetNodeId : aliased unsigned_char;  -- ./oplk/frame.h:285
      aNmtCommandData : aliased tNmtRequestService_aNmtCommandData_array;  -- ./oplk/frame.h:286
   end record;
   pragma Convention (C_Pass_By_Copy, tNmtRequestService);  -- ./oplk/frame.h:287

   --  skipped anonymous struct anon_42

  --/< Offset 18:
   type tAsndPayload_aPayload_array is array (0 .. 255) of aliased unsigned_char;
   type tAsndPayload (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            statusResponse : aliased tStatusResponse;  -- ./oplk/frame.h:292
         when 1 =>
            identResponse : aliased tIdentResponse;  -- ./oplk/frame.h:293
         when 2 =>
            nmtCommandService : aliased tNmtCommandService;  -- ./oplk/frame.h:294
         when 3 =>
            nmtRequestService : aliased tNmtRequestService;  -- ./oplk/frame.h:295
         when 4 =>
            sdoSequenceFrame : aliased tAsySdoSeq;  -- ./oplk/frame.h:296
         when 5 =>
            syncResponse : aliased tSyncResponse;  -- ./oplk/frame.h:297
         when others =>
            aPayload : aliased tAsndPayload_aPayload_array;  -- ./oplk/frame.h:298
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tAsndPayload);
   pragma Unchecked_Union (tAsndPayload);  -- ./oplk/frame.h:299

   --  skipped anonymous struct anon_43

  --/< Offset 17
   type tAsndFrame is record
      serviceId : aliased unsigned_char;  -- ./oplk/frame.h:303
      payload : tAsndPayload;  -- ./oplk/frame.h:304
   end record;
   pragma Convention (C_Pass_By_Copy, tAsndFrame);  -- ./oplk/frame.h:305

   --  skipped anonymous struct anon_44

  --/< Offset 18
  --/< Offset 17
   type tFrameData (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            soc : aliased tSocFrame;  -- ./oplk/frame.h:309
         when 1 =>
            preq : aliased tPreqFrame;  -- ./oplk/frame.h:310
         when 2 =>
            pres : aliased tPresFrame;  -- ./oplk/frame.h:311
         when 3 =>
            soa : aliased tSoaFrame;  -- ./oplk/frame.h:312
         when others =>
            asnd : aliased tAsndFrame;  -- ./oplk/frame.h:313
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tFrameData);
   pragma Unchecked_Union (tFrameData);  -- ./oplk/frame.h:314

   --  skipped anonymous struct anon_45

  --/< Offset 0: MAC address of the addressed nodes
   type tPlkFrame_aDstMac_array is array (0 .. 5) of aliased unsigned_char;
   type tPlkFrame_aSrcMac_array is array (0 .. 5) of aliased unsigned_char;
   type tPlkFrame is record
      aDstMac : aliased tPlkFrame_aDstMac_array;  -- ./oplk/frame.h:318
      aSrcMac : aliased tPlkFrame_aSrcMac_array;  -- ./oplk/frame.h:319
      etherType : aliased unsigned_short;  -- ./oplk/frame.h:320
      messageType : aliased unsigned_char;  -- ./oplk/frame.h:321
      dstNodeId : aliased unsigned_char;  -- ./oplk/frame.h:322
      srcNodeId : aliased unsigned_char;  -- ./oplk/frame.h:323
      data : tFrameData;  -- ./oplk/frame.h:324
   end record;
   pragma Convention (C_Pass_By_Copy, tPlkFrame);  -- ./oplk/frame.h:325

   --  skipped anonymous struct anon_46

  --/< Offset 6: MAC address of the transmitting node
  --/< Offset 12: Ethernet message type (big endian)
  --/< Offset 14: POWERLINK message type
  --/< Offset 15: POWERLINK node ID of the addressed nodes
  --/< Offset 16: POWERLINK node ID of the transmitting node
  --/< Offset 17:
  -- reset byte-align of structures
   subtype tMsgType is unsigned;
   kMsgTypeNonPowerlink : constant tMsgType := 0;
   kMsgTypeSoc : constant tMsgType := 1;
   kMsgTypePreq : constant tMsgType := 3;
   kMsgTypePres : constant tMsgType := 4;
   kMsgTypeSoa : constant tMsgType := 5;
   kMsgTypeAsnd : constant tMsgType := 6;
   kMsgTypeAInv : constant tMsgType := 13;  -- ./oplk/frame.h:342

end oplk.frame_h;

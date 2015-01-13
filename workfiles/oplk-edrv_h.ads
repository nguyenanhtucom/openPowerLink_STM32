pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with oplk.errordefs_h;
with oplk.timer_h;
with System;
with Interfaces.C.Extensions;
with oplk.oplkinc_h;
with Interfaces.C.Strings;

package oplk.edrv_h is


   EDRV_MAX_MTU : constant := 1500;  --  oplk/edrv.h:51
   EDRV_MIN_MTU : constant := 46;  --  oplk/edrv.h:52
   EDRV_ETH_HDR_OFFSET : constant := 0;  --  oplk/edrv.h:53
   EDRV_ETH_HDR_SIZE : constant := 14;  --  oplk/edrv.h:54
   EDRV_ETH_CRC_SIZE : constant := 4;  --  oplk/edrv.h:55
   --  unsupported macro: EDRV_MAX_ETH_SIZE (EDRV_MAX_MTU + EDRV_ETH_HDR_SIZE)
   --  unsupported macro: EDRV_MIN_ETH_SIZE (EDRV_MIN_MTU + EDRV_ETH_HDR_SIZE)

   EDRV_FILTER_CHANGE_VALUE : constant := 16#01#;  --  oplk/edrv.h:60
   EDRV_FILTER_CHANGE_MASK : constant := 16#02#;  --  oplk/edrv.h:61
   EDRV_FILTER_CHANGE_STATE : constant := 16#04#;  --  oplk/edrv.h:62
   EDRV_FILTER_CHANGE_AUTO_RESPONSE : constant := 16#08#;  --  oplk/edrv.h:63

   EDRV_FILTER_CHANGE_AUTO_RESPONSE_DELAY_DEF : constant := 0;  --  oplk/edrv.h:68
   --  unsupported macro: EDRV_FILTER_CHANGE_ALL (0 | EDRV_FILTER_CHANGE_VALUE | EDRV_FILTER_CHANGE_MASK | EDRV_FILTER_CHANGE_STATE | EDRV_FILTER_CHANGE_AUTO_RESPONSE | EDRV_FILTER_CHANGE_AUTO_RESPONSE_DELAY_DEF )
   --  unsupported macro: CONFIG_EDRV_USE_DIAGNOSTICS FALSE
   --  unsupported macro: CONFIG_EDRV_CYCLIC_USE_DIAGNOSTICS FALSE

   EDRV_CYCLIC_SAMPLE_NUM : constant := 501;  --  oplk/edrv.h:88
   --  unsupported macro: EDRV_USE_TTTX FALSE

  --*
  --********************************************************************************
  --\file   edrv.h
  --\brief  Definitions for Ethernet driver module
  --This file contains definitions for the Ethernet driver module.
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
  -- Type definitions
  --------------------------------------------------------------------------------
   type sEdrvTxBuffer;
   subtype tEdrvTxBuffer is sEdrvTxBuffer;

   type sEdrvRxBuffer;
   subtype tEdrvRxBuffer is sEdrvRxBuffer;

  --*
  --\brief Enumeration for Rx buffer release
  --This enumeration lists the Rx buffer release commands.
  -- 

  --/< Release the Rx buffer immediately
  --/< The Rx buffer is released later
   type tEdrvReleaseRxBuffer is 
     (kEdrvReleaseRxBufferImmediately,
      kEdrvReleaseRxBufferLater);
   pragma Convention (C, tEdrvReleaseRxBuffer);  -- oplk/edrv.h:110

  --/ Callback function pointer for Rx frames
   type tEdrvRxHandler is access function (arg1 : access tEdrvRxBuffer) return tEdrvReleaseRxBuffer;
   pragma Convention (C, tEdrvRxHandler);  -- oplk/edrv.h:113

  --/ Callback function pointer for Tx frames
   type tEdrvTxHandler is access procedure (arg1 : access tEdrvTxBuffer);
   pragma Convention (C, tEdrvTxHandler);  -- oplk/edrv.h:116

  --/ Callback function pointer for Edrv cyclic sync
   type tEdrvCyclicCbSync is access function return oplk.errordefs_h.tOplkError;
   pragma Convention (C, tEdrvCyclicCbSync);  -- oplk/edrv.h:119

  --/ Callback function pointer for Edrv cyclic error
   type tEdrvCyclicCbError is access function (arg1 : oplk.errordefs_h.tOplkError; arg2 : access tEdrvTxBuffer) return oplk.errordefs_h.tOplkError;
   pragma Convention (C, tEdrvCyclicCbError);  -- oplk/edrv.h:122

  --/ Callback function pointer for hres timer callback function
   type tHresCallback is access procedure (arg1 : access oplk.timer_h.tTimerHdl);
   pragma Convention (C, tHresCallback);  -- oplk/edrv.h:125

  --*
  --\brief Enumeration for Rx buffer transfer state
  --This enumeration lists the transfer state of the Rx buffer.
  --Note that the Ethernet controller must support early Rx interrupts to access
  --the first or middle data of a frame!
  -- 

  --/< First data of frame received
  --/< Middle data of frame received
  --/< Last data of frame received
   subtype tEdrvBufferInFrame is unsigned;
   kEdrvBufferFirstInFrame : constant tEdrvBufferInFrame := 1;
   kEdrvBufferMiddleInFrame : constant tEdrvBufferInFrame := 2;
   kEdrvBufferLastInFrame : constant tEdrvBufferInFrame := 4;  -- oplk/edrv.h:139

  --*
  --\brief Union for Tx buffer number
  --This union is used to identify the Tx buffer in the Ethernet driver module.
  -- 

  --/< Number of the TX buffer
   type tEdrvTxBufferNumber (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            value : aliased unsigned;  -- oplk/edrv.h:148
         when others =>
            pArg : System.Address;  -- oplk/edrv.h:149
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tEdrvTxBufferNumber);
   pragma Unchecked_Union (tEdrvTxBufferNumber);  -- oplk/edrv.h:150

   --  skipped anonymous struct anon_53

  --/< Pointer to the TX buffer
  --*
  --\brief Structure for Tx buffer
  --This structure is the Tx buffer descriptor.
  -- 

  --/< Size of Tx frame (without CRC)
   type sEdrvTxBuffer is record
      txFrameSize : aliased unsigned;  -- oplk/edrv.h:159
      timeOffsetNs : aliased unsigned;  -- oplk/edrv.h:160
      timeOffsetAbs : aliased unsigned;  -- oplk/edrv.h:161
      launchTime : aliased Extensions.unsigned_long_long;  -- oplk/edrv.h:162
      pfnTxHandler : tEdrvTxHandler;  -- oplk/edrv.h:163
      txBufferNumber : tEdrvTxBufferNumber;  -- oplk/edrv.h:164
      pBuffer : access unsigned_char;  -- oplk/edrv.h:165
      maxBufferSize : aliased unsigned;  -- oplk/edrv.h:166
   end record;
   pragma Convention (C_Pass_By_Copy, sEdrvTxBuffer);  -- oplk/edrv.h:157

  --/< Tx delay to a previously sent frame [ns]
  --/< Absolute Tx time [ticks]
  --/< Launch time of frame [ns]
  --/< Tx callback function
  --/< Edrv Tx buffer number
  --/< Pointer to the Tx buffer
  --/< Maximum size of the Tx buffer
  --*
  --\brief Structure for Rx buffer
  --This structure is the Rx buffer descriptor.
  -- 

  --/< Position of Rx buffer in a frame
   type sEdrvRxBuffer is record
      bufferInFrame : aliased tEdrvBufferInFrame;  -- oplk/edrv.h:176
      rxFrameSize : aliased unsigned;  -- oplk/edrv.h:177
      pBuffer : access unsigned_char;  -- oplk/edrv.h:178
      pRxTimeStamp : access oplk.oplkinc_h.tTimestamp;  -- oplk/edrv.h:179
   end record;
   pragma Convention (C_Pass_By_Copy, sEdrvRxBuffer);  -- oplk/edrv.h:174

  --/< Size of Rx frame (without CRC)
  --/< Pointer to the Rx buffer
  --/< Pointer to Rx time stamp
  --*
  --\brief Structure for initialization
  --This structure is used to initialize the Ethernet driver module.
  -- 

  --/< Ethernet controller MAC address
   type tEdrvInitParam_aMacAddr_array is array (0 .. 5) of aliased unsigned_char;
   type tEdrvInitParam is record
      aMacAddr : aliased tEdrvInitParam_aMacAddr_array;  -- oplk/edrv.h:189
      pfnRxHandler : tEdrvRxHandler;  -- oplk/edrv.h:190
      hwParam : aliased oplk.oplkinc_h.tHwParam;  -- oplk/edrv.h:191
   end record;
   pragma Convention (C_Pass_By_Copy, tEdrvInitParam);  -- oplk/edrv.h:192

   --  skipped anonymous struct anon_54

  --/< Rx frame callback function pointer
  --/< Hardware parameter
  --*
  --\brief Structure for Rx filter
  --This structure is used to control the Rx filters.
  -- 

  --/< Handle to Rx filter
   type tEdrvFilter_aFilterValue_array is array (0 .. 21) of aliased unsigned_char;
   type tEdrvFilter_aFilterMask_array is array (0 .. 21) of aliased unsigned_char;
   type tEdrvFilter is record
      handle : aliased unsigned;  -- oplk/edrv.h:201
      fEnable : aliased unsigned_char;  -- oplk/edrv.h:202
      aFilterValue : aliased tEdrvFilter_aFilterValue_array;  -- oplk/edrv.h:203
      aFilterMask : aliased tEdrvFilter_aFilterMask_array;  -- oplk/edrv.h:204
      pTxBuffer : access tEdrvTxBuffer;  -- oplk/edrv.h:205
   end record;
   pragma Convention (C_Pass_By_Copy, tEdrvFilter);  -- oplk/edrv.h:209

   --  skipped anonymous struct anon_55

  --/< Enable the Rx filter
  --/< Rx filter values
  --/< Rx filter mask
  --/< Tx frame to be transmitted when filter matches
  --/< Rx frame callback function pointer for this filter
  --*
  --\brief Structure for cyclic Ethernet driver diagnostics
  --This structure is used to provide diagnostics of the cyclic Ethernet driver.
  -- 

  -- continuous min/max/avg measurement
  --/< Cycle counter
   type tEdrvCyclicDiagnostics_aSampleTimeStamp_array is array (0 .. 500) of aliased Extensions.unsigned_long_long;
   type tEdrvCyclicDiagnostics_aCycleTime_array is array (0 .. 500) of aliased unsigned;
   type tEdrvCyclicDiagnostics_aUsedCycleTime_array is array (0 .. 500) of aliased unsigned;
   type tEdrvCyclicDiagnostics_aSpareCycleTime_array is array (0 .. 500) of aliased unsigned;
   type tEdrvCyclicDiagnostics is record
      cycleCount : aliased Extensions.unsigned_long_long;  -- oplk/edrv.h:219
      cycleTimeMin : aliased unsigned;  -- oplk/edrv.h:220
      cycleTimeMax : aliased unsigned;  -- oplk/edrv.h:221
      cycleTimeMeanSum : aliased Extensions.unsigned_long_long;  -- oplk/edrv.h:222
      usedCycleTimeMin : aliased unsigned;  -- oplk/edrv.h:223
      usedCycleTimeMax : aliased unsigned;  -- oplk/edrv.h:224
      usedCycleTimeMeanSum : aliased Extensions.unsigned_long_long;  -- oplk/edrv.h:225
      spareCycleTimeMin : aliased unsigned;  -- oplk/edrv.h:226
      spareCycleTimeMax : aliased unsigned;  -- oplk/edrv.h:227
      spareCycleTimeMeanSum : aliased Extensions.unsigned_long_long;  -- oplk/edrv.h:228
      sampleNum : aliased unsigned;  -- oplk/edrv.h:230
      sampleBufferedNum : aliased unsigned;  -- oplk/edrv.h:231
      aSampleTimeStamp : aliased tEdrvCyclicDiagnostics_aSampleTimeStamp_array;  -- oplk/edrv.h:232
      aCycleTime : aliased tEdrvCyclicDiagnostics_aCycleTime_array;  -- oplk/edrv.h:233
      aUsedCycleTime : aliased tEdrvCyclicDiagnostics_aUsedCycleTime_array;  -- oplk/edrv.h:234
      aSpareCycleTime : aliased tEdrvCyclicDiagnostics_aSpareCycleTime_array;  -- oplk/edrv.h:235
   end record;
   pragma Convention (C_Pass_By_Copy, tEdrvCyclicDiagnostics);  -- oplk/edrv.h:236

   --  skipped anonymous struct anon_56

  --/< Minimum measured cycle time
  --/< Maximum measured cycle time
  --/< Sum of the mean measured cycle times
  --/< Minimum utilized cycle time
  --/< Maximum utilized cycle time
  --/< Sum of the mean utilized cycle times
  --/< Minimum spare cycle time
  --/< Maximum spare cycle time
  --/< Sum of the mean spare cycle times
  -- sampling of runaway cycles
  --/< Sample number
  --/< Buffered sample number
  --/< Array of sampled timestamps (SoC send)
  --/< Array of cycle time values (until next SoC send)
  --/< Array of used cycle time values
  --/< Array of spare cycle time values
  --------------------------------------------------------------------------------
  -- function prototypes
  --------------------------------------------------------------------------------
   function edrv_init (pEdrvInitParam_p : access tEdrvInitParam) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:245
   pragma Import (C, edrv_init, "edrv_init");

   function edrv_shutdown return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:246
   pragma Import (C, edrv_shutdown, "edrv_shutdown");

   function edrv_setRxMulticastMacAddr (pMacAddr_p : access unsigned_char) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:247
   pragma Import (C, edrv_setRxMulticastMacAddr, "edrv_setRxMulticastMacAddr");

   function edrv_clearRxMulticastMacAddr (pMacAddr_p : access unsigned_char) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:248
   pragma Import (C, edrv_clearRxMulticastMacAddr, "edrv_clearRxMulticastMacAddr");

   function edrv_allocTxBuffer (pBuffer_p : access tEdrvTxBuffer) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:249
   pragma Import (C, edrv_allocTxBuffer, "edrv_allocTxBuffer");

   function edrv_freeTxBuffer (pBuffer_p : access tEdrvTxBuffer) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:250
   pragma Import (C, edrv_freeTxBuffer, "edrv_freeTxBuffer");

   function edrv_updateTxBuffer (pBuffer_p : access tEdrvTxBuffer) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:251
   pragma Import (C, edrv_updateTxBuffer, "edrv_updateTxBuffer");

   function edrv_sendTxBuffer (pBuffer_p : access tEdrvTxBuffer) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:252
   pragma Import (C, edrv_sendTxBuffer, "edrv_sendTxBuffer");

   function edrv_setTxBufferReady (pBuffer_p : access tEdrvTxBuffer) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:253
   pragma Import (C, edrv_setTxBufferReady, "edrv_setTxBufferReady");

   function edrv_startTxBuffer (pBuffer_p : access tEdrvTxBuffer) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:254
   pragma Import (C, edrv_startTxBuffer, "edrv_startTxBuffer");

   function edrv_releaseRxBuffer (pBuffer_p : access tEdrvRxBuffer) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:255
   pragma Import (C, edrv_releaseRxBuffer, "edrv_releaseRxBuffer");

   function edrv_changeRxFilter
     (pFilter_p : access tEdrvFilter;
      count_p : unsigned;
      entryChanged_p : unsigned;
      changeFlags_p : unsigned) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:256
   pragma Import (C, edrv_changeRxFilter, "edrv_changeRxFilter");

   function edrv_getDiagnostics (pBuffer_p : Interfaces.C.Strings.chars_ptr; size_p : int) return int;  -- oplk/edrv.h:257
   pragma Import (C, edrv_getDiagnostics, "edrv_getDiagnostics");

   function edrvcyclic_init return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:259
   pragma Import (C, edrvcyclic_init, "edrvcyclic_init");

   function edrvcyclic_shutdown return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:260
   pragma Import (C, edrvcyclic_shutdown, "edrvcyclic_shutdown");

   function edrvcyclic_setCycleTime (cycleTimeUs_p : unsigned) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:261
   pragma Import (C, edrvcyclic_setCycleTime, "edrvcyclic_setCycleTime");

   function edrvcyclic_startCycle return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:262
   pragma Import (C, edrvcyclic_startCycle, "edrvcyclic_startCycle");

   function edrvcyclic_stopCycle return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:263
   pragma Import (C, edrvcyclic_stopCycle, "edrvcyclic_stopCycle");

   function edrvcyclic_setMaxTxBufferListSize (maxListSize_p : unsigned) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:264
   pragma Import (C, edrvcyclic_setMaxTxBufferListSize, "edrvcyclic_setMaxTxBufferListSize");

  -- jdk SECTION_EDRVCYC_SET_NEXT_TX;
   function edrvcyclic_setNextTxBufferList (ppTxBuffer_p : System.Address; txBufferCount_p : unsigned) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:265
   pragma Import (C, edrvcyclic_setNextTxBufferList, "edrvcyclic_setNextTxBufferList");

   function edrvcyclic_regSyncHandler (pfnEdrvCyclicCbSync_p : tEdrvCyclicCbSync) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:266
   pragma Import (C, edrvcyclic_regSyncHandler, "edrvcyclic_regSyncHandler");

   function edrvcyclic_regErrorHandler (pfnEdrvCyclicCbError_p : tEdrvCyclicCbError) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:267
   pragma Import (C, edrvcyclic_regErrorHandler, "edrvcyclic_regErrorHandler");

   function edrvcyclic_getDiagnostics (ppDiagnostics_p : System.Address) return oplk.errordefs_h.tOplkError;  -- oplk/edrv.h:268
   pragma Import (C, edrvcyclic_getDiagnostics, "edrvcyclic_getDiagnostics");

end oplk.edrv_h;

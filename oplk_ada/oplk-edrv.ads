
--\brief  Definitions for Ethernet driver module
--This file contains definitions for the Ethernet driver module.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with oplk.errordefs;
with oplk.timer;
with System;
with Interfaces.C.Extensions;
with oplk.oplkinc;
with Interfaces.C.Strings;

package oplk.Edrv is
   pragma Elaborate_Body;
   
   EDRV_MAX_MTU : constant := 1500;
   -- 
   EDRV_MIN_MTU : constant := 46;
   -- 
   EDRV_ETH_HDR_OFFSET : constant := 0;
   -- Ethernet header at the top of the frame
   EDRV_ETH_HDR_SIZE : constant := 14;
   -- size of Ethernet header
   EDRV_ETH_CRC_SIZE : constant := 4;
   -- size of Ethernet CRC, i.e. FCS
   EDRV_MAX_ETH_SIZE : constant := EDRV_MAX_MTU + EDRV_ETH_HDR_SIZE;
   -- without CRC
   EDRV_MIN_ETH_SIZE : constant := EDRV_MIN_MTU + EDRV_ETH_HDR_SIZE;
   -- without CRC
   EDRV_FILTER_CHANGE_VALUE                   : constant Unsigned_Char := 16#01#;
   -- filter value changed
   EDRV_FILTER_CHANGE_MASK                    : constant Unsigned_Char := 16#02#;
   -- filter mask changed
   EDRV_FILTER_CHANGE_STATE                   : constant Unsigned_Char := 16#04#;
   -- filter state changed
   EDRV_FILTER_CHANGE_AUTO_RESPONSE           : constant Unsigned_Char := 16#08#;
   -- filter auto-resp. state changed
   EDRV_FILTER_CHANGE_AUTO_RESPONSE_DELAY     : constant := 16#10#;
   -- filter auto-resp. delay changed
   EDRV_FILTER_CHANGE_AUTO_RESPONSE_DELAY_DEF : constant Unsigned_Char := 0;
   -- 
   EDRV_FILTER_CHANGE_ALL                     : constant := 0 or 
     EDRV_FILTER_CHANGE_VALUE or EDRV_FILTER_CHANGE_MASK or 
     EDRV_FILTER_CHANGE_STATE or EDRV_FILTER_CHANGE_AUTO_RESPONSE or 
     EDRV_FILTER_CHANGE_AUTO_RESPONSE_DELAY_DEF;
   --
   CONFIG_EDRV_USE_DIAGNOSTICS                : Boolean := False;
   --
   CONFIG_EDRV_CYCLIC_USE_DIAGNOSTICS         : Boolean := False;
   --
   
------------------------------------------------------------------------------
-- Type Definitions                                                         --
------------------------------------------------------------------------------
   
   type sEdrvTxBuffer;
   subtype tEdrvTxBuffer is sEdrvTxBuffer;

   type sEdrvRxBuffer;
   subtype tEdrvRxBuffer is sEdrvRxBuffer;
   
------------------------------------------------------------------------------
--\brief Enumeration for Rx buffer release                                  --
-- This enumeration lists the Rx buffer release commands.                   --
------------------------------------------------------------------------------
   type tEdrvReleaseRxBuffer is 
     (kEdrvReleaseRxBufferImmediately,
      -- Release the Rx buffer immediately
      KEdrvReleaseRxBufferLater
      -- The Rx buffer is released later
     );
   pragma Convention (C, tEdrvReleaseRxBuffer);  -- oplk/edrv.h:110
   
   
   type tEdrvRxHandler is access function
     -- Callback function pointer for Rx frames
     (arg1 : access tEdrvRxBuffer) 
     return tEdrvReleaseRxBuffer;
   pragma Convention (C, tEdrvRxHandler);  -- oplk/edrv.h:113
   
   
   type tEdrvTxHandler is access procedure 
     -- Callback function pointer for Tx frames
     (arg1 : access tEdrvTxBuffer);
   pragma Convention (C, tEdrvTxHandler);  -- oplk/edrv.h:116
   
   
   type tEdrvCyclicCbSync is access function
     -- Callback function pointer for Edrv cyclic sync
     return errordefs.tOplkError;
   pragma Convention (C, tEdrvCyclicCbSync);  -- oplk/edrv.h:119
   
   
   type tEdrvCyclicCbError is access function 
     -- Callback function pointer for Edrv cyclic error
     (arg1 : errordefs.tOplkError; 
      arg2 : access tEdrvTxBuffer) 
     return errordefs.tOplkError;
   pragma Convention (C, tEdrvCyclicCbError);  -- oplk/edrv.h:122
   
   
   type tHresCallback is access procedure 
     -- Callback function pointer for hres timer callback function
     (arg1 : access timer.tTimerHdl);
   pragma Convention (C, tHresCallback);  -- oplk/edrv.h:125
   
   
------------------------------------------------------------------------------
--\brief Enumeration for Rx buffer transfer state                           --
-- This enumeration lists the transfer state of the Rx buffer.              --
-- Note that the Ethernet controller must support early Rx interrupts to    --
-- access the first or middle data of a frame!                              --
------------------------------------------------------------------------------
   subtype tEdrvBufferInFrame is unsigned;
   kEdrvBufferFirstInFrame  : constant tEdrvBufferInFrame := 1;
   -- First data of frame received
   kEdrvBufferMiddleInFrame : constant tEdrvBufferInFrame := 2;
   -- Middle data of frame received
   kEdrvBufferLastInFrame   : constant tEdrvBufferInFrame := 4;
   -- Last data of frame received
   
   
------------------------------------------------------------------------------
--\brief Union for Tx buffer number                                         --
-- This union is used to identify the Tx buffer in the Ethernet driver      --
-- module.                                                                  --
------------------------------------------------------------------------------
   type tEdrvTxBufferNumber (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            value : aliased unsigned;
	    -- Number of the TX buffer
         when others =>
            pArg : System.Address;
	    -- Pointer to the TX buffer
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, tEdrvTxBufferNumber);
   pragma Unchecked_Union (tEdrvTxBufferNumber);  -- oplk/edrv.h:150
   
   
------------------------------------------------------------------------------
--\brief Structure for Tx buffer                                            --
-- This structure is the Tx buffer descriptor.                              --
------------------------------------------------------------------------------
   type sEdrvTxBuffer is record
      txFrameSize    : aliased unsigned;
      -- Size of Tx frame (without CRC)
      timeOffsetNs   : aliased unsigned;
      -- Tx delay to a previously sent frame [ns]
      timeOffsetAbs  : aliased unsigned;
      -- Absolute Tx time [ticks]
      launchTime     : aliased Extensions.unsigned_long_long;
      -- Launch time of frame [ns]
      pfnTxHandler   : tEdrvTxHandler;
      -- Tx callback function
      txBufferNumber : tEdrvTxBufferNumber;
      -- Edrv Tx buffer number
      pBuffer        : access unsigned_char;
      -- Pointer to the Tx buffer
      maxBufferSize  : aliased unsigned;
      -- Maximum size of the Tx buffer
   end record;
   pragma Convention (C_Pass_By_Copy, sEdrvTxBuffer);  -- oplk/edrv.h:157
   
   
------------------------------------------------------------------------------
--/brief Structure for Rx buffer                                            --
-- This structure is the Rx buffer descriptor.                              --
------------------------------------------------------------------------------
   type sEdrvRxBuffer is record
      bufferInFrame : aliased tEdrvBufferInFrame;
      -- Position of Rx buffer in a frame
      rxFrameSize   : aliased unsigned;
      -- Size of Rx frame (without CRC)
      pBuffer       : access unsigned_char;
      -- Pointer to the Rx buffer
      pRxTimeStamp  : access oplkinc.tTimestamp;
      -- Pointer to Rx time stamp
   end record;
   pragma Convention (C_Pass_By_Copy, sEdrvRxBuffer);  -- oplk/edrv.h:174
   
   
------------------------------------------------------------------------------
--\brief Structure for initialization                                       --
-- This structure is used to initialize the Ethernet driver module.         --
------------------------------------------------------------------------------
   type tEdrvInitParam_aMacAddr_array is array (0 .. 5) of aliased unsigned_char;
   type tEdrvInitParam is record
      aMacAddr     : aliased tEdrvInitParam_aMacAddr_array;
      -- Ethernet controller MAC address
      pfnRxHandler : tEdrvRxHandler;
      -- Rx frame callback function pointer
      hwParam      : aliased oplkinc.tHwParam;
      -- Hardware parameter
   end record;
   pragma Convention (C_Pass_By_Copy, tEdrvInitParam);  -- oplk/edrv.h:192
   
   
------------------------------------------------------------------------------
--\brief Structure for Rx filter                                            --
-- This structure is used to control the Rx filters.                        --
------------------------------------------------------------------------------
   type tEdrvFilter_aFilterValue_array is array (0 .. 21) of aliased unsigned_char;
   type tEdrvFilter_aFilterMask_array  is array (0 .. 21) of aliased unsigned_char;
   type tEdrvFilter is record
      handle       : aliased unsigned;
      -- Handle to Rx filter
      fEnable      : aliased unsigned_char;
      -- Enable the Rx filter
      aFilterValue : aliased tEdrvFilter_aFilterValue_array;
      -- Rx filter values
      aFilterMask  : aliased tEdrvFilter_aFilterMask_array;
      -- filter mask
      pTxBuffer    : access tEdrvTxBuffer;
      -- Tx frame to be transmitted when filter matches
      PfnRxHandler : TEdrvRxHandler := null;
      -- Rx frame callback function pointer for this filter
   end record;
   pragma Convention (C_Pass_By_Copy, tEdrvFilter);  -- oplk/edrv.h:209

   
------------------------------------------------------------------------------
--\brief Structure for cyclic Ethernet driver diagnostics                   --
-- This structure is used to provide diagnostics of the cyclic Ethernet     --
-- driver.                                                                  --
------------------------------------------------------------------------------
   type tEdrvCyclicDiagnostics_aSampleTimeStamp_array is 
     array (0 .. 500) of aliased Extensions.unsigned_long_long;
   type tEdrvCyclicDiagnostics_aCycleTime_array is 
     array (0 .. 500) of aliased unsigned;
   type tEdrvCyclicDiagnostics_aUsedCycleTime_array is 
     array (0 .. 500) of aliased unsigned;
   type tEdrvCyclicDiagnostics_aSpareCycleTime_array is 
     array (0 .. 500) of aliased unsigned;
   type tEdrvCyclicDiagnostics is record
      
      -- continuous min/max/avg measurement
      cycleCount            : aliased Extensions.unsigned_long_long;
      -- Cycle counter
      cycleTimeMin          : aliased unsigned;
      -- Minimum measured cycle time
      cycleTimeMax          : aliased unsigned;
      -- Maximum measured cycle time
      cycleTimeMeanSum      : aliased Extensions.unsigned_long_long;
      -- Sum of the mean measured cycle times
      usedCycleTimeMin      : aliased unsigned;
      -- Minimum utilized cycle time
      usedCycleTimeMax      : aliased unsigned;
      -- Maximum utilized cycle time
      usedCycleTimeMeanSum  : aliased Extensions.unsigned_long_long;
      -- Sum of the mean utilized cycle times
      spareCycleTimeMin     : aliased unsigned;
      -- Minimum spare cycle time
      spareCycleTimeMax     : aliased unsigned;
      -- Maximum spare cycle time
      spareCycleTimeMeanSum : aliased Extensions.unsigned_long_long;
      -- Sum of the mean spare cycle times
      
      -- sampling of runaway cycles
      sampleNum             : aliased unsigned;
      -- Sample number
      sampleBufferedNum     : aliased unsigned;
      -- Buffered sample number
      aSampleTimeStamp      : aliased tEdrvCyclicDiagnostics_aSampleTimeStamp_array;
      -- Array of sampled timestamps (SoC send)
      aCycleTime            : aliased tEdrvCyclicDiagnostics_aCycleTime_array;
      -- Array of cycle time values (until next SoC send)
      aUsedCycleTime        : aliased tEdrvCyclicDiagnostics_aUsedCycleTime_array;
      -- Array of used cycle time values
      aSpareCycleTime       : aliased tEdrvCyclicDiagnostics_aSpareCycleTime_array;
      -- Array of spare cycle time values
   end record;
   pragma Convention (C_Pass_By_Copy, tEdrvCyclicDiagnostics);  -- oplk/edrv.h:236
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
------------------------------------------------------------------------------
   
   function edrv_init 
     -- pEdrvInitParam_p    Edrv initialization parameters
     -- The function returns a tOplkError error code.
     -- 
     -- This function initializes the Ethernet driver.
     (pEdrvInitParam_p : access tEdrvInitParam) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_init, "edrv_init");  -- oplk/edrv.h:245
   
   
   function edrv_shutdown 
     -- The function returns a tOplkError error code.
     -- 
     -- This function shuts down the Ethernet driver.
     return errordefs.tOplkError;
   pragma Export (C, edrv_shutdown, "edrv_shutdown");  -- oplk/edrv.h:246
   
   
   function edrv_setRxMulticastMacAddr 
     -- pMacAddr_p  Multicast address
     -- The function returns a tOplkError error code.
     -- 
     -- This function sets a multicast entry into the Ethernet controller.
     (pMacAddr_p : access unsigned_char) 
     return errordefs.tOplkError;
   pragma Export 
     (C, edrv_setRxMulticastMacAddr, "edrv_setRxMulticastMacAddr");  -- oplk/edrv.h:247
   
   
   function edrv_clearRxMulticastMacAddr 
     -- pMacAddr_p  Multicast address
     -- The function returns a tOplkError error code.
     -- 
     -- This function removes the multicast entry from the Ethernet controller.
     (pMacAddr_p : access unsigned_char) 
     return errordefs.tOplkError;
   pragma Export 
     (C, edrv_clearRxMulticastMacAddr, "edrv_clearRxMulticastMacAddr");  -- oplk/edrv.h:248
   
   
   function edrv_allocTxBuffer 
     -- pBuffer_p           Tx buffer descriptor
     -- The function returns a tOplkError error code.
     -- 
     -- This function allocates a Tx buffer.
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_allocTxBuffer, "edrv_allocTxBuffer");  -- oplk/edrv.h:249
   
   
   function edrv_freeTxBuffer 
     -- pBuffer_p           Tx buffer descriptor
     -- The function returns a tOplkError error code.
     -- 
     -- This function releases the Tx buffer.
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_freeTxBuffer, "edrv_freeTxBuffer");  -- oplk/edrv.h:250
   
   
   function edrv_updateTxBuffer 
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_updateTxBuffer, "edrv_updateTxBuffer");  -- oplk/edrv.h:251
   
   
   function edrv_sendTxBuffer 
     -- pBuffer_p           Tx buffer descriptor
     -- The function returns a tOplkError error code.
     -- 
     -- This function sends the Tx buffer.
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_sendTxBuffer, "edrv_sendTxBuffer");  -- oplk/edrv.h:252
   
   
   function edrv_setTxBufferReady 
     -- pBuffer_p   Tx buffer buffer descriptor
     -- The function returns a tOplkError error code.
     -- 
     -- This function sets the Tx buffer buffer ready for transmission.
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_setTxBufferReady, "edrv_setTxBufferReady");  -- oplk/edrv.h:253
   
   
   function edrv_startTxBuffer 
     -- pBuffer_p   Tx buffer buffer descriptor
     -- The function returns a tOplkError error code.
     -- 
     -- This function sends the Tx buffer marked as ready.
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_startTxBuffer, "edrv_startTxBuffer");  -- oplk/edrv.h:254
   
   
   function edrv_releaseRxBuffer 
     -- pRxBuffer_p     Rx buffer to be released
     -- The function returns a tOplkError error code.
     -- 
     -- This function releases a late release Rx buffer.
     (pBuffer_p : access tEdrvRxBuffer) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_releaseRxBuffer, "edrv_releaseRxBuffer");  -- oplk/edrv.h:255
   
   
   function Edrv_ChangeRxFilter
     -- pFilter_p           Base pointer of Rx filter array
     -- count_p             Number of Rx filter array entries
     -- entryChanged_p      Index of Rx filter entry that shall be changed
     -- changeFlags_p       Bit mask that selects the changing Rx filter property
     -- The function returns a tOplkError error code.
     -- 
     -- This function changes the Rx filter setup. The parameter entryChanged_p
     -- selects the Rx filter entry that shall be changed and changeFlags_p 
     -- determines the property.
     -- If entryChanged_p is equal or larger count_p all Rx filters shall 
     -- be changed.
     -- 
     -- \note Rx filters are not supported by this driver! (maybe)!!!!!!!!!!!!!!!!
     (pFilter_p      : access tEdrvFilter;
      count_p        : unsigned;
      entryChanged_p : unsigned;
      changeFlags_p  : unsigned) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_changeRxFilter, "edrv_changeRxFilter");  -- oplk/edrv.h:256
   
   
   function edrv_getDiagnostics 
     -- pBuffer_p   Pointer to buffer filled with diagnostics.
     -- size_p      Size of buffer
     -- The function returns a tOplkError error code.
     -- 
     -- This function returns the Edrv diagnostics to a provided buffer.
     (pBuffer_p : Interfaces.C.Strings.chars_ptr; 
      size_p    : int) 
     return errordefs.tOplkError;
   pragma Export (C, edrv_getDiagnostics, "edrv_getDiagnostics");  -- oplk/edrv.h:257
   
   
  --   function edrvcyclic_init 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_init, "edrvcyclic_init");  -- oplk/edrv.h:259

  --   function edrvcyclic_shutdown 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_shutdown, "edrvcyclic_shutdown");  -- oplk/edrv.h:260

  --   function edrvcyclic_setCycleTime 
  --     (cycleTimeUs_p : unsigned) 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_setCycleTime, "edrvcyclic_setCycleTime");  -- oplk/edrv.h:261

  --   function edrvcyclic_startCycle 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_startCycle, "edrvcyclic_startCycle");  -- oplk/edrv.h:262

  --   function edrvcyclic_stopCycle 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_stopCycle, "edrvcyclic_stopCycle");  -- oplk/edrv.h:263

  --   function edrvcyclic_setMaxTxBufferListSize 
  --     (maxListSize_p : unsigned) 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_setMaxTxBufferListSize, "edrvcyclic_setMaxTxBufferListSize");  -- oplk/edrv.h:264

  --  -- jdk SECTION_EDRVCYC_SET_NEXT_TX;
  --   function edrvcyclic_setNextTxBufferList 
  --     (ppTxBuffer_p    : System.Address; 
  --      txBufferCount_p : unsigned) 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_setNextTxBufferList, "edrvcyclic_setNextTxBufferList");  -- oplk/edrv.h:265

  --   function edrvcyclic_regSyncHandler 
  --     (pfnEdrvCyclicCbSync_p : tEdrvCyclicCbSync) 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_regSyncHandler, "edrvcyclic_regSyncHandler");  -- oplk/edrv.h:266

  --   function edrvcyclic_regErrorHandler 
  --     (pfnEdrvCyclicCbError_p : tEdrvCyclicCbError) 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_regErrorHandler, "edrvcyclic_regErrorHandler");  -- oplk/edrv.h:267

  --   function edrvcyclic_getDiagnostics 
  --     (ppDiagnostics_p : System.Address) 
  --     return errordefs.tOplkError;
  --   pragma Export (C, edrvcyclic_getDiagnostics, "edrvcyclic_getDiagnostics");  -- oplk/edrv.h:268

   
   
   
   
end oplk.Edrv;



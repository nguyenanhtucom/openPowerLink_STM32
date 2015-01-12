-- \brief  Definitions for OBD module
--  This file contains definitions for the OBD module

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Oplk.oplkinc;
with Interfaces.C.Extensions;
with System;
with Interfaces.C.Strings;
with Oplk.errordefs;

package Oplk.obd is

   OBD_TABLE_INDEX_END               : constant := 16#FFFF#;
   -- 
   
   OBD_TRUE                          : constant := 16#01#;
   -- 
   OBD_FALSE                         : constant := 16#00#;
   -- 
   
   OBD_NODE_ID_INDEX                 : constant := 16#1F93#;
   -- default OD index for Node id
   OBD_NODE_ID_SUBINDEX              : constant := 16#01#;
   -- default subindex for NodeId in OD
   OBD_NODE_ID_HWBOOL_SUBINDEX       : constant := 16#02#;
   -- default subindex for NodeIDByHW_BOOL
   
   --   object IDs of error handling objects   --
   OID_DLL_MN_CRCERROR_REC           : constant := 16#1C00#;
   -- 
   OID_DLL_MN_CYCTIME_EXCEED_REC     : constant := 16#1C02#;
   -- 
   OID_DLL_CN_LOSSSOC_REC            : constant := 16#1C0B#;
   -- 
   OID_DLL_CN_LOSSPREQ_REC           : constant := 16#1C0D#;
   -- 
   OID_DLL_CN_CRCERROR_REC           : constant := 16#1C0F#;
   -- 
   
   SUBIDX_DLL_ERROR_CUM_CNT          : constant := 1;
   -- 
   SUBIDX_DLL_ERROR_THR_CNT          : constant := 2;
   -- 
   SUBIDX_DLL_ERROR_THRESHOLD        : constant := 3;
   -- 
   
   NUM_DLL_MNCN_LOSSPRES_OBJS        : constant := 254;
   -- 
   OID_DLL_MNCN_LOSSPRES_CUMCNT_AU32 : constant := 16#1C07#;
   -- 
   OID_DLL_MNCN_LOSSPRES_THRCNT_AU32 : constant := 16#1C08#;
   -- 
   OID_DLL_MNCN_LOSSPRES_THRESHOLD_AU32 : constant := 16#1C09#;
   -- 
   
   --   Definitions for parts of the OD (bit oriented)   --
   kObdPartNo                        : constant Unsigned_Char := 16#00#;
   -- Nothing
   kObdPartGen                       : constant Unsigned_Char := 16#01#;
   -- Communication part (0x1000 - 0x1FFF)
   kObdPartMan                       : constant Unsigned_Char := 16#02#;
   -- Manufacturer part (0x2000 - 0x5FFF)
   kObdPartDev                       : constant Unsigned_Char := 16#04#;
   -- Device part (0x6000 - 0x9FFF)
   kObdPartUsr                       : constant Unsigned_Char := 16#08#;
   -- Dynamic part e.g. for ICE61131-3
   kObdPartApp : constant := kObdPartMan or kObdPartDev or KObdPartUsr;
   -- Manufacturer, device part and user OD
   kObdPartAll : constant := kObdPartGen or kObdPartMan or kObdPartDev or KObdPartUsr;
   -- The whole OD
   
   
------------------------------------------------------------------------------
--           \anchor sect_obdAccessRights                                   --
-- \name   Access rights for objects                                        --
--                                                                          --
-- The following macros define the access rights for objects.               --
------------------------------------------------------------------------------
   kObdAccRead                       : constant Unsigned_Char := 16#01#;
   -- Object can be read
   kObdAccWrite                      : constant Unsigned_Char := 16#02#;
   -- Object can be written
   kObdAccConst                      : constant Unsigned_Char := 16#04#;
   -- Object contains a constant value
   kObdAccPdo                        : constant Unsigned_Char := 16#08#;
   -- Object can be mapped to a PDO (always in conjunction with kObdAccVar)
   kObdAccArray                      : constant Unsigned_Char := 16#10#;
   -- Object contains an array of numerical values
   kObdAccRange                      : constant Unsigned_Char := 16#20#;
   -- Object contains lower and upper limit
   kObdAccVar                        : constant Unsigned_Char := 16#40#;
   -- Object data is placed in application 
   -- (contains a variable information structure)
   kObdAccStore                      : constant Unsigned_Char := 16#80#;
   -- Object data can be stored to non-volatile memory
   
   --   combinations (not all combinations are required)    --
   KObdAccR     : constant := KObdAccRead;
   KObdAccW     : constant := KObdAccWrite;
   KObdAccRW    : constant := kObdAccWrite or KObdAccRead;
   kObdAccCR    : constant := kObdAccConst or KObdAccRead;
   KObdAccGR    : constant := kObdAccRange or KObdAccRead;
   kObdAccGW    : constant := kObdAccRange or KObdAccWrite;
   KObdAccGRW   : constant := kObdAccRange or kObdAccWrite or KObdAccRead;
   KObdAccVR    : constant := kObdAccVar   or KObdAccRead;
   kObdAccVW    : constant := kObdAccVar   or KObdAccWrite;
   KObdAccVRW   : constant := kObdAccVar   or kObdAccWrite or KObdAccRead;
   KObdAccVPR   : constant := kObdAccVar   or KObdAccPdo   or KObdAccRead;
   kObdAccVPW   : constant := KObdAccVar   or KObdAccPdo   or KObdAccWrite;
   KObdAccVPRW  : constant := kObdAccVar   or kObdAccPdo   or kObdAccWrite or KObdAccRead;
   kObdAccVGR   : constant := KObdAccVar   or KObdAccRange or KObdAccRead;
   KObdAccVGW   : constant := KObdAccVar   or kObdAccRange or KObdAccWrite;
   KObdAccVGRW  : constant := KObdAccVar   or kObdAccRange or kObdAccWrite or KObdAccRead;
   kObdAccVGPR  : constant := KObdAccVar   or kObdAccRange or KObdAccPdo   or KObdAccRead;
   kObdAccVGPW  : constant := KObdAccVar   or kObdAccRange or KObdAccPdo   or KObdAccWrite;
   kObdAccVGPRW : constant := KObdAccVar   or kObdAccRange or KObdAccPdo   or kObdAccWrite or KObdAccRead;
   kObdAccSR    : constant := KObdAccStore or KObdAccRead;
   kObdAccSW    : constant := kObdAccStore or KObdAccWrite;
   kObdAccSRW   : constant := kObdAccStore or kObdAccWrite or KObdAccRead;
   kObdAccSCR   : constant := kObdAccStore or kObdAccConst or KObdAccRead;
   kObdAccSGR   : constant := kObdAccStore or kObdAccRange or KObdAccRead;
   kObdAccSGW   : constant := kObdAccStore or kObdAccRange or KObdAccWrite;
   kObdAccSGRW  : constant := kObdAccStore or kObdAccRange or kObdAccWrite or KObdAccRead;
   kObdAccSVR   : constant := kObdAccStore or kObdAccVar   or KObdAccRead;
   kObdAccSVW   : constant := kObdAccStore or KObdAccVar   or KObdAccWrite;
   kObdAccSVRW  : constant := kObdAccStore or KObdAccVar   or kObdAccWrite or KObdAccRead;
   kObdAccSVPR  : constant := kObdAccStore or KObdAccVar   or KObdAccPdo   or KObdAccRead;
   kObdAccSVPW  : constant := kObdAccStore or KObdAccVar   or KObdAccPdo   or KObdAccWrite;
   kObdAccSVPRW : constant := kObdAccStore or KObdAccVar   or KObdAccPdo   or kObdAccWrite or KObdAccRead;
   kObdAccSVGR  : constant := kObdAccStore or KObdAccVar   or kObdAccRange or KObdAccRead;
   kObdAccSVGW  : constant := kObdAccStore or kObdAccVar   or kObdAccRange or KObdAccWrite;
   kObdAccSVGRW : constant := kObdAccStore or kObdAccVar   or kObdAccRange or kObdAccWrite or KObdAccRead;
   kObdAccSVGPR : constant := kObdAccStore or kObdAccVar   or kObdAccRange or kObdAccPdo   or KObdAccRead;
   kObdAccSVGPW : constant := kObdAccStore or kObdAccVar   or kObdAccRange or kObdAccPdo   or KObdAccWrite;
   kObdAccSVGPRW : constant := kObdAccStore or kObdAccVar  or kObdAccRange or kObdAccPdo   or kObdAccWrite or KObdAccRead;
   
------------------------------------------------------------------------------
--\brief Directions for access to object dictionary                         --
--                                                                          --
-- This enumeration defines valid "directions" for accesses to the object   --
-- dictionary.                                                              --
------------------------------------------------------------------------------
   subtype tObdDir is unsigned;
   kObdDirInit     : constant tObdDir := 0;
   -- Initialising after power on
   kObdDirStore    : constant tObdDir := 1;
   -- Store all object values to non volatile memory
   kObdDirLoad     : constant tObdDir := 2;
   -- Load all object values from non volatile memory
   kObdDirRestore  : constant tObdDir := 3;
   -- Deletes non volatile memory (restore)
   kObdDirOBKCheck : constant tObdDir := 255;
   -- Reserved
                                                            -- ./oplk/obd.h:93
   
   
------------------------------------------------------------------------------
--\brief Valid OD store commands                                            --
--                                                                          --
-- This enumeration defines valid store commands for the OD                 --
------------------------------------------------------------------------------
   subtype tObdCommand is unsigned;
   kObdCmdOpenWrite  : constant tObdCommand := 1;
   kObdCmdWriteObj   : constant tObdCommand := 2;
   kObdCmdCloseWrite : constant tObdCommand := 3;
   kObdCmdOpenRead   : constant tObdCommand := 4;
   kObdCmdReadObj    : constant tObdCommand := 5;
   kObdCmdCloseRead  : constant tObdCommand := 6;
   kObdCmdClear      : constant tObdCommand := 7;  -- ./oplk/obd.h:109

------------------------------------------------------------------------------
   --\brief Events of object callback function                                 --
--                                                                          --
-- This enumeration defines events that can be handled by the object        --
-- callback function.                                                       --
------------------------------------------------------------------------------
   subtype tObdEvent is unsigned;
   kObdEvCheckExist     : constant tObdEvent := 6;
   -- Checking if object does exist (reading and writing).  pArg points to: NULL
   kObdEvPreRead        : constant tObdEvent := 0;
   -- Called before reading an object. pArg points to: source data buffer in OD
   kObdEvPostRead       : constant tObdEvent := 1;
   -- Called after reading an object. pArg points to: 
   -- destination data buffer from caller
   kObdEvWrStringDomain : constant tObdEvent := 7;
   -- Event for changing string/domain data pointer or size. pArg points to: 
   -- struct tObdVStringDomain in RAM
   kObdEvInitWrite      : constant tObdEvent := 4;
   -- Initializes writing an object (checking object size). pArg points to: 
   -- size of object in OD (tObdSize)
   kObdEvPreWrite       : constant tObdEvent := 2;
   -- Called before writing an object. pArg points to: 
   -- source data buffer from caller
   kObdEvPostWrite      : constant tObdEvent := 3;
   -- Called after writing an object. pArg points to: 
   -- destination data buffer in OD
   kObdEvPostDefault    : constant tObdEvent := 8;
   -- Called after setting default values. pArg points to: data buffer in OD
                                                           -- ./oplk/obd.h:127

------------------------------------------------------------------------------
-- Data type for OD part definitions                                        --
------------------------------------------------------------------------------
   subtype tObdPart is unsigned;  -- ./oplk/obd.h:129

------------------------------------------------------------------------------
--         Data type for OD access types                                    --
------------------------------------------------------------------------------
   subtype tObdAccess is unsigned;  -- ./oplk/obd.h:142

   subtype tObdSize is unsigned;  
   -- For all objects as objects size are used an unsigned int.
   -- ./oplk/obd.h:202
   
   
------------------------------------------------------------------------------
--\brief Enumeration for object data types (DS301)                          --
--                                                                          --
-- This enumeration defines the data types of objects in object dictionary. --
-- DS-301 defines these types as UINT16.                                    --
-- openPOWERLINK supports only the listed data types. Other types are       --
-- not supported in this version.                                           --
------------------------------------------------------------------------------
   subtype tObdType is unsigned;
   kObdTypeBool      : constant tObdType := 1;
   -- 0001 - BOOLEAN
   kObdTypeInt8      : constant tObdType := 2;
   -- 0002 - INTEGER8
   kObdTypeInt16     : constant tObdType := 3;
   -- 0003 - INTEGER16
   kObdTypeInt32     : constant tObdType := 4;
   -- 0004 - INTEGER32
   kObdTypeUInt8     : constant tObdType := 5;
   -- 0005 - UNSIGNED8
   kObdTypeUInt16    : constant tObdType := 6;
   -- 0006 - UNSIGNED16
   kObdTypeUInt32    : constant tObdType := 7;
   -- 0007 - UNSIGNED32
   kObdTypeReal32    : constant tObdType := 8;
   -- 0008 - REAL32
   kObdTypeVString   : constant tObdType := 9;
   -- 0009 - VISIBLE_STRING
   kObdTypeOString   : constant tObdType := 10;
   -- 000A - OCTET_STRING
   kObdTypeTimeOfDay : constant tObdType := 12;
   -- 000C - TIME_OF_DAY
   kObdTypeTimeDiff  : constant tObdType := 13;
   -- 000D - TIME_DIFFERENCE
   kObdTypeDomain    : constant tObdType := 15;
   -- 000F - DOMAIN
   kObdTypeInt24     : constant tObdType := 16;
   -- 0010 - INTEGER24
   kObdTypeReal64    : constant tObdType := 17;
   -- 0011 - REAL64
   kObdTypeInt40     : constant tObdType := 18;
   -- 0012 - INTEGER40
   kObdTypeInt48     : constant tObdType := 19;
   -- 0013 - INTEGER48
   kObdTypeInt56     : constant tObdType := 20;
   -- 0014 - INTEGER56
   kObdTypeInt64     : constant tObdType := 21;
   -- 0015 - INTEGER64
   kObdTypeUInt24    : constant tObdType := 22;
   -- 0016 - UNSIGNED24
   kObdTypeUInt40    : constant tObdType := 24;
   -- 0018 - UNSIGNED40
   kObdTypeUInt48    : constant tObdType := 25;
   -- 0019 - UNSIGNED48
   kObdTypeUInt56    : constant tObdType := 26;
   -- 001A - UNSIGNED56
   kObdTypeUInt64    : constant tObdType := 27;
   -- 001B - UNSIGNED64
   kObdTypeMax       : constant tObdType := 28;
   --                                                      -- ./oplk/obd.h:243
   

------------------------------------------------------------------------------
--\name C type definitions for data types defined in POWERLINK DS301        --
--                                                                          --
-- The following C data types are defined according to the POWERLINK DS301  --
-- specification.                                                           --
------------------------------------------------------------------------------
   subtype tObdBoolean        is unsigned_char;
   -- for DS301 data type \ref kObdTypeBool
   subtype tObdInteger8       is signed_char;
   -- for DS301 data type \ref kObdTypeInt8
   subtype tObdInteger16      is short;
   -- for DS301 data type \ref kObdTypeInt16
   subtype tObdInteger32      is int;
   -- for DS301 data type \ref kObdTypeInt32
   subtype tObdUnsigned8      is unsigned_char;
   -- for DS301 data type \ref kObdTypeUInt8
   subtype tObdUnsigned16     is unsigned_short;
   -- for DS301 data type \ref kObdTypeUInt16
   subtype tObdUnsigned32     is unsigned;
   -- for DS301 data type \ref kObdTypeUInt32
   subtype tObdReal32         is Float;
   -- for DS301 data type \ref kObdTypeReal32
   subtype tObdTimeOfDay      is Oplk.oplkinc.tTimeOfDay;
   -- for DS301 data type \ref kObdTypeTimeOfDay
   subtype tObdTimeDifference is Oplk.oplkinc.tTimeOfDay;
   -- for DS301 data type \ref kObdTypeTimeDiff
   subtype tObdDomain         is unsigned_char;
   -- for DS301 data type \ref kObdTypeDomain
   subtype tObdInteger24      is int;
   -- for DS301 data type \ref kObdTypeInt24
   subtype tObdReal64         is double;
   -- for DS301 data type \ref kObdTypeReal64
   subtype tObdInteger40      is Long_Long_Integer;
   -- for DS301 data type \ref kObdTypeInt40
   subtype tObdInteger48      is Long_Long_Integer;
   -- for DS301 data type \ref kObdTypeInt48
   subtype tObdInteger56      is Long_Long_Integer;
   -- for DS301 data type \ref kObdTypeInt56
   subtype tObdInteger64      is Long_Long_Integer;
   -- for DS301 data type \ref kObdTypeInt64
   subtype tObdUnsigned24     is unsigned;
   -- for DS301 data type \ref kObdTypeUInt24
   subtype tObdUnsigned40     is Extensions.unsigned_long_long;
   -- for DS301 data type \ref kObdTypeUInt40
   subtype tObdUnsigned48     is Extensions.unsigned_long_long;
   -- for DS301 data type \ref kObdTypeUInt48
   subtype tObdUnsigned56     is Extensions.unsigned_long_long;
   -- for DS301 data type \ref kObdTypeUInt56
   subtype tObdUnsigned64     is Extensions.unsigned_long_long;  
   -- for DS301 data type \ref kObdTypeUInt64
   -- ./oplk/obd.h:276
   
   
   --   currently only size and data are implemented and used  --
   subtype tVarParamValid is unsigned;
   kVarValidSize : constant tVarParamValid := 1;
   kVarValidData : constant tVarParamValid := 2;
   kVarValidAll  : constant tVarParamValid := 3;  -- ./oplk/obd.h:284
   
   
   type tVarParam is record
      validFlag : aliased tVarParamValid;
      -- 
      index     : aliased unsigned;
      -- 
      subindex  : aliased unsigned;
      -- 
      size      : aliased tObdSize;
      -- 
      pData     : System.Address;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tVarParam);  -- ./oplk/obd.h:295
   
   
   type tObdVarEntry is record
      pData : System.Address;
      -- 
      size  : aliased tObdSize;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tObdVarEntry);  -- ./oplk/obd.h:301
   
   
------------------------------------------------------------------------------
--/ C type definition for DS301 data type \ref kObdTypeOString              --
------------------------------------------------------------------------------
   type tObdOString is record
      size    : aliased tObdSize;
      pString : access unsigned_char;
   end record;                         -- 0009
   pragma Convention (C_Pass_By_Copy, tObdOString);  -- ./oplk/obd.h:308
   
   
   type tObdOStringDef is record
      size       : aliased tObdSize;
      -- 
      pDefString : access unsigned_char;
      -- must be same offset as pString in tObdVString
      pString    : access unsigned_char;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tObdOStringDef);  -- ./oplk/obd.h:315
   
   
------------------------------------------------------------------------------
--/ C type definition for DS301 data type \ref kObdTypeVString              --
------------------------------------------------------------------------------
   type tObdVString is record
      size    : aliased tObdSize;
      -- 
      pString : Interfaces.C.Strings.chars_ptr;
      -- 
   end record;                         -- 000A
   pragma Convention (C_Pass_By_Copy, tObdVString);  -- ./oplk/obd.h:323
   
   
   type tObdVStringDef is record
      size       : aliased tObdSize;
      -- 
      pDefString : Interfaces.C.Strings.chars_ptr;
      -- must be same offset as pString in tObdVString
      pString    : Interfaces.C.Strings.chars_ptr;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tObdVStringDef);  -- ./oplk/obd.h:330
   
   
------------------------------------------------------------------------------
-- r.d. parameter struct for changing object size and/or pointer to data of --
--  Strings or Domains                                                        --
------------------------------------------------------------------------------
   type tObdVStringDomain is record
      downloadSize : aliased tObdSize;
      -- download size from SDO or APP
      objSize      : aliased tObdSize;
      -- current object size from OD - should be changed from callback function
      pData        : System.Address;
      -- current object ptr  from OD - should be changed from callback function
   end record;                         -- 000D
   pragma Convention (C_Pass_By_Copy, tObdVStringDomain);  -- ./oplk/obd.h:338
   
   
------------------------------------------------------------------------------
--\brief Parameters for callback function                                   --
--                                                                          --
-- This structure defines the parameters for the OD callback function.      --
------------------------------------------------------------------------------
   type tObdCbParam is record
      obdEvent  : aliased tObdEvent;
      -- Event that caused calling the function.
      index     : aliased unsigned;
      -- Index of the accessed object.
      subIndex  : aliased unsigned;
      -- Subindex of the accessed object.
      pArg      : System.Address;
      -- Additional argument.
      abortCode : aliased unsigned;
      -- Abort Code.
   end record;
   pragma Convention (C_Pass_By_Copy, tObdCbParam);  -- ./oplk/obd.h:352
   
   
------------------------------------------------------------------------------
-- define type for callback function: pParam_p points to tObdCbParam        --
------------------------------------------------------------------------------
   type tObdCallback is access function
     (arg1 : access tObdCbParam) return Oplk.errordefs.tOplkError;
   pragma Convention (C, tObdCallback);  -- ./oplk/obd.h:355
   
   
------------------------------------------------------------------------------
--\brief Structure for subindices                                           --
--                                                                          --
-- This structure defines a subindex in the OD.                             --
------------------------------------------------------------------------------
   type tObdSubEntry is record
      subIndex : aliased unsigned;
      -- Subindex of the object
      c_type   : aliased tObdType;
      -- Data type of the object
      c_access : aliased tObdAccess;
      -- Access type of the object
      pDefault : System.Address;
      -- Pointer to default data
      pCurrent : System.Address;
      -- Pointer to data (points always to RAM)
   end record;
   pragma Convention (C_Pass_By_Copy, tObdSubEntry);  -- ./oplk/obd.h:369

   type tObdSubEntryPtr is access all tObdSubEntry;  -- ./oplk/obd.h:371
   
   
------------------------------------------------------------------------------
--\brief Structure for indices                                              --
--                                                                          --
-- This structure defines an index in the OD.                               --
------------------------------------------------------------------------------
   type tObdEntry is record
      index       : aliased unsigned;
      -- Index of the object
      pSubIndex   : tObdSubEntryPtr;
      -- Points to subindex structures of this object
      count       : aliased unsigned;
      -- number of subindices.
      pfnCallback : tObdCallback;
      -- function is called back if object access
   end record;
   pragma Convention (C_Pass_By_Copy, tObdEntry);  -- ./oplk/obd.h:385

   type tObdEntryPtr is access all tObdEntry;  -- ./oplk/obd.h:387
   
   
------------------------------------------------------------------------------
--\brief Structure for OBD init parameters                                  --
--                                                                          --
-- This structure defines the init parameters of the OBD module.            --
------------------------------------------------------------------------------
   type u_tObdInitParam is record
      pGenericPart      : tObdEntryPtr;
      -- Pointer to generic part of OD
      numGeneric        : aliased unsigned;
      -- Number of entries in generic partition
      pManufacturerPart : tObdEntryPtr;
      -- Pointer to manufacturer part of OD
      numManufacturer   : aliased unsigned;
      -- Number of entries in manufacturer partition
      pDevicePart       : tObdEntryPtr;
      -- Pointer to device part of OD
      numDevice         : aliased unsigned;
      -- Number of entries in device partition
 --  #if (defined (OBD_USER_OD) && (OBD_USER_OD != FALSE))
 --       pUserPart         : tObdEntryPtr;
 --       -- Pointer to user part of OD
 --       NumUser           : aliased unsigned;
 --       -- Number of entries in user partition
 --  #endif
   end record;
   pragma Convention (C_Pass_By_Copy, u_tObdInitParam);  -- ./oplk/obd.h:394

   subtype tObdInitParam is u_tObdInitParam;
   
   
------------------------------------------------------------------------------
--\brief Structure for parameters of the store/restore commands             --
--                                                                          --
-- This structure specifies the parameters for the store/restore commands.  --
------------------------------------------------------------------------------
   type tObdCbStoreParam is record
      command       : aliased tObdCommand;
      -- 
      currentOdPart : aliased tObdPart;
      -- 
      pData         : System.Address;
      -- 
      objSize       : aliased tObdSize;
      -- 
   end record;
   pragma Convention (C_Pass_By_Copy, tObdCbStoreParam);  -- ./oplk/obd.h:421
   
   
   type tInitTabEntryCallback is access function
     (arg1 : System.Address;
      arg2 : unsigned) 
     return Oplk.errordefs.tOplkError;
   pragma Convention (C, tInitTabEntryCallback);  -- ./oplk/obd.h:423
   
   
   type tObdStoreLoadCallback is access function
     (arg1 : access tObdCbStoreParam) 
     return Oplk.errordefs.tOplkError;
   pragma Convention (C, tObdStoreLoadCallback);  -- ./oplk/obd.h:424
   
   
------------------------------------------------------------------------------
--\brief Enumeration for Node ID setting types                              --
--                                                                          --
-- This structure defines constants for the types of setting the node ID.   --
-- They are used in the function obd_setNodeId()                            --
------------------------------------------------------------------------------
   type tObdNodeIdType is
     (kObdNodeIdUnknown, 
      -- unknown how the node id was set
      kObdNodeIdSoftware, 
      -- node id set by software
      KObdNodeIdHardware
      -- node id set by hardware
     );
   pragma Convention (C, tObdNodeIdType);  -- ./oplk/obd.h:437
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
--   from obd.c                                                             --
-- Implementation of object dictionary (OD) module                          --
------------------------------------------------------------------------------
   function obd_init
     (pInitParam_p : access tObdInitParam)
     return Oplk.errordefs.tOplkError;
   -- pInitParam_p : Pointer to OD initialization parameters.
   -- tOplkError      : 
   --   kErrorObdIllegalPart      illegal OD part was specified!
   --   kErrorOk
   -- 
   -- The function initializes the OD module.
   pragma Import (C, obd_init, "obd_init");  -- ./oplk/obd.h:446
   
   
   function obd_deleteInstance
     return Oplk.errordefs.tOplkError;
   -- tOplkError      : 
   --   kErrorOk                  always
   -- The function de-initializes the OD module, but is not implemented
   pragma Import 
     (C, obd_deleteInstance, "obd_deleteInstance");  -- ./oplk/obd.h:447
   
   
   function obd_writeEntry
     (index_p    : unsigned;
      subIndex_p : unsigned;
      pSrcData_p : System.Address;
      size_p     : tObdSize) 
     return Oplk.errordefs.tOplkError;
   -- index_p    : Index to write.
   -- subIndex_p : Sub-index to write.
   -- pSrcData_p : Pointer to data which should be written.
   -- size_p     : Size of data to write.
   -- tOplkError      : 
   --   kErrorObdAccessViolation 
   --   kErrorObdValueLengthError source string is not 0-terminated and / or 
   --                             destination buffer is too short
   --   kErrorObdIndexNotExist
   --   
   --   and many more
   --   kErrorOk                  all checks are ok
   -- 
   -- The function writes data to an OD entry. Strings are stored with 
   -- added '\0' character.
   pragma Import (C, obd_writeEntry, "obd_writeEntry");  -- ./oplk/obd.h:448
   
   
   function obd_readEntry
     (index_p    : unsigned;
      subIndex_p : unsigned;
      pDstData_p : System.Address;
      pSize_p    : access tObdSize)
     return Oplk.errordefs.tOplkError;
   -- index_p    : Index to read.
   -- subIndex_p : Sub-index to read.
   -- pDstData_p : Pointer to store the read data.
   -- pSize_p    : Pointer to size of buffer. The real data size will
   --              be written to this location.
   -- tOplkError      : 
   --   kErrorInvalidInstanceParam 
   --   kErrorObdIndexNotExist
   --   kErrorObdIllegalPart
   --   kErrorObdReadViolation
   --   kErrorObdValueLengthError
   --    and more
   --   kErrorOk
   --   
   -- The function reads an OD entry. The object can always be read, even if the
   -- attribute kObdAccRead is not set. The attribute is only checked for SDO
   -- transfers.
   pragma Import (C, obd_readEntry, "obd_readEntry");  -- ./oplk/obd.h:449
   
   
   function obd_accessOdPart
     (obdPart_p   : tObdPart;
      direction_p : tObdDir)
     return Oplk.errordefs.tOplkError;
   -- obdPart_p   : 
   -- direction_p : 
   -- tOplkError      : 
   --   kErrorObdIllegalPart     illegal OD part was specified!
   --   kErrorOk
   -- 
   -- restores default values of one part of OD
   pragma Import (C, obd_accessOdPart, "obd_accessOdPart");  -- ./oplk/obd.h:450
   
   
   function obd_defineVar
     (pVarParam_p : access tVarParam)
     return Oplk.errordefs.tOplkError;
   -- pVarParam_p : Pointer to the object variable structure.
   -- tOplkError      : 
   --   kErrorObdValueLengthError
   --   and others
   --   kErrorOk
   -- 
   -- The function defines an OD variable.
   pragma Import (C, obd_defineVar, "obd_defineVar");  -- ./oplk/obd.h:451
   
   
   function obd_getObjectDataPtr
     (index_p    : unsigned;
      subIndex_p : unsigned) 
     return System.Address;
   -- index_p    : Index of the entry.
   -- subIndex_p : Sub-index of the entry.
   -- The function returns the pointer to the object data
   -- 
   -- The function returns the current data pointer. If object is a
   -- constant object it returns the default pointer.
   pragma Import 
     (C, obd_getObjectDataPtr, "obd_getObjectDataPtr");  -- ./oplk/obd.h:452
   
   
   function obd_registerUserOd
     (pUserOd_p : tObdEntryPtr)
     return Oplk.errordefs.tOplkError;
   -- pUserOd_p : Pointer to user OD.
   -- tOplkError      : 
   --   kErrorOk          always.
   -- 
   -- The function registers a user object dictionary.
   pragma Import 
     (C, obd_registerUserOd, "obd_registerUserOd");  -- ./oplk/obd.h:453
   
   
   procedure obd_initVarEntry
     (pVarEntry_p : access tObdVarEntry;
      type_p      : tObdType;
      obdSize_p   : tObdSize);
   -- pVarEntry_p : Pointer to VarEntry structure.
   -- type_p      : Object type.
   -- obdSize_p   : Size of object data.
   -- 
   -- The function initializes the VarEntry dependant on the object type.
   -- The function will not be used for strings.
   pragma Import (C, obd_initVarEntry, "obd_initVarEntry");  -- ./oplk/obd.h:454
   
   
   function obd_getDataSize
     (index_p    : unsigned;
      subIndex_p : unsigned) 
     return tObdSize;
   -- index_p    : 
   -- subIndex_p : 
   -- The function returns the data size.
   --   tObdSize will be 0 on error.
   -- 
   -- The function gets the data size of an object. For string objects it 
   -- returns the string length without terminating null-character.
   pragma Import (C, obd_getDataSize, "obd_getDataSize");  -- ./oplk/obd.h:455
   
   
   function obd_getNodeId 
     return unsigned;
   -- The function returns the node ID.
   --   unsigned will be 0 on error.
   -- 
   -- The function gets the node ID which is stored in object 0x1F93.
   pragma Import (C, obd_getNodeId, "obd_getNodeId");  -- ./oplk/obd.h:456
   
   
   function obd_setNodeId
     (nodeId_p     : unsigned;
      nodeIdType_p : tObdNodeIdType)
     return Oplk.errordefs.tOplkError;
   -- nodeId_p     : Node ID to set.
   -- nodeIdType_p : Node ID setting type.
   -- tOplkError      : 
   --   kErrorObdAccessViolation
   --   kErrorObdValueLengthError
   --   and others
   --   kErrorOk
   -- 
   -- The function sets the node ID in object 0x1F93.
   pragma Import (C, obd_setNodeId, "obd_setNodeId");  -- ./oplk/obd.h:457
   
   
   function obd_isNumerical
     (index_p            : unsigned;
      subIndex_p         : unsigned;
      pfEntryNumerical_p : access unsigned_char)
     return Oplk.errordefs.tOplkError;
   -- index_p            : Index of object to check.
   -- subIndex_p         : Sub-index of object to check.
   -- pfEntryNumerical_p : Pointer to store result. TRUE if entry is numerical,
   --                                          FALSE if entry is not numerical.
   -- tOplkError             : 
   -- 
   -- The function checks if a entry is numerical or not.
   pragma Import (C, obd_isNumerical, "obd_isNumerical");  -- ./oplk/obd.h:458
   
   
   function obd_getType
     (index_p    : unsigned;
      subIndex_p : unsigned;
      pType_p    : access tObdType)
     return Oplk.errordefs.tOplkError;
   -- index_p    : Index of object to check.
   -- subIndex_p : Sub-index of object to check.
   -- pType_p    : Pointer to store the type of the entry.
   -- tOplkError             : 
   -- 
   -- 
   -- The function returns the data type of the specified entry.
   pragma Import (C, obd_getType, "obd_getType");  -- ./oplk/obd.h:459
   
   
   function obd_writeEntryFromLe
     (index_p    : unsigned;
      subIndex_p : unsigned;
      pSrcData_p : System.Address;
      size_p     : tObdSize) 
     return Oplk.errordefs.tOplkError;
   -- index_p    : Index of object to write.
   -- subIndex_p : Sub-index of object to write.
   -- pSrcData_p : Pointer to the data which should be written.
   -- size_p     : Size of the data to be written.
   -- tOplkError             : 
   -- 
   -- The function writes an object entry and converts numerical types from 
   -- the little endian byte order into the system byte order. 
   -- For other types a normal write will be performed. 
   -- Strings are stored with added '\0' character.
   pragma Import 
     (C, obd_writeEntryFromLe, "obd_writeEntryFromLe");  -- ./oplk/obd.h:460

   function obd_readEntryToLe
     (index_p    : unsigned;
      subIndex_p : unsigned;
      pDstData_p : System.Address;
      pSize_p    : access tObdSize)
     return Oplk.errordefs.tOplkError;
   -- index_p    : Index of object to read.
   -- subIndex_p : Sub-index of object to read.
   -- pDstData_p : Pointer to location where to store the read data.
   -- pSize_p    : Pointer to the size of the buffer. The function
   --               stores the number of read bytes at this
   --               location.
   -- tOplkError             : 
   -- 
   -- The function reads an object entry and converts numerical types into the 
   -- little endian byte order for numerical types. 
   -- For other types a normal read will be performed. 
   -- This is useful for the PDO and SDO module.
   -- 
   -- The application can always read the data even if the attribute 
   -- kObdAccRead is not set. The attribute is only checked on SDO transfers.
   pragma Import 
     (C, obd_readEntryToLe, "obd_readEntryToLe");  -- ./oplk/obd.h:461
   
   
   function obd_getAccessType
     (index_p       : unsigned;
      subIndex_p    : unsigned;
      pAccessType_p : access tObdAccess)
     return Oplk.errordefs.tOplkError;
   -- index_p       : Index of object.
   -- subIndex_p    : Sub-index of object.
   -- pAccessType_p : Pointer to store the access type.
   -- tOplkError             : 
   -- 
   -- The function gets the access type of the entry
   --  (note that this is not the usual Ada definition of access type)
   pragma Import 
     (C, obd_getAccessType, "obd_getAccessType");  -- ./oplk/obd.h:462
   
   
   function obd_searchVarEntry
     (index_p      : unsigned;
      subindex_p   : unsigned;
      ppVarEntry_p : System.Address)
     return Oplk.errordefs.tOplkError;
   -- index_p       : Index of object.
   -- subIndex_p    : Sub-index of object.
   -- ppVarEntry_p  : Pointer to store pointer to the VarEntry structure.
   -- tOplkError             : 
   -- 
   -- The function gets the VarEntry structure of an object.
   pragma Import 
     (C, obd_searchVarEntry, "obd_searchVarEntry");  -- ./oplk/obd.h:463
   
   
------------------------------------------------------------------------------
-- function prototypes                                                      --
-- from obdcreate.c                                                         --
--                                                                          --  
--      Object dictionary creation                                          --
-- This file contains the OD data tables and the OD data structure 
-- initialization function.                                                 --
--  (dont ask me what this means in the .c file)                            --
-- The OD data structure initialization is a very tricky part of the        --
-- openPOWERLINK stack.                                                     --
-- To create the different tables and code parts a set of macros defined in --
-- obdmacro.h is used. These macros are redefined depending on some other   --
-- "type definition" macros. To create the different tables the specific    --
-- "type definition" macro will be set, the file objdict.h is included and  --
-- Therefore the specified data structures are created. Afterwards the      --
-- "type definition" macro is unset, the next one is set and objdict.h is   --
-- included again to Generate the next table.                               --
------------------------------------------------------------------------------

   
   function obd_initObd
     (pInitParam_p : access tObdInitParam)
     return Oplk.errordefs.tOplkError;
   -- pInitParam_p : Pointer to OD initialization parameters.
   -- tOplkError             : 
   -- 
   -- The function initializes the object dictionary data structures.
   pragma Import (C, obd_initObd, "obd_initObd");  -- ./oplk/obd.h:465
   
   
--  #if defined(CONFIG_OBD_USE_STORE_RESTORE) && (CONFIG_OBD_USE_STORE_RESTORE != FALSE)
--     function obd_storeLoadObjCallback
--       (PfnCallback_P : tObdStoreLoadCallback)
--       return Oplk.errordefs.tOplkError;
-- -- PfnCallback_P : Pointer to the callback function.
-- -- tOplkError             : 
-- -- 
-- -- The function sets the callback function for the load/store command.
--     pragma Import 
--   (C, Obd_StoreLoadObjCallback, "obd_storeLoadObjCallback");  -- ./oplk/obd.h:468
--  #endif
   
end Oplk.obd;

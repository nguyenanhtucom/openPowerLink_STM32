pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package Oplk.nmt is
   
   --   define super-states and masks to identify a super-state   --
   NMT_GS_POWERED        : constant := 16#0008#;
   -- super state
   NMT_GS_INITIALISATION : constant := 16#0009#;
   -- super state
   NMT_GS_COMMUNICATING  : constant := 16#000C#;
   -- super state
   NMT_CS_PLKMODE        : constant := 16#000D#;
   -- super state
   NMT_MS_PLKMODE        : constant := 16#000D#;
   -- super state
   
   NMT_SUPERSTATE_MASK   : constant := 16#000F#;
   -- mask to select state
   
   NMT_TYPE_UNDEFINED    : constant := 16#0000#;
   -- type of NMT state is still undefined
   NMT_TYPE_CS           : constant := 16#0100#;
   -- CS type of NMT state
   NMT_TYPE_MS           : constant := 16#0200#;
   -- MS type of NMT state
   NMT_TYPE_MASK         : constant := 16#0300#;
   -- mask to select type of NMT state (i.e. CS or MS)   --  ./oplk/nmt.h:61


------------------------------------------------------------------------------
--\brief NMT states                                                         --
--                                                                          --
-- The enumeration lists all valid NMT states.                              --
-- The lower Byte of the NMT-State is encoded like the values in the        --
-- POWERLINK standard. The higher byte is used to encode MN                 --
-- (Bit 1 of the higher byte = 1) or CN (Bit 0 of the higher byte  = 1).    --
-- The super-states are not mentioned in this enum because they are no real --
-- states --> there are masks defined to identify the super-states.         --
--                                                                          --
-- The order of the states is important as it is used in the source code to --
-- determine several things:                                                --
--                                                                          --
-- state > kMntGsResetConfiguration:  No reset state                        --
-- state >= kNmtMsNotActive:          Node is running as MN                 --
-- state < kNmtMsNotActive:           Node is running as CN                 --
------------------------------------------------------------------------------
   subtype tNmtState is unsigned;
   kNmtGsOff                : constant tNmtState := 0;
   kNmtGsInitialising       : constant tNmtState := 25;
   kNmtGsResetApplication   : constant tNmtState := 41;
   kNmtGsResetCommunication : constant tNmtState := 57;
   kNmtGsResetConfiguration : constant tNmtState := 121;
   kNmtCsNotActive          : constant tNmtState := 284;
   kNmtCsPreOperational1    : constant tNmtState := 285;
   kNmtCsStopped            : constant tNmtState := 333;
   kNmtCsPreOperational2    : constant tNmtState := 349;
   kNmtCsReadyToOperate     : constant tNmtState := 365;
   kNmtCsOperational        : constant tNmtState := 509;
   kNmtCsBasicEthernet      : constant tNmtState := 286;
   kNmtMsNotActive          : constant tNmtState := 540;
   kNmtMsPreOperational1    : constant tNmtState := 541;
   kNmtMsPreOperational2    : constant tNmtState := 605;
   kNmtMsReadyToOperate     : constant tNmtState := 621;
   kNmtMsOperational        : constant tNmtState := 765;
   kNmtMsBasicEthernet      : constant tNmtState := 542;  -- ./oplk/nmt.h:104
   

------------------------------------------------------------------------------
--\brief NMT events                                                         --
--                                                                          --
-- This enumeration lists all valid NMT events.                             --
------------------------------------------------------------------------------

   --   Events from DLL                                --
   --   Events defined by the POWERLINK specification  --
   subtype tNmtEvent is unsigned;
   kNmtEventNoEvent              : constant tNmtEvent := 0;
   -- No event has occured
   kNmtEventDllMePresTimeout     : constant tNmtEvent := 2;
   -- A PRes timeout event has occured on the MN
   kNmtEventDllMeAsndTimeout     : constant tNmtEvent := 3;
   -- An ASnd timeout event has occured on the MN
   kNmtEventDllMeSocTrig         : constant tNmtEvent := 5;
   -- An SoC trigger event has occured on the MN
   kNmtEventDllMeSoaTrig         : constant tNmtEvent := 6;
   -- An SoA trigger event has occured on the MN
   kNmtEventDllCeSoc             : constant tNmtEvent := 7;
   -- An SoC event has occured on the CN
   kNmtEventDllCePreq            : constant tNmtEvent := 8;
   -- A PReq event has occured on the CN
   kNmtEventDllCePres            : constant tNmtEvent := 9;
   -- A PRes event has occured on the CN
   kNmtEventDllCeSoa             : constant tNmtEvent := 10;
   -- An SoA event has occured on the CN
   kNmtEventDllCeAInv            : constant tNmtEvent := 11;
   -- An AInv event has occured on the CN
   kNmtEventDllCeAsnd            : constant tNmtEvent := 12;
   -- An ASnd event has occured on the CN
   kNmtEventDllCeFrameTimeout    : constant tNmtEvent := 13;
   -- A frame timeout has occured on the CN

   --   Events triggered by NMT-Commands   --
   kNmtEventSwReset              : constant tNmtEvent := 16;
   -- A SwReset event has occured (NMT_GT1, NMT_GT2, NMT_GT8)
   kNmtEventResetNode            : constant tNmtEvent := 17;
   -- A ResetNode event has occured
   kNmtEventResetCom             : constant tNmtEvent := 18;
   -- A ResetCommunication event has occured
   kNmtEventResetConfig          : constant tNmtEvent := 19;
   -- A ResetConfiguration event has occured
   kNmtEventEnterPreOperational2 : constant tNmtEvent := 20;
   -- An EnterPreOperational2 event has occured
   kNmtEventEnableReadyToOperate : constant tNmtEvent := 21;
   -- An EnableReadyToOperate event has occured
   kNmtEventStartNode            : constant tNmtEvent := 22;
   -- A StartNode event has occured (NMT_CT7)
   kNmtEventStopNode             : constant tNmtEvent := 23;
   -- A StopNode event has occured

   --   Events triggered by higher layer   --
   kNmtEventEnterResetApp        : constant tNmtEvent := 32;
   -- An EnterResetApplication event has occured
   kNmtEventEnterResetCom        : constant tNmtEvent := 33;
   -- An EnterResetCommunication event has occured
   kNmtEventInternComError       : constant tNmtEvent := 34;
   -- An internal communication error has occured
   -- (NMT_GT6 -> enter ResetCommunication)
   kNmtEventEnterResetConfig     : constant tNmtEvent := 35;
   -- An EnterResetConfiguration event has occured
   kNmtEventEnterCsNotActive     : constant tNmtEvent := 36;
   -- An EnterCsNotActive event has occured
   kNmtEventEnterMsNotActive     : constant tNmtEvent := 37;
   -- An EnterMsNotActive event has occured
   kNmtEventTimerBasicEthernet   : constant tNmtEvent := 38;
   -- NMT_CT3; timer triggered state change (NotActive -> BasicEth)
   kNmtEventTimerMsPreOp1        : constant tNmtEvent := 39;
   -- enter PreOp1 on MN (NotActive -> MsPreOp1)
   kNmtEventNmtCycleError        : constant tNmtEvent := 40;
   -- NMT_CT11, NMT_MT6; error during cycle -> enter PreOp1
   kNmtEventTimerMsPreOp2        : constant tNmtEvent := 41;
   -- enter PreOp2 on MN (MsPreOp1 -> MsPreOp2 if kNmtEventAllMandatoryCNIdent)
   kNmtEventAllMandatoryCNIdent  : constant tNmtEvent := 42;
   -- enter PreOp2 on MN if kNmtEventTimerMsPreOp2
   kNmtEventEnterReadyToOperate  : constant tNmtEvent := 43;
   -- application ready for the state ReadyToOp
   kNmtEventEnterMsOperational   : constant tNmtEvent := 44;
   -- enter Operational on MN
   kNmtEventSwitchOff            : constant tNmtEvent := 45;
   -- enter state Off
   kNmtEventCriticalError        : constant tNmtEvent := 46;
   -- enter state Off because of critical error -- ./oplk/nmt.h:157
   

------------------------------------------------------------------------------
--\brief NMT state change event                                             --
--                                                                          --
-- This structure defines the NMT state change event.                       --
------------------------------------------------------------------------------
   type tEventNmtStateChange is record
      newNmtState : aliased tNmtState;
      -- New NMT state
      oldNmtState : aliased tNmtState;
      -- Old NMT state
      nmtEvent    : aliased tNmtEvent;
      -- NMT event
   end record;
   pragma Convention
     (C_Pass_By_Copy, tEventNmtStateChange);  -- ./oplk/nmt.h:169
   

------------------------------------------------------------------------------
--\brief Heartbeat event                                                    --
--                                                                          --
-- This structure defines the heartbeat event.                              --
------------------------------------------------------------------------------
   type tHeartbeatEvent is record
      nodeId    : aliased unsigned;
      -- Node ID
      nmtState  : aliased tNmtState;
      -- NMT state (remember distinguish between MN / CN)
      errorCode : aliased unsigned_short;
      -- POWERLINK error code in case of NMT state NotActive
   end record;
   pragma Convention (C_Pass_By_Copy, tHeartbeatEvent);  -- ./oplk/nmt.h:181
   

------------------------------------------------------------------------------
--\brief Node events                                                        --
--                                                                          --
-- The enumeration lists the valid node events.                             --
------------------------------------------------------------------------------
   type tNmtNodeEvent is
     (kNmtNodeEventFound,
   -- A configured node has been found.
      kNmtNodeEventUpdateSw,
   -- The application shall update the software on the CN.
      kNmtNodeEventCheckConf,
   -- The application / Configuration Manager shall check and update the
   -- configuration on the CN.
      kNmtNodeEventUpdateConf,
   -- The application / Configuration Manager shall update the
   -- configuration on the CN (check was done by NmtMn module).
      kNmtNodeEventVerifyConf,
   -- The application / Configuration Manager shall verify the
   -- configuration of the CN.
      kNmtNodeEventReadyToStart,
   -- Issued if NMT_STARTUP_NO_STARTNODE set, application must call
   -- oplk_execNmtCommand(kErrorNmtCmdStartNode) manually.
      kNmtNodeEventNmtState,
   -- Issued if the NMT state of the CN has changed.
      KNmtNodeEventError
   -- NMT error of the CN.
   );
   pragma Convention (C, tNmtNodeEvent);  -- ./oplk/nmt.h:199
   

------------------------------------------------------------------------------
--\brief NMT node commands
--
-- This enumeration lists all valid NMT node commands.
------------------------------------------------------------------------------
   subtype tNmtNodeCommand is unsigned;
   kNmtNodeCommandBoot         : constant tNmtNodeCommand := 1;
   -- If NMT_NODEASSIGN_START_CN is not set,
   -- this command must be issued after kNmtNodeEventFound.
   kNmtNodeCommandSwOk         : constant tNmtNodeCommand := 2;
   -- The application has verified the software on the CN to be fine.
   kNmtNodeCommandSwUpdated    : constant tNmtNodeCommand := 3;
   -- The application has successfully updated the software on the CN.
   kNmtNodeCommandConfOk       : constant tNmtNodeCommand := 4;
   -- The application / Configuration Manager has successfully updated the
   -- configuration on the CN.
   kNmtNodeCommandConfRestored : constant tNmtNodeCommand := 5;
   -- The application / Configuration Manager has restored the original
   -- CN configuration, and the CN needs a ResetNode to complete the
   -- restore process. Afterwards, the new configuration can be downloaded.
   kNmtNodeCommandConfReset    : constant tNmtNodeCommand := 6;
   -- The application / Configuration Manager has successfully updated the
   -- configuration on the CN, and the CN needs ResetConf so that the
   -- configuration gets activated.
   kNmtNodeCommandConfErr      : constant tNmtNodeCommand := 7;
   -- The application / Configuration Manager failed on updating configuration
   -- on the CN.
   kNmtNodeCommandStart        : constant tNmtNodeCommand := 8;
-- If NMT_STARTUP_NO_STARTNODE is set, this command must be issued after
-- kNmtNodeEventReadyToStart.                             -- ./oplk/nmt.h:216
   

------------------------------------------------------------------------------
--\brief NMT boot events                                                    --
--                                                                          --
-- This enumeration lists all valid NMT boot events.                        --
------------------------------------------------------------------------------
   type tNmtBootEvent is
     (kNmtBootEventBootStep1Finish,
   -- PreOp2 is possible
      kNmtBootEventBootStep2Finish,
   -- ReadyToOp is possible for MN
      kNmtBootEventEnableReadyToOp,
   -- ReadyToOP is possible for CN
      kNmtBootEventCheckComFinish,
   -- Operational is possible
      kNmtBootEventOperational,
   --all mandatory CNs are Operational
      KNmtBootEventError
   -- boot process halted because of an error
   );
   pragma Convention (C, tNmtBootEvent);  -- ./oplk/nmt.h:231

end Oplk.nmt;

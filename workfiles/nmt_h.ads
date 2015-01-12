pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package nmt_h is


   NMT_GS_POWERED : constant := 16#0008#;  --  ./oplk/nmt.h:50
   NMT_GS_INITIALISATION : constant := 16#0009#;  --  ./oplk/nmt.h:51
   NMT_GS_COMMUNICATING : constant := 16#000C#;  --  ./oplk/nmt.h:52
   NMT_CS_PLKMODE : constant := 16#000D#;  --  ./oplk/nmt.h:53
   NMT_MS_PLKMODE : constant := 16#000D#;  --  ./oplk/nmt.h:54

   NMT_SUPERSTATE_MASK : constant := 16#000F#;  --  ./oplk/nmt.h:56

   NMT_TYPE_UNDEFINED : constant := 16#0000#;  --  ./oplk/nmt.h:58
   NMT_TYPE_CS : constant := 16#0100#;  --  ./oplk/nmt.h:59
   NMT_TYPE_MS : constant := 16#0200#;  --  ./oplk/nmt.h:60
   NMT_TYPE_MASK : constant := 16#0300#;  --  ./oplk/nmt.h:61

  --*
  --********************************************************************************
  --\file   oplk/nmt.h
  --\brief  Global include file for NMT modules
  --This file is the global include file for all NMT modules
  --****************************************************************************** 

  --------------------------------------------------------------------------------
  --Copyright (c) 2013, SYSTEC electronic GmbH
  --Copyright (c) 2014, Bernecker+Rainer Industrie-Elektronik Ges.m.b.H. (B&R)
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
  -- define super-states and masks to identify a super-state
  --------------------------------------------------------------------------------
  -- typedef
  --------------------------------------------------------------------------------
  --*
  --* \brief NMT states
  --*
  --* The enumeration lists all valid NMT states. The lower Byte of the NMT-State
  --* is encoded like the values in the POWERLINK standard. The higher byte is used
  --* to encode MN (Bit 1 of the higher byte = 1) or CN (Bit 0 of the
  --* higher byte  = 1). The super-states are not mentioned in this enum because
  --* they are no real states --> there are masks defined to identify the
  --* super-states.
  --*
  --* The order of the states is important as it is used in the source code to
  --* determine several things:
  --*
  --* state > kMntGsResetConfiguration:  No reset state
  --* state >= kNmtMsNotActive:          Node is running as MN
  --* state < kNmtMsNotActive:           Node is running as CN
  -- 

  --/< NMT_GS_OFF
  --/< NMT_GS_INITIALIZING
  --/< NMT_GS_RESET_APPLICATION
  --/< NMT_GS_RESET_COMMUNICATION
  --/< NMT_GS_RESET_CONFIGURATION
  --/< NMT_CS_NOT_ACTIVE
  --/< NMT_CS_PRE_OPERATIONAL_1
  --/< NMT_CS_STOPPED
  --/< NMT_CS_PRE_OPERATIONAL_2
  --/< NMT_CS_READY_TO_OPERATE
  --/< NMT_CS_OPERATIONAL
  --/< NMT_CS_BASIC_ETHERNET
  --/< NMT_MS_NOT_ACTIVE
  --/< NMT_MS_PRE_OPERATIONAL_1
  --/< NMT_MS_PRE_OPERATIONAL_2
  --/< NMT_MS_READY_TO_OPERATE
  --/< NMT_MS_OPERATIONAL
  --/< NMT_MS_BASIC_ETHERNET
   subtype tNmtState is unsigned;
   kNmtGsOff : constant tNmtState := 0;
   kNmtGsInitialising : constant tNmtState := 25;
   kNmtGsResetApplication : constant tNmtState := 41;
   kNmtGsResetCommunication : constant tNmtState := 57;
   kNmtGsResetConfiguration : constant tNmtState := 121;
   kNmtCsNotActive : constant tNmtState := 284;
   kNmtCsPreOperational1 : constant tNmtState := 285;
   kNmtCsStopped : constant tNmtState := 333;
   kNmtCsPreOperational2 : constant tNmtState := 349;
   kNmtCsReadyToOperate : constant tNmtState := 365;
   kNmtCsOperational : constant tNmtState := 509;
   kNmtCsBasicEthernet : constant tNmtState := 286;
   kNmtMsNotActive : constant tNmtState := 540;
   kNmtMsPreOperational1 : constant tNmtState := 541;
   kNmtMsPreOperational2 : constant tNmtState := 605;
   kNmtMsReadyToOperate : constant tNmtState := 621;
   kNmtMsOperational : constant tNmtState := 765;
   kNmtMsBasicEthernet : constant tNmtState := 542;  -- ./oplk/nmt.h:104

  --*
  --* \brief NMT events
  --*
  --* This enumeration lists all valid NMT events.
  -- 

  -- Events from DLL
  -- Events defined by the POWERLINK specification
  --/< No event has occured
  -- kNmtEventDllMePres           =   0x01,
  --/< A PRes timeout event has occured on the MN
  -- kNmtEventDllMeAsnd           =   0x03,
  --/< An ASnd timeout event has occured on the MN
  -- kNmtEventDllMeSoaSent        =   0x04,
  --/< An SoC trigger event has occured on the MN
  --/< An SoA trigger event has occured on the MN
  --/< An SoC event has occured on the CN
  --/< A PReq event has occured on the CN
  --/< A PRes event has occured on the CN
  --/< An SoA event has occured on the CN
  --/< An AInv event has occured on the CN
  --/< An ASnd event has occured on the CN
  --/< A frame timeout has occured on the CN
  -- Events triggered by NMT-Commands
  --/< A SwReset event has occured (NMT_GT1, NMT_GT2, NMT_GT8)
  --/< A ResetNode event has occured
  --/< A ResetCommunication event has occured
  --/< A ResetConfiguration event has occured
  --/< An EnterPreOperational2 event has occured
  --/< An EnableReadyToOperate event has occured
  --/< A StartNode event has occured (NMT_CT7)
  --/< A StopNode event has occured
  -- Events triggered by higher layer
  --/< An EnterResetApplication event has occured
  --/< An EnterResetCommunication event has occured
  --/< An internal communication error has occured (NMT_GT6 -> enter ResetCommunication)
  --/< An EnterResetConfiguration event has occured
  --/< An EnterCsNotActive event has occured
  --/< An EnterMsNotActive event has occured
  --/< NMT_CT3; timer triggered state change (NotActive -> BasicEth)
  --/< enter PreOp1 on MN (NotActive -> MsPreOp1)
  --/< NMT_CT11, NMT_MT6; error during cycle -> enter PreOp1
  --/< enter PreOp2 on MN (MsPreOp1 -> MsPreOp2 if kNmtEventAllMandatoryCNIdent)
  --/< enter PreOp2 on MN if kNmtEventTimerMsPreOp2
  --/< application ready for the state ReadyToOp
  --/< enter Operational on MN
  --/< enter state Off
  --/< enter state Off because of critical error
   subtype tNmtEvent is unsigned;
   kNmtEventNoEvent : constant tNmtEvent := 0;
   kNmtEventDllMePresTimeout : constant tNmtEvent := 2;
   kNmtEventDllMeAsndTimeout : constant tNmtEvent := 3;
   kNmtEventDllMeSocTrig : constant tNmtEvent := 5;
   kNmtEventDllMeSoaTrig : constant tNmtEvent := 6;
   kNmtEventDllCeSoc : constant tNmtEvent := 7;
   kNmtEventDllCePreq : constant tNmtEvent := 8;
   kNmtEventDllCePres : constant tNmtEvent := 9;
   kNmtEventDllCeSoa : constant tNmtEvent := 10;
   kNmtEventDllCeAInv : constant tNmtEvent := 11;
   kNmtEventDllCeAsnd : constant tNmtEvent := 12;
   kNmtEventDllCeFrameTimeout : constant tNmtEvent := 13;
   kNmtEventSwReset : constant tNmtEvent := 16;
   kNmtEventResetNode : constant tNmtEvent := 17;
   kNmtEventResetCom : constant tNmtEvent := 18;
   kNmtEventResetConfig : constant tNmtEvent := 19;
   kNmtEventEnterPreOperational2 : constant tNmtEvent := 20;
   kNmtEventEnableReadyToOperate : constant tNmtEvent := 21;
   kNmtEventStartNode : constant tNmtEvent := 22;
   kNmtEventStopNode : constant tNmtEvent := 23;
   kNmtEventEnterResetApp : constant tNmtEvent := 32;
   kNmtEventEnterResetCom : constant tNmtEvent := 33;
   kNmtEventInternComError : constant tNmtEvent := 34;
   kNmtEventEnterResetConfig : constant tNmtEvent := 35;
   kNmtEventEnterCsNotActive : constant tNmtEvent := 36;
   kNmtEventEnterMsNotActive : constant tNmtEvent := 37;
   kNmtEventTimerBasicEthernet : constant tNmtEvent := 38;
   kNmtEventTimerMsPreOp1 : constant tNmtEvent := 39;
   kNmtEventNmtCycleError : constant tNmtEvent := 40;
   kNmtEventTimerMsPreOp2 : constant tNmtEvent := 41;
   kNmtEventAllMandatoryCNIdent : constant tNmtEvent := 42;
   kNmtEventEnterReadyToOperate : constant tNmtEvent := 43;
   kNmtEventEnterMsOperational : constant tNmtEvent := 44;
   kNmtEventSwitchOff : constant tNmtEvent := 45;
   kNmtEventCriticalError : constant tNmtEvent := 46;  -- ./oplk/nmt.h:157

  --*
  --* \brief NMT state change event
  --*
  --* This structure defines the NMT state change event.
  -- 

  --/< New NMT state
   type tEventNmtStateChange is record
      newNmtState : aliased tNmtState;  -- ./oplk/nmt.h:166
      oldNmtState : aliased tNmtState;  -- ./oplk/nmt.h:167
      nmtEvent : aliased tNmtEvent;  -- ./oplk/nmt.h:168
   end record;
   pragma Convention (C_Pass_By_Copy, tEventNmtStateChange);  -- ./oplk/nmt.h:169

   --  skipped anonymous struct anon_73

  --/< Old NMT state
  --/< NMT event
  --*
  --* \brief Heartbeat event
  --*
  --* This structure defines the heartbeat event.
  -- 

  --/< Node ID
   type tHeartbeatEvent is record
      nodeId : aliased unsigned;  -- ./oplk/nmt.h:178
      nmtState : aliased tNmtState;  -- ./oplk/nmt.h:179
      errorCode : aliased unsigned_short;  -- ./oplk/nmt.h:180
   end record;
   pragma Convention (C_Pass_By_Copy, tHeartbeatEvent);  -- ./oplk/nmt.h:181

   --  skipped anonymous struct anon_74

  --/< NMT state (remember distinguish between MN / CN)
  --/< POWERLINK error code in case of NMT state NotActive
  --*
  --* \brief Node events
  --*
  --* The enumeration lists the valid node events.
  -- 

  --/< A configured node has been found.
  --/< The application shall update the software on the CN.
  --/< The application / Configuration Manager shall check and update the configuration on the CN.
  --/< The application / Configuration Manager shall update the configuration on the CN (check was done by NmtMn module).
  --/< The application / Configuration Manager shall verify the configuration of the CN.
  --/< Issued if NMT_STARTUP_NO_STARTNODE set, application must call oplk_execNmtCommand(kErrorNmtCmdStartNode) manually.
  --/< Issued if the NMT state of the CN has changed.
  --/< NMT error of the CN.
   type tNmtNodeEvent is 
     (kNmtNodeEventFound,
      kNmtNodeEventUpdateSw,
      kNmtNodeEventCheckConf,
      kNmtNodeEventUpdateConf,
      kNmtNodeEventVerifyConf,
      kNmtNodeEventReadyToStart,
      kNmtNodeEventNmtState,
      kNmtNodeEventError);
   pragma Convention (C, tNmtNodeEvent);  -- ./oplk/nmt.h:199

  --*
  --* \brief NMT node commands
  --*
  --* This enumeration lists all valid NMT node commands.
  -- 

  --/< If NMT_NODEASSIGN_START_CN is not set, this command must be issued after kNmtNodeEventFound.
  --/< The application has verified the software on the CN to be fine.
  --/< The application has successfully updated the software on the CN.
  --/< The application / Configuration Manager has successfully updated the configuration on the CN.
  --/< The application / Configuration Manager has restored the original CN configuration, and the CN needs a ResetNode to complete the restore process. Afterwards, the new configuration can be downloaded.
  --/< The application / Configuration Manager has successfully updated the configuration on the CN, and the CN needs ResetConf so that the configuration gets activated.
  --/< The application / Configuration Manager failed on updating configuration on the CN.
  --/< If NMT_STARTUP_NO_STARTNODE is set, this command must be issued after kNmtNodeEventReadyToStart.
   subtype tNmtNodeCommand is unsigned;
   kNmtNodeCommandBoot : constant tNmtNodeCommand := 1;
   kNmtNodeCommandSwOk : constant tNmtNodeCommand := 2;
   kNmtNodeCommandSwUpdated : constant tNmtNodeCommand := 3;
   kNmtNodeCommandConfOk : constant tNmtNodeCommand := 4;
   kNmtNodeCommandConfRestored : constant tNmtNodeCommand := 5;
   kNmtNodeCommandConfReset : constant tNmtNodeCommand := 6;
   kNmtNodeCommandConfErr : constant tNmtNodeCommand := 7;
   kNmtNodeCommandStart : constant tNmtNodeCommand := 8;  -- ./oplk/nmt.h:216

  --*
  --* \brief NMT boot events
  --*
  --* This enumeration lists all valid NMT boot events.
  -- 

  --/< PreOp2 is possible
  --/< ReadyToOp is possible for MN
  --/< ReadyToOP is possible for CN
  --/< Operational is possible
  --/< all mandatory CNs are Operational
  --/< boot process halted because of an error
   type tNmtBootEvent is 
     (kNmtBootEventBootStep1Finish,
      kNmtBootEventBootStep2Finish,
      kNmtBootEventEnableReadyToOp,
      kNmtBootEventCheckComFinish,
      kNmtBootEventOperational,
      kNmtBootEventError);
   pragma Convention (C, tNmtBootEvent);  -- ./oplk/nmt.h:231

end nmt_h;

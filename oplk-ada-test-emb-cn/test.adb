
with System;
with Test1.Event;
with Oplk.Oplk;
with Oplk.errordefs;
procedure Test 
is
   Arg1 : Oplk.Oplk.TOplkApiEventType;
   Arg2a : aliased Oplk.Oplk.TOplkApiEventArg;
   Arg2 : constant access Oplk.Oplk.TOplkApiEventArg := Arg2a'Access;
   Arg3 : constant System.Address := System.Null_Address;
   Ret : Oplk.errordefs.tOplkError;
begin
   Ret := Test1.Event.Process_Events_Cb (Arg1, Arg2, Arg3);
   null;
end Test;

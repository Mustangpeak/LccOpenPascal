unit lcc_utilities;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  strutils,
  lcc_defines;

function FormatStrToInt(AStr: string): string;
function EqualNodeID(NodeID1: TNodeID; NodeID2: TNodeID;
  IncludeNullNode: boolean): boolean;
function EqualNode(NodeID1: TNodeID; Node1AliasID: word; NodeID2: TNodeID;
  Node2AliasID: word; NodeID_OR_Alias: boolean): boolean;
function EqualEventID(EventID1, EventID2: TEventID): boolean;
procedure NodeIDToEventID(NodeID: TNodeID; LowBytes: word; var EventID: TEventID);
function NullNodeID(ANodeID: TNodeID): boolean;
function EventIDToString(EventID: TEventID; InsertDots: boolean): string;
function NodeIDToString(NodeID: TNodeID; InsertDots: boolean): string;
function NodeAliasToString(AliasID: word): string;
function ValidateIPString(IP4: string): boolean;
function ValidateNodeIDString(NodeIDStr: string): boolean;
function ValidateEventIDString(NodeIDStr: string): boolean;
function ValidatePort(PortStr: string): boolean; overload;
function ValidatePort(Port: integer): boolean; overload;
procedure NodeIDStringToNodeID(ANodeIDStr: string; var ANodeID: TNodeID);
function StrToNodeID(NodeID: string; DotConvention: boolean = False): TNodeID;
function StrToEventID(Event: string): TEventID;
function _Lo(Data: DWORD): byte;
function _Hi(Data: DWORD): byte;
function _Higher(Data: DWORD): byte;
function _Highest(Data: DWORD): byte;
function _Highest1(Data: QWord): byte;
function _Highest2(Data: QWord): byte;
function IsPrintableChar(C: char): boolean;
function IsNumericChar(C: char): boolean;
function AddressSpaceToStr(AddressSpace: byte): string;
function UnknownStrToInt(Str: string): Integer;
function ErrorCodeToStr(Code: Word): string;
function ByteArrayAsHexStr(ADataArray: TLccDynamicByteArray; Dots: Boolean): String;
function ByteArrayAsDecStr(ADataArray: TLccDynamicByteArray; Dots: Boolean): String;
function ByteArrayAsCharStr(ADataArray: TLccDynamicByteArray; Dots: Boolean): String;


function ThreadListCount(AThreadedList: TThreadList): int64;
procedure ThreadListClearObjects(AThreadList: TThreadList);

procedure ListClearObjects(AList: TList);
procedure ListClearNilObjects(AList: TList);

// Delphi stream methods are different than Lazarus
function StreamReadByte(AStream: TStream): byte;
procedure StreamWriteByte(AStream: TStream; AByte: byte);

function ValidateExtendedDccAddress(AddressStr: string; var DccAddress: integer;
  var IsLong: boolean): boolean;

function GridConnectToDetailedGridConnect(MessageString: string): string;

function FormatComPortString(ComPort: string): string;


implementation

uses
  lcc_node_messages;

function ValidateExtendedDccAddress(AddressStr: string; var DccAddress: integer;
  var IsLong: boolean): boolean;
var
  i: integer;
begin
  Result := True;

  DccAddress := 0;
  if AddressStr = '' then
    Result := False
  else
  begin
    if Length(AddressStr) = 1 then
    begin
      if not TryStrToInt(AddressStr, DccAddress) then
        Result := False;
    end
    else
    begin
      for i := 1 to Length(AddressStr) do
      begin
        if i < Length(AddressStr) then
        begin
          if (AddressStr[i] < '0') or (AddressStr[i] > '9') then
            Result := False;
        end
        else
        begin
          if (AddressStr[i] >= '0') and (AddressStr[i] <= '9') then
          begin // all numbers
            if not TryStrToInt(AddressStr, DccAddress) then
              // This should always succeed
              Result := False;
          end
          else
          begin
            if (AddressStr[i] = 'L') or (AddressStr[i] = 'l') or
              (AddressStr[i] = 'S') or (AddressStr[i] = 's') then
            begin
              IsLong := (AddressStr[i] = 'L') or (AddressStr[i] = 'l');
              SetLength(AddressStr, Length(AddressStr) - 1);  // strip it off
              if not TryStrToInt(AddressStr, DccAddress) then
                // This should always succeed
                Result := False;
            end
            else
              Result := False;
          end;
        end;
      end;
      Result := (DccAddress > 0) and (DccAddress <= MAX_DCC_ADDRESS);
    end;
  end;
end;


function IsPrintableChar(C: char): boolean;
begin
  Result := ((Ord(C) >= 32) and (Ord(C) <= 126));
  { or ((Ord( C) >= 128) and (Ord( C) <= 255)) }
end;

function IsNumericChar(C: char): boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;


function FormatStrToInt(AStr: string): string;
begin
  //  {$IFDEF IOS32}
  Result := '0x' + AStr;

  // {$ELSE}
  // Result := '$' + AStr;
  // {$ENDIF}

end;

function ValidateEventIDString(NodeIDStr: string): boolean;
var
  Octet: string;
  Dots, I: integer;
begin
  NodeIDStr := UpperCase(NodeIDStr);

  NodeIDStr := NodeIDStr + '.';
  //add a dot. We use a dot to trigger the Octet check, so need the last one
  Dots := 0;
  Octet := '0';
  {$IFDEF LCC_MOBILE}
  For I := 0 To Length(NodeIDStr) - 1 Do
  {$ELSE}
  for I := 1 to Length(NodeIDStr) do
  {$ENDIF}
  begin
    if CharInSet(NodeIDStr[I], ['0'..'9', 'A'..'F', '.']) then
    begin
      if NodeIDStr[I] = '.' then //found a dot so inc dots and check octet value
      begin
        Inc(Dots);
        if (length(Octet) = 1) or (StrToInt('$' + Octet) > 255) then
          Dots := 9;
        //Either there's no number or it's higher than 255 so push dots out of range to fail below
        Octet := '0'; // Reset to check the next octet
      end
      else// End is a dot   // Else is not a dot so
        Octet := Octet + NodeIDStr[I]; // Add the next character to the octet
    end
    else // End is not a dot   // Else Is not in CheckSet so
      Dots := 9; // Push dots out of range to fail below
  end;
  Result := (Dots = 8);
  // The only way that Dots will equal 8 is if we passed all the tests
end;

function ValidatePort(PortStr: string): boolean;
var
  Port: integer;
begin
  Result := False;
  if TryStrToInt(PortStr, Port) then
    Result := ValidatePort(Port);
end;

function ValidatePort(Port: integer): boolean;
begin
  Result := Port < 65535;
end;

function ValidateNodeIDString(NodeIDStr: string): boolean;
var
  Octet: string;
  Dots, I: integer;
begin
  NodeIDStr := UpperCase(NodeIDStr);
  if (NodeIDStr[1] = '0') and (NodeIDStr[2] = 'X') then
  begin
    for i := 1 to Length(NodeIDStr) - 2 do
      NodeIDStr[i] := NodeIDStr[i + 2];
    SetLength(NodeIDStr, Length(NodeIDStr) - 2);
  end;

  NodeIDStr := NodeIDStr + '.';
  //add a dot. We use a dot to trigger the Octet check, so need the last one
  Dots := 0;
  Octet := '0';
  {$IFDEF LCC_MOBILE}
  For I := 0 To Length(NodeIDStr) - 1 Do
  {$ELSE}
  for I := 1 to Length(NodeIDStr) do
  {$ENDIF}
  begin
    if CharInSet(NodeIDStr[I], ['0'..'9', 'A'..'F', '.']) then
    begin
      if NodeIDStr[I] = '.' then //found a dot so inc dots and check octet value
      begin
        Inc(Dots);
        if (length(Octet) = 1) or (StrToInt('$' + Octet) > 255) then
          Dots := 7;
        //Either there's no number or it's higher than 255 so push dots out of range to fail below
        Octet := '0'; // Reset to check the next octet
      end
      else// End is a dot   // Else is not a dot so
        Octet := Octet + NodeIDStr[I]; // Add the next character to the octet
    end
    else // End is not a dot   // Else Is not in CheckSet so
      Dots := 7; // Push dots out of range to fail below
  end;
  Result := (Dots = 6);
  // The only way that Dots will equal 6 is if we passed all the tests
end;

function ValidateIPString(IP4: string): boolean; // Coding by Dave Sonsalla
var
  Octet: string;
  Dots, I: integer;
begin
  IP4 := IP4 + '.'; //add a dot. We use a dot to trigger the Octet check, so need the last one
  Dots := 0;
  Octet := '0';
  {$IFDEF LCC_MOBILE}
  For I := 0 To Length(IP4) - 1 Do
  {$ELSE}
  for I := 1 to Length(IP4) do
  {$ENDIF}
  begin
    if CharInSet(IP4[I], ['0'..'9', '.']) then
    begin
      if IP4[I] = '.' then //found a dot so inc dots and check octet value
      begin
        Inc(Dots);
        if (length(Octet) = 1) or (StrToInt(Octet) > 255) then Dots := 5;
        //Either there's no number or it's higher than 255 so push dots out of range
        Octet := '0'; // Reset to check the next octet
      end // End of IP4[I] is a dot
      else // Else IP4[I] is not a dot so
        Octet := Octet + IP4[I]; // Add the next character to the octet
    end // End of IP4[I] is not a dot
    else // Else IP4[I] Is not in CheckSet so
      Dots := 5; // Push dots out of range
  end;
  Result := (Dots = 4);
  // The only way that Dots will equal 4 is if we passed all the tests
end;

function EqualNodeID(NodeID1: TNodeID; NodeID2: TNodeID;
  IncludeNullNode: boolean): boolean;
begin
  if IncludeNullNode then
    Result := (NodeID1[0] = NodeID2[0]) and (NodeID1[1] = NodeID2[1])
  else
    Result := not NullNodeID(NodeID1) and not NullNodeID(NodeID2) and
      (NodeID1[0] = NodeID2[0]) and (NodeID1[1] = NodeID2[1]);
end;

function EqualNode(NodeID1: TNodeID; Node1AliasID: word; NodeID2: TNodeID;
  Node2AliasID: word; NodeID_OR_Alias: boolean): boolean;
var
  ValidNodeIDs, ValidAliases: boolean;
begin
  Result := False;

  ValidNodeIDs := not NullNodeID(NodeID1) and not NullNodeID(NodeID2);
  ValidAliases := (Node1AliasID <> 0) and (Node1AliasID <> 0);

  if NodeID_OR_Alias then
  begin
    if ValidAliases then
      Result := (Node1AliasID = Node2AliasID)
    else
    if ValidNodeIDs then
      Result := EqualNodeID(NodeID1, NodeID2, False);
  end
  else
  begin
    if ValidNodeIDs and ValidAliases then
      Result := EqualNodeID(NodeID1, NodeID2, False) and (Node1AliasID = Node2AliasID);
  end;
end;

function EqualEventID(EventID1, EventID2: TEventID): boolean;
begin
  Result := (EventID1[0] = EventID2[0]) and (EventID1[1] = EventID2[1]) and
    (EventID1[2] = EventID2[2]) and (EventID1[3] = EventID2[3]) and
    (EventID1[4] = EventID2[4]) and (EventID1[5] = EventID2[5]) and
    (EventID1[6] = EventID2[6]) and (EventID1[7] = EventID2[7]);
end;

procedure NodeIDToEventID(NodeID: TNodeID; LowBytes: word; var EventID: TEventID);
begin
  EventID[0] := _Higher(NodeID[1]);
  // But these all need the 48 Bit Full ID in the Byte Fields
  EventID[1] := _Hi(NodeID[1]);
  EventID[2] := _Lo(NodeID[1]);
  EventID[3] := _Higher(NodeID[0]);
  EventID[4] := _Hi(NodeID[0]);
  EventID[5] := _Lo(NodeID[0]);
  EventID[6] := _Hi(LowBytes);
  EventID[7] := _Lo(LowBytes);
end;

function NullNodeID(ANodeID: TNodeID): boolean;
begin
  Result := (ANodeID[0] = 0) and (ANodeID[1] = 0);
end;

function EventIDToString(EventID: TEventID; InsertDots: boolean): string;
var
  i: integer;
begin
  Result := '';
  if InsertDots then
  begin
    for i := 0 to LEN_EVENT_MAX - 1 do
    begin
      if i < LEN_EVENT_MAX - 1 then
        Result := Result + IntToHex(EventID[i], 2) + '.'
      else
        Result := Result + IntToHex(EventID[i], 2);
    end;
  end
  else
  begin
    for i := 0 to LEN_EVENT_MAX - 1 do
      Result := Result + IntToHex(EventID[i], 2);
  end;
end;

function NodeIDToString(NodeID: TNodeID; InsertDots: boolean): string;
var
  i: integer;
begin
  Result := '';
  if InsertDots then
  begin
    for i := LEN_NODEID_MAX - 1 downto 0 do
    begin
      if i > 0 then
      begin
        if i < LEN_NODEID_MAX div 2 then
          Result := Result + IntToHex(((NodeID[0] shr (i * 8)) and $0000FF), 2) + '.'
        else
          Result := Result + IntToHex(((NodeID[1] shr ((i - 3) * 8)) and $0000FF), 2) + '.';
      end
      else
      begin
        if i < LEN_NODEID_MAX div 2 then
          Result := Result + IntToHex(((NodeID[0] shr (i * 8)) and $0000FF), 2)
        else
          Result := Result + IntToHex(((NodeID[1] shr ((i - 3) * 8)) and $0000FF), 2);
      end;
    end;
  end
  else
  begin
    Result := IntToHex(NodeID[1], 6);
    Result := Result + IntToHex(NodeID[0], 6);
    Result := '0x' + Result;

   { for i := LEN_NODEID_MAX - 1 downto 0 do
    begin
      if i < LEN_NODEID_MAX div 2 then
        Result := Result + IntToHex(((NodeID[0] shr (i*8)) and $0000FF), 2)
      else
        Result := Result + IntToHex(((NodeID[1] shr ((i-3)*8)) and $0000FF), 2)
    end  }
  end;
end;

function NodeAliasToString(AliasID: word): string;
begin
  Result := '0x' + IntToHex(AliasID, 4);
end;

procedure NodeIDStringToNodeID(ANodeIDStr: string; var ANodeID: TNodeID);
var
  TempStr: string;
  TempNodeID: QWord;
begin
  ANodeIDStr := Trim(string(ANodeIDStr));
  TempStr := StringReplace(string(ANodeIDStr), '0x', '', [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(string(TempStr), '$', '', [rfReplaceAll, rfIgnoreCase]);
  try
    TempNodeID := StrToInt64('$' + string(TempStr));
    ANodeID[0] := DWord(TempNodeID and $0000000000FFFFFF);
    ANodeID[1] := DWord((TempNodeID shr 24) and $0000000000FFFFFF);
  except
    ANodeID[0] := 0;
    ANodeID[1] := 0;
  end;
end;

function StrToEventID(Event: string): TEventID;
var
  TempEvent: string;
  TempChar: char;
  i: integer;
begin
  Event := Trim(Event);
  if Length(Event) = 23 then
  begin
    TempEvent := '';
    {$IFDEF LCC_MOBILE}
    for i := 0 to 22 do
    {$ELSE}
    for i := 1 to 23 do
    {$ENDIF}
    begin
      TempChar := Event[i];
      if TempChar <> '.' then
        TempEvent := TempEvent + TempChar;
    end;
    Event := TempEvent;
  end;

  if Length(Event) = 16 then
  begin
    {$IFDEF LCC_MOBILE}
    Result[0] := StrToInt('0x' + Event[0] + Event[1]);
    Result[1] := StrToInt('0x' + Event[2] + Event[3]);
    Result[2] := StrToInt('0x' + Event[4] + Event[5]);
    Result[3] := StrToInt('0x' + Event[6] + Event[7]);
    Result[4] := StrToInt('0x' + Event[8] + Event[9]);
    Result[5] := StrToInt('0x' + Event[10] + Event[11]);
    Result[6] := StrToInt('0x' + Event[12] + Event[13]);
    Result[7] := StrToInt('0x' + Event[14] + Event[15]);
    {$ELSE}
    Result[0] := StrToInt('0x' + Event[1] + Event[2]);
    Result[1] := StrToInt('0x' + Event[3] + Event[4]);
    Result[2] := StrToInt('0x' + Event[5] + Event[6]);
    Result[3] := StrToInt('0x' + Event[7] + Event[8]);
    Result[4] := StrToInt('0x' + Event[9] + Event[10]);
    Result[5] := StrToInt('0x' + Event[11] + Event[12]);
    Result[6] := StrToInt('0x' + Event[13] + Event[14]);
    Result[7] := StrToInt('0x' + Event[15] + Event[16]);
    {$ENDIF}
  end
  else
  begin
    for i := 0 to LEN_EVENT_MAX - 1 do
      Result[i] := 0;
  end;
end;

function StrToNodeID(NodeID: string; DotConvention: boolean = False): TNodeID;
var
  TempQ: QWord;
  i: integer;
  TempStr: string;
begin
  NodeID := Trim(NodeID);

  if DotConvention then
  begin
    TempStr := '';
    {$IFDEF LCC_MOBILE}
    for i := 0 to Length(NodeID) - 1 do
    {$ELSE}
    for i := 1 to Length(NodeID) do
    {$ENDIF}
    begin
      if NodeID[i] <> '.' then
        TempStr := TempStr + NodeID[i];
    end;

  end
  else
    TempStr := NodeID;

  {$IFDEF LCC_FPC}
  TempQ := StrToQWord(TempStr);
  {$ELSE}
  TempQ := StrToUInt64('$' + TempStr);
  {$ENDIF}
  Result[0] := TempQ and $0000000000FFFFFF;
  Result[1] := (TempQ and $FFFFFFFFFF000000) shr 24;
  // allow the upper nibble to be part of it to catch incorrect node id values
end;

function _Lo(Data: DWORD): byte;
begin
  Result := byte(Data) and $000000FF;
end;

function _Hi(Data: DWORD): byte;
begin
  Result := byte((Data shr 8) and $000000FF);
end;

function _Higher(Data: DWORD): byte;
begin
  Result := byte((Data shr 16) and $000000FF);
end;

function _Highest(Data: DWORD): byte;
begin
  Result := byte((Data shr 24) and $000000FF);
end;

function _Highest1(Data: QWord): byte;
begin
  Result := byte((Data shr 32) and $00000000000000FF);
end;

function _Highest2(Data: QWord): byte;
begin
  Result := byte((Data shr 40) and $00000000000000FF);
end;

function MTI_ToString(MTI: DWord): string;
begin
  case MTI of
    MTI_CAN_CID0: Result := 'Check ID 0';
    MTI_CAN_CID1: Result := 'Check ID 1';
    MTI_CAN_CID2: Result := 'Check ID 2';
    MTI_CAN_CID3: Result := 'Check ID 3';
    MTI_CAN_CID4: Result := 'Check ID 4';
    MTI_CAN_CID5: Result := 'Check ID 5';
    MTI_CAN_CID6: Result := 'Check ID 6';

    MTI_CAN_RID: Result := 'Reserve ID [RID]';
    MTI_CAN_AMD: Result := 'Alias Map Definition [AMD]';
    MTI_CAN_AME: Result := 'Alias Map Enquiry [AME]';
    MTI_CAN_AMR: Result := 'Alias Map Reset [AMR]';

    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY: begin
      Result := 'Datagram Single Frame:';

    end;
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START: begin
      Result := 'Datagram Start Frame:';

    end;
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME: Result := 'Datagram Frame';
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END: Result := 'Datagram End Frame';

    MTI_INITIALIZATION_COMPLETE: Result := 'Initialization Complete';
    MTI_VERIFY_NODE_ID_NUMBER_DEST: Result := 'Verify Node ID with Destination Address';
    MTI_VERIFY_NODE_ID_NUMBER: Result := 'Verify Node ID Global';
    MTI_VERIFIED_NODE_ID_NUMBER: Result := 'Verified Node ID';
    MTI_OPTIONAL_INTERACTION_REJECTED: Result := 'Optional Interaction Rejected';
    MTI_TERMINATE_DUE_TO_ERROR: Result := 'Terminate Due to Error';

    MTI_PROTOCOL_SUPPORT_INQUIRY: Result := 'Protocol Support Inquiry';
    MTI_PROTOCOL_SUPPORT_REPLY: Result := 'Protocol Support Reply';

    MTI_CONSUMER_IDENTIFY: Result := 'Consumer Identify';
    MTI_CONSUMER_IDENTIFY_RANGE: Result := 'Consumer Identify Range';
    MTI_CONSUMER_IDENTIFIED_UNKNOWN: Result := 'Consumer Identified Unknown';
    MTI_CONSUMER_IDENTIFIED_SET: Result := 'Consumer Identified Valid';
    MTI_CONSUMER_IDENTIFIED_CLEAR: Result := 'Consumer Identified Clear';
    MTI_CONSUMER_IDENTIFIED_RESERVED: Result := 'Consumer Identified Reserved';
    MTI_PRODUCER_IDENDIFY: Result := 'Producer Identify';
    MTI_PRODUCER_IDENTIFY_RANGE: Result := 'Producer Identify Range';
    MTI_PRODUCER_IDENTIFIED_UNKNOWN: Result := 'Producer Identified Unknown';
    MTI_PRODUCER_IDENTIFIED_SET: Result := 'Producer Identified Valid';
    MTI_PRODUCER_IDENTIFIED_CLEAR: Result := 'Producer Identified Clear';
    MTI_PRODUCER_IDENTIFIED_RESERVED: Result := 'Producer Identified Reserved';
    MTI_EVENTS_IDENTIFY_DEST: Result :=
        'Events Identify with Destination Address';
    MTI_EVENTS_IDENTIFY: Result := 'Events Identify Global';
    MTI_EVENT_LEARN: Result := 'Event Learn';
    MTI_PC_EVENT_REPORT: Result :=
        'Producer/Consumer Event Report [PCER] ';

    MTI_SIMPLE_NODE_INFO_REQUEST: Result := 'Simple Node Info Request [SNIP]';
    MTI_SIMPLE_NODE_INFO_REPLY: Result := 'Simple Node Info Reply [SNIP]';

    MTI_TRACTION_SIMPLE_TRAIN_INFO_REQUEST: Result :=
        'Simple Train Node Info Request [STNIP]';
    MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY: Result :=
        'Simple Train Node Info Reply [STNIP]';

    MTI_DATAGRAM: Result := 'Datagram';
    MTI_DATAGRAM_OK_REPLY: Result := 'Datagram Reply OK';
    MTI_DATAGRAM_REJECTED_REPLY: Result := 'Datagram Rejected Reply';

    MTI_TRACTION_REQUEST: Result := 'Traction Protocol';
    MTI_TRACTION_REPLY: Result := 'Traction Reply';
    MTI_STREAM_INIT_REQUEST: Result := 'Stream Init Request';
    MTI_STREAM_INIT_REPLY: Result := 'Stream Init Reply';
    MTI_CAN_FRAME_TYPE_CAN_STREAM_SEND: Result := 'Stream Send - CAN Frame';
    MTI_STREAM_PROCEED: Result := 'Stream Proceed';
    MTI_STREAM_COMPLETE: Result := 'Stream Complete';
    else
      Result := 'Unknown MTI';
  end;
end;

function RawHelperDataToStr(Message: TLccMessage; ASCII: boolean): string;
var
  j, iStart: integer;
begin
  Result := '';
  iStart := 0;
  Result := Result + ' [';
  for j := iStart to Message.DataCount - 1 do                     // Skip the Address
  begin
    if ASCII then
    begin
      if IsPrintableChar(Chr(Message.DataArray[j])) then
        Result := Result + Chr(Message.DataArray[j])
      else
        Result := Result + '.';
    end
    else
    begin
      Result := Result + IntToHex(Message.DataArray[j], 2);
      if j < Message.DataCount then
        Result := Result + '.';
    end;
  end;
  Result := Result + ']';
end;

function StreamReadByte(AStream: TStream): byte;
begin
  {$IFDEF LCC_FPC}
    Result := AStream.ReadByte;
  {$ELSE}
  Result := 0;
  AStream.Read(Result, 1);
  {$ENDIF}
end;

procedure StreamWriteByte(AStream: TStream; AByte: byte);
begin
  {$IFDEF LCC_FPC}
    AStream.WriteByte(AByte);
  {$ELSE}
  AStream.Write(AByte, 1);
  {$ENDIF}
end;

function GridConnectToDetailedGridConnect(MessageString: string): string;
var
  j, S_Len: integer;
  //  f: single;
  //  Half: Word;
  Message: TLccMessage;

  //  MultiFrame: TMultiFrameBuffer;
  //  LocalHelper: TLccMessageHelper;
begin
  //  LocalHelper := TLccMessageHelper.Create;

  Message := TLccMessage.Create;
  try
    if Message.LoadByGridConnectStr(MessageString) then
    begin
      Result := MessageString;
      S_Len := Length(Result);
      for j := 0 to (28 - S_Len) do
        Result := Result + ' ';

      if Message.HasDestination then
        Result := Result + '0x' + IntToHex(Message.SourceAlias, 4) +
          ' -> ' + '0x' + IntToHex(Message.DestAlias, 4)
      else
        Result := Result + '0x' + IntToHex(Message.SourceAlias, 4);

      if Message.MTI = MTI_DATAGRAM then
        Result := Result + RawHelperDataToStr(Message, True) + ' MTI: ' +
          MTI_ToString(Message.MTI)
      else
        Result := Result + '   MTI: ' + MTI_ToString(Message.MTI) + ' - ';

      if Message.MTI = MTI_STREAM_SEND then
      begin
        case Message.MTI of
          MTI_STREAM_INIT_REQUEST: Result :=
              Result + ' Suggested Bufer Size: ' + IntToStr(
              (Message.DataArray[2] shl 8) or Message.DataArray[3]) + ' Flags: 0x' +
              IntToHex(Message.DataArray[4], 2) + ' Additional Flags: 0x' +
              IntToHex(Message.DataArray[5], 2) + ' Source Stream ID: ' +
              IntToStr(Message.DataArray[6]);
          MTI_STREAM_INIT_REPLY: Result :=
              Result + ' Negotiated Bufer Size: ' + IntToStr(
              (Message.DataArray[2] shl 8) or Message.DataArray[3]) + ' Flags: 0x' +
              IntToHex(Message.DataArray[4], 2) + ' Additional Flags: 0x' +
              IntToHex(Message.DataArray[5], 2) + ' Source Stream ID: ' +
              IntToStr(Message.DataArray[6]) + ' Destination Stream ID: ' +
              IntToStr(Message.DataArray[7]);
          MTI_STREAM_SEND: begin
          end;
          MTI_STREAM_PROCEED: Result :=
              Result + ' Source Stream ID: ' + IntToStr(Message.DataArray[2]) +
              ' Destination Stream ID: ' + IntToStr(Message.DataArray[3]) +
              ' Flags: 0x' + IntToHex(Message.DataArray[4], 2) + ' Additional Flags: 0x' +
              IntToHex(Message.DataArray[5], 2);
          MTI_STREAM_COMPLETE: Result :=
              Result + ' Source Stream ID: ' + IntToStr(Message.DataArray[2]) +
              ' Destination Stream ID: ' + IntToStr(Message.DataArray[3]) +
              ' Flags: 0x' + IntToHex(Message.DataArray[4], 2) + ' Additional Flags: 0x' +
              IntToHex(Message.DataArray[5], 2);
        end;
      end;

      if Message.MTI = MTI_OPTIONAL_INTERACTION_REJECTED then
      begin
      end;

      // SNII/SNIP
      if Message.MTI = MTI_SIMPLE_NODE_INFO_REPLY then
        Result := Result + RawHelperDataToStr(Message, True);

      // STNIP
      if Message.MTI = MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY then
        Result := Result + RawHelperDataToStr(Message, True);

      // Events
      if (Message.MTI = MTI_PRODUCER_IDENDIFY) or
        (Message.MTI = MTI_PRODUCER_IDENTIFIED_SET) or
        (Message.MTI = MTI_PRODUCER_IDENTIFIED_CLEAR) or
        (Message.MTI = MTI_PRODUCER_IDENTIFIED_UNKNOWN) or
        (Message.MTI = MTI_CONSUMER_IDENTIFY) or (Message.MTI = MTI_CONSUMER_IDENTIFIED_SET) or
        (Message.MTI = MTI_CONSUMER_IDENTIFIED_CLEAR) or
        (Message.MTI = MTI_CONSUMER_IDENTIFIED_UNKNOWN) or
        (Message.MTI = MTI_PC_EVENT_REPORT) then
      begin
        Result := Result + 'EventID: ' +
          EventIDToString(Message.ExtractDataBytesAsEventID(0), False);
      end;

    (*
      // Traction Protocol
      if Message.MTI = MTI_TRACTION_PROTOCOL then
      begin
        MultiFrame := MultiFrames.ProcessFrame(Message);
        if Assigned(MultiFrame) then
        begin
          case MultiFrame.DataArray[0] of
              TRACTION_SET_SPEED_DIR :
                begin
                  Result := Result + ' LCC Speed/Dir Operation; Speed = ';
                  f := HalfToFloat( (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2]);
                  if f = 0 then
                  begin
                    if DWord( f) and $80000000 = $80000000 then
                      Result := Result + '-0.0'
                    else
                      Result := Result + '+0.0'
                  end else
                    Result := Result + IntToStr( round(f));
                end;
              TRACTION_SET_FUNCTION : Result := Result + ' LCC Traction Operation - Function Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 3), 6) + '], Value: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(4, 5), 2) + ']';
              TRACTION_SET_E_STOP : Result := Result + ' LCC Traction Emergency Stop';
              TRACTION_QUERY_SPEED : Result := Result + ' Query Speeds';
              TRACTION_QUERY_FUNCTION : Result := Result + ' Query Function - Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 3), 6) + ']';
              TRACTION_CONTROLLER_CONFIG :
                begin;
                  case MultiFrame.DataArray[1] of
                    TRACTION_CONTROLLER_CONFIG_ASSIGN :
                      begin
                        if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                          Result := Result + ' Controller Config Assign - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                        else
                          Result := Result + ' Controller Config Assign - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                      end;
                    TRACTION_CONTROLLER_CONFIG_RELEASE :
                      begin
                        if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                          Result := Result + ' Controller Config Release - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                        else
                          Result := Result + ' Controller Config Release - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                      end;
                    TRACTION_CONTROLLER_CONFIG_QUERY :
                      begin
                        Result := Result + ' Controller Config Query';
                      end;
                    TRACTION_CONTROLLER_CONFIG_NOTIFY :
                      begin
                        if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                          Result := Result + ' Controller Config Notify - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                        else
                          Result := Result + ' Controller Config Notify - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                      end
                  end
                end;
              TRACTION_CONSIST :
                begin
                  case MultiFrame.DataArray[1] of
                    TRACTION_CONSIST_ATTACH : Result := Result + 'Consist Config Attach';
                    TRACTION_CONSIST_DETACH : Result := Result + 'Consist Config Detach';
                    TRACTION_CONSIST_QUERY : Result := Result + 'Consit Config Query';
                  end
                end;
              TRACTION_MANAGE :
                begin
                  case MultiFrame.DataArray[1] of
                      TRACTION_MANAGE_RESERVE : Result := Result + 'Traction Management Reserve';
                      TRACTION_MANAGE_RELEASE : Result := Result + 'Traction Management Release'
                  end
                end
          else
            Result := Result + 'Unknown Traction Operation';
          end;

          FreeAndNil(MultiFrame);
        end;
      end;

      // Traction Protocol Reply
      if LocalHelper.MTI = MTI_TRACTION_REPLY then
      begin
        MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
        if Assigned(MultiFrame) then
        begin
          case MultiFrame.DataArray[0] of
              TRACTION_QUERY_SPEED :
                begin
                  Result := Result + 'Query Speed Reply : Set Speed = ';
                    Half := (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2];
                    if Half = $FFFF then
                    begin
                      Result := Result + 'NaN'
                    end else
                    begin
                      f := HalfToFloat( Half);
                      if f = 0 then
                      begin
                        if DWord( f) and $80000000 = $80000000 then
                          Result := Result + '-0.0'
                        else
                          Result := Result + '+0.0'
                      end else
                        Result := Result + IntToStr( round(f));
                    end;

                    Result := Result + ': Status = ' + MultiFrame.ExtractDataBytesAsHex(3, 3);

                    Result := Result + ': Commanded Speed = ';
                    Half := (MultiFrame.DataArray[4] shl 8) or MultiFrame.DataArray[5];
                    if Half = $FFFF then
                    begin
                      Result := Result + 'NaN'
                    end else
                    begin
                      f := HalfToFloat( Half);
                      if f = 0 then
                      begin
                        if DWord( f) and $80000000 = $80000000 then
                          Result := Result + '-0.0'
                        else
                          Result := Result + '+0.0'
                      end else
                        Result := Result + IntToStr( round(f));
                    end;

                    Result := Result + ': Actual Speed = ';
                    Half := (MultiFrame.DataArray[6] shl 8) or MultiFrame.DataArray[7];
                    if Half = $FFFF then
                    begin
                      Result := Result + 'NaN'
                    end else
                    begin
                      f := HalfToFloat( Half);
                      if f = 0 then
                      begin
                        if DWord( f) and $80000000 = $80000000 then
                          Result := Result + '-0.0'
                        else
                          Result := Result + '+0.0'
                      end else
                        Result := Result + IntToStr( round(f));
                    end
                end;
              TRACTION_QUERY_FUNCTION : Result := Result + 'Query Function Reply - Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ', Value: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5));
              TRACTION_CONTROLLER_CONFIG :
                begin;
                  case MultiFrame.DataArray[1] of
                    TRACTION_CONTROLLER_CONFIG_ASSIGN :
                      begin
                        Result := Result + 'Controller Config Assign Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2)
                      end;
                    TRACTION_CONTROLLER_CONFIG_QUERY :
                      begin
                        if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                          Result := Result + 'Controller Config Query Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Result = ' + MultiFrame.ExtractDataBytesAsHex(3, 3) + ' Active Controller = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(4, 9), 12) + ' Alias = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(10, 11), 4)
                        else
                          Result := Result + 'Controller Config Query Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Result = ' + MultiFrame.ExtractDataBytesAsHex(3, 3) + ' Active Controller = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(4, 9), 12);
                      end;
                    TRACTION_CONTROLLER_CONFIG_NOTIFY :
                      begin
                        Result := Result + 'Controller Config Notify Reply - Result = ' + MultiFrame.ExtractDataBytesAsHex(2, 2)
                      end;
                  end
                end;
              TRACTION_CONSIST :
                begin
                  case MultiFrame.DataArray[1] of
                    TRACTION_CONSIST_ATTACH : Result := Result + 'Consist Config Attach Reply';
                    TRACTION_CONSIST_DETACH : Result := Result + 'Consist Config Detach Reply';
                    TRACTION_CONSIST_QUERY : Result := Result + 'Consit Config Query Reply';
                  end
                end;
              TRACTION_MANAGE :
                begin
                  case MultiFrame.DataArray[1] of
                      TRACTION_MANAGE_RESERVE : Result := Result +  'Manage: Reserve' + 'Result = ' + MultiFrame.ExtractDataBytesAsHex(2, 2);
                  end
                end
          else
            Result := Result + 'Unknown Traction Reply Operation';
          end;

          FreeAndNil(MultiFrame);
        end;
      end;

      *)
    end;
  finally
    Message.Free
  end;
end;

function FormatComPortString(ComPort: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := ComPort;
  {$ELSE}
    {$IFDEF DARWIN}
    Result := PATH_OSX_DEV + ComPort;
    {$ELSE}
    Result := PATH_LINUX_DEV + ComPort;
    {$ENDIF}
  {$ENDIF}
end;

{$IFNDEF LCC_MOBILE}
function GridConnectToJMRI(GridStr: string): string;
var
  NPos: integer;
  Header: pchar;
  i: integer;
begin
  Result := GridStr;
  NPos := Pos('N', string(GridStr));
  GridStr[NPos] := #0;
  Header := @GridStr[3];
  Result := '[' + Header + ']';
  Header := @GridStr[NPos];
  Inc(Header);
  if Header^ <> ';' then
    Result := Result + ' ';
  while Header^ <> ';' do
  begin
    Result := Result + Header^;
    Inc(Header);
    if Header^ = ';' then
      Break;
    Result := Result + Header^ + ' ';
    Inc(Header);
  end;
  Result := Trim(string(Result));
  for i := 0 to (40 - Length(Result)) do
    Result := Result + ' ';  // Pad them all to the same length
end;
{$ENDIF}

function ThreadListCount(AThreadedList: TThreadList): int64;
var
  L: TList;
begin
  L := AThreadedList.LockList;
  try
    Result := L.Count;
  finally
    AThreadedList.UnlockList;
  end;
end;

procedure ThreadListClearObjects(AThreadList: TThreadList);
var
  L: TList;
  i: integer;
begin
  L := AThreadList.LockList;
  try
    for i := 0 to L.Count - 1 do
      TObject(L[i]).Free;
  finally
    L.Clear;
    AThreadList.UnlockList;
  end;
end;

procedure ListClearObjects(AList: TList);
var
  i: integer;
begin
  try
    for i := 0 to AList.Count - 1 do
      TObject(AList[i]).Free;
  finally
    AList.Clear;
  end;
end;

procedure ListClearNilObjects(AList: TList);
var
  i: integer;
begin
  for i := AList.Count - 1 downto 0 do
    if AList[i] = nil then
      AList.Delete(i);
end;

function AddressSpaceToStr(AddressSpace: byte): string;
begin
  case AddressSpace of

    ADDRESS_SPACE_CONFIG_DEFINITION_INFO:
      Result := 'Configuration Definition Info (CDI) Space';
    ADDRESS_SPACE_ALL:
      Result := 'All Space';
    ADDRESS_SPACE_CONFIG_MEMORY:
      Result := 'Configuration Memory Space';
    ADDRESS_SPACE_ACDI_MFG:
      Result := 'Abbreviated Configuration Definition Info Manufacturer (ACDI) Space';
    ADDRESS_SPACE_ACDI_USER:
      Result := 'Abbreviated Configuration Definition Info User (ACDI) Space';
    ADDRESS_SPACE_FUNCTION_DEFINITION_INFO:
      Result := 'Function Definition Info (FDI) Space';
    ADDRESS_SPACE_FUNCTION_MEMORY:
      Result := 'Function Memory Space';
    else
      Result := '[Unknown]';
  end;
end;

function UnknownStrToInt(Str: string): Integer;
begin
   Str := Trim( StringReplace(Str, '0x', '$', [rfReplaceAll, rfIgnoreCase]));
   if Str[1] = '$' then
     Result := Hex2Dec(Str)
   else
     Result := StrToInt(Str);
end;

function ErrorCodeToStr(Code: Word): string;
begin
  Result := '';

    // Very specific codes
    case Code of

      // Permanent error modifier
                                                                                          //             of the expected range, or do not match the expectations of the receiving node.
    ERROR_CODE_PERMANENT_SUBCOMMAND_UNKNOWN    : Result := 'Permanent Error - Unknown subcommand';     // $1041; // complete code: Not implemented, subcommand is unknown.
    ERROR_CODE_PERMANENT_TYPE_UNKNOWN          : Result := 'Permanent Error - type unknown';           // $1042; // complete code: Not implemented, Datagram-type, Stream-type, or command is unknown.
    ERROR_CODE_PERMANENT_MTI_TRANSPORT_UNKNOWN : Result := 'Permanent Error - MTI transport unknown';  // $1043; // complete code: Not implemented, unknown MTI, or Transport protocol (datagrams/streams) is not supported.

    // Temporary error modifier

    ERROR_CODE_TEMPORARY_TIMEOUT_OF_END_FRAME  : Result := 'Temporary Error - Timeout waiting for end frame';  // $2011;  // complete code: Time-out, waiting for End-frame.
    ERROR_CODE_OUT_OF_ORDER_NO_START_FRAME     : Result := 'Temporary Error - Outof order no start frame';     // $2041;  // complete code: Out of Order, Middle- or End-frame without a Start-frame.
    ERROR_CODE_OUT_OF_ORDER_START_BEFORE_END   : Result := 'Temporary Error - Out of order, new start frame before finshing previous message';   // $2042;  // complete code: Out of Orde

  end;

  if Result <> '' then
    Exit;

  // More general codes that may have custer first nibble
  case Code and $FFF0 of

      // Permanent error modifier
    ERROR_CODE_PERMANENT                       : Result := 'Permanent Error';                          // $1000; // major code: Permanent error.
    ERROR_CODE_PERMANENT_NOT_FOUND             : Result := 'Permanent Error - Not found';
    ERROR_CODE_PERMANENT_SOURCE_NOT_PERMITED   : Result := 'Permanent Error - Source not permited';    // $1020; // major code: Source not permitted.
    ERROR_CODE_PERMANENT_NOT_IMPLEMENTED       : Result := 'Permanent Error - Not implemented';        // $1040; // major code: Not implemented.
    ERROR_CODE_PERMANENT_INVALID_ARGUMENTS     : Result := 'Permanent Error - Invalid arguments';      // $1080; // major code: Invalid arguments. Some of the values sent in the message fall outside
                                                                                                       //             of the expected range, or do not match the expectations of the receiving node.
    // Temporary error modifier
    ERROR_CODE_TEMPORARY                       : Result := 'Temporary Error';                          // $2000; // major code: Temporary error, not further not specified
    ERROR_CODE_TEMPORARY_TIMEOUT               : Result := 'Temporary Error - Timeout';                // $2010; // major code: Timeout, the expected message or message-part did not arrive in time.
    ERROR_CODE_TEMPORARY_BUFFER_UNAVAILABLE    : Result := 'Temporary Error - Buffer unavailable';     // $2020; // major code: Buffer unavailable or destination node busy.
    ERROR_CODE_TEMPORARY_NOT_EXPECTED          : Result := 'Temporary Error - Not expected';           // $2040; // major code: Not expected, Out of order. An inconsistency was found in the message or frame

    ERROR_CODE_TEMPORARY_TRANSFER_ERROR        : Result := 'Temporary Error - Transfer error';         // $2080; // major code: Transfer error. The message or received message was ill-formed, failed checksum, or is

    else
      Result := '[Unknown Error Code]';
  end;
end;

function ByteArrayAsHexStr(ADataArray: TLccDynamicByteArray; Dots: Boolean): String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Length(ADataArray) - 1 do
  begin
    Result := Result + IntToHex(ADataArray[i]);
    if Dots then
    begin
      if i < Length(ADataArray) - 1 then
        Result := Result + '.'
    end;
  end;
end;

function ByteArrayAsDecStr(ADataArray: TLccDynamicByteArray; Dots: Boolean): String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Length(ADataArray) - 1 do
  begin
    Result := Result + IntToStr(ADataArray[i]);
    if Dots then
    begin
      if i < Length(ADataArray) - 1 then
        Result := Result + '.'
    end;
  end;

end;

function ByteArrayAsCharStr(ADataArray: TLccDynamicByteArray; Dots: Boolean): String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Length(ADataArray) - 1 do
  begin
    if (ADataArray[i] > $20) and (ADataArray[i] < $7F) then
      Result := Result + Char(ADataArray[i])
    else
      Result := Result + '.';

    if Dots then
    begin
      if i < Length(ADataArray) - 1 then
        Result := Result + '.'
    end;
  end;

end;


end.

unit lcc_node_messages_can_assembler_disassembler;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF LCC_FPC}
    contnrs,
  {$ELSE}
    System.Generics.Collections,
  {$ENDIF}

  {$IFDEF LCC_WINDOWS}
    {$IFNDEF LCC_FPC}
    System.Types,
    {$ENDIF}
  {$ENDIF}
  lcc_node_messages,
  lcc_defines;

// TLccMessageQueue holds TLccMessages that are being received piece-meal over
// an interface such as CAN where it can't sent entire message arrays and decodes
// TCP message frames into easy and common TLccMessage structures that the application
// can use_  Pass all messages from the wire protocols through this class to receive
// a TLccMessage to use independantly of wire protocol

type

  TIncomingMessageGridConnectReply = (imgcr_False, imgcr_True, imgcr_ErrorToSend, imgcr_UnknownError);

{ TLccGridConnectMessageAssembler }

TLccGridConnectMessageAssembler = class
private
  FInProcessMessageList: TList;
  FOutOfBuffersList: TList;
  FWorkerMessage: TLccMessage;
protected
  property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
public
  property InProcessMessageList: TList read FInProcessMessageList write FInProcessMessageList;
  property OutOfBuffersList: TList read FOutOfBuffersList write FOutOfBuffersList;

  constructor Create;
  destructor Destroy; override;

  procedure Add(ALccMessageList: TList; ALccMessage: TLccMessage);
  procedure Clear(ALccMessageList: TList);
  procedure Remove(ALccMessageList: TList; ALccMessage: TLccMessage);
  function FindByAliasPairsAndMessageType(ALccMessageList: TList; ALccMessage: TLccMessage): TLccMessage;
  procedure FlushMessagesByAlias(ALccMessageList: TList; Alias: Word);
  function IncomingMessageGridConnect(AGridConnectStr: String; ALccMessage: TLccMessage): TIncomingMessageGridConnectReply; overload;
  function IncomingMessageGridConnect(ALccMessage: TLccMessage): TIncomingMessageGridConnectReply; overload;
end;

{ TLccGridConnectMessageDisAssembler }

TLccGridConnectMessageDisAssembler = class
public
  function OutgoingMsgToGridConnect(Msg: TLccMessage): String;
  procedure OutgoingMsgToMsgList(Msg: TLccMessage; MsgList: TStringList);
end;

var
  AllocatedDatagrams: Integer;
  InProcessMessageCount: Integer;
  OutOfBuffersMessageCount: Integer;


implementation

{ TLccGridConnectMessageDisAssembler }

function TLccGridConnectMessageDisAssembler.OutgoingMsgToGridConnect(Msg: TLccMessage): String;
begin
  // Unsure if there is anything special to do here yet
  Result := Msg.ConvertToGridConnectStr(#10);
end;

procedure TLccGridConnectMessageDisAssembler.OutgoingMsgToMsgList(Msg: TLccMessage; MsgList: TStringList);
begin
  if Assigned(MsgList) then
    MsgList.Text := Msg.ConvertToGridConnectStr(#10);
end;

{ TLccGridConnectMessageAssembler }

constructor TLccGridConnectMessageAssembler.Create;
begin
  inherited Create;
  FInProcessMessageList := TList.Create;
  FOutOfBuffersList := TList.Create;
  FWorkerMessage := TLccMessage.Create;
end;

procedure TLccGridConnectMessageAssembler.Remove(ALccMessageList: TList; ALccMessage: TLccMessage);
begin
  if Assigned(ALccMessage) then
  begin
    ALccMessageList.Remove(ALccMessage);
    ALccMessage.Free;
    if ALccMessageList = InProcessMessageList then
      Dec(InProcessMessageCount);
    if ALccMessageList = OutOfBuffersList then
      Dec(OutOfBuffersMessageCount);
  end;
end;

destructor TLccGridConnectMessageAssembler.Destroy;
begin
  Clear(InProcessMessageList);
  FInProcessMessageList.Free;
  Clear(OutOfBuffersList);
  FOutOfBuffersList.Free;
  FWorkerMessage.Free;
  inherited Destroy;
end;

procedure TLccGridConnectMessageAssembler.Add(ALccMessageList: TList; ALccMessage: TLccMessage);
begin
  ALccMessageList.Add(ALccMessage);
  if ALccMessageList = InProcessMessageList then
    Inc(InProcessMessageCount);
  if ALccMessageList = OutOfBuffersList then
    Inc(OutOfBuffersMessageCount);
end;

procedure TLccGridConnectMessageAssembler.Clear(ALccMessageList: TList);
var
  i: Integer;
begin
  try
    for i := 0 to ALccMessageList.Count - 1 do
    begin
      TObject(ALccMessageList[i]).Free;
      if ALccMessageList = InProcessMessageList then
        Dec(InProcessMessageCount);
      if ALccMessageList = OutOfBuffersList then
        Dec(OutOfBuffersMessageCount);
    end;
  finally
    ALccMessageList.Clear;
  end;
end;


function TLccGridConnectMessageAssembler.FindByAliasPairsAndMessageType(
  ALccMessageList: TList; ALccMessage: TLccMessage): TLccMessage;
var
  i: Integer;
  LocalLccMessage: TLccMessage;
begin
  Result := nil;
  for i := 0 to ALccMessageList.Count - 1 do
  begin
    LocalLccMessage := TLccMessage(ALccMessageList[i]);
    if (ALccMessage.SourceAlias = LocalLccMessage.SourceAlias) and (ALccMessage.DestAlias = LocalLccMessage.DestAlias) and (ALccMessage.MTI = LocalLccMessage.MTI) then
    begin
      Result := LocalLccMessage;
      Break
    end;
  end;
end;


procedure TLccGridConnectMessageAssembler.FlushMessagesByAlias(ALccMessageList: TList;
  Alias: Word);
var
  i: Integer;
  LocalLccMessage: TLccMessage;
begin
  for i := ALccMessageList.Count - 1 downto 0  do
  begin
    LocalLccMessage := TLccMessage(ALccMessageList[i]);
    if (LocalLccMessage.SourceAlias = Alias) or (LocalLccMessage.DestAlias = Alias) then
    begin
      if LocalLccMessage.MTI = MTI_DATAGRAM then
        Dec(AllocatedDatagrams);
      ALccMessageList.Delete(i);
      LocalLccMessage.Free
    end;
  end;
end;

function TLccGridConnectMessageAssembler.IncomingMessageGridConnect(AGridConnectStr: String; ALccMessage: TLccMessage): TIncomingMessageGridConnectReply;
begin
  if ALccMessage.LoadByGridConnectStr(AGridConnectStr) then
    Result := IncomingMessageGridConnect(ALccMessage)
  else
    Result := imgcr_UnknownError
end;

function TLccGridConnectMessageAssembler.IncomingMessageGridConnect(ALccMessage: TLccMessage): TIncomingMessageGridConnectReply;
var
  LocalLccMessage: TLccMessage;
  i: Integer;
  ErrorCode: Word;
begin                                                                           // The result of ALccMessage is undefined if false is returned!
  Result := imgcr_False;
  if Assigned(ALccMessage) then
  begin
    if ALccMessage.IsCAN then
    begin  // CAN Only frames
      case ALccMessage.CAN_MTI of
        MTI_CAN_AMR :
          begin
            // If the Alias is being reset flush all messages associated with it
            FlushMessagesByAlias(InProcessMessageList, ALccMessage.SourceAlias);
            FlushMessagesByAlias(OutOfBuffersList, ALccMessage.SourceAlias);
            Result := imgcr_True  // Pass it on
          end;
//        MTI_CAN_AMD :       // 10/16/2023 don't think this should be here.. this message is not a problem and can be called anytime for many reasons
 //         begin
 //           FlushMessagesByAlias(ALccMessage.SourceAlias);
 //           Result := imgcr_True  // Pass it on
 //         end;
        MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY :
          begin
            LocalLccMessage := FindByAliasPairsAndMessageType(InProcessMessageList, ALccMessage);
            if Assigned(LocalLccMessage) then  // A node can not start start to send a multiframe datagram to a node then
            begin
              ALccMessage.LoadDatagramRejected(ALccMessage.DestID, ALccMessage.DestAlias, ALccMessage.SourceID, ALccMessage.SourceAlias, ERROR_CODE_OUT_OF_ORDER_START_BEFORE_END);
              Result := imgcr_ErrorToSend;
              Remove(InProcessMessageList, LocalLccMessage)    // send a single new one in the middle of the previous one out of order.  Throw it away
            end else
            begin
              if AllocatedDatagrams < MAX_ALLOWED_BUFFERS then  // Another node may have the datagram space filled up with a multiframe
              begin                                             // datagram so this single one for another one will have to wait
                ALccMessage.IsCAN := False;                     // All done, converted to a real LCC Datagram Message
                ALccMessage.CAN_MTI := 0;
                Result := imgcr_True
              end else
              begin
                // Reuse the Message to load up the rejection mession and send it back
                ALccMessage.LoadDatagramRejected(ALccMessage.DestID, ALccMessage.DestAlias, ALccMessage.SourceID, ALccMessage.SourceAlias, ERROR_CODE_TEMPORARY_BUFFER_UNAVAILABLE);
                // Don't dispatch this message back to the target node
                Result := imgcr_ErrorToSend
              end;
            end;
          end;
        MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START :
          begin
            LocalLccMessage := FindByAliasPairsAndMessageType(InProcessMessageList, ALccMessage);
            if Assigned(LocalLccMessage) then
            begin
              ALccMessage.LoadDatagramRejected(ALccMessage.DestID, ALccMessage.DestAlias, ALccMessage.SourceID, ALccMessage.SourceAlias, ERROR_CODE_OUT_OF_ORDER_START_BEFORE_END);
              Result := imgcr_ErrorToSend;
              Remove(InProcessMessageList, LocalLccMessage) // Something is wrong, out of order.  Throw it away what was there
            end
            else begin
              LocalLccMessage := ALccMessage.Clone;
              if AllocatedDatagrams < MAX_ALLOWED_BUFFERS then
              begin
                Add(InProcessMessageList, LocalLccMessage);
                Inc(AllocatedDatagrams)
              end else
                Add(OutOfBuffersList, LocalLccMessage) // We wait for the successful final frame before we buffer full messages, for now just throw it away and return false
            end;
          end;
        MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME :
          begin
            LocalLccMessage := FindByAliasPairsAndMessageType(InProcessMessageList, ALccMessage);
            if Assigned(LocalLccMessage) then
              LocalLccMessage.AppendDataArray(ALccMessage)
            else begin   // Python Script does not expect this to occur
              {
              ALccMessage.LoadDatagramRejected(ALccMessage.DestID, ALccMessage.DestAlias, ALccMessage.SourceID, ALccMessage.SourceAlias, ERROR_CODE_OUT_OF_ORDER_NO_START_FRAME);
              Result := imgcr_ErrorToSend;
              }
            end  // We wait for the successful final frame before we send buffer full error messages
          end;
        MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END :
          begin
            LocalLccMessage := FindByAliasPairsAndMessageType(InProcessMessageList, ALccMessage);
            if Assigned(LocalLccMessage) then
            begin
              LocalLccMessage.AppendDataArray(ALccMessage);
              LocalLccMessage.CopyToTarget(ALccMessage);
              Remove(InProcessMessageList, LocalLccMessage);
              ALccMessage.IsCAN := False;  // Is a fully qualified LCC Datagram now
              ALccMessage.CAN_MTI := 0;
              Dec(AllocatedDatagrams);
              Result := imgcr_True
            end else
            begin
              LocalLccMessage := FindByAliasPairsAndMessageType(OutOfBuffersList, ALccMessage);
              if Assigned(LocalLccMessage) then
                ErrorCode := ERROR_CODE_TEMPORARY_BUFFER_UNAVAILABLE
              else
                ErrorCode := ERROR_CODE_OUT_OF_ORDER_NO_START_FRAME;
              Remove(OutOfBuffersList, LocalLccMessage);
              ALccMessage.LoadDatagramRejected(ALccMessage.DestID, ALccMessage.DestAlias, ALccMessage.SourceID, ALccMessage.SourceAlias, ErrorCode);
              Result := imgcr_ErrorToSend
            end;
          end;
        MTI_CAN_FRAME_TYPE_CAN_STREAM_SEND :
          begin

          end
      else
        Result := imgcr_True
      end
    end else
    begin
      if ALccMessage.CAN_FramingBits <> $00 then                                // Is it a Multi Frame Message?
      begin
        case ALccMessage.CAN_FramingBits of                                     // Train SNIP falls under this now
          $10 : begin   // First Frame
                  LocalLccMessage := FindByAliasPairsAndMessageType(InProcessMessageList, ALccMessage);
                  if Assigned(LocalLccMessage) then
                  begin
                    ALccMessage.LoadOptionalInteractionRejected(ALccMessage.DestID, ALccMessage.DestAlias, ALccMessage.SourceID, ALccMessage.SourceAlias, ERROR_CODE_OUT_OF_ORDER_START_BEFORE_END, ALccMessage.MTI);
                    Result := imgcr_ErrorToSend;
                    Remove(InProcessMessageList, LocalLccMessage)                              // Something is wrong, out of order.  Throw it away
                  end else
                  begin
                    LocalLccMessage := TLccMessage.Create;
                    ALccMessage.CopyToTarget(LocalLccMessage);
                    Add(InProcessMessageList, LocalLccMessage);
                  end;
                end;
          $20 : begin   // Last Frame
                  LocalLccMessage := FindByAliasPairsAndMessageType(InProcessMessageList, ALccMessage);
                  if Assigned(LocalLccMessage) then
                  begin
                    LocalLccMessage.AppendDataArray(ALccMessage);
                    LocalLccMessage.CopyToTarget(ALccMessage);
                    Remove(InProcessMessageList, LocalLccMessage);
                    Result := imgcr_True
                  end else
                  begin
                    // Out of order but let the node handle that if needed (Owned Nodes Only)
                    // Don't swap the IDs, need to find the right target node first
                    ALccMessage.LoadOptionalInteractionRejected(ALccMessage.DestID, ALccMessage.DestAlias, ALccMessage.SourceID, ALccMessage.SourceAlias, ERROR_CODE_OUT_OF_ORDER_NO_START_FRAME, ALccMessage.MTI);
                    Result := imgcr_ErrorToSend
                  end;
                end;
          $30 : begin   // Middle Frame
                  LocalLccMessage := FindByAliasPairsAndMessageType(InProcessMessageList, ALccMessage);
                  if Assigned(LocalLccMessage) then
                    LocalLccMessage.AppendDataArray(ALccMessage)
                end;
        end;
      end else                                                                  // Is it a Multi Frame String Message?
      if (ALccMessage.MTI = MTI_SIMPLE_NODE_INFO_REPLY) then
      begin
        LocalLccMessage := FindByAliasPairsAndMessageType(InProcessMessageList, ALccMessage);
        if not Assigned(LocalLccMessage) then
        begin
          LocalLccMessage := TLccMessage.Create;
          ALccMessage.CopyToTarget(LocalLccMessage);
          for i := 0 to LocalLccMessage.DataCount - 1 do
          begin
            if LocalLccMessage.DataArray[i] = Ord(#0) then
              LocalLccMessage.iTag := LocalLccMessage.iTag + 1
          end;
          Add(InProcessMessageList, LocalLccMessage);
        end else
        begin
          if LocalLccMessage.AppendDataArrayAsString(ALccMessage, 6) then
          begin
            LocalLccMessage.CopyToTarget(ALccMessage);
            Remove(InProcessMessageList, LocalLccMessage);
            Result := imgcr_True;
          end
        end
      end else
        Result := imgcr_True;                                        // Single frame just create a message
    end
  end
end;

initialization
  AllocatedDatagrams := 0;
  InProcessMessageCount := 0;
  OutOfBuffersMessageCount := 0;

end.


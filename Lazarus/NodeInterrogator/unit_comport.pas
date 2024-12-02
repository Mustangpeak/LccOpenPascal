unit unit_comport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, lcc_defines, lcc_utilities, lcc_node, lcc_node_manager,
  lcc_node_messages, lcc_node_commandstation, lcc_node_controller,
  lcc_node_train, lcc_comport, lcc_alias_server,
  lcc_node_messages_can_assembler_disassembler, lcc_alias_server_thread,
  LazSynaSer, lcc_threaded_stringlist, lcc_gridconnect, lcc_connection_common;

type

  TLccComPort = class;    // forward;

  TOnComPortReceiveGridConnectString = procedure(Sender: TObject; GridConnectString: ansistring) of object;
  TOnComPortReceiveMessage = procedure(Sender: TObject; AMessage: TLccMessage) of object;
  TOnComPortError = procedure(Sender: TObject; ErrorString: string; ErrorCode: word) of object;
  TOnComPortLogIn = procedure(Sender: TObject) of object;


  { TLccComPortThread }

  TLccComPortThread = class(TThread)
  private
    FConnected: boolean;
    FErrorCode: word;
    FErrorMsg: string;
    FGridConnectAssembler: TLccGridConnectMessageAssembler;
    FGridConnectDisassembler: TLccGridConnectMessageDisAssembler;
    FGridConnectHelper: TGridConnectDecodeStateMachine;
    FWorkerMsg: TLccMessage;
    FIncomingMsg: string;
    FOutgoingGridConnectList: TThreadStringList;
    FOwner: TLccComPort;
    FPort: string;
    FRawData: boolean;
    FSerial: TBlockSerial;
    // Serial object
  protected
    procedure Execute; override;
    procedure ReceiveMessage;  // For Syncronize
    procedure ReceiveGridConnectStr;  // ForSyncronize
    procedure ErrorMessage;    // For Syncronize;
    procedure AssemblerErrorMessageReply; // For Syncronize;
    procedure LogIn; // For Syncronize
    procedure LogOut; // For Syncronize

    property Serial: TBlockSerial read FSerial write FSerial;
    property Owner: TLccComPort read FOwner write FOwner;
    property IncomingMsg: string read FIncomingMsg write FIncomingMsg;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;
    property ErrorCode: word read FErrorCode write FErrorCode;
    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
  public
    property Connected: boolean read FConnected;
    property RawData: boolean read FRawData write FRawData;

    property Port: string read FPort write FPort;
    property OutgoingGridConnectList: TThreadStringList read FOutgoingGridConnectList write FOutgoingGridConnectList;
    property GridConnectHelper: TGridConnectDecodeStateMachine read FGridConnectHelper write FGridConnectHelper;
    property GridConnectDisassembler: TLccGridConnectMessageDisAssembler read FGridConnectDisassembler write FGridConnectDisassembler;
    property GridConnectAssembler: TLccGridConnectMessageAssembler read FGridConnectAssembler write FGridConnectAssembler;

  end;

  TLccComPort = class

  private
    FOnComPortError: TOnComPortError;
    FOnComPortGridConnectString: TOnComPortReceiveGridConnectString;
    FOnComPortLogIn: TOnComPortLogIn;
    FOnComPortLogOut: TOnComPortLogIn;
    FOnComPortMessage: TOnComPortReceiveMessage;
    FOnComPortAssemblerErrorReply: TOnComPortReceiveMessage;
    FThread: TLccComPortThread;
  protected
    procedure DoComPortError(ErrorString: string; ErrorCode: word);
    procedure DoComPortReceive(AMessage: TLccMessage);
    procedure DoComPortReceiveGridConnectStr(AGridConnectString: ansistring);
    procedure DoComPortAssemblerErrorReply(AMessage: TLccMessage);
    procedure DoComPortLogIn;
    procedure DoComPortLogOut;
    function FormatComPortString(AComPort: ansistring): ansistring;
  public

    property Thread: TLccComPortThread read FThread write FThread;
    property OnComPortGridConnectString: TOnComPortReceiveGridConnectString read FOnComPortGridConnectString write FOnComPortGridConnectString;
    property OnComPortMessage: TOnComPortReceiveMessage read FOnComPortMessage write FOnComPortMessage;
    property OnComPortAssemblerErrorReply: TOnComPortReceiveMessage read FOnComPortAssemblerErrorReply write FOnComPortAssemblerErrorReply;
    property OnComPortError: TOnComPortError read FOnComPortError write FOnComPortError;
    property OnComPortLogIn: TOnComPortLogIn read FOnComPortLogIn write FOnComPortLogIn;
    property OnComPortLogOut: TOnComPortLogIn read FOnComPortLogOut write FOnComPortLogOut;

    function Connect(ComPortPath: string): TLccComPortThread;
    procedure Disconnect;

    destructor Destroy; override;

    procedure SendString(GridConnectString: ansistring);

  end;

implementation

{ TLccComPortThread }

procedure TLccComPortThread.Execute;
var
  TxStr, RcvStr: string;
  i: integer;
  GridConnectStrPtr: PGridConnectString;
  TxList: TStringList;
begin

  try
    Serial := TBlockSerial.Create;
    // Create the Serial object in the context of the thread
    Serial.LinuxLock := False;
    Serial.RaiseExcept := False;
    {$IFDEF UNIX}
   Serial.NonBlock := True;
    {$ENDIF}
    Serial.Connect(Port);
    if Serial.LastError <> 0 then
    begin
      ErrorMsg := Serial.LastErrorDesc;
      ErrorCode := Serial.LastError;
      Synchronize(@ErrorMessage);
    end
    else
    begin

      Serial.Config(230400, 8, 'N', 1, False, False);
      if Serial.LastError <> 0 then
      begin
        ErrorMsg := SysErrorMessage(GetLastOSError);
        ErrorCode := GetLastOSError;
        Synchronize(@ErrorMessage);
      end
      else
      begin

        try
          try
            FConnected := True;

            Synchronize(@LogIn);
            while not Terminated do
            begin

              // Get all the strings from the outgoing buffer into a single concatinated string
              TxStr := '';
              TxList := OutgoingGridConnectList.LockList;
              try
                for i := 0 to TxList.Count - 1 do
                  TxStr := TxStr + TxList[i] + #10;
                TxList.Clear;
              finally
                OutgoingGridConnectList.UnlockList;
              end;

              // Outside of the threaded string list (so not to block the main thread sending more messages)
              // dispatch this string to all the connections
              if TxStr <> '' then
              begin
                Serial.SendString(TxStr);
                if Serial.LastError <> 0 then
                begin
                  ErrorMsg := Serial.LastErrorDesc;
                  ErrorCode := Serial.LastError;
                  Synchronize(@ErrorMessage);
                end;
              end;

              RcvStr := Serial.Recvstring(1);
              case Serial.LastError of
                0, ErrTimeout: begin

                end;
                else
                  ErrorMsg := Serial.LastErrorDesc;
                  ErrorCode := Serial.LastError;
                  Synchronize(@ErrorMessage);
              end;

              for i := 1 to Length(RcvStr) do
              begin
                GridConnectStrPtr := nil;

                if GridConnectHelper.GridConnect_DecodeMachine(Ord(RcvStr[i]), GridConnectStrPtr) then
                begin
                  IncomingMsg := GridConnectBufferToString(GridConnectStrPtr^);
                  WorkerMsg.LoadByGridConnectStr(IncomingMsg);

                  Synchronize(@ReceiveGridConnectStr);

                  case GridConnectAssembler.IncomingMessageGridConnect(WorkerMsg) of
                    imgcr_True         : Synchronize(@ReceiveMessage);
                    imgcr_ErrorToSend  : Synchronize(@AssemblerErrorMessageReply);
                    imgcr_False,
                    imgcr_UnknownError : begin end;
                  end;

                  //    if not RawData then
                  //     ConnectionInfo.LccMessage.LoadByGridConnectStr(ConnectionInfo.MessageStr);


                end;
              end;

            end;
          finally

            FConnected := False;
            if Serial.InstanceActive then
              Serial.CloseSocket;
            Serial.Free;

            Synchronize(@LogOut);
          end;
        finally

        end;
      end;
    end;

  finally

  end;

end;

procedure TLccComPortThread.ReceiveMessage;
begin
  if Assigned(Owner) then
    Owner.DoComPortReceive(WorkerMsg);
end;

procedure TLccComPortThread.ReceiveGridConnectStr;
begin
  if Assigned(Owner) then
    Owner.DoComPortReceiveGridConnectStr(IncomingMsg);
end;

procedure TLccComPortThread.ErrorMessage;
begin
  if Assigned(Owner) then
    Owner.DoComPortError(ErrorMsg, ErrorCode);
end;

procedure TLccComPortThread.AssemblerErrorMessageReply;
begin
  if Assigned(Owner) then
    Owner.DoComPortAssemblerErrorReply(WorkerMsg);
end;

procedure TLccComPortThread.LogIn;
begin
  Owner.DoComPortLogIn;
end;

procedure TLccComPortThread.LogOut;
begin
  Owner.DoComPortLogOut;
end;

{ TLccComPort }

procedure TLccComPort.DoComPortError(ErrorString: string; ErrorCode: word);
begin
  if Assigned(OnComPortError) then
    OnComPortError(Self, ErrorString, ErrorCode);
end;

procedure TLccComPort.DoComPortReceive(AMessage: TLccMessage);
begin
  if Assigned(OnComPortMessage) then
    OnComPortMessage(Self, AMessage);
end;

procedure TLccComPort.DoComPortReceiveGridConnectStr(AGridConnectString: ansistring);
begin
  if Assigned(OnComPortGridConnectString) then
    OnComPortGridConnectString(Self, AGridConnectString);
end;

procedure TLccComPort.DoComPortAssemblerErrorReply(AMessage: TLccMessage);
begin
  if Assigned(OnComPortAssemblerErrorReply) then
    OnComPortAssemblerErrorReply(Self, AMessage);
end;

procedure TLccComPort.DoComPortLogIn();
begin
  if Assigned(OnComPortLogIn) then
    OnComPortLogIn(Self);
end;

procedure TLccComPort.DoComPortLogOut;
begin
 if Assigned(OnComPortLogOut) then
    OnComPortLogOut(Self);
end;

function TLccComPort.FormatComPortString(AComPort: ansistring): ansistring;
begin
  {$IFDEF MSWINDOWS}
  Result := AComPort;
  {$ELSE}
    {$IFDEF DARWIN}
    if Pos('dev', AComPort) = 0 then
      Result := PATH_OSX_DEV + AComPort
    else
        Result := AComPort;
    {$ELSE}
    if Pos('dev', AComPort) = 0 then
      Result := PATH_LINUX_DEV + AComPort;
    else
      Result := AComPort;
    {$ENDIF}
  {$ENDIF}
end;


function TLccComPort.Connect(ComPortPath: string): TLccComPortThread;
begin
  Thread := TLccComPortThread.Create(True);
  Thread.FreeOnTerminate := False;
  Thread.GridConnectHelper := TGridConnectDecodeStateMachine.Create;
  Thread.OutgoingGridConnectList := TThreadStringList.Create;
  Thread.GridConnectDisassembler := TLccGridConnectMessageDisAssembler.Create;
  Thread.GridConnectAssembler := TLccGridConnectMessageAssembler.Create;
  Thread.WorkerMsg := TLccMessage.Create;
  Thread.Owner := Self;
  Thread.Port := FormatComPortString(ComPortPath);
  Thread.Suspended := False;
  Result := Thread;
end;

procedure TLccComPort.Disconnect;
begin
  if Assigned(Thread) then
  begin
    Thread.Terminate;
    while Thread.Connected do
    begin
      Application.ProcessMessages;
      Sleep(100);
      // There may be some reentrant thing going on here on shut down
      if not Assigned(Thread) then
        Exit;
    end;

    Thread.GridConnectHelper.Free;
    Thread.OutgoingGridConnectList.Free;
    Thread.GridConnectDisassembler.Free;
    Thread.GridConnectAssembler.Free;
    Thread.WorkerMsg.Free;
    Thread.Free;
    Thread := nil;
  end;
end;

destructor TLccComPort.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TLccComPort.SendString(GridConnectString: ansistring);
var
  List: TStringList;
begin

  if not Assigned(Thread) then
    Exit;

  List := Thread.OutgoingGridConnectList.LockList;
  try
    List.Add(GridConnectString);
  finally
    Thread.OutgoingGridConnectList.UnlockList;
  end;

end;


end.

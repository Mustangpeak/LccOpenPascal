unit form_main;

{$mode objfpc}{$H+}

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

  TOnComPortString = procedure(Sender: TObject; GridConnectString: ansistring) of object;
  TOnComPortError = procedure(Sender: TObject; ErrorString: string;
    ErrorCode: word) of object;



  { TLccComPortThread }

  TLccComPortThread = class(TThread)
  private
    FConnected: boolean;
    FErrorCode: word;
    FErrorMsg: string;
    FGridConnectHelper: TGridConnectDecodeStateMachine;
    FIncomingMsg: string;
    FOutgoingGridConnectList: TThreadStringList;
    FOwner: TLccComPort;
    FPort: string;
    FRawData: boolean;
    FRunning: boolean;
    FSerial: TBlockSerial;
    // Serial object
  protected
    procedure Execute; override;
    procedure ReceiveMessage;  // For Syncronize
    procedure ErrorMessage;    // For Syncronize;

    property Serial: TBlockSerial read FSerial write FSerial;
    property Owner: TLccComPort read FOwner write FOwner;
    property IncomingMsg: string read FIncomingMsg write FIncomingMsg;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;
    property ErrorCode: word read FErrorCode write FErrorCode;
  public
    property Connected: boolean read FConnected;
    property RawData: boolean read FRawData write FRawData;
    property Running: boolean read FRunning;

    property Port: string read FPort write FPort;
    property OutgoingGridConnectList: TThreadStringList
      read FOutgoingGridConnectList write FOutgoingGridConnectList;
    property GridConnectHelper: TGridConnectDecodeStateMachine
      read FGridConnectHelper write FGridConnectHelper;

  end;

  TLccComPort = class

  private
    FOnComPortError: TOnComPortError;
    FOnComPortString: TOnComPortString;
    FThread: TLccComPortThread;
  protected
    property Thread: TLccComPortThread read FThread write FThread;

    procedure DoComPortError(ErrorString: string; ErrorCode: word);
    procedure DoComPortString(AGridConnectString: ansistring);
    function FormatComPortString(AComPort: ansistring): ansistring;
  public

    property OnComPortString: TOnComPortString
      read FOnComPortString write FOnComPortString;
    property OnComPortError: TOnComPortError read FOnComPortError write FOnComPortError;

    function Connect(ComPortPath: string): TLccComPortThread;
    procedure Disconnect;

    destructor Destroy; override;

    procedure SendString(GridConnectString: ansistring);

  end;

  { TForm1 }

  TForm1 = class(TForm)
    ButtonCreateNode: TButton;
    ButtonClearMemo: TButton;
    ButtonRefreshComPort: TButton;
    ButtonComPortConnect: TButton;
    ComboBoxComPorts: TComboBox;
    LabelAliasID: TLabel;
    LabelNodeID: TLabel;
    MemoComPort: TMemo;
    Panel1: TPanel;
    StatusBarMain: TStatusBar;
    procedure ButtonClearMemoClick(Sender: TObject);
    procedure ButtonCreateNodeClick(Sender: TObject);
    procedure ButtonRefreshComPortClick(Sender: TObject);
    procedure ButtonComPortConnectClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FEmulateCANBus: boolean;
    FNodeManager: TLccNodeManager;
    FSerialLink: TLccComPort;

  protected

    procedure OnComPortString(Sender: TObject; GridConnectString: ansistring);
    procedure OnComPortError(Sender: TObject; ErrorString: string; ErrorCode: word);

    procedure OnConnectionFactorySendMessage(Sender: TObject; LccMessage: TLccMessage);

    procedure OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
    // Note there is not defined way for a node to log out of OpenLCB
    procedure OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);


    procedure OnComPortSendMessage(Sender: TObject;
      var GridConnectStyleMessage: string);
  public

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property SerialLink: TLccComPort read FSerialLink write FSerialLink;

    property EmulateCANBus: boolean read FEmulateCANBus write FEmulateCANBus;


  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  MAX_LOGGING_LINES = 200;

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

                  //    if not RawData then
                  //     ConnectionInfo.LccMessage.LoadByGridConnectStr(ConnectionInfo.MessageStr);

                  Synchronize(@ReceiveMessage);
                end;
              end;

            end;
          finally

            FConnected := False;
            if Serial.InstanceActive then
              Serial.CloseSocket;
            Serial.Free;
          end;
        finally

        end;
      end;
    end;

  finally
    // Running := False;
  end;

end;

procedure TLccComPortThread.ReceiveMessage;
begin
  if Assigned(Owner) then
    Owner.DoComPortString(IncomingMsg);
end;

procedure TLccComPortThread.ErrorMessage;
begin
  if Assigned(Owner) then
    Owner.DoComPortError(ErrorMsg, ErrorCode);
end;

{ TLccComPort }

procedure TLccComPort.DoComPortError(ErrorString: string; ErrorCode: word);
begin
  if Assigned(OnComPortError) then
    OnComPortError(Self, ErrorString, ErrorCode);
end;

procedure TLccComPort.DoComPortString(AGridConnectString: ansistring);
begin
  if Assigned(OnComPortString) then
    OnComPortString(Self, AGridConnectString);
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
  Thread.GridConnectHelper := TGridConnectDecodeStateMachine.Create;
  Thread.OutgoingGridConnectList := TThreadStringList.Create;
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
    while Thread.Running do
      Sleep(100);

    Thread.GridConnectHelper.Free;
    Thread.OutgoingGridConnectList.Free;
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

  List := Thread.OutgoingGridConnectList.LockList;
  try
    List.Add(GridConnectString);
  finally
    Thread.OutgoingGridConnectList.UnlockList;
  end;

end;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

  EmulateCANBus := True;

  NodeManager := TLccNodeManager.Create(nil);
  NodeManager.EmulateCanNetworkLogin := EmulateCANBus;
  NodeManager.OnNodeAliasIDChanged := @OnNodeManagerAliasIDChanged;
  NodeManager.OnNodeIDChanged := @OnNodeManagerIDChanged;
  NodeManager.OnNodeLogin := @OnNodeManagerNodeLogin;
  NodeManager.OnAliasRelease := @OnNodeManagerAliasRelease;
  NodeManager.OnNodeDestroy := @OnNodeManagerNodeDestroy;

  SerialLink := TLccComPort.Create;
  SerialLink.OnComPortString := @OnComPortString;
  SerialLink.OnComPortError := @OnComPortError;

  ConnectionFactory.OnLccMessageSend := @OnConnectionFactorySendMessage;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSerialLink);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  SerialLink.Disconnect;
end;

procedure TForm1.ButtonComPortConnectClick(Sender: TObject);
var
  AComPort: string;
begin
  if Assigned(SerialLink.Thread) then
    if SerialLink.Thread.Connected then
    begin
      SerialLink.Disconnect;
      StatusBarMain.Panels[1].Text := 'ComPort Disconnected';
      ButtonComPortConnect.Caption := 'Open ComPort';
      Exit;
    end;

  AComPort := ComboBoxComPorts.Items[ComboBoxComPorts.ItemIndex];
  if Assigned(SerialLink.Connect(AComPort)) then
  begin
    StatusBarMain.Panels[1].Text := 'ComPort: ' + AComPort;
    ButtonComPortConnect.Caption := 'Close ComPort';
  end;

end;

procedure TForm1.ButtonRefreshComPortClick(Sender: TObject);
begin
  ComboBoxComPorts.Items.DelimitedText := GetSerialPortNames;
end;

procedure TForm1.ButtonClearMemoClick(Sender: TObject);
begin
  MemoComPort.Clear;
end;

procedure TForm1.ButtonCreateNodeClick(Sender: TObject);
begin
  NodeManager.AddNode('', True);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ComboBoxComPorts.Items.Delimiter := ';';
  ComboBoxComPorts.Items.DelimitedText :=
    StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  ComboBoxComPorts.ItemIndex := 0;
end;

procedure TForm1.OnComPortString(Sender: TObject; GridConnectString: ansistring);
begin

  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Add('R: ' + GridConnectString)

  finally
    if MemoComPort.Lines.Count > MAX_LOGGING_LINES then
      MemoComPort.Lines.Delete(0);
    MemoComPort.Lines.EndUpdate;

    MemoComPort.SelStart := Length(MemoComPort.Lines.Text) - 1;
    MemoComPort.SelLength := 1;
    MemoComPort.SelLength := 0;
  end;

end;

procedure TForm1.OnComPortError(Sender: TObject; ErrorString: string; ErrorCode: word);
begin

end;

procedure TForm1.OnConnectionFactorySendMessage(Sender: TObject; LccMessage: TLccMessage);
begin

  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Add('S: ' + LccMessage.ConvertToGridConnectStr(''));

  finally
    if MemoComPort.Lines.Count > MAX_LOGGING_LINES then
      MemoComPort.Lines.Delete(0);
    MemoComPort.Lines.EndUpdate;

    MemoComPort.SelStart := Length(MemoComPort.Lines.Text) - 1;
    MemoComPort.SelLength := 1;
    MemoComPort.SelLength := 0;
  end;
end;

procedure TForm1.OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
    LabelAliasID.Caption := LccSourceNode.AliasIDStr;
end;

procedure TForm1.OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin

    LabelNodeID.Caption := LccSourceNode.NodeIDStr[True];
end;

procedure TForm1.OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
begin

end;

procedure TForm1.OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
begin

end;

procedure TForm1.OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);
begin

end;

procedure TForm1.OnComPortSendMessage(Sender: TObject;
  var GridConnectStyleMessage: string);
begin

end;

end.

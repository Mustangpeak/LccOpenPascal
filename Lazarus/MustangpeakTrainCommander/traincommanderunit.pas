unit TrainCommanderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons,
  lcc_ethernet_server,
  lcc_defines,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_node_commandstation,
  lcc_node_controller,
  lcc_node_train,
  lcc_comport,
  LazSynaSer,
 // lcc_ethernet_http,
  lcc_common_classes,
  lcc_ethernet_common,
  servervisualunit,
  lcc_train_server,
  lcc_utilities;
 // lcc_syn_ethenet_server;
  //lcc_syn_ethenet_client;


const
  EMULATE_CAN_BUS = True;

type

  { TFormTrainCommander }

  TFormTrainCommander = class(TForm)
    Button1: TButton;
    ButtonCreateConsist: TButton;
    ButtonHTTPServer: TButton;
    ButtonWebserverConnect: TButton;
    ButtonManualConnectComPort: TButton;
    ButtonTrainsClear: TButton;
    ButtonClear: TButton;
    ButtonEthernetConnect: TButton;
    CheckBox1: TCheckBox;
    CheckBoxDetailedLog: TCheckBox;
    CheckBoxLogMessages: TCheckBox;
    CheckBoxLoopBackIP: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    ComboBoxComPorts: TComboBox;
    Edit1: TEdit;
    ImageListMain: TImageList;
    LabelNodeID: TLabel;
    LabelAliasID: TLabel;
    LabelAliasIDCaption: TLabel;
    LabelNodeIDCaption: TLabel;
    ListviewConnections: TListView;
    MemoComPort: TMemo;
    MemoLog: TMemo;
    PanelTrainsHeader: TPanel;
    PanelTrains: TPanel;
    PanelConnections: TPanel;
    PanelDetails: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SplitterConnections1: TSplitter;
    SplitterTrains: TSplitter;
    SplitterConnections: TSplitter;
    StatusBarMain: TStatusBar;
    ToggleBoxServerForm: TToggleBox;
    TreeViewTrains: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure ButtonCreateConsistClick(Sender: TObject);
    procedure ButtonHTTPServerClick(Sender: TObject);
    procedure ButtonWebserverConnectClick(Sender: TObject);
    procedure ButtonManualConnectComPortClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonEthernetConnectClick(Sender: TObject);
    procedure ButtonTrainsReleaseAliasClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure ToggleBoxServerFormChange(Sender: TObject);
  private
    FAutoCreateTrainAddress: Word;
    FCommandStationNode: TLccCommandStationNode;
    FComPort: TLccComPort;
 //   FLccHTTPServer: TLccHTTPServer;
    FLccWebsocketServer: TLccWebSocketServerThreadManager;
    FNodeManager: TLccNodeManager;
    FWorkerMessage: TLccMessage;
    FLccServer: TLccEthernetServerThreadManager;
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    // Events occuring on the connections
    procedure OnServerManagerReceiveMessage(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; ALccMessage: TLccMessage);
    procedure OnServerManagerSendMessage(Sender: TObject; ALccMessage: TLccMessage);
    procedure OnServerManagerConnectionState(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; Info: TLccConnectionInfo);
    procedure OnServerErrorMessage(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; Info: TLccConnectionInfo);

    // Callbacks from the ComPort
    procedure OnComPortReceiveMessage(Sender: TObject; Info: TLccConnectionInfo);

    // Callbacks from the Node Manager
    procedure OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);  // Note there is not defined way for a node to log out of OpenLCB
    procedure OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);

    procedure OnNodeTractionListenerAttached(Sender: TObject; LccNode: TLccNode; AMessage: TLccMessage);
    procedure OnNodeTractionListenerDetached(Sender: TObject; LccNode: TLccNode; AMessage: TLccMessage);
    procedure OnNodeTractionListenerQuery(Sender: TObject; LccNode: TLccNode; AMessage: TLccMessage; var DoDefault: Boolean);

    // Other
    procedure OnTractionRegisterNotify(TractionObject: TLccTractionObject; IsRegistered: Boolean);
    procedure OnTractionNotify(TractionObject: TLccTractionObject);

    function TrainNodeToCaption(ATrainNode: TLccTrainDccNode): string;
    function FindSingleLevelNodeWithData(ParentNode: TTreeNode; const NodeData: Pointer): TTreeNode;
    procedure RebuildTrainTreeview;
    procedure ReleaseAliasOnTrains;
    procedure NodeIsGoingAway(LccSourceNode: TLccNode);

  public
    property LccServer: TLccEthernetServerThreadManager read FLccServer write FLccServer;
    property LccWebsocketServer: TLccWebSocketServerThreadManager read FLccWebsocketServer write FLccWebsocketServer;
  //  property LccHTTPServer: TLccHTTPServer read FLccHTTPServer write FLccHTTPServer;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property LccComPort: TLccComPort read FComPort write FComPort;

    property CommandStationNode: TLccCommandStationNode read FCommandStationNode write FCommandStationNode;

    property AutoCreateTrainAddress: Word read FAutoCreateTrainAddress write FAutoCreateTrainAddress;
  end;

var
  FormTrainCommander: TFormTrainCommander;

implementation

{$R *.lfm}

{ TFormTrainCommander }

procedure TFormTrainCommander.ButtonHTTPServerClick(Sender: TObject);
begin
 // if LccHTTPServer.ListenerConnected then
 //   LccHTTPServer.CloseConnection
 // else
 //   LccHTTPServer.OpenConnection;
end;

procedure TFormTrainCommander.Button1Click(Sender: TObject);
var
  i: Integer;
  Train: TLccTrainDccNode;
begin
  for i := 0 to StrToInt(Edit1.Text) - 1 do
  begin
    Train := CommandStationNode.AddTrain('', AutoCreateTrainAddress, True, ldssDefault);
    Inc(FAutoCreateTrainAddress);
    Train.Login(NULL_NODE_ID);
  end;
end;

procedure TFormTrainCommander.ButtonCreateConsistClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeViewTrains.SelectionCount - 1 do
  begin

  end;
end;

procedure TFormTrainCommander.ButtonWebserverConnectClick(Sender: TObject);
begin
  if LccWebsocketServer.Connected then
    LccWebsocketServer.CloseConnection
  else
    LccWebsocketServer.OpenConnection;
end;

procedure TFormTrainCommander.ButtonManualConnectComPortClick(Sender: TObject);
begin
  if LccComPort.Connected then
    LccComPort.CloseConnection
  else
    LccComPort.OpenConnection;
end;

procedure TFormTrainCommander.ButtonClearClick(Sender: TObject);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Clear;
  finally
    MemoLog.Lines.EndUpdate;
  end;

  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Clear;
  finally
    MemoComPort.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.ButtonEthernetConnectClick(Sender: TObject);
begin
  if LccServer.Connected then
    LccServer.CloseConnection
  else
    LccServer.OpenConnection;
end;

procedure TFormTrainCommander.ButtonTrainsReleaseAliasClick(Sender: TObject);
begin
  ReleaseAliasOnTrains
end;

procedure TFormTrainCommander.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    Max_Allowed_Buffers := 1;
  end else
  begin
    Max_Allowed_Buffers := 2048;
  end;
end;

procedure TFormTrainCommander.OnServerManagerReceiveMessage(Sender: TObject;
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  ALccMessage: TLccMessage);
var
  ByteArray: TLccDynamicByteArray;
  Preamble: String;
begin
  if CheckBoxLogMessages.Checked then
  begin
    if Manager = LccServer then
      Preamble := 'TCP:R: '
    else
    if Manager = LccWebsocketServer then
      Preamble := 'WebSocket:R: '
  {  else
    if Manager = LccHTTPServer then
      Preamble := 'HTTP:R: '   }
    else
    if Manager = LccComPort then      // Should never hit as the Comport is SendMessage Only
      Preamble := 'ComPort:R: ';

    MemoLog.Lines.BeginUpdate;
    try
      if EMULATE_CAN_BUS then
      begin
        if CheckBoxDetailedLog.Checked then
          MemoLog.Lines.Add(Preamble + MessageToDetailedMessage(ALccMessage))
        else
          MemoLog.Lines.Add(Preamble + ALccMessage.ConvertToGridConnectStr('', False));
      end else
      begin
        ByteArray := nil;
        ALccMessage.ConvertToLccTcp(ByteArray);
        MemoLog.Lines.Add(Preamble + ALccMessage.ConvertToLccTcpString(ByteArray));
      end;

      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainCommander.OnServerManagerSendMessage(Sender: TObject; ALccMessage: TLccMessage);
var
  ByteArray: TLccDynamicByteArray;
  Preamble: String;
begin

  if CheckBoxLogMessages.Checked then
  begin
  {  if Manager = LccServer then
      Preamble := 'TCP:S: '
    else
    if Manager = LccWebsocketServer then
      Preamble := 'WebSocket:S: '
    else
    if Manager = LccHTTPServer then
      Preamble := 'HTTP:S: '
    else
    if Manager = LccComPort then      // Should never hit as the Comport is SendMessage Only
    begin
      Preamble := 'ComPort:S: ';

      LccComPort.SendMessageRawGridConnect(GridConnectStyleMessage);

      MemoComPort.Lines.BeginUpdate;
      try
        MemoComPort.Lines.Add(Preamble + GridConnectStyleMessage);
        MemoComPort.SelStart := Length(MemoComPort.Lines.Text);
      finally
        MemoComPort.Lines.EndUpdate;
      end;
      Exit;
    end;
   }

    MemoLog.Lines.BeginUpdate;
    try
      if EMULATE_CAN_BUS then
      begin
        if CheckBoxDetailedLog.Checked then
          MemoLog.Lines.Add('TCP: S: ' + MessageToDetailedMessage(ALccMessage))
        else
          MemoLog.Lines.Add('TCP: S: ' + ALccMessage.ConvertToGridConnectStr('', False));
      end else
      begin
        ByteArray := nil;
        ALccMessage.ConvertToLccTcp(ByteArray);
        MemoLog.Lines.Add('TCP: S: ' + ALccMessage.ConvertToLccTcpString(ByteArray));
      end;
      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainCommander.OnServerManagerConnectionState(Sender: TObject;
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  Info: TLccConnectionInfo);
var
  ListItem: TListItem;
begin
  if Manager = LccServer then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
          ButtonEthernetConnect.Enabled := False;
          StatusBarMain.Panels[0].Text := 'Connecting to Ethernet';
        end;
      lcsConnected :
        begin
          ButtonEthernetConnect.Enabled := True;
          ButtonEthernetConnect.Caption := 'Disconnect Ethernet';
          StatusBarMain.Panels[0].Text := 'Ethernet: Command Station Connected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort);
          if NodeManager.Nodes.Count = 0 then
            CommandStationNode := NodeManager.AddNodeByClass('', TLccCommandStationNode, True, NULL_NODE_ID) as TLccCommandStationNode;
          if Assigned(CommandStationNode) then
          begin
            CommandStationNode.TractionServer.OnSpeedChange := @OnTractionNotify;
            CommandStationNode.TractionServer.OnEmergencyStopChange := @OnTractionNotify;
            CommandStationNode.TractionServer.OnFunctionChange := @OnTractionNotify;
            CommandStationNode.TractionServer.OnSNIPChange := @OnTractionNotify;
            CommandStationNode.TractionServer.OnTrainSNIPChange := @OnTractionNotify;
            CommandStationNode.TractionServer.OnRegisterChange := @OnTractionRegisterNotify;
          end;
        end;
      lcsDisconnecting :
        begin
          ButtonEthernetConnect.Enabled := False;
          ButtonEthernetConnect.Caption := 'Command Station Disconnecting from Ethernet';
          // We are not clearing the nodes, only disconnecting the connection.  There is no such thing
          // as taking a node offline in OpenLCB, only reallocating an Alias if a duplicate is found (handled
          // automatically in the library) or totally off line if the NodeID is found to be a duplicate
        end;
      lcsDisconnected :
        begin
          ButtonEthernetConnect.Enabled := True;
          ButtonEthernetConnect.Caption := 'Connect Ethernet';
          StatusBarMain.Panels[0].Text := 'Command Station Disconnected from Ethernet';
        end;
      lcsClientConnected :
        begin
          ListItem := ListviewConnections.Items.Add;
          Listitem.Caption := 'Throttle connected via Ethernet: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
          ListItem.ImageIndex := 3;
        end;
      lcsClientDisconnected :
        begin
          ListItem := ListviewConnections.FindCaption(0, 'Throttle connected via Ethernet: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort), True, True, True, True);
          if Assigned(ListItem) then
            ListviewConnections.Items.Delete(ListItem.Index);
        end;
    end;
  end else
  if Manager = LccWebsocketServer then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
        end;
      lcsConnected :
        begin
          ButtonWebserverConnect.Enabled := True;
          ButtonWebserverConnect.Caption := 'Disconnect Webserver';
          StatusBarMain.Panels[1].Text := 'Ethernet: WebSocket Connected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort);
        end;
      lcsDisconnecting :
        begin
        end;
      lcsDisconnected :
        begin
          ButtonWebserverConnect.Enabled := True;
          ButtonWebserverConnect.Caption := 'Connect Webserver';
          StatusBarMain.Panels[1].Text := 'WebSocket Disconnected';
        end;
      lcsClientConnected :
        begin
          ListItem := ListviewConnections.Items.Add;
          Listitem.Caption := 'WebSocket Client Connected: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
          ListItem.ImageIndex := 3;
        end;
      lcsClientDisconnected :
        begin
          ListItem := ListviewConnections.FindCaption(0, 'WebSocket Client Connected: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort), True, True, True, True);
          if Assigned(ListItem) then
            ListviewConnections.Items.Delete(ListItem.Index);
        end;
    end;
  end else
{  if Manager = LccHTTPServer then
  begin
    if Sender is TLccEthernetServerThread then
    begin
      case Info.ConnectionState of
        lcsConnecting :
          begin
            StatusBarMain.Panels[2].Text := 'HTTP Server Connecting';
            ButtonHTTPServer.Enabled := False;
          end;
        lcsConnected :
          begin
            StatusBarMain.Panels[2].Text := 'HTTP Server Connected: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort);
            ButtonHTTPServer.Caption := 'HTTP Disconnect';
            ButtonHTTPServer.Enabled := True;
          end;
        lcsDisconnecting :
          begin
            ButtonHTTPServer.Caption := 'Disconnecting from HTTP Server';
            ButtonHTTPServer.Enabled := False;
          end;
        lcsDisconnected :
          begin
            StatusBarMain.Panels[2].Text := 'HTTP Server Disconnected';
            ButtonHTTPServer.Caption := 'HTTP Connect';
            ButtonHTTPServer.Enabled := True;
          end;
      end;

    end else
    if Sender is TLccConnectionThread then
    begin
      case Info.ConnectionState of
        lcsConnecting :
          begin
          end;
        lcsConnected :
          begin
            ListItem := ListviewConnections.Items.Add;
            Listitem.Caption := 'Device connected HTTP: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
            ListItem.ImageIndex := 3;
          end;
        lcsDisconnecting :
          begin
          end;
        lcsDisconnected :
          begin
            ListItem := ListviewConnections.FindCaption(1, 'Device connected HTTP: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort), True, True, True, True);
            if Assigned(ListItem) then
              ListviewConnections.Items.Delete(ListItem.Index);
          end;
      end;
  end else   }
  if Manager = LccComPort then
  begin
    case (Info as TLccComPortConnectionInfo).ConnectionState of
      lcsConnecting :    StatusBarMain.Panels[3].Text := 'ComPort Connecting';
      lcsConnected :
        begin
          StatusBarMain.Panels[3].Text := 'ComPort: ' + (Info as TLccComPortConnectionInfo).ComPort;
          ButtonManualConnectComPort.Caption := 'Close ComPort';
        end;
      lcsDisConnecting : StatusBarMain.Panels[3].Text := 'ComPort Disconnectiong';
      lcsDisconnected :
        begin
          ButtonManualConnectComPort.Caption := 'Open ComPort';
          StatusBarMain.Panels[3].Text := 'ComPort Disconnected';
        end;
    end;
  end;
end;

procedure TFormTrainCommander.OnServerErrorMessage(Sender: TObject;
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  Info: TLccConnectionInfo);
begin
  if Manager = LccServer then
  begin
    ShowMessage('Server: ' + Info.MessageStr);
  end else
  if Manager = LccWebsocketServer then
  begin
     ShowMessage('WebSocket Server: ' + Info.MessageStr);
  end else
{  if Manager = LccHTTPServer then
  begin
    ShowMessage('HTTP Server: ' + Info.MessageStr);
  end else  }
  if Manager = LccComPort then
  begin
    ShowMessage('ComPort: ' + Info.MessageStr);
  end;
end;

procedure TFormTrainCommander.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose; // Keep Hints quiet
  LccComPort.CloseConnection;
  LccServer.CloseConnection;
  LccWebsocketServer.CloseConnection;
//  LccHTTPServer.CloseConnection;
end;

procedure TFormTrainCommander.FormCreate(Sender: TObject);
var
  ConnectionInfo: TLccConnectionInfo;
begin

  NodeManager := TLccNodeManager.Create(nil);
  NodeManager.EmulateCanNetworkLogin := True;
  NodeManager.OnNodeAliasIDChanged := @OnNodeManagerAliasIDChanged;
  NodeManager.OnNodeIDChanged := @OnNodeManagerIDChanged;
  NodeManager.OnNodeLogin := @OnNodeManagerNodeLogin;
  NodeManager.OnNodeTractionListenerAttached := @OnNodeTractionListenerAttached;
  NodeManager.OnNodeTractionListenerDetached := @OnNodeTractionListenerDetached;
  NodeManager.OnNodeTractionListenerQuery := @OnNodeTractionListenerQuery;
  NodeManager.OnAliasRelease := @OnNodeManagerAliasRelease;
  NodeManager.OnNodeDestroy := @OnNodeManagerNodeDestroy;

  ConnectionInfo := TLccEthernetConnectionInfo.Create;
  (ConnectionInfo as TLccEthernetConnectionInfo).AutoResolveIP := not CheckBoxLoopBackIP.Checked;
  (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := '127.0.0.1';
  (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := 12021;
  LccServer := ConnectionFactory.CreateLccMessageConnection(TLccEthernetServerThreadManager, ConnectionInfo, EMULATE_CAN_BUS) as TLccEthernetServerThreadManager;


  ConnectionInfo := TLccEthernetConnectionInfo.Create;
  (ConnectionInfo as TLccEthernetConnectionInfo).AutoResolveIP := not CheckBoxLoopBackIP.Checked;
  (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := '127.0.0.1';
  (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := 12022;
  LccWebsocketServer := ConnectionFactory.CreateLccMessageConnection(TLccWebSocketServerThreadManager, ConnectionInfo, EMULATE_CAN_BUS) as TLccWebSocketServerThreadManager;

 { ConnectionInfo := TLccEthernetConnectionInfo.Create;
  (ConnectionInfo as TLccEthernetConnectionInfo).AutoResolveIP := not CheckBoxLoopBackIP.Checked;
  (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := '127.0.0.1';
  (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := 12020;
  LccHTTPServer := ConnectionFactory.CreateConnection(TLccHTTPServer, ConnectionInfo, EMULATE_CAN_BUS) as TLccHTTPServer;
 }

 // Comports not initialized or selected yet... may be an issue doing it this way.

   ConnectionInfo := TLccComPortConnectionInfo.Create;
 /// (ConnectionInfo as TLccComPortConnectionInfo).ComPort := ComboBoxComPorts.Items[ComboBoxComPorts.ItemIndex];
  (ConnectionInfo as TLccComPortConnectionInfo).Baud := 9600;
  (ConnectionInfo as TLccComPortConnectionInfo).StopBits := 8;
  (ConnectionInfo as TLccComPortConnectionInfo).Parity := 'N';
  LccComPort := ConnectionFactory.CreateConnection(TLccComPort, ConnectionInfo) as TLccComPort;
  LccComPort.RawData := True;


  ConnectionFactory.OnStateChange := @OnServerManagerConnectionState;
  ConnectionFactory.OnError := @OnServerErrorMessage;
  ConnectionFactory.OnLccMessageReceive := @OnServerManagerReceiveMessage;
  ConnectionFactory.OnLccMessageSend := @OnServerManagerSendMessage;


  Max_Allowed_Buffers := 1;
  AutoCreateTrainAddress := 1;

  FWorkerMessage := TLccMessage.Create;
end;

procedure TFormTrainCommander.FormDestroy(Sender: TObject);
begin
  NodeManager.ReleaseAliasAll;
  // Factory destroys all connections

//  FreeAndNil(FComPort);
  FreeAndNil(FNodeManager);   // after the servers are destroyed
  FreeAndNil(FWorkerMessage);
end;

procedure TFormTrainCommander.FormShow(Sender: TObject);
begin
  ComboBoxComPorts.Items.Delimiter := ';';
  ComboBoxComPorts.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  ComboBoxComPorts.ItemIndex := 0;
end;

procedure TFormTrainCommander.SpeedButton1Click(Sender: TObject);
var
  AValue, Size, Result_: Integer;
  Hex, Temp: string;
  CharPtr: PChar;
  B: Byte;
  StartIndex, i: Integer;
  MemoryStream: TMemoryStream;
begin
  AValue := 567;
  Size := 2;


  MemoryStream := TMemoryStream.Create;


  MemoryStream.Clear;

  Hex := IntToHex(AValue, Size * 2);

  {$IFDEF LCC_MOBILE}
  StartIndex := 0;
  {$ELSE}
  StartIndex := 1;
  {$ENDIF}
  CharPtr := @Hex[StartIndex];

  for i := 0 to Size - 1 do
  begin
    Temp := '';
    Temp := CharPtr^;
    Inc(CharPtr);
    Temp := Temp + CharPtr^;
    Inc(CharPtr);
    B := StrToInt('$' + Temp);
    MemoryStream.Write(B, SizeOf(B));
  end;

  // Reset for use
  MemoryStream.Position := 0;


  // Undo
  MemoryStream.Position := 0;

  Temp := '';
  for i := 0 to MemoryStream.Size - 1 do
  begin
    B := 0;
    MemoryStream.Read(B, SizeOf(B));
    Hex := IntToHex(B, 2);
    Temp := Temp + Hex;
  end;

  if not TryStrToInt('$' + Temp, Result_) then
    Result_ := 0;

  // Reset for use
  MemoryStream.Position := 0;


  MemoryStream.Free;
end;

procedure TFormTrainCommander.SpeedButton2Click(Sender: TObject);
var
  EventIDStr, Temp, Hex: AnsiString;
  StartIndex, i: Integer;
  CharPtr: PAnsiChar;
  B: Byte;
  MemoryStream: TMemoryStream;
  AValue: TEventID;
begin
  MemoryStream := TMemoryStream.Create;

  AValue := EVENT_IS_TRAIN;

  MemoryStream.Position := 0;

  EventIDStr := EventIDToString(AValue, False);

  {$IFDEF LCC_MOBILE}
  StartIndex := 0;
  {$ELSE}
  StartIndex := 1;
  {$ENDIF}
  CharPtr := @EventIDStr[StartIndex];

  // Implicit 8 Bytes
  for i := 0 to 7 do
  begin
    Temp := '';
    Temp := CharPtr^;
    Inc(CharPtr);
    Temp := Temp + CharPtr^;
    Inc(CharPtr);
    B := StrToInt('$' + Temp);
    MemoryStream.Write(B, SizeOf(B));
  end;

  // Reset for use
  MemoryStream.Position := 0;


  // Read it back
  MemoryStream.Position := 0;

  Temp := '';
  for i := 0 to MemoryStream.Size - 1 do
  begin
    B := 0;
    MemoryStream.Read(B, SizeOf(B));
    Hex := IntToHex(B, 2);
    Temp := Temp + Hex;
  end;

  // Reset for use
  MemoryStream.Position := 0;

  MemoryStream.Free;
end;

procedure TFormTrainCommander.SpeedButton3Click(Sender: TObject);
begin
  SpeedButton3.Caption := 'Datagram: ' + IntToStr(CommandStationNode.DatagramResendQueue.Count);
end;

procedure TFormTrainCommander.OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode is TLccCommandStationNode then
    LabelAliasID.Caption := LccSourceNode.AliasIDStr;
end;

procedure TFormTrainCommander.OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode is TLccCommandStationNode then
    LabelNodeID.Caption := LccSourceNode.NodeIDStr[True];
end;

procedure TFormTrainCommander.OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
var
  TrainNode: TLccTrainDccNode;
  TreeNode: TTreeNode;
begin
  if LccSourceNode is TLccTrainDccNode then
  begin
    TrainNode := LccSourceNode as TLccTrainDccNode;

 //   TrainNode.OnSendMessageComPort := @OnComPortSendMessage;

    TreeNode := TreeViewTrains.Items.Add(nil, TrainNodeToCaption(TrainNode));
    TreeNode.Data := TrainNode;
    TreeNode.ImageIndex := 18;
  end;
end;

procedure TFormTrainCommander.OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
begin
  NodeIsGoingAway(ALccNode);
end;

procedure TFormTrainCommander.OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);
begin
  NodeIsGoingAway(ALccNode);
end;

procedure TFormTrainCommander.OnNodeTractionListenerAttached(Sender: TObject;
  LccNode: TLccNode; AMessage: TLccMessage);
begin
  RebuildTrainTreeview;
end;

procedure TFormTrainCommander.OnNodeTractionListenerDetached(Sender: TObject;
  LccNode: TLccNode; AMessage: TLccMessage);
begin
  RebuildTrainTreeview;
end;

procedure TFormTrainCommander.OnNodeTractionListenerQuery(Sender: TObject;
  LccNode: TLccNode; AMessage: TLccMessage; var DoDefault: Boolean);
begin
  RebuildTrainTreeview;
end;

procedure TFormTrainCommander.OnTractionRegisterNotify(
  TractionObject: TLccTractionObject; IsRegistered: Boolean);
begin
  if IsRegistered then
    FormServerInfo.AddTrainObject(TractionObject)
  else
    FormServerInfo.RemoveTrainObject(TractionObject);
end;

procedure TFormTrainCommander.OnTractionNotify(
  TractionObject: TLccTractionObject);
begin

end;

function TFormTrainCommander.TrainNodeToCaption(ATrainNode: TLccTrainDccNode): string;
var
  SpeedStep: string;
begin
  case ATrainNode.DccSpeedStep of
    ldssDefault : SpeedStep := 'Default Step';
    ldss14      : SpeedStep := '14 Step';
    ldss28      : SpeedStep := '28 Step';
    ldss128     : SpeedStep := '128 Step';
  end;

  if ATrainNode.DccLongAddress then
    Result := IntToStr(ATrainNode.DccAddress) + ' Long ' + SpeedStep + ' [Listeners: ' + IntToStr(ATrainNode.Listeners.Count) + '] [' + ATrainNode.AliasIDStr + ']' + ' [' + ATrainNode.NodeIDStr[True] + ']'
  else
    Result := IntToStr(ATrainNode.DccAddress) + ' Short ' + SpeedStep + ' [Listeners: ' + IntToStr(ATrainNode.Listeners.Count) + '] [' + ATrainNode.AliasIDStr + ']' + ' [' + ATrainNode.NodeIDStr[True] + ']';
end;

function TFormTrainCommander.FindSingleLevelNodeWithData(ParentNode: TTreeNode;
  const NodeData: Pointer): TTreeNode;
begin
  if ParentNode = nil then
    Result := TreeViewTrains.Items.GetFirstNode
  else
    Result := ParentNode.GetFirstChild;
  while Assigned(Result) and (Result.Data <> NodeData) do
    Result := Result.GetNextSibling;
end;

procedure TFormTrainCommander.RebuildTrainTreeview;
var
  ListenerTrainNode, ParentTrainNode: TLccTrainDccNode;
  ParentTreeNode, ChildTreeNode: TTreeNode;
  i, j: Integer;
begin
  TreeViewTrains.Items.Clear;

  for i := 0 to NodeManager.Nodes.Count - 1 do
  begin
    if TLccNode( NodeManager.Nodes[i]) is TLccTrainDccNode then
    begin
      ParentTrainNode := TLccNode( NodeManager.Nodes[i]) as TLccTrainDccNode;
      ParentTreeNode := TreeViewTrains.Items.Add(nil, TrainNodeToCaption(ParentTrainNode));
      ParentTreeNode.ImageIndex := 18;
      for j := 0 to ParentTrainNode.Listeners.Count - 1 do
      begin
        ListenerTrainNode := NodeManager.FindNode(ParentTrainNode.Listeners.Listener[j].NodeID) as TLccTrainDccNode;
        if Assigned(ListenerTrainNode) then
        begin
           ChildTreeNode := TreeViewTrains.Items.AddChild(ParentTreeNode, TrainNodeToCaption(ListenerTrainNode));
           ChildTreeNode.ImageIndex := 7;
        end else
        begin
           ChildTreeNode := TreeViewTrains.Items.AddChild(ParentTreeNode, '[Unknown]');
           ChildTreeNode.ImageIndex := 7;
        end;
      end;
    end;
  end;
end;

procedure TFormTrainCommander.ReleaseAliasOnTrains;
var
  i: Integer;
  TrainNode: TLccTrainDccNode;
begin
  for i := NodeManager.Nodes.Count - 1 downto 0  do
  begin
    if TLccNode(NodeManager.Nodes.Items[i]) is TLccTrainDccNode then
    begin
      TrainNode := TLccNode(NodeManager.Nodes.Items[i]) as TLccTrainDccNode;
      TrainNode.ReleaseAlias(100);
    end;
  end;
end;

procedure TFormTrainCommander.NodeIsGoingAway(LccSourceNode: TLccNode);
var
 TreeNode: TTreeNode;
 TrainNode: TLccTrainDccNode;
begin
  if Assigned(LccSourceNode) then
  begin
    if LccSourceNode is TLccTrainDccNode then
    begin
      TrainNode := LccSourceNode as TLccTrainDccNode;
      TreeNode := TreeViewTrains.Items.FindNodeWithData(TrainNode);
      while Assigned(TreeNode) do
      begin
        if Assigned(TreeNode) then
        begin
          TreeViewTrains.Items.Delete(TreeNode);
          Break;
        end;
        TreeNode := TreeViewTrains.Items.FindNodeWithData(LccSourceNode);
      end;
    end;
  end;
end;

procedure TFormTrainCommander.OnComPortReceiveMessage(Sender: TObject; Info: TLccConnectionInfo);
begin
  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Add('R: ' + Info.MessageStr);
    MemoComPort.SelStart := Length(MemoComPort.Lines.Text);
  finally
    MemoComPort.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.ToggleBoxServerFormChange(Sender: TObject);
begin
  if ToggleBoxServerForm.Checked then
    FormServerInfo.Show
  else
    FormServerInfo.Hide
end;

end.


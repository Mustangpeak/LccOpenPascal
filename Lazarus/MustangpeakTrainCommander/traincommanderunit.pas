unit TrainCommanderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons,
  lcc_ethernet_server,
  lcc_defines,
  lcc_utilities,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_node_commandstation,
  lcc_node_controller,
  lcc_node_train,
  LazSynaSer,
  lcc_alias_server,
 // lcc_ethernet_http,
  lcc_connection_common,
  lcc_ethernet_common,
  servervisualunit,
  lcc_node_messages_can_assembler_disassembler,
  lcc_alias_server_thread;


const
  MAX_LOGGING_LINES = 50;

type

  { TMustangPeakCommanderSerialLink }

  TMustangPeakCommanderSerialLink = class
  private
    FSerial: TBlockSerial;
    FConnected: Boolean;
  protected
    property Serial: TBlockSerial read FSerial write FSerial;
  public
    property Connected: Boolean read FConnected;
    constructor Create; virtual;
    destructor Destroy; override;

    function OpenConnection(ComPort: String; out ErrorCode: Integer; out ErrorDesc: String): Boolean;
    procedure CloseConnection;
    procedure SendString(AString: AnsiString);
  end;

  { TFormTrainCommander }

  TFormTrainCommander = class(TForm)
    Button1: TButton;
    ButtonCreateTrains: TButton;
    ButtonRawGridConnectLogEnable: TButton;
    ButtonLiveMessages: TButton;
    ButtonGridConnectStrClear: TButton;
    ButtonCreateConsist: TButton;
    ButtonHTTPServer: TButton;
    ButtonWebserverConnect: TButton;
    ButtonComPortConnect: TButton;
    ButtonTrainsClear: TButton;
    ButtonClear: TButton;
    ButtonEthernetConnect: TButton;
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
    MemoRawGridConnectReceive: TMemo;
    Panel1: TPanel;
    PanelTrainsHeader: TPanel;
    PanelTrains: TPanel;
    PanelConnections: TPanel;
    PanelDetails: TPanel;
    SplitterConnections1: TSplitter;
    SplitterTrains: TSplitter;
    SplitterConnections: TSplitter;
    StatusBarMain: TStatusBar;
    ToggleBoxServerForm: TToggleBox;
    TreeViewTrains: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure ButtonCreateTrainsClick(Sender: TObject);
    procedure ButtonLiveMessagesClick(Sender: TObject);
    procedure ButtonCreateConsistClick(Sender: TObject);
    procedure ButtonGridConnectStrClearClick(Sender: TObject);
    procedure ButtonHTTPServerClick(Sender: TObject);
    procedure ButtonRawGridConnectLogEnableClick(Sender: TObject);
    procedure ButtonWebserverConnectClick(Sender: TObject);
    procedure ButtonComPortConnectClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonEthernetConnectClick(Sender: TObject);
    procedure ButtonTrainsReleaseAliasClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToggleBoxServerFormChange(Sender: TObject);
  private
    FAutoCreateTrainAddress: Word;
    FCommandStationNode: TLccCommandStationNode;
    FEmulateCANBus: Boolean;
 //   FLccHTTPServer: TLccHTTPServer;
    FLccWebsocketServer: TLccWebSocketServerThreadManager;
    FNodeManager: TLccNodeManager;
    FSerialLink: TMustangPeakCommanderSerialLink;
    FWorkerMessage: TLccMessage;
    FLccServer: TLccEthernetServerThreadManager;
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    // Events occuring on the connections
    procedure OnServerManagerReceiveMessage(Sender: TObject; ALccMessage: TLccMessage);
    procedure OnServerManagerReceiveGridConnectStr(Sender: TObject; AGridConnectStr: string);
    procedure OnServerManagerSendMessage(Sender: TObject; ALccMessage: TLccMessage);
    procedure OnServerManagerConnectionState(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; Info: TLccConnectionInfo);
    procedure OnServerErrorMessage(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; Info: TLccConnectionInfo);

    // Callbacks from the Node Manager
    procedure OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);  // Note there is not defined way for a node to log out of OpenLCB
    procedure OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);

    procedure OnNodeTractionListenerAttached(Sender: TObject; LccNode: TLccNode; AMessage: TLccMessage);
    procedure OnNodeTractionListenerDetached(Sender: TObject; LccNode: TLccNode; AMessage: TLccMessage);
    procedure OnNodeTractionListenerQuery(Sender: TObject; LccNode: TLccNode; AMessage: TLccMessage; var DoDefault: Boolean);

    procedure OnComPortSendMessage(Sender: TObject; var GridConnectStyleMessage: string);

    // Other

    function TrainNodeToCaption(ATrainNode: TLccTrainDccNode): string;
    function FindSingleLevelNodeWithData(ParentNode: TTreeNode; const NodeData: Pointer): TTreeNode;
    procedure RebuildTrainTreeview;
    procedure ReleaseAliasOnTrains;
    procedure NodeIsGoingAway(LccSourceNode: TLccNode);

  public
    property EmulateCANBus: Boolean read FEmulateCANBus write FEmulateCANBus;
    property LccServer: TLccEthernetServerThreadManager read FLccServer write FLccServer;
    property LccWebsocketServer: TLccWebSocketServerThreadManager read FLccWebsocketServer write FLccWebsocketServer;
  //  property LccHTTPServer: TLccHTTPServer read FLccHTTPServer write FLccHTTPServer;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;

    property CommandStationNode: TLccCommandStationNode read FCommandStationNode write FCommandStationNode;

    property AutoCreateTrainAddress: Word read FAutoCreateTrainAddress write FAutoCreateTrainAddress;

    property SerialLink: TMustangPeakCommanderSerialLink read FSerialLink write FSerialLink;
  end;

var
  FormTrainCommander: TFormTrainCommander;

implementation

{$R *.lfm}

{ TMustangPeakCommanderSerialLink }

constructor TMustangPeakCommanderSerialLink.Create;
begin
  inherited Create;
  FSerial := TBlockSerial.Create;
end;

destructor TMustangPeakCommanderSerialLink.Destroy;
begin
  Serial.CloseSocket;
  FreeAndNil(FSerial);
  inherited Destroy;
end;

function TMustangPeakCommanderSerialLink.OpenConnection(ComPort: String; out ErrorCode: Integer; out ErrorDesc: String): Boolean;
begin
  Result := False;
  if Connected then
    CloseConnection;

  ErrorCode := 0;
  ErrorDesc := '';

  try
    Serial.Connect(ComPort);
    if Serial.LastError = 0 then
    begin
      Serial.Config(9600, 8, 'N', SB1, False, False);
      if Serial.LastError = 0 then
      begin
        FConnected := True;
        Result := True;
      end else
      begin
        CloseConnection;
        ErrorCode := Serial.LastError;
        ErrorDesc := Serial.LastErrorDesc;
      end;
    end else
    begin
      ErrorCode := Serial.LastError;
      ErrorDesc := Serial.LastErrorDesc;
    end;
  except
     ErrorCode := Serial.LastError;
     ErrorDesc := Serial.LastErrorDesc;
  end;
end;

procedure TMustangPeakCommanderSerialLink.CloseConnection;
begin
  if Connected then
  begin
    Serial.CloseSocket;
    FConnected := False;
  end;
end;

procedure TMustangPeakCommanderSerialLink.SendString(AString: AnsiString);
begin
  if Connected then
    Serial.SendString(AString);
end;

{ TFormTrainCommander }

procedure TFormTrainCommander.ButtonHTTPServerClick(Sender: TObject);
var
  ConnectionInfo: TLccEthernetConnectionInfo;
begin
 { if Assigned(LccHTTPServer) then
  begin
    LccHTTPServer := ConnectionFactory.DestroyConnection(LccHTTPServer);
    LccHTTPServer := nil;
  end else
  begin
    ConnectionInfo := TLccEthernetConnectionInfo.Create;
    ConnectionInfo.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
    ConnectionInfo.ListenerIP := '127.0.0.1';
    ConnectionInfo.ListenerPort := 12020;
    LccHTTPServer := ConnectionFactory.CreateConnection(TLccHTTPServer, ConnectionInfo, EMULATE_CAN_BUS) as TLccHTTPServer;
    if Assigned(LccHTTPServer) then
      LccHTTPServer.;
  end;  }
end;

procedure TFormTrainCommander.ButtonRawGridConnectLogEnableClick(Sender: TObject);
begin
  if Assigned(ConnectionFactory.OnLccGridConnectStrReceive) then
  begin
    ConnectionFactory.OnLccGridConnectStrReceive := nil;   // Allows threads to not have to use Syncronize
    ButtonRawGridConnectLogEnable.Caption := 'Enable';
  end else
  begin
    ConnectionFactory.OnLccGridConnectStrReceive := @OnServerManagerReceiveGridConnectStr;
    ButtonRawGridConnectLogEnable.Caption := 'Disable';
  end;
end;

procedure TFormTrainCommander.ButtonCreateTrainsClick(Sender: TObject);
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

procedure TFormTrainCommander.Button1Click(Sender: TObject);
begin
  AliasServer.Clear(False);
end;

procedure TFormTrainCommander.ButtonLiveMessagesClick(Sender: TObject);
begin
  ButtonLiveMessages.Caption := 'Live Messages: ' + IntToStr(LccMessagesAllocated);
end;

procedure TFormTrainCommander.ButtonCreateConsistClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeViewTrains.SelectionCount - 1 do
  begin

  end;
end;

procedure TFormTrainCommander.ButtonGridConnectStrClearClick(Sender: TObject);
begin
  MemoRawGridConnectReceive.Lines.BeginUpdate;
  try
    MemoRawGridConnectReceive.Lines.Clear;
  finally
    MemoRawGridConnectReceive.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.ButtonWebserverConnectClick(Sender: TObject);
var
  ConnectionInfo: TLccEthernetConnectionInfo;
begin
  if Assigned(LccWebsocketServer) then
  begin
    ConnectionFactory.DestroyConnection(FLccWebsocketServer);
    LccWebsocketServer := nil;
  end else
  begin
    ConnectionInfo := TLccEthernetConnectionInfo.Create;
    ConnectionInfo.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
    ConnectionInfo.ListenerIP := '127.0.0.1';
    ConnectionInfo.ListenerPort := 12022;
    LccWebsocketServer := ConnectionFactory.CreateLccMessageConnection(TLccWebSocketServerThreadManager, ConnectionInfo, EmulateCANBus) as TLccWebSocketServerThreadManager;
    if Assigned(LccWebsocketServer) then
      LccWebsocketServer.OpenConnection;
  end;
end;

procedure TFormTrainCommander.ButtonComPortConnectClick(Sender: TObject);
var
  AComPort: String;
  ErrorCode: Integer;
  ErrorDesc: String;
begin
  if SerialLink.Connected then
  begin
    SerialLink.CloseConnection;
    StatusBarMain.Panels[3].Text := 'ComPort Disconnected';
    ButtonComPortConnect.Caption := 'Open ComPort';
  end else
  begin
    AComPort := ComboBoxComPorts.Items[ ComboBoxComPorts.ItemIndex];
    if SerialLink.OpenConnection(AComPort, ErrorCode, ErrorDesc) then
    begin
      StatusBarMain.Panels[3].Text := 'ComPort: ' + AComPort;
      ButtonComPortConnect.Caption := 'Close ComPort';
    end else
      StatusBarMain.Panels[3].Text :=  'ErrorCode: ' + IntToStr(ErrorCode) + ', ' + ErrorDesc;
  end;
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
var
  ConnectionInfo: TLccEthernetConnectionInfo;
begin
  if Assigned(LccServer) then
  begin
    ConnectionFactory.DestroyConnection(LccServer);
    LccServer := nil;
  end else
  begin
    ConnectionInfo := TLccEthernetConnectionInfo.Create;
    ConnectionInfo.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
    ConnectionInfo.ListenerIP := '127.0.0.1';
    ConnectionInfo.ListenerPort := 12021;
    LccServer := ConnectionFactory.CreateLccMessageConnection(TLccEthernetServerThreadManager, ConnectionInfo, EmulateCANBus) as TLccEthernetServerThreadManager;
    if Assigned(LccServer) then
      LccServer.OpenConnection;
  end;
end;

procedure TFormTrainCommander.ButtonTrainsReleaseAliasClick(Sender: TObject);
begin
  ReleaseAliasOnTrains
end;

procedure TFormTrainCommander.OnServerManagerReceiveMessage(Sender: TObject;
  ALccMessage: TLccMessage);
var
  Preamble: String;
  ConnectionThread: TLccConnectionThread;
  ConnectionManager: TLccConnectionThreadManager;
begin
  if CheckBoxLogMessages.Checked then
  begin
    ConnectionThread := ALccMessage.ConnectionThread as TLccConnectionThread;
    ConnectionManager := ConnectionThread.OwnerConnectionManager as TLccConnectionThreadManager;
    if ConnectionManager = LccServer then
      Preamble := 'TCP:R: '
    else
    if ConnectionManager = LccWebsocketServer then
      Preamble := 'WS:R: '
  {  else
    if Manager = LccHTTPServer then
      Preamble := 'HTTP:R: '   }
    ;

    MemoLog.Lines.BeginUpdate;
    try
      if CheckBoxDetailedLog.Checked then
        MemoLog.Lines.Add(Preamble + MessageToDetailedMessageStr(ALccMessage, EmulateCANBus))
      else
        MemoLog.Lines.Add(Preamble + MessageToMessageStr(ALccMessage, EmulateCANBus));
    finally
      if MemoLog.Lines.Count > MAX_LOGGING_LINES then
        MemoLog.Lines.Delete(0);
       MemoLog.Lines.EndUpdate;

      MemoLog.SelStart := Length(MemoLog.Lines.Text)-1;
      MemoLog.SelLength := 1;
      MemoLog.SelLength := 0;
    end;
  end;
end;

procedure TFormTrainCommander.OnServerManagerReceiveGridConnectStr(
  Sender: TObject; AGridConnectStr: string);
begin
  MemoRawGridConnectReceive.Lines.BeginUpdate;
  try
    MemoRawGridConnectReceive.Lines.Add('R: ' + AGridConnectStr);
    MemoRawGridConnectReceive.SelStart := Length( MemoRawGridConnectReceive.Text);
  finally
    if MemoRawGridConnectReceive.Lines.Count > MAX_LOGGING_LINES then
      MemoRawGridConnectReceive.Lines.Delete(0);

    MemoRawGridConnectReceive.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.OnServerManagerSendMessage(Sender: TObject; ALccMessage: TLccMessage);
var
  Preamble: String;
begin
  if CheckBoxLogMessages.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      Preamble := '';
      if EmulateCANBus then
        Preamble := 'TCP:';
      if CheckBoxDetailedLog.Checked then
        MemoLog.Lines.Add(Preamble + 'S: ' + MessageToDetailedMessageStr(ALccMessage, EmulateCANBus))
      else
        MemoLog.Lines.Add(Preamble + 'S: ' + MessageToMessageStr(ALccMessage, EmulateCANBus))
    finally
      if MemoLog.Lines.Count > MAX_LOGGING_LINES then
        MemoLog.Lines.Delete(0);
      MemoLog.Lines.EndUpdate;

      MemoLog.SelStart := Length(MemoLog.Lines.Text)-1;
      MemoLog.SelLength := 1;
      MemoLog.SelLength := 0;
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
  ;
end;

procedure TFormTrainCommander.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose; // Keep Hints quiet
  ConnectionFactory.DestroyConnection(LccWebsocketServer);
  ConnectionFactory.DestroyConnection(LccServer);
//   ConnectionFactory.DestroyConnection(LccHTTPServer);
  LccWebsocketServer:= nil;
  LccServer:= nil;
//  LccHTTPServer:= nil;
end;

procedure TFormTrainCommander.FormCreate(Sender: TObject);
begin
  EmulateCANBus := True;

  {$IFDEF PYTHON_SCRIPT_COMPATIBLE}
  Caption := 'Train Commander [LccOpenPascal]' + ' PYTHON SCRIPT ENABLED';
  EmulateCANBus := True;
  {$ELSE}
  Caption := 'Train Commander [LccOpenPascal]' + ' PYTHON SCRIPT DISABLED';
  {$ENDIF}

  NodeManager := TLccNodeManager.Create(nil);
  NodeManager.EmulateCanNetworkLogin := EmulateCANBus;
  NodeManager.OnNodeAliasIDChanged := @OnNodeManagerAliasIDChanged;
  NodeManager.OnNodeIDChanged := @OnNodeManagerIDChanged;
  NodeManager.OnNodeLogin := @OnNodeManagerNodeLogin;
  NodeManager.OnNodeTractionListenerAttached := @OnNodeTractionListenerAttached;
  NodeManager.OnNodeTractionListenerDetached := @OnNodeTractionListenerDetached;
  NodeManager.OnNodeTractionListenerQuery := @OnNodeTractionListenerQuery;
  NodeManager.OnAliasRelease := @OnNodeManagerAliasRelease;
  NodeManager.OnNodeDestroy := @OnNodeManagerNodeDestroy;


  ConnectionFactory.OnStateChange := @OnServerManagerConnectionState;
  ConnectionFactory.OnError := @OnServerErrorMessage;
  ConnectionFactory.OnLccMessageReceive := @OnServerManagerReceiveMessage;
 // ConnectionFactory.OnLccGridConnectStrReceive := @OnServerManagerReceiveGridConnectStr;   // Default Off
  ConnectionFactory.OnLccMessageSend := @OnServerManagerSendMessage;

  AutoCreateTrainAddress := 1;

  FWorkerMessage := TLccMessage.Create;
  FSerialLink := TMustangPeakCommanderSerialLink.Create;
end;

procedure TFormTrainCommander.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSerialLink);
  NodeManager.ReleaseAliasAll;
  FreeAndNil(FNodeManager);   // after the servers are destroyed
  FreeAndNil(FWorkerMessage);
end;

procedure TFormTrainCommander.FormShow(Sender: TObject);
begin
  ComboBoxComPorts.Items.Delimiter := ';';
  ComboBoxComPorts.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  ComboBoxComPorts.ItemIndex := 0;
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

    TrainNode.OnSendMessageComPort := @OnComPortSendMessage;

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

procedure TFormTrainCommander.OnComPortSendMessage(Sender: TObject; var GridConnectStyleMessage: string);
begin
  if SerialLink.Connected then
  begin
    SerialLink.SendString(GridConnectStyleMessage);

    MemoComPort.Lines.BeginUpdate;
    try
      MemoComPort.Lines.Add(GridConnectStyleMessage)

    finally
      if MemoComPort.Lines.Count > MAX_LOGGING_LINES then
        MemoComPort.Lines.Delete(0);
       MemoComPort.Lines.EndUpdate;

      MemoComPort.SelStart := Length(MemoComPort.Lines.Text)-1;
      MemoComPort.SelLength := 1;
      MemoComPort.SelLength := 0;
    end;
  end;
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

procedure TFormTrainCommander.ToggleBoxServerFormChange(Sender: TObject);
begin
  if ToggleBoxServerForm.Checked then
    FormServerInfo.Show
  else
    FormServerInfo.Hide
end;

end.


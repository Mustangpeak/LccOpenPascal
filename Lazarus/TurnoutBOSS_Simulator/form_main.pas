unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons,
  lcc_defines,
  lcc_utilities,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_node_commandstation,
  lcc_node_controller,
  lcc_node_train,
  lcc_alias_server,
  lcc_connection_common,
  lcc_ethernet_common,
  lcc_ethernet_client,
  lcc_node_messages_can_assembler_disassembler,
  frame_left_hand_turnout,
  frame_right_hand_turnout;

type

  { TForm_turnoutboss_simulator }

  TForm_turnoutboss_simulator = class(TForm)
    ButtonEthernetConnect: TButton;
    Edit_turnoutboss_left: TEdit;
    Edit_turnoutboss_right: TEdit;
    Edit_alias: TEdit;
    Edit_node_id: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel_header: TPanel;
    Panel_frame_container: TPanel;
    RadioGroup_turnout_type: TRadioGroup;
    StatusBarMain: TStatusBar;
    procedure ButtonEthernetConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup_turnout_typeClick(Sender: TObject);
  private
    FEmulateCANBus: Boolean;
    FLccClient: TLccEthernetClientThreadManager;
    FNode: TLccNode;
    FNodeManager: TLccNodeManager;
    FTurnoutFrame: TFrame;
    FWorkerMessage: TLccMessage;

  protected
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

    procedure BuildFrame;

  public

    property EmulateCANBus: Boolean read FEmulateCANBus write FEmulateCANBus;
    property LccClient: TLccEthernetClientThreadManager read FLccClient write FLccClient;
    property TurnoutFrame: TFrame read FTurnoutFrame write FTurnoutFrame;
    property Node: TLccNode read FNode write FNode;

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property WorkerMessage : TLccMessage read FWorkerMessage write FWorkerMessage;
  end;

var
  Form_turnoutboss_simulator: TForm_turnoutboss_simulator;

implementation

{$R *.lfm}

{ TForm_turnoutboss_simulator }

procedure TForm_turnoutboss_simulator.ButtonEthernetConnectClick(Sender: TObject);
var
  ConnectionInfo: TLccEthernetConnectionInfo;
begin
  if Assigned(LccClient) then
  begin
    ConnectionFactory.DestroyConnection(LccClient);
    LccClient := nil;
  end else
  begin
    ConnectionInfo := TLccEthernetConnectionInfo.Create;
    ConnectionInfo.AutoResolveIP := False;
    ConnectionInfo.ListenerIP := '127.0.0.1';
    ConnectionInfo.ListenerPort := 12021;
    LccClient := ConnectionFactory.CreateLccMessageConnection(TLccEthernetClientThreadManager, ConnectionInfo, EmulateCANBus) as TLccEthernetClientThreadManager;
    if Assigned(LccClient) then
      LccClient.OpenConnection;
  end;

end;

procedure TForm_turnoutboss_simulator.FormCreate(Sender: TObject);
begin

  EmulateCANBus := True;

  NodeManager := TLccNodeManager.Create(nil);
  NodeManager.EmulateCanNetworkLogin := EmulateCANBus;
  NodeManager.OnNodeAliasIDChanged := @OnNodeManagerAliasIDChanged;
  NodeManager.OnNodeIDChanged := @OnNodeManagerIDChanged;
  NodeManager.OnNodeLogin := @OnNodeManagerNodeLogin;
  NodeManager.OnAliasRelease := @OnNodeManagerAliasRelease;
  NodeManager.OnNodeDestroy := @OnNodeManagerNodeDestroy;


  ConnectionFactory.OnStateChange := @OnServerManagerConnectionState;
  ConnectionFactory.OnError := @OnServerErrorMessage;
  ConnectionFactory.OnLccMessageReceive := @OnServerManagerReceiveMessage;
  ConnectionFactory.OnLccMessageSend := @OnServerManagerSendMessage;

  FWorkerMessage := TLccMessage.Create;

 end;

procedure TForm_turnoutboss_simulator.FormShow(Sender: TObject);
begin
  if not Assigned(TurnoutFrame) then
    BuildFrame;
end;

procedure TForm_turnoutboss_simulator.RadioGroup_turnout_typeClick(
  Sender: TObject);
begin
  BuildFrame;
end;

procedure TForm_turnoutboss_simulator.OnServerManagerReceiveMessage(Sender: TObject;
  ALccMessage: TLccMessage);
begin

end;

procedure TForm_turnoutboss_simulator.OnServerManagerReceiveGridConnectStr(Sender: TObject;
  AGridConnectStr: string);
begin

end;

procedure TForm_turnoutboss_simulator.OnServerManagerSendMessage(Sender: TObject;
  ALccMessage: TLccMessage);
begin

end;

procedure TForm_turnoutboss_simulator.OnServerManagerConnectionState(Sender: TObject;
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  Info: TLccConnectionInfo);
begin
 if Manager = LccClient then
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
          StatusBarMain.Panels[0].Text := 'Connected: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort);
          if NodeManager.Nodes.Count = 0 then
            Node := NodeManager.AddNodeByClass('', TLccNode, True, StrToNodeID(Edit_node_id.Text, True));

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
          StatusBarMain.Panels[0].Text := 'Disconnected: ';
        end;
    end;
  end;
end;

procedure TForm_turnoutboss_simulator.OnServerErrorMessage(Sender: TObject;
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  Info: TLccConnectionInfo);
begin
  if Manager = LccClient then
  begin
    ShowMessage('Client: ' + Info.MessageStr);
  end
end;

procedure TForm_turnoutboss_simulator.OnNodeManagerAliasIDChanged(Sender: TObject;
  LccSourceNode: TLccNode);
begin
  Edit_alias.Text := '0x' + IntToHex(LccSourceNode.AliasID, 4);
end;

procedure TForm_turnoutboss_simulator.OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode
  );
begin

end;

procedure TForm_turnoutboss_simulator.OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode
  );
begin

end;

procedure TForm_turnoutboss_simulator.OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
begin

end;

procedure TForm_turnoutboss_simulator.OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);
begin

end;

procedure TForm_turnoutboss_simulator.BuildFrame;
begin
  case RadioGroup_turnout_type.ItemIndex of
    0: begin
      if not (TurnoutFrame is TFrame_left_hand_turnoutboss) then
      begin
         if Assigned(TurnoutFrame) then
           TurnoutFrame.Free;
         TurnoutFrame := TFrame_left_hand_turnoutboss.Create(Panel_frame_container);
         TurnoutFrame.Align := alClient;
         TurnoutFrame.Parent := Panel_frame_container;
      end;
    end;
    1: begin
      if not (TurnoutFrame is TFrame_right_hand_turnoutboss) then
      begin
        if Assigned(TurnoutFrame) then
           TurnoutFrame.Free;
         TurnoutFrame := TFrame_right_hand_turnoutboss.Create(Panel_frame_container);
         TurnoutFrame.Align := alClient;
         TurnoutFrame.Parent := Panel_frame_container;
      end;
    end;
  end;
end;

end.


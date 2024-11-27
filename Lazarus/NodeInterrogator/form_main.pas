unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, lcc_defines, lcc_utilities, lcc_node, lcc_node_manager,
  lcc_node_messages, lcc_node_commandstation, lcc_node_controller,
  lcc_node_train, lcc_comport, lcc_alias_server,
  lcc_node_messages_can_assembler_disassembler, lcc_alias_server_thread,
  LazSynaSer, lcc_connection_common, unit_comport;

const
  MAX_LOGGING_LINES = 200;


type

  { TNodeInfoObject }

  TNodeInfoObject = class
  private
    FAlias: word;
    FNodeID: TNodeID;
  public

    property NodeID: TNodeID read FNodeID write FNodeID;
    property Alias: word read FAlias write FAlias;

    constructor Create(ANodeID: TNodeID; AnAlias: word);
  end;

  { TInterrogatorNode }

  TInterrogatorNode = class(TLccNode)

  protected
    procedure BeforeLogin; override;
  end;

  TInterrogatorNodeClass = class of TInterrogatorNode;

  { TForm1 }

  TForm1 = class(TForm)
    ButtonConfigMemAddressSpaceInfoWellKnown: TButton;
    ButtonConfigMemOpClear: TButton;
    ButtonMemConfgAddressSpaceInfoCancel: TButton;
    ButtonConfigMemClearAddressInfo: TButton;
    ButtonConfigMemAddressSpaceInfoAll: TButton;
    ButtonConfigMemAddressSpaceInfo: TButton;
    ButtonSendConfigMemConfigOptions: TButton;
    ButtonVerifyNodesGlobal: TButton;
    ButtonClearMemo: TButton;
    ButtonRefreshComPort: TButton;
    ButtonComPortConnect: TButton;
    ButtonVerifyNodesAddressed: TButton;
    CheckBoxConfigMemReserved0x80: TCheckBox;
    CheckBoxConfigMemReserved0x40: TCheckBox;
    CheckBoxConfigMemReserved0x20: TCheckBox;
    CheckBoxConfigMemReserved0x10: TCheckBox;
    CheckBoxConfigMemReserved0x02: TCheckBox;
    CheckBoxConfigMemStreamReadWriteSupport: TCheckBox;
    CheckBoxConfigMemWriteLengthAllOtherBitsZero: TCheckBox;
    CheckBoxConfigMemUnalignedWrite: TCheckBox;
    CheckBoxConfigMemWriteUnderMask: TCheckBox;
    CheckBoxConfigMemUnalignedRead: TCheckBox;
    CheckBoxConfigMemSupportRead0xFC: TCheckBox;
    CheckBoxConfigMemSupportRead0xFB: TCheckBox;
    CheckBoxConfigMemSupportWrite0xFB: TCheckBox;
    CheckBoxConfigMemOpAllBitsZero: TCheckBox;
    ComboBoxConfigMemAddressSpace: TComboBox;
    ComboBoxNodeList: TComboBox;
    ComboBoxComPorts: TComboBox;
    EditCongMemOpNameString: TEdit;
    EditConfigMemHighestAddressSpace: TEdit;
    EditConfigMemLowestAddressSpace: TEdit;
    GroupBoxConfigMemAddressSpaceInfo: TGroupBox;
    GroupBoxConfMemOptions: TGroupBox;
    GroupBoxConfigMemWriteLenghts: TGroupBox;
    GroupBoxConfigMemAvailableCommands: TGroupBox;
    Label1: TLabel;
    LabelConfigSpaceAddressInfo: TLabel;
    LabelConfigMemLowestAddressSpaceName: TLabel;
    LabelConfigMemHighestAddressSpaceName: TLabel;
    LabelConfigMemOpNameString: TLabel;
    LabelConfigMemeHighestAddressSpace: TLabel;
    LabelConfigMemLowestAddressSpace: TLabel;
    LabelAliasID: TLabel;
    LabelNodeID: TLabel;
    MemoComPort: TMemo;
    PageControlMain: TPageControl;
    PanelNodeList: TPanel;
    PanelHeader: TPanel;
    PanelLogging: TPanel;
    PanelLoggingHeader: TPanel;
    PanelTabDatagramReadWrite: TPanel;
    PanelTabEvents: TPanel;
    PanelTabSheetConfigMem: TPanel;
    PanelMemo: TPanel;
    PanelTabMessageNetwork: TPanel;
    Splitter1: TSplitter;
    StatusBarMain: TStatusBar;
    TabSheetEvents: TTabSheet;
    TabSheetMessageNetwork: TTabSheet;
    TabSheetConfigMem: TTabSheet;
    TabSheetDatagramReadWrite: TTabSheet;
    TreeViewConfigMemAddressSpaceInfo: TTreeView;
    procedure ButtonConfigMemAddressSpaceInfoAllClick(Sender: TObject);
    procedure ButtonConfigMemAddressSpaceInfoClick(Sender: TObject);
    procedure ButtonConfigMemAddressSpaceInfoWellKnownClick(Sender: TObject);
    procedure ButtonConfigMemClearAddressInfoClick(Sender: TObject);
    procedure ButtonConfigMemOpClearClick(Sender: TObject);
    procedure ButtonMemConfgAddressSpaceInfoCancelClick(Sender: TObject);
    procedure ButtonSendConfigMemConfigOptionsClick(Sender: TObject);
    procedure ButtonVerifyNodesGlobalClick(Sender: TObject);
    procedure ButtonClearMemoClick(Sender: TObject);
    procedure ButtonCreateNodeClick(Sender: TObject);
    procedure ButtonRefreshComPortClick(Sender: TObject);
    procedure ButtonComPortConnectClick(Sender: TObject);
    procedure ComboBoxConfigMemAddressSpaceChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAckWorker: TLccMessage;
    FAddressSpaceEnumerating: Boolean;
    FAddressSpaceEnumeratingWellKnown: Boolean;
    FAddressSpaceQueried: Byte;
    FEmulateCANBus: boolean;
    FNodeManager: TLccNodeManager;

    FSerialLink: TLccComPort;
    FWorker: TLccMessage;
    function GetNode: TLccNode;
    function GetTargetNode: TNodeInfoObject;

  protected

    property AddressSpaceQueried: Byte read FAddressSpaceQueried write FAddressSpaceQueried;
    property AddressSpaceEnumerating: Boolean read FAddressSpaceEnumerating write FAddressSpaceEnumerating;
    property AddressSpaceEnumeratingWellKnown: Boolean read FAddressSpaceEnumeratingWellKnown write FAddressSpaceEnumeratingWellKnown;

    procedure OnComPortString(Sender: TObject; GridConnectString: ansistring);
    procedure OnComPortError(Sender: TObject; ErrorString: string; ErrorCode: word);
    procedure OnComPortLogIn(Sender: TObject);
    procedure OnComPortLogOut(Sender: TObject);

    procedure OnConnectionFactorySendMessage(Sender: TObject; LccMessage: TLccMessage);

    procedure OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
    // Note there is not defined way for a node to log out of OpenLCB
    procedure OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);

    procedure OnComPortSendMessage(Sender: TObject; var GridConnectStyleMessage: string);

    procedure ShowNoTargetMessage;
    procedure SendWorkerMessage(AWorker: TLccMessage);
    procedure SendAck(Message: TLccMessage);
    procedure SendAddressSpaceQuery(AddressSpace: Byte);
    procedure ClearConfigMemOperation;
    procedure DisconnectSerialLink;

    procedure HandleVerifiedNode(AWorker: TLccMessage);
    procedure HandleGetConfigOptionsReply(AWorker: TLccMessage);
    procedure HandleGetAddressSpaceInfoPresentReply(AWorker: TLccMessage);
    procedure HandleGetAddressSpaceInfoNotPresentReply(AWorker: TLccMessage);
  public

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property SerialLink: TLccComPort read FSerialLink write FSerialLink;

    property EmulateCANBus: boolean read FEmulateCANBus write FEmulateCANBus;

    property Worker: TLccMessage read FWorker write FWorker;
    property AckWorker: TLccMessage read FAckWorker write FAckWorker;
    property Node: TLccNode read GetNode;
    property TargetNode: TNodeInfoObject read GetTargetNode;

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TNodeInfoObject }


constructor TNodeInfoObject.Create(ANodeID: TNodeID; AnAlias: word);
begin
  FNodeID := ANodeID;
  FAlias := AnAlias;
end;


{ TInterrogatorNode }

procedure TInterrogatorNode.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := False;
  ProtocolSupportedProtocols.Datagram := False;
  ProtocolSupportedProtocols.EventExchange := False;
  ProtocolSupportedProtocols.SimpleNodeInfo := False;
  ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := False;
  ProtocolSupportedProtocols.TractionControl := False;
  ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := False;
  ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := False;
  ProtocolSupportedProtocols.TractionFunctionConfiguration := False;

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
  SerialLink.OnComPortLogIn := @OnComPortLogIn;
  SerialLink.OnComPortLogOut := @OnComPortLogOut;

  Worker := TLccMessage.Create;
  Worker.CheckNodeIDsBeforeDelivery := True;
  AckWorker := TLccMessage.Create;

  ConnectionFactory.OnLccMessageSend := @OnConnectionFactorySendMessage;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SerialLink.Free;
  Worker.Free;
  AckWorker.Free;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  DisconnectSerialLink
end;

procedure TForm1.ButtonComPortConnectClick(Sender: TObject);
var
  AComPort: string;
begin
  if Assigned(SerialLink.Thread) then
    if SerialLink.Thread.Connected then
    begin
      DisconnectSerialLink;
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

procedure TForm1.ComboBoxConfigMemAddressSpaceChange(Sender: TObject);
begin

end;

procedure TForm1.ButtonRefreshComPortClick(Sender: TObject);
begin
  ComboBoxComPorts.Items.DelimitedText := GetSerialPortNames;
end;

procedure TForm1.ButtonClearMemoClick(Sender: TObject);
begin
  MemoComPort.Clear;
end;

procedure TForm1.ButtonVerifyNodesGlobalClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComboBoxNodeList.Items.Count - 1 do
    ComboBoxNodeList.Items.Objects[i].Free;

  ComboBoxNodeList.Clear;

  if Assigned(Node) then
  begin
    Worker.LoadVerifyNodeID(Node.NodeID, Node.AliasID, NULL_NODE_ID);

    SendWorkerMessage(Worker);

  end;
end;

procedure TForm1.ButtonSendConfigMemConfigOptionsClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     ClearConfigMemOperation;
     Worker.LoadConfigMemOptions(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias);
     SendWorkerMessage(Worker);
  end else
    ShowNoTargetMessage;
end;

procedure TForm1.ButtonConfigMemAddressSpaceInfoClick(Sender: TObject);

  function IndexToAddressSpace(AnIndex: Integer): Byte;
  begin
    Result := $FF - AnIndex;
  end;

begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     AddressSpaceEnumerating := False;
     AddressSpaceEnumeratingWellKnown := False;
     AddressSpaceQueried := IndexToAddressSpace(ComboBoxConfigMemAddressSpace.ItemIndex);
     SendAddressSpaceQuery(AddressSpaceQueried);
  end else
    ShowNoTargetMessage;
end;

procedure TForm1.ButtonConfigMemAddressSpaceInfoWellKnownClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     AddressSpaceEnumeratingWellKnown := True;
     AddressSpaceEnumerating := False;
     TreeViewConfigMemAddressSpaceInfo.Items.Clear;
     AddressSpaceQueried := $FF;
     SendAddressSpaceQuery(AddressSpaceQueried);
  end else
    ShowNoTargetMessage;
end;

procedure TForm1.ButtonConfigMemClearAddressInfoClick(Sender: TObject);
begin
  TreeViewConfigMemAddressSpaceInfo.Items.Clear;
end;

procedure TForm1.ButtonConfigMemOpClearClick(Sender: TObject);
begin
  ClearConfigMemOperation;
end;

procedure TForm1.ButtonMemConfgAddressSpaceInfoCancelClick(Sender: TObject);
begin
  AddressSpaceEnumerating := False;
  AddressSpaceEnumeratingWellKnown := False;
end;

procedure TForm1.ButtonConfigMemAddressSpaceInfoAllClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     AddressSpaceEnumerating := True;
     AddressSpaceEnumeratingWellKnown := False;
     TreeViewConfigMemAddressSpaceInfo.Items.Clear;
     AddressSpaceQueried := $FF;
     SendAddressSpaceQuery(AddressSpaceQueried);
  end else
    ShowNoTargetMessage;
end;

procedure TForm1.ButtonCreateNodeClick(Sender: TObject);
begin

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ComboBoxComPorts.Items.Delimiter := ';';
  ComboBoxComPorts.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  ComboBoxComPorts.ItemIndex := 2;
end;

function TForm1.GetNode: TLccNode;
begin
  if NodeManager.Nodes.Count > 0 then
    Result := NodeManager.Node[0]
  else
    Result := nil;
end;

function TForm1.GetTargetNode: TNodeInfoObject;
begin
  if ComboBoxNodeList.ItemIndex < 0 then
  begin
    result := nil;
    exit;
  end;

   Result := ComboBoxNodeList.Items.Objects[ComboBoxNodeList.ItemIndex] as TNodeInfoObject;
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

  Worker.LoadByGridConnectStr(GridConnectString);

  // Always handle this message to load the selection of nodes
  if Worker.MTI = MTI_VERIFIED_NODE_ID_NUMBER then
    HandleVerifiedNode(Worker);

  if not Assigned(TargetNode) then
    Exit;

  if not (TargetNode.Alias = Worker.SourceAlias) then
    Exit;

  case (Worker.MTI) of

    MTI_PROTOCOL_SUPPORT_REPLY:
    begin

    end;

    MTI_PRODUCER_IDENTIFIED_CLEAR:
    begin
    end;

    MTI_PRODUCER_IDENTIFIED_SET:
    begin
    end;

    MTI_PRODUCER_IDENTIFIED_UNKNOWN:
    begin
    end;

    MTI_CONSUMER_IDENTIFIED_CLEAR:
    begin
    end;

    MTI_CONSUMER_IDENTIFIED_SET:
    begin
    end;

    MTI_CONSUMER_IDENTIFIED_UNKNOWN:
    begin
    end;

    MTI_SIMPLE_NODE_INFO_REPLY:
    begin
    end;

    MTI_DATAGRAM:
    begin

      SendAck(Worker);

      case Worker.DataArray[0] of

        DATAGRAM_PROTOCOL_CONFIGURATION:
        begin

          case Worker.DataArray[1] of

            MCP_OP_GET_CONFIG_OPTIONS_REPLY:
            begin
               HandleGetConfigOptionsReply(Worker);
            end;
            MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY:
            begin
               HandleGetAddressSpaceInfoPresentReply(Worker);
            end;
            MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY:
            begin
               HandleGetAddressSpaceInfoNotPresentReply(Worker);
            end;

            MCP_OP_LOCK_REPLY:
            begin

            end;
            MCP_OP_GET_UNIQUEID_REPLY:
            begin

            end;
            MC_OP_UNFREEZE:
            begin

            end;
            MCP_OP_UPDATE_COMPLETE:
            begin

            end;

          end;
        end;
      end;
    end;
  end;

end;

procedure TForm1.OnComPortError(Sender: TObject; ErrorString: string; ErrorCode: word);
begin

  ShowMessage('ComPort Error: ' + ErrorString + '.  Error Code: ' + IntToStr(ErrorCode));

end;

procedure TForm1.OnComPortLogIn(Sender: TObject);
begin
  NodeManager.AddNodeByClass('', TInterrogatorNode, True, NULL_NODE_ID);
end;

procedure TForm1.OnComPortLogOut(Sender: TObject);
begin

end;

procedure TForm1.OnConnectionFactorySendMessage(Sender: TObject;
  LccMessage: TLccMessage);
begin

  SerialLink.SendString(LccMessage.ConvertToGridConnectStr(''));

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
  ButtonVerifyNodesGlobalClick(Self);
end;

procedure TForm1.OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
begin
  LabelNodeID.Caption := '[None]';
end;

procedure TForm1.OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);
begin

  LabelAliasID.Caption := '[None]';
  LabelNodeID.Caption := '[None]';

end;

procedure TForm1.OnComPortSendMessage(Sender: TObject; var GridConnectStyleMessage: string);
begin

end;

procedure TForm1.ShowNoTargetMessage;
begin
  ShowMessage('There is no target Node selected... please select a node to test');
end;

procedure TForm1.SendWorkerMessage(AWorker: TLccMessage);
begin
  OnConnectionFactorySendMessage(Self, AWorker);
end;

procedure TForm1.SendAck(Message: TLccMessage);
begin
  AckWorker.LoadDatagramAck(Message.DestID, Message.DestAlias, Message.SourceID, Message.SourceAlias, True, False, 0);
  SendWorkerMessage(AckWorker);
end;

procedure TForm1.SendAddressSpaceQuery(AddressSpace: Byte);
begin
  Worker.LoadConfigMemAddressSpaceInfo(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias, AddressSpace);
  SendWorkerMessage(Worker);
end;

procedure TForm1.ClearConfigMemOperation;
begin
  EditConfigMemHighestAddressSpace.Clear;
  EditConfigMemLowestAddressSpace.Clear;
  EditCongMemOpNameString.Clear;
  LabelConfigMemHighestAddressSpaceName.Caption := '';
  LabelConfigMemLowestAddressSpaceName.Caption := '';
  CheckBoxConfigMemOpAllBitsZero.Checked := False;
  CheckBoxConfigMemReserved0x02.Checked := False;
  CheckBoxConfigMemReserved0x10.Checked := False;
  CheckBoxConfigMemReserved0x20.Checked := False;
  CheckBoxConfigMemReserved0x40.Checked := False;
  CheckBoxConfigMemReserved0x80.Checked := False;
  CheckBoxConfigMemStreamReadWriteSupport.Checked := False;
  CheckBoxConfigMemSupportRead0xFB.Checked := False;
  CheckBoxConfigMemSupportRead0xFC.Checked := False;
  CheckBoxConfigMemUnalignedRead.Checked := False;
  CheckBoxConfigMemSupportWrite0xFB.Checked := False;
  CheckBoxConfigMemWriteLengthAllOtherBitsZero.Checked := False;
  CheckBoxConfigMemWriteUnderMask.Checked := False;
  CheckBoxConfigMemUnalignedWrite.Checked := False;
end;

procedure TForm1.DisconnectSerialLink;
begin
  NodeManager.Clear;
  Sleep(500);
  SerialLink.Disconnect;
end;

procedure TForm1.HandleVerifiedNode(AWorker: TLccMessage);
var
  NodeID: TNodeID;
  Str: string;
  NodeObj: TNodeInfoObject;
begin

  NodeID := NULL_NODE_ID;
  NodeID := Worker.ExtractDataBytesAsNodeID(0, NodeID);

  NodeObj :=  TNodeInfoObject.Create(Worker.ExtractDataBytesAsNodeID(0, NodeID), Worker.SourceAlias);
  Str := '[0x' + IntToHex(Worker.SourceAlias) + '] 0x' + NodeIDToString(NodeID, True);

  ComboBoxNodeList.Items.AddObject(Str, NodeObj);

  if ComboBoxNodeList.ItemIndex < 0 then
    ComboBoxNodeList.ItemIndex := 0;
end;

procedure TForm1.HandleGetConfigOptionsReply(AWorker: TLccMessage);
var
  Commands: Word;
  StringLen: LongInt;
  WriteLengths: Byte;
begin

  Commands := AWorker.ExtractDataBytesAsWord(2);
  CheckBoxConfigMemOpAllBitsZero.Checked := Commands and (not $EE00) = 0;
  CheckBoxConfigMemSupportRead0xFC.Checked:= Commands and MCO_ACDI_MFG_READS = MCO_ACDI_MFG_READS;
  CheckBoxConfigMemSupportRead0xFB.Checked := Commands and MCO_ACDI_USER_READS = MCO_ACDI_USER_READS;
  CheckBoxConfigMemSupportWrite0xFB.Checked := Commands and MCO_ACDI_USER_WRITES = MCO_ACDI_USER_WRITES;
  CheckBoxConfigMemUnalignedRead.Checked := Commands and MCO_UNALIGNED_READS = MCO_UNALIGNED_READS;
  CheckBoxConfigMemUnalignedWrite.Checked := Commands and MCO_UNALIGNED_WRITES = MCO_UNALIGNED_WRITES;
  CheckBoxConfigMemWriteUnderMask.Checked := Commands and MCO_WRITE_UNDER_MASK = MCO_WRITE_UNDER_MASK;

  WriteLengths := AWorker.DataArray[4];
  CheckBoxConfigMemWriteLengthAllOtherBitsZero.Checked := WriteLengths and (not $F3) = 0;;
  CheckBoxConfigMemReserved0x80.Checked := WriteLengths and $80 <> 0;
  CheckBoxConfigMemReserved0x40.Checked := WriteLengths and $40 <> 0;
  CheckBoxConfigMemReserved0x20.Checked := WriteLengths and $20 <> 0;
  CheckBoxConfigMemReserved0x10.Checked := WriteLengths and $10 <> 0;
  CheckBoxConfigMemReserved0x02.Checked := WriteLengths and $02 <> 0;
  CheckBoxConfigMemStreamReadWriteSupport.Checked := WriteLengths and $01 <> 0;

  EditConfigMemHighestAddressSpace.Text := '0x' + IntToHex( AWorker.DataArray[5], 2);

  if AWorker.DataCount > 6 then
  begin
    EditConfigMemLowestAddressSpace.Text := '0x' + IntToHex( AWorker.DataArray[6], 2);
    LabelConfigMemLowestAddressSpaceName.Caption := AddressSpaceToStr(AWorker.DataArray[6]);
    if AWorker.DataCount > 7 then
      EditCongMemOpNameString.Text := Worker.ExtractDataBytesAsString(7, StringLen)
    else
      EditCongMemOpNameString.Text := '[not included in message]';
  end else
  begin
     EditConfigMemLowestAddressSpace.Text := '0x00';
     LabelConfigMemLowestAddressSpaceName.Caption := AddressSpaceToStr(0);
  end;

  LabelConfigMemHighestAddressSpaceName.Caption := AddressSpaceToStr(AWorker.DataArray[5]);


end;

procedure TForm1.HandleGetAddressSpaceInfoPresentReply(AWorker: TLccMessage);

  procedure HandleAddressReadWrite(ATreeNode: TTreeNode);
  begin
    if AWorker.DataArray[7] and $01 = $01 then
    begin
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Address is Writable');
    end else
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Address is Read Only');
  end;

  procedure HandleLowAddressPresent(ATreeNode: TTreeNode);
  var
    ACount: Integer;
  begin
    ACount := 0;

    TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Lowest Address: 0x' + IntToHex(AWorker.ExtractDataBytesAsDWord(8), 4));

    HandleAddressReadWrite(ATreeNode);

    if AWorker.DataCount > 12 then
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Description: ' + AWorker.ExtractDataBytesAsString(12, ACount))
    else
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Description: [None]');

  end;

  procedure HandleLowAddressNotPresent(ATreeNode: TTreeNode);
    var
    ACount: Integer;
  begin
    ACount := 0;

    TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Lowest Address: Assumed to be 0x00000000');

    HandleAddressReadWrite(ATreeNode);

    if AWorker.DataCount > 8 then
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Description: ' + AWorker.ExtractDataBytesAsString(8, ACount))
    else
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Description: [None]');

  end;

var
  TreeNode: TTreeNode;
begin

  TreeNode := TreeViewConfigMemAddressSpaceInfo.Items.AddChild(nil, 'Address Space 0x' + IntToHex(AddressSpaceQueried) + ' Present');

  TreeViewConfigMemAddressSpaceInfo.Items.AddChild(TreeNode, 'Space Reported: 0x' + IntToHex(AWorker.DataArray[2], 2));
  TreeViewConfigMemAddressSpaceInfo.Items.AddChild(TreeNode, 'Highest Address: 0x' + IntToHex(AWorker.ExtractDataBytesAsDWord(3), 4));

  if AWorker.DataArray[7] and $02 = $02 then
    HandleLowAddressPresent(TreeNode)
  else
    HandleLowAddressNotPresent(TreeNode);

  if AddressSpaceEnumeratingWellKnown then
  begin
    AddressSpaceQueried := AddressSpaceQueried - 1;
    if AddressSpaceQueried <= ADDRESS_SPACE_FUNCTION_MEMORY then
       AddressSpaceEnumeratingWellKnown := False;
    SendAddressSpaceQuery(AddressSpaceQueried);
  end;

  if AddressSpaceEnumerating then
  begin
    AddressSpaceQueried := AddressSpaceQueried - 1;
    if AddressSpaceQueried <= 0 then
       AddressSpaceEnumerating := False;
    SendAddressSpaceQuery(AddressSpaceQueried);
  end;

end;

procedure TForm1.HandleGetAddressSpaceInfoNotPresentReply(AWorker: TLccMessage);
begin

  TreeViewConfigMemAddressSpaceInfo.Items.AddChild(nil, 'Address Space 0x' + IntToHex(AddressSpaceQueried) + ' Not Present');

  if AddressSpaceEnumeratingWellKnown then
  begin
    AddressSpaceQueried := AddressSpaceQueried - 1;
    if AddressSpaceQueried <= ADDRESS_SPACE_FUNCTION_MEMORY then
       AddressSpaceEnumeratingWellKnown := False;
    SendAddressSpaceQuery(AddressSpaceQueried);
  end;

  if AddressSpaceEnumerating then
  begin
    AddressSpaceQueried := AddressSpaceQueried - 1;
    if AddressSpaceQueried <= 0 then
       AddressSpaceEnumerating := False;
    SendAddressSpaceQuery(AddressSpaceQueried);
  end;

end;

end.

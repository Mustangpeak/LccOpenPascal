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

  { TStateVariablesAddressSpaceMessage }

  TStateVariablesAddressSpaceMessage = class

  private
    FEnumerating: Boolean;
    FEnumeratingWellKnown: Boolean;
    FQueried: Byte;

  public

    property Queried: Byte read FQueried write FQueried;
    property Enumerating: Boolean read FEnumerating write FEnumerating;
    property EnumeratingWellKnown: Boolean read FEnumeratingWellKnown write FEnumeratingWellKnown;

  end;

  { TStateVariablesReadDatagram }

  TStateVariablesReadDatagram = class

  private
    FData: TLccDynamicByteArray;
    FWaitingForAck: Boolean;
  public

    property WaitingForAck: Boolean read FWaitingForAck write FWaitingForAck;
    property Data: TLccDynamicByteArray read FData write FData;
  end;

  { StateVariablesTWriteDatagram }

  StateVariablesTWriteDatagram = class

  private
    FData: TLccDynamicByteArray;
    FWaitingForAck: Boolean;
   public

    property WaitingForAck: Boolean read FWaitingForAck write FWaitingForAck;
    property Data: TLccDynamicByteArray read FData write FData;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ButtonDatagramRead: TButton;
    ButtonDatagram_Write: TButton;
    ButtonDatagramWrite_FindWritableAddressSpaces: TButton;
    ButtonDatagramRead_ScanAddressSpacesCancel: TButton;
    ButtonDatagramRead_FindWritableAddressSpaces: TButton;
    ButtonConfigMemAddressSpaceInfoWellKnown: TButton;
    ButtonConfigMemOpClear: TButton;
    ButtonDatagramWrite_ScanAddressSpacesCancel: TButton;
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
    CheckBoxDatagramWrite_AckOk: TCheckBox;
    CheckBoxDatagramWrite_AckRejected: TCheckBox;
    CheckBoxDatagramWrite_AckReplyPending: TCheckBox;
    CheckBoxDatagramWrite_AckReservedBitsClear: TCheckBox;
    CheckBoxDatagramRead_UseDots: TCheckBox;
    CheckBoxDatagramRead_AckReservedBitsClear: TCheckBox;
    CheckBoxDatagramRead_AckReplyPending: TCheckBox;
    CheckBoxDatagramRead_AckOk: TCheckBox;
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
    CheckBoxDatagramRead_AckRejected: TCheckBox;
    CheckBoxDatagramWrite_UseDots: TCheckBox;
    ComboBoxDatagramWrite_ReadableAddressSpaces: TComboBox;
    ComboBoxDatagramRead_WriteableAddressSpaces: TComboBox;
    ComboBoxDatagramRead_ReadableAddressSpaces: TComboBox;
    ComboBoxConfigMemAddressSpace: TComboBox;
    ComboBoxDatagramWrite_WriteableAddressSpaces: TComboBox;
    ComboBoxNodeList: TComboBox;
    ComboBoxComPorts: TComboBox;
    EditDatagramWrite: TEdit;
    EditDatagramWrite_AckEstimatedWaitTime: TEdit;
    EditDatagramWrite_AckAdditionalInfo: TEdit;
    EditDatagramWrite_AckDataBytes: TEdit;
    EditDatagramWrite_AckErrorCode: TEdit;
    EditDatagramWrite_Count: TEdit;
    EditDatagramRead_RawData: TEdit;
    EditDatagramRead_AckDataBytes: TEdit;
    EditDatagramRead_AckAdditionalInfo: TEdit;
    EditDatagramRead_AckErrorCode: TEdit;
    EditDatagramRead_AckEstimatedWaitTime: TEdit;
    EditDatagramWrite_RawData: TEdit;
    EditDatagramRead_Space: TEdit;
    EditDatagramWrite_Space: TEdit;
    EditDatagramRead_StartAddress: TEdit;
    EditDatagramRead_Count: TEdit;
    EditDatagramRead: TEdit;
    EditCongMemOpNameString: TEdit;
    EditConfigMemHighestAddressSpace: TEdit;
    EditConfigMemLowestAddressSpace: TEdit;
    EditDatagramWrite_StartAddress: TEdit;
    GroupBoxDatagramRead_AddressSpaceScan: TGroupBox;
    GroupBoxDatagramWrite_AddressSpaceScan: TGroupBox;
    GroupBoxDatagramRead: TGroupBox;
    GroupBoxDatagram_Write: TGroupBox;
    GroupBoxDatagram_ReadAck: TGroupBox;
    GroupBoxConfigMemAddressSpaceInfo: TGroupBox;
    GroupBoxConfMemOptions: TGroupBox;
    GroupBoxConfigMemWriteLenghts: TGroupBox;
    GroupBoxConfigMemAvailableCommands: TGroupBox;
    GroupBoxDatagramWrite_Ack: TGroupBox;
    Label1: TLabel;
    LabelDatagramWrite_AdditionalInfo: TLabel;
    LabelDatagramWrite_Address: TLabel;
    LabelDatagramWrite_BytesOfData: TLabel;
    LabelDatagramWrite_Count: TLabel;
    LabelDatagramWrite_ErrorCode: TLabel;
    LabelDatagramWrite_RawData: TLabel;
    LabelDatagramWrite_ReadableAddressSpaces: TLabel;
    LabelDatagramWrite_Space: TLabel;
    LabelDatagramRead_Waittime: TLabel;
    LabelDatagramRead_AdditionalInfo: TLabel;
    LabelDatagramRead_ErrorCode: TLabel;
    LabelDatagramRead_RawData: TLabel;
    LabelDatagramWrite_Waittime: TLabel;
    LabelDatagramRead_WritableAddressSpaces: TLabel;
    LabelDatagramRead_ReadableAddressSpaces: TLabel;
    LabelDatagramRead_BytesOfData: TLabel;
    LabelDatagramRead_Space: TLabel;
    LabelDatagramRead_Address: TLabel;
    LabelDatagramRead_Count: TLabel;
    LabelConfigSpaceAddressInfo: TLabel;
    LabelConfigMemLowestAddressSpaceName: TLabel;
    LabelConfigMemHighestAddressSpaceName: TLabel;
    LabelConfigMemOpNameString: TLabel;
    LabelConfigMemeHighestAddressSpace: TLabel;
    LabelConfigMemLowestAddressSpace: TLabel;
    LabelAliasID: TLabel;
    LabelDatagramWrite_WritableAddressSpaces: TLabel;
    LabelNodeID: TLabel;
    MemoComPort: TMemo;
    PageControlMain: TPageControl;
    PanelNodeList: TPanel;
    PanelHeader: TPanel;
    PanelMain: TPanel;
    PanelLoggingHeader: TPanel;
    PanelTabDatagramRead: TPanel;
    PanelTabDatagramWrite: TPanel;
    PanelTabEvents: TPanel;
    PanelTabSheetConfigMem: TPanel;
    PanelMemo: TPanel;
    PanelTabMessageNetwork: TPanel;
    RadioGroupDatagramWrite_MessageOptions: TRadioGroup;
    RadioGroupDatagramRead_ViewOptions: TRadioGroup;
    RadioGroupDatagramRead_MessageOptions: TRadioGroup;
    RadioGroupDatagramWrite_ViewOptions: TRadioGroup;
    Splitter1: TSplitter;
    StatusBarMain: TStatusBar;
    TabSheetDatagramWrite: TTabSheet;
    TabSheetEvents: TTabSheet;
    TabSheetMessageNetwork: TTabSheet;
    TabSheetConfigMem: TTabSheet;
    TabSheetDatagramRead: TTabSheet;
    TreeViewConfigMemAddressSpaceInfo: TTreeView;
    procedure ButtonConfigMemAddressSpaceInfoAllClick(Sender: TObject);
    procedure ButtonConfigMemAddressSpaceInfoClick(Sender: TObject);
    procedure ButtonConfigMemAddressSpaceInfoWellKnownClick(Sender: TObject);
    procedure ButtonConfigMemClearAddressInfoClick(Sender: TObject);
    procedure ButtonConfigMemOpClearClick(Sender: TObject);
    procedure ButtonDatagramRead_FindWritableAddressSpacesClick(Sender: TObject);
    procedure ButtonDatagramReadClick(Sender: TObject);
    procedure ButtonDatagramRead_ScanAddressSpacesCancelClick(Sender: TObject);
    procedure ButtonDatagram_WriteClick(Sender: TObject);
    procedure ButtonMemConfgAddressSpaceInfoCancelClick(Sender: TObject);
    procedure ButtonSendConfigMemConfigOptionsClick(Sender: TObject);
    procedure ButtonVerifyNodesAddressedClick(Sender: TObject);
    procedure ButtonVerifyNodesGlobalClick(Sender: TObject);
    procedure ButtonClearMemoClick(Sender: TObject);
    procedure ButtonCreateNodeClick(Sender: TObject);
    procedure ButtonRefreshComPortClick(Sender: TObject);
    procedure ButtonComPortConnectClick(Sender: TObject);
    procedure CheckBoxDatagramRead_UseDotsChange(Sender: TObject);
    procedure CheckBoxDatagramWrite_UseDotsChange(Sender: TObject);
    procedure ComboBoxConfigMemAddressSpaceChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelTabDatagramReadClick(Sender: TObject);
    procedure RadioGroupDatagramRead_ViewOptionsClick(Sender: TObject);
    procedure RadioGroupDatagramWrite_ViewOptionsClick(Sender: TObject);

  private
    FAckWorker: TLccMessage;
    FStatesAddressSpace: TStateVariablesAddressSpaceMessage;
    FStatesDatagramRead: TStateVariablesReadDatagram;
    FStatesDatagramWrite: StateVariablesTWriteDatagram;
    FEmulateCANBus: boolean;
    FNodeManager: TLccNodeManager;

    FSerialLink: TLccComPort;
    FTargetReplyEdit: TEdit;
    FWorkerMessage: TLccMessage;
    function GetNode: TLccNode;
    function GetTargetNode: TNodeInfoObject;

  protected

    property StatesAddressSpace: TStateVariablesAddressSpaceMessage read FStatesAddressSpace write FStatesAddressSpace;
    property StatesDatagramRead: TStateVariablesReadDatagram read FStatesDatagramRead write FStatesDatagramRead;
    property StatesDatagramWrite: StateVariablesTWriteDatagram read FStatesDatagramWrite write FStatesDatagramWrite;

    // Datagram Read/Write States
    property TargetReplyEdit: TEdit read FTargetReplyEdit write FTargetReplyEdit;

    procedure OnComPortRecieveGridConnectStr(Sender: TObject; ReceiveGridConnectStr: ansistring);
    procedure OnComPortRecieveMessage(Sender: TObject; ReceiveMessage: TLccMessage);
    procedure OnComPortAssemblerErrorReply(Sender: TObject; ReceiveMessage: TLccMessage);
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
    procedure SendMessage(AMessage: TLccMessage);
    procedure SendAck(Message: TLccMessage);
    procedure SendAddressSpaceQuery(AddressSpace: Byte);
    procedure ClearConfigMemOperation;
    procedure DisconnectSerialLink;
    procedure PrintReadDataArray;
    procedure PrintWriteDataArray;

    procedure HandleVerifiedNode(ReceivedMessage: TLccMessage);
    procedure HandleGetConfigOptionsReply(ReceivedMessage: TLccMessage);
    procedure HandleReadReply(ReceivedMessage: TLccMessage);
    procedure HandleReadReplyCDI(ReceivedMessage: TLccMessage);
    procedure HandleReadReplyAll(ReceivedMessage: TLccMessage);
    procedure HandleReadReplyConfig(ReceivedMessage: TLccMessage);
    procedure HandleReadReplyFailure(ReceivedMessage: TLccMessage);
    procedure HandleReadReplyFailureCDI(ReceivedMessage: TLccMessage);
    procedure HandleReadReplyFailureAll(ReceivedMessage: TLccMessage);
    procedure HandleReadReplyFailureConfig(ReceivedMessage: TLccMessage);
    procedure HandleWriteReply(ReceivedMessage: TLccMessage);
    procedure HandleWriteReplyCDI(ReceivedMessage: TLccMessage);
    procedure HandleWriteReplyAll(ReceivedMessage: TLccMessage);
    procedure HandleWriteReplyConfig(ReceivedMessage: TLccMessage);
    procedure HandleWriteReplyFailure(ReceivedMessage: TLccMessage);
    procedure HandleWriteReplyFailureCDI(ReceivedMessage: TLccMessage);
    procedure HandleWriteReplyFailureAll(ReceivedMessage: TLccMessage);
    procedure HandleWriteReplyFailureConfig(ReceivedMessage: TLccMessage);
    procedure HandleDatagramAckOk(ReceivedMessage: TLccMessage);
    procedure HandleDatagramAckRejected(ReceivedMessage: TLccMessage);

    procedure HandleGetAddressSpaceInfoPresentReply(ReceivedMessage: TLccMessage);
    procedure HandleGetAddressSpaceInfoNotPresentReply(ReceivedMessage: TLccMessage);



  public

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property SerialLink: TLccComPort read FSerialLink write FSerialLink;

    property EmulateCANBus: boolean read FEmulateCANBus write FEmulateCANBus;

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
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
  SerialLink.OnComPortGridConnectString := @OnComPortRecieveGridConnectStr;
  SerialLink.OnComPortAssemblerErrorReply := @OnComPortAssemblerErrorReply;
  SerialLink.OnComPortMessage := @OnComPortRecieveMessage;
  SerialLink.OnComPortError := @OnComPortError;
  SerialLink.OnComPortLogIn := @OnComPortLogIn;
  SerialLink.OnComPortLogOut := @OnComPortLogOut;

  AckWorker := TLccMessage.Create;
  WorkerMessage := TLccMessage.Create;

  StatesAddressSpace := TStateVariablesAddressSpaceMessage.Create;
  StatesDatagramRead := TStateVariablesReadDatagram.Create;
  StatesDatagramWrite := StateVariablesTWriteDatagram.Create;

  ConnectionFactory.OnLccMessageSend := @OnConnectionFactorySendMessage;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SerialLink.Free;
  AckWorker.Free;
  WorkerMessage.Free;
  StatesAddressSpace.Free;
  StatesDatagramRead.Free;
  StatesDatagramWrite.Free;
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

procedure TForm1.CheckBoxDatagramRead_UseDotsChange(Sender: TObject);
begin
  PrintReadDataArray;
end;

procedure TForm1.CheckBoxDatagramWrite_UseDotsChange(Sender: TObject);
begin
  PrintWriteDataArray;
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
    WorkerMessage.LoadVerifyNodeID(Node.NodeID, Node.AliasID, NULL_NODE_ID);

    SendMessage(WorkerMessage);

  end;
end;

procedure TForm1.ButtonSendConfigMemConfigOptionsClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     ClearConfigMemOperation;
     WorkerMessage.LoadConfigMemOptions(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias);
     SendMessage(WorkerMessage);
  end else
    ShowNoTargetMessage;
end;

procedure TForm1.ButtonVerifyNodesAddressedClick(Sender: TObject);

begin


end;

procedure TForm1.ButtonConfigMemAddressSpaceInfoClick(Sender: TObject);

  function IndexToAddressSpace(AnIndex: Integer): Byte;
  begin
    Result := $FF - AnIndex;
  end;

begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     StatesAddressSpace.Enumerating := False;
     StatesAddressSpace.EnumeratingWellKnown := False;
     TreeViewConfigMemAddressSpaceInfo.Items.Clear;
     StatesAddressSpace.Queried := IndexToAddressSpace(ComboBoxConfigMemAddressSpace.ItemIndex);
     SendAddressSpaceQuery(StatesAddressSpace.Queried);
  end else
    ShowNoTargetMessage;
end;

procedure TForm1.ButtonConfigMemAddressSpaceInfoWellKnownClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     StatesAddressSpace.EnumeratingWellKnown := True;
     StatesAddressSpace.Enumerating := False;
     TreeViewConfigMemAddressSpaceInfo.Items.Clear;
     StatesAddressSpace.Queried := $FF;
     SendAddressSpaceQuery(StatesAddressSpace.Queried);
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

procedure TForm1.ButtonDatagramRead_FindWritableAddressSpacesClick(Sender: TObject);
begin
  ComboBoxDatagramRead_ReadableAddressSpaces.Clear;
  ComboBoxDatagramRead_WriteableAddressSpaces.Clear;
  ComboBoxDatagramWrite_ReadableAddressSpaces.Clear;
  ComboBoxDatagramWrite_WriteableAddressSpaces.Clear;
  ButtonConfigMemAddressSpaceInfoAllClick(Sender);
end;

procedure TForm1.ButtonDatagramReadClick(Sender: TObject);
var
  AddressSpace: Byte;
begin

  if Assigned(TargetNode) and Assigned(Node) then
  begin

    CheckBoxDatagramRead_AckOk.Checked := False;
    CheckBoxDatagramRead_AckRejected.Checked := False;
    CheckBoxDatagramRead_AckReplyPending.Checked := False;
    CheckBoxDatagramRead_AckReservedBitsClear.Checked := False;
    EditDatagramRead_AckEstimatedWaitTime.Text := '';
    EditDatagramRead_AckErrorCode.Text := '';
    EditDatagramRead_AckAdditionalInfo.Text := '';
    EditDatagramRead_AckDataBytes.Text := '';
    EditDatagramRead_RawData.Text := '';
    EditDatagramRead.Text := '';

    StatesDatagramRead.WaitingForAck := True;


    WorkerMessage.SourceAlias := Node.AliasID;
    WorkerMessage.SourceID := Node.NodeID;
    WorkerMessage.DestAlias := TargetNode.Alias;
    WorkerMessage.DestID := TargetNode.NodeID;
    WorkerMessage.MTI := MTI_DATAGRAM;
    WorkerMessage.DataArrayIndexer[0] := DATAGRAM_PROTOCOL_CONFIGURATION;


    case RadioGroupDatagramRead_MessageOptions.ItemIndex of
      0: begin
            AddressSpace := UnknownStrToInt( EditDatagramRead_Space.Text);
            case AddressSpace of
              ADDRESS_SPACE_CONFIG_DEFINITION_INFO: WorkerMessage.DataArrayIndexer[1] := MCP_READ_CDI;
              ADDRESS_SPACE_ALL                   : WorkerMessage.DataArrayIndexer[1] := MCP_READ_ALL;
              ADDRESS_SPACE_CONFIG_MEMORY         : WorkerMessage.DataArrayIndexer[1] := MCP_READ_CONFIGURATION;
            else
               WorkerMessage.DataArrayIndexer[1] := MCP_READ;
            end;
      end;

      1: begin
           WorkerMessage.DataArrayIndexer[1] := MCP_READ;
      end;
    end;

    WorkerMessage.InsertDWordAsDataBytes( StrToInt( EditDatagramRead_StartAddress.Text), 2);

    if WorkerMessage.DataArrayIndexer[1] = MCP_READ then
    begin
       WorkerMessage.DataArrayIndexer[6] := UnknownStrToInt( EditDatagramRead_Space.Text);
       WorkerMessage.DataArrayIndexer[7] := UnknownStrToInt( EditDatagramRead_Count.Text);
       WorkerMessage.DataCount := 8;
    end else
    begin
      WorkerMessage.DataArrayIndexer[6] := UnknownStrToInt( EditDatagramRead_Count.Text);
      WorkerMessage.DataCount := 7;
    end;

    SendMessage(WorkerMessage);

  end else
    ShowNoTargetMessage;
end;

procedure TForm1.ButtonDatagramRead_ScanAddressSpacesCancelClick(Sender: TObject);
begin
  StatesAddressSpace.Enumerating := False;
end;

procedure TForm1.ButtonDatagram_WriteClick(Sender: TObject);
var
  AddressSpace: Byte;
begin

  if Assigned(TargetNode) and Assigned(Node) then
  begin

    CheckBoxDatagramWrite_AckOk.Checked := False;
    CheckBoxDatagramWrite_AckRejected.Checked := False;
    CheckBoxDatagramWrite_AckReplyPending.Checked := False;
    CheckBoxDatagramWrite_AckReservedBitsClear.Checked := False;
    EditDatagramWrite_AckEstimatedWaitTime.Text := '';
    EditDatagramWrite_AckErrorCode.Text := '';
    EditDatagramWrite_AckAdditionalInfo.Text := '';
    EditDatagramWrite_AckDataBytes.Text := '';
    EditDatagramWrite_RawData.Text := '';
    EditDatagramWrite.Text := '';

    StatesDatagramWrite.WaitingForAck := True;


    WorkerMessage.SourceAlias := Node.AliasID;
    WorkerMessage.SourceID := Node.NodeID;
    WorkerMessage.DestAlias := TargetNode.Alias;
    WorkerMessage.DestID := TargetNode.NodeID;
    WorkerMessage.MTI := MTI_DATAGRAM;
    WorkerMessage.DataArrayIndexer[0] := DATAGRAM_PROTOCOL_CONFIGURATION;


    case RadioGroupDatagramWrite_MessageOptions.ItemIndex of
      0: begin
            AddressSpace := UnknownStrToInt( EditDatagramRead_Space.Text);
            case AddressSpace of
              ADDRESS_SPACE_CONFIG_DEFINITION_INFO: WorkerMessage.DataArrayIndexer[1] := MCP_WRITE_CDI;
              ADDRESS_SPACE_ALL                   : WorkerMessage.DataArrayIndexer[1] := MCP_WRITE_ALL;
              ADDRESS_SPACE_CONFIG_MEMORY         : WorkerMessage.DataArrayIndexer[1] := MCP_WRITE_CONFIGURATION;
            else
               WorkerMessage.DataArrayIndexer[1] := MCP_WRITE;
            end;
      end;

      1: begin
           WorkerMessage.DataArrayIndexer[1] := MCP_WRITE;
      end;
    end;

    WorkerMessage.InsertDWordAsDataBytes( StrToInt( EditDatagramWrite_StartAddress.Text), 2);

    if WorkerMessage.DataArrayIndexer[1] = MCP_WRITE then
    begin
       WorkerMessage.DataArrayIndexer[6] := UnknownStrToInt( EditDatagramWrite_Space.Text);
       WorkerMessage.DataArrayIndexer[7] := UnknownStrToInt( EditDatagramWrite_Count.Text);
       WorkerMessage.DataCount := 8;
    end else
    begin
      WorkerMessage.DataArrayIndexer[6] := UnknownStrToInt( EditDatagramWrite_Count.Text);
      WorkerMessage.DataCount := 7;
    end;

    SendMessage(WorkerMessage);

  end else
    ShowNoTargetMessage;
end;

procedure TForm1.ButtonMemConfgAddressSpaceInfoCancelClick(Sender: TObject);
begin
  StatesAddressSpace.Enumerating := False;
  StatesAddressSpace.EnumeratingWellKnown := False;
end;

procedure TForm1.ButtonConfigMemAddressSpaceInfoAllClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     StatesAddressSpace.Enumerating := True;
     StatesAddressSpace.EnumeratingWellKnown := False;
     TreeViewConfigMemAddressSpaceInfo.Items.Clear;
     StatesAddressSpace.Queried := $FF;
     SendAddressSpaceQuery(StatesAddressSpace.Queried);
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

procedure TForm1.PanelTabDatagramReadClick(Sender: TObject);
begin

end;

procedure TForm1.RadioGroupDatagramRead_ViewOptionsClick(Sender: TObject);
begin
    PrintReadDataArray;
end;

procedure TForm1.RadioGroupDatagramWrite_ViewOptionsClick(Sender: TObject);
begin
   PrintWriteDataArray;
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

procedure TForm1.OnComPortRecieveGridConnectStr(Sender: TObject; ReceiveGridConnectStr: ansistring);
begin
  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Add('R: ' + ReceiveGridConnectStr)

  finally
    if MemoComPort.Lines.Count > MAX_LOGGING_LINES then
      MemoComPort.Lines.Delete(0);
    MemoComPort.Lines.EndUpdate;

    MemoComPort.SelStart := Length(MemoComPort.Lines.Text) - 1;
    MemoComPort.SelLength := 1;
    MemoComPort.SelLength := 0;
  end;
end;

procedure TForm1.OnComPortRecieveMessage(Sender: TObject; ReceiveMessage: TLccMessage);
begin

  // Always handle this message to load the selection of nodes
    if ReceiveMessage.MTI = MTI_VERIFIED_NODE_ID_NUMBER then
      HandleVerifiedNode(ReceiveMessage);

    if not Assigned(TargetNode) then
      Exit;

    if not (TargetNode.Alias = ReceiveMessage.SourceAlias) then
      Exit;

    case (ReceiveMessage.MTI) of

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

      MTI_DATAGRAM_OK_REPLY:
      begin
        HandleDatagramAckOk(ReceiveMessage);
      end;

      MTI_DATAGRAM_REJECTED_REPLY:
      begin
        HandleDatagramAckRejected(ReceiveMessage);
      end;

      MTI_DATAGRAM:
      begin

        SendAck(ReceiveMessage);

        case ReceiveMessage.DataArray[0] of

          DATAGRAM_PROTOCOL_CONFIGURATION:

            case ReceiveMessage.DataArray[1] of

              MCP_READ_REPLY:
              begin
                HandleReadReply(ReceiveMessage);
              end;
              MCP_READ_REPLY_CDI:
              begin
                HandleReadReplyCDI(ReceiveMessage);
              end;
              MCP_READ_REPLY_ALL:
              begin
                HandleReadReplyAll(ReceiveMessage);
              end;
              MCP_READ_REPLY_CONFIG:
              begin
                HandleReadReplyConfig(ReceiveMessage);
              end;

              MCP_READ_REPLY_FAILURE:
              begin
                HandleReadReplyFailure(ReceiveMessage);
              end;

              MCP_READ_REPLY_FAILURE_CDI:
              begin
                HandleReadReplyFailureCDI(ReceiveMessage);
              end;

              MCP_READ_REPLY_FAILURE_ALL:
              begin
                HandleReadReplyFailureAll(ReceiveMessage);
              end;

              MCP_READ_REPLY_FAILURE_CONFIG:
              begin
                HandleReadReplyFailureConfig(ReceiveMessage);
              end;


              MCP_WRITE_REPLY:
              begin
                HandleWriteReply(ReceiveMessage);
              end;
              MCP_WRITE_REPLY_CDI:
              begin
                HandleWriteReplyCDI(ReceiveMessage);
              end;
              MCP_WRITE_REPLY_ALL:
              begin
                HandleWriteReplyAll(ReceiveMessage);
              end;
              MCP_WRITE_REPLY_CONFIG:
              begin
                HandleWriteReplyConfig(ReceiveMessage);
              end;

              MCP_WRITE_REPLY_FAILURE:
              begin
                HandleWriteReplyFailure(ReceiveMessage);
              end;

              MCP_WRITE_REPLY_FAILURE_CDI:
              begin
                HandleWriteReplyFailureCDI(ReceiveMessage);
              end;

              MCP_WRITE_REPLY_FAILURE_ALL:
              begin
                HandleWriteReplyFailureAll(ReceiveMessage);
              end;

              MCP_WRITE_REPLY_FAILURE_CONFIG:
              begin
                 HandleWriteReplyFailureConfig(ReceiveMessage);
              end;


              MCP_OP_GET_CONFIG_OPTIONS_REPLY:
              begin
                 HandleGetConfigOptionsReply(ReceiveMessage);
              end;
              MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY:
              begin
                 HandleGetAddressSpaceInfoPresentReply(ReceiveMessage);
              end;
              MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY:
              begin
                 HandleGetAddressSpaceInfoNotPresentReply(ReceiveMessage);
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

procedure TForm1.OnComPortAssemblerErrorReply(Sender: TObject; ReceiveMessage: TLccMessage);
begin
  // TODO:  Signal user somehow that this occured
   SendMessage(ReceiveMessage);
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

procedure TForm1.OnConnectionFactorySendMessage(Sender: TObject; LccMessage: TLccMessage);
begin

  SerialLink.SendString(LccMessage.ConvertToGridConnectStr(#10));

  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Add('S: ' + LccMessage.ConvertToGridConnectStr(#10));

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

procedure TForm1.SendMessage(AMessage: TLccMessage);
begin
  OnConnectionFactorySendMessage(Self, AMessage);
end;

procedure TForm1.SendAck(Message: TLccMessage);
begin
  AckWorker.LoadDatagramAck(Message.DestID, Message.DestAlias, Message.SourceID, Message.SourceAlias, True, False, 0);
  SendMessage(AckWorker);
end;

procedure TForm1.SendAddressSpaceQuery(AddressSpace: Byte);
begin
  WorkerMessage.LoadConfigMemAddressSpaceInfo(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias, AddressSpace);
  SendMessage(WorkerMessage);
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

procedure TForm1.PrintReadDataArray();
begin
  case RadioGroupDatagramRead_ViewOptions.ItemIndex of
    0: EditDatagramRead.Text := ByteArrayAsHexStr(StatesDatagramRead.Data, CheckBoxDatagramRead_UseDots.Checked);
    1: EditDatagramRead.Text := ByteArrayAsDecStr(StatesDatagramRead.Data, CheckBoxDatagramRead_UseDots.Checked);
    2: EditDatagramRead.Text := ByteArrayAsCharStr(StatesDatagramRead.Data, CheckBoxDatagramRead_UseDots.Checked);
  end;
end;

procedure TForm1.PrintWriteDataArray;
begin
  case RadioGroupDatagramWrite_ViewOptions.ItemIndex of
    0: EditDatagramWrite.Text := ByteArrayAsHexStr(StatesDatagramWrite.Data, CheckBoxDatagramWrite_UseDots.Checked);
    1: EditDatagramWrite.Text := ByteArrayAsDecStr(StatesDatagramWrite.Data, CheckBoxDatagramWrite_UseDots.Checked);
    2: EditDatagramWrite.Text := ByteArrayAsCharStr(StatesDatagramWrite.Data, CheckBoxDatagramWrite_UseDots.Checked);
  end;
end;


procedure TForm1.HandleVerifiedNode(ReceivedMessage: TLccMessage);
var
  NodeID: TNodeID;
  Str: string;
  NodeObj: TNodeInfoObject;
begin

  NodeID := NULL_NODE_ID;
  NodeID := ReceivedMessage.ExtractDataBytesAsNodeID(0, NodeID);

  NodeObj :=  TNodeInfoObject.Create(ReceivedMessage.ExtractDataBytesAsNodeID(0, NodeID), ReceivedMessage.SourceAlias);
  Str := '[0x' + IntToHex(ReceivedMessage.SourceAlias) + '] 0x' + NodeIDToString(NodeID, True);

  ComboBoxNodeList.Items.AddObject(Str, NodeObj);

  if ComboBoxNodeList.ItemIndex < 0 then
    ComboBoxNodeList.ItemIndex := 0;
end;

procedure TForm1.HandleGetConfigOptionsReply(ReceivedMessage: TLccMessage);
var
  Commands: Word;
  StringLen: LongInt;
  WriteLengths: Byte;
begin

  StringLen := 0;

  Commands := ReceivedMessage.ExtractDataBytesAsWord(2);
  CheckBoxConfigMemOpAllBitsZero.Checked := Commands and (not $EE00) = 0;
  CheckBoxConfigMemSupportRead0xFC.Checked:= Commands and MCO_ACDI_MFG_READS = MCO_ACDI_MFG_READS;
  CheckBoxConfigMemSupportRead0xFB.Checked := Commands and MCO_ACDI_USER_READS = MCO_ACDI_USER_READS;
  CheckBoxConfigMemSupportWrite0xFB.Checked := Commands and MCO_ACDI_USER_WRITES = MCO_ACDI_USER_WRITES;
  CheckBoxConfigMemUnalignedRead.Checked := Commands and MCO_UNALIGNED_READS = MCO_UNALIGNED_READS;
  CheckBoxConfigMemUnalignedWrite.Checked := Commands and MCO_UNALIGNED_WRITES = MCO_UNALIGNED_WRITES;
  CheckBoxConfigMemWriteUnderMask.Checked := Commands and MCO_WRITE_UNDER_MASK = MCO_WRITE_UNDER_MASK;

  WriteLengths := ReceivedMessage.DataArray[4];
  CheckBoxConfigMemWriteLengthAllOtherBitsZero.Checked := WriteLengths and (not $F3) = 0;;
  CheckBoxConfigMemReserved0x80.Checked := WriteLengths and $80 <> 0;
  CheckBoxConfigMemReserved0x40.Checked := WriteLengths and $40 <> 0;
  CheckBoxConfigMemReserved0x20.Checked := WriteLengths and $20 <> 0;
  CheckBoxConfigMemReserved0x10.Checked := WriteLengths and $10 <> 0;
  CheckBoxConfigMemReserved0x02.Checked := WriteLengths and $02 <> 0;
  CheckBoxConfigMemStreamReadWriteSupport.Checked := WriteLengths and $01 <> 0;

  EditConfigMemHighestAddressSpace.Text := '0x' + IntToHex( ReceivedMessage.DataArray[5], 2);

  if ReceivedMessage.DataCount > 6 then
  begin
    EditConfigMemLowestAddressSpace.Text := '0x' + IntToHex( ReceivedMessage.DataArray[6], 2);
    LabelConfigMemLowestAddressSpaceName.Caption := AddressSpaceToStr(ReceivedMessage.DataArray[6]);
    if ReceivedMessage.DataCount > 7 then
      EditCongMemOpNameString.Text := ReceivedMessage.ExtractDataBytesAsString(7, StringLen)
    else
      EditCongMemOpNameString.Text := '[not included in message]';
  end else
  begin
     EditConfigMemLowestAddressSpace.Text := '0x00';
     LabelConfigMemLowestAddressSpaceName.Caption := AddressSpaceToStr(0);
  end;

  LabelConfigMemHighestAddressSpaceName.Caption := AddressSpaceToStr(ReceivedMessage.DataArray[5]);


end;

procedure TForm1.HandleReadReply(ReceivedMessage: TLccMessage);
begin
  // TODO: Set a user Flag for which version was used.....
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData);
  PrintReadDataArray;
end;

procedure TForm1.HandleReadReplyCDI(ReceivedMessage: TLccMessage);
begin
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData);
  PrintReadDataArray;
end;

procedure TForm1.HandleReadReplyAll(ReceivedMessage: TLccMessage);
begin
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData);
  PrintReadDataArray;
end;

procedure TForm1.HandleReadReplyConfig(ReceivedMessage: TLccMessage);
begin
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData);
  PrintReadDataArray;
end;

procedure TForm1.HandleReadReplyFailure(ReceivedMessage: TLccMessage);
begin
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData);
  PrintReadDataArray;
end;

procedure TForm1.HandleReadReplyFailureCDI(ReceivedMessage: TLccMessage);
begin
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData);
  PrintReadDataArray;
end;

procedure TForm1.HandleReadReplyFailureAll(ReceivedMessage: TLccMessage);
begin
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData);
  PrintReadDataArray;
end;

procedure TForm1.HandleReadReplyFailureConfig(ReceivedMessage: TLccMessage);
begin
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData);
  PrintReadDataArray;
end;

procedure TForm1.HandleWriteReply(ReceivedMessage: TLccMessage);
begin

end;

procedure TForm1.HandleWriteReplyCDI(ReceivedMessage: TLccMessage);
begin

end;

procedure TForm1.HandleWriteReplyAll(ReceivedMessage: TLccMessage);
begin

end;

procedure TForm1.HandleWriteReplyConfig(ReceivedMessage: TLccMessage);
begin

end;

procedure TForm1.HandleWriteReplyFailure(ReceivedMessage: TLccMessage);
begin

end;

procedure TForm1.HandleWriteReplyFailureCDI(ReceivedMessage: TLccMessage);
begin

end;

procedure TForm1.HandleWriteReplyFailureAll(ReceivedMessage: TLccMessage);
begin

end;

procedure TForm1.HandleWriteReplyFailureConfig(ReceivedMessage: TLccMessage);
begin

end;

procedure TForm1.HandleDatagramAckOk(ReceivedMessage: TLccMessage);
begin
  // Datagram Read Created the ACK
  if StatesDatagramRead.WaitingForAck then
  begin
    CheckBoxDatagramRead_AckOk.Checked := True;

    EditDatagramRead_AckDataBytes.Text := IntToStr(ReceivedMessage.DataCount);

    if ReceivedMessage.DataCount > 0 then
    begin
        CheckBoxDatagramRead_AckReplyPending.Checked := ReceivedMessage.DataArray[0] and ACK_REPLY_PENDING_MASK = ACK_REPLY_PENDING_MASK;
        EditDatagramRead_AckEstimatedWaitTime.Text := IntToStr(ReceivedMessage.DataArray[0] and ACK_REPLY_TIMEOUT_MASK);
        CheckBoxDatagramRead_AckReservedBitsClear.Checked :=  ReceivedMessage.DataArray[0] and ACK_REPLY_RESERVED_MASK = 0;
    end;
    StatesDatagramRead.WaitingForAck := False;
  end;

  // Datagram Write Created the ACK
  if StatesDatagramWrite.WaitingForAck then
  begin
    CheckBoxDatagramWrite_AckOk.Checked := True;

    EditDatagramWrite_AckDataBytes.Text := IntToStr(ReceivedMessage.DataCount);

    if ReceivedMessage.DataCount > 0 then
    begin
        CheckBoxDatagramWrite_AckReplyPending.Checked := ReceivedMessage.DataArray[0] and ACK_REPLY_PENDING_MASK = ACK_REPLY_PENDING_MASK;
        EditDatagramWrite_AckEstimatedWaitTime.Text := IntToStr(ReceivedMessage.DataArray[0] and ACK_REPLY_TIMEOUT_MASK);
        CheckBoxDatagramWrite_AckReservedBitsClear.Checked :=  ReceivedMessage.DataArray[0] and ACK_REPLY_RESERVED_MASK = 0;
    end;
    StatesDatagramWrite.WaitingForAck := False;
  end;
end;

procedure TForm1.HandleDatagramAckRejected(ReceivedMessage: TLccMessage);
var
  i: Integer;
  s: String;
begin
  // Datagram Read Created the NACK
  if StatesDatagramRead.WaitingForAck then
  begin
    CheckBoxDatagramRead_AckRejected.Checked := True;

    EditDatagramRead_AckDataBytes.Text := IntToStr(ReceivedMessage.DataCount);

    EditDatagramRead_RawData.Text := ReceivedMessage.ExtractDataByteArrayAsHex(True);

    if ReceivedMessage.DataCount > 0 then
    begin

      if ReceivedMessage.DataCount > 2 then
      s := '';
      begin
        for i := 2 to ReceivedMessage.DataCount - 1 do
        begin
           s := s + IntToHex(ReceivedMessage.DataArray[i], 2);
           if i < ReceivedMessage.DataCount - 1 then
             s := s + '.';
        end;
        EditDatagramRead_AckAdditionalInfo.Text := s;
      end;

      EditDatagramRead_AckErrorCode.Text := '[0x' + IntToHex(ReceivedMessage.ExtractDataBytesAsWord(0), 4) + '] ' + ErrorCodeToStr( ReceivedMessage.ExtractDataBytesAsWord(0));
    end;
    StatesDatagramRead.WaitingForAck := False;

  end;

  // Datagram Write Created the NACK
  if StatesDatagramWrite.WaitingForAck then
  begin
    CheckBoxDatagramWrite_AckRejected.Checked := True;

    EditDatagramWrite_AckDataBytes.Text := IntToStr(ReceivedMessage.DataCount);

    EditDatagramWrite_RawData.Text := ReceivedMessage.ExtractDataByteArrayAsHex(True);

    if ReceivedMessage.DataCount > 0 then
    begin

      if ReceivedMessage.DataCount > 2 then
      s := '';
      begin
        for i := 2 to ReceivedMessage.DataCount - 1 do
        begin
           s := s + IntToHex(ReceivedMessage.DataArray[i], 2);
           if i < ReceivedMessage.DataCount - 1 then
             s := s + '.';
        end;
        EditDatagramWrite_AckAdditionalInfo.Text := s;
      end;

      EditDatagramWrite_AckErrorCode.Text := '[0x' + IntToHex(ReceivedMessage.ExtractDataBytesAsWord(0), 4) + '] ' + ErrorCodeToStr( ReceivedMessage.ExtractDataBytesAsWord(0));
    end;
    StatesDatagramWrite.WaitingForAck := False;
  end;


end;

procedure TForm1.HandleGetAddressSpaceInfoPresentReply(ReceivedMessage: TLccMessage);

  procedure HandleAddressReadWrite(ATreeNode: TTreeNode);
  begin
    if ReceivedMessage.DataArray[7] and $01 = $01 then
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

    TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Lowest Address: 0x' + IntToHex(ReceivedMessage.ExtractDataBytesAsDWord(8), 4));

    HandleAddressReadWrite(ATreeNode);

    if ReceivedMessage.DataCount > 12 then
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Description: ' + ReceivedMessage.ExtractDataBytesAsString(12, ACount))
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

    if ReceivedMessage.DataCount > 8 then
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Description: ' + ReceivedMessage.ExtractDataBytesAsString(8, ACount))
    else
      TreeViewConfigMemAddressSpaceInfo.Items.AddChild(ATreeNode, 'Description: [None]');

  end;

  procedure UpdateDatagramReadPage(AddressWritable: Boolean);
  begin

    ComboBoxDatagramRead_ReadableAddressSpaces.Items.AddPair('Space: 0x' + IntToHex(StatesAddressSpace.Queried, 2) + ' - ' + AddressSpaceToStr(StatesAddressSpace.Queried), IntToStr(StatesAddressSpace.Queried));
    if AddressWritable then
      ComboBoxDatagramRead_WriteableAddressSpaces.Items.AddPair('Space: 0x' + IntToHex(StatesAddressSpace.Queried, 2) + ' - ' + AddressSpaceToStr(StatesAddressSpace.Queried), IntToStr(StatesAddressSpace.Queried));      ;

    if (ComboBoxDatagramRead_WriteableAddressSpaces.Items.Count > 0) and (ComboBoxDatagramRead_WriteableAddressSpaces.ItemIndex < 0) then
      ComboBoxDatagramRead_WriteableAddressSpaces.ItemIndex := 0;

    if (ComboBoxDatagramRead_ReadableAddressSpaces.Items.Count > 0) and (ComboBoxDatagramRead_ReadableAddressSpaces.ItemIndex < 0) then
      ComboBoxDatagramRead_ReadableAddressSpaces.ItemIndex := 0;

  end;

  procedure UpdateDatagramWritePage(AddressWritable: Boolean);
  begin

    ComboBoxDatagramWrite_ReadableAddressSpaces.Items.AddPair('Space: 0x' + IntToHex(StatesAddressSpace.Queried, 2) + ' - ' + AddressSpaceToStr(StatesAddressSpace.Queried), IntToStr(StatesAddressSpace.Queried));
    if AddressWritable then
      ComboBoxDatagramWrite_WriteableAddressSpaces.Items.AddPair('Space: 0x' + IntToHex(StatesAddressSpace.Queried, 2) + ' - ' + AddressSpaceToStr(StatesAddressSpace.Queried), IntToStr(StatesAddressSpace.Queried));      ;

    if (ComboBoxDatagramWrite_WriteableAddressSpaces.Items.Count > 0) and (ComboBoxDatagramWrite_WriteableAddressSpaces.ItemIndex < 0) then
      ComboBoxDatagramWrite_WriteableAddressSpaces.ItemIndex := 0;

    if (ComboBoxDatagramWrite_ReadableAddressSpaces.Items.Count > 0) and (ComboBoxDatagramWrite_ReadableAddressSpaces.ItemIndex < 0) then
      ComboBoxDatagramWrite_ReadableAddressSpaces.ItemIndex := 0;

  end;

var
  TreeNode: TTreeNode;
  LowAddressPresent: Boolean;
  AddressWritable: Boolean;
begin

  TreeNode := TreeViewConfigMemAddressSpaceInfo.Items.AddChild(nil, 'Address Space 0x' + IntToHex(StatesAddressSpace.Queried) + ' Present');

  TreeViewConfigMemAddressSpaceInfo.Items.AddChild(TreeNode, 'Space Reported: 0x' + IntToHex(ReceivedMessage.DataArray[2], 2));
  TreeViewConfigMemAddressSpaceInfo.Items.AddChild(TreeNode, 'Highest Address: 0x' + IntToHex(ReceivedMessage.ExtractDataBytesAsDWord(3), 4));

  LowAddressPresent := ReceivedMessage.DataArray[7] and $02 = $02;
  AddressWritable := ReceivedMessage.DataArray[7] and $01 = $01;

  if LowAddressPresent then
    HandleLowAddressPresent(TreeNode)
  else
    HandleLowAddressNotPresent(TreeNode);

  if StatesAddressSpace.EnumeratingWellKnown then
  begin
    StatesAddressSpace.Queried := StatesAddressSpace.Queried - 1;
    if StatesAddressSpace.Queried <= ADDRESS_SPACE_FUNCTION_MEMORY then
       StatesAddressSpace.EnumeratingWellKnown := False;
    SendAddressSpaceQuery(StatesAddressSpace.Queried);
  end;

  if StatesAddressSpace.Enumerating then
  begin
    UpdateDatagramReadPage(AddressWritable);
    UpdateDatagramWritePage(AddressWritable);

    StatesAddressSpace.Queried := StatesAddressSpace.Queried - 1;
    if StatesAddressSpace.Queried <= 0 then
       StatesAddressSpace.Enumerating := False;
    SendAddressSpaceQuery(StatesAddressSpace.Queried);
  end;

end;

procedure TForm1.HandleGetAddressSpaceInfoNotPresentReply(
  ReceivedMessage: TLccMessage);
begin

  TreeViewConfigMemAddressSpaceInfo.Items.AddChild(nil, 'Address Space 0x' + IntToHex(StatesAddressSpace.Queried) + ' Not Present');

  if StatesAddressSpace.EnumeratingWellKnown then
  begin
    StatesAddressSpace.Queried := StatesAddressSpace.Queried - 1;
    if StatesAddressSpace.Queried <= ADDRESS_SPACE_FUNCTION_MEMORY then
       StatesAddressSpace.EnumeratingWellKnown := False;
    SendAddressSpaceQuery(StatesAddressSpace.Queried);
  end;

  if StatesAddressSpace.Enumerating then
  begin
    StatesAddressSpace.Queried := StatesAddressSpace.Queried - 1;
    if StatesAddressSpace.Queried <= 0 then
       StatesAddressSpace.Enumerating := False;
    SendAddressSpaceQuery(StatesAddressSpace.Queried);
  end;

end;


end.

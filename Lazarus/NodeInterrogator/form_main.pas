unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, lcc_defines, lcc_utilities, lcc_node, lcc_node_manager,
  lcc_node_messages, lcc_node_commandstation, lcc_node_controller,
  lcc_node_train, lcc_comport, lcc_alias_server,
  lcc_node_messages_can_assembler_disassembler, lcc_alias_server_thread,
  LazSynaSer, lcc_connection_common, unit_comport, LCLType, contnrs, Types;

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
    FAddressSpaceQueried: Byte;

  public

    property AddressSpaceQueried: Byte read FAddressSpaceQueried write FAddressSpaceQueried;
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


  { TStateVariableResultOIR }

  TStateVariableResultOIR = class
  private
    FErrorCode: Word;
    FMti: Word;
    FOptional: TLccDynamicByteArray;
  public
    property ErrorCode: Word read FErrorCode write FErrorCode;
    property Mti: Word read FMti write FMti;
    property Optional: TLccDynamicByteArray read FOptional write FOptional;
  end;

  { TStateVariableOIR }

  TStateVariableOIR = class
  private
    FEnumerating: Boolean;
    FMtiQueried: Word;
    FResults: TObjectList;
  public
     constructor Create;
     destructor Destroy; override;

     property Enumerating: Boolean read FEnumerating write FEnumerating;
     property MtiQueried: Word read FMtiQueried write FMtiQueried;
     property Results: TObjectList read FResults write FResults;
  end;

  { TStateVariablesTWriteDatagram }

  TStateVariablesTWriteDatagram = class

  private
    FData: TLccDynamicByteArray;
    FWaitingForAck: Boolean;
   public

    property WaitingForAck: Boolean read FWaitingForAck write FWaitingForAck;
    property Data: TLccDynamicByteArray read FData write FData;
  end;

  { TStateVariabledSnip }

  TStateVariabledSnip = class
  private
    FData: TLccDynamicByteArray;
  published

    property Data: TLccDynamicByteArray read FData write FData;

  end;

  { TStateVariableCdi }

  TStateVariableCdi = class

  private
    FCurrentAddressPointer: DWord;
    FData: TLccDynamicByteArray;
    FStartingAddress: DWord;
    FWaitingForCdi: Boolean;
    FWaitingForSpaceInfo: Boolean;
  public

    property WaitingForSpaceInfo: Boolean read FWaitingForSpaceInfo write FWaitingForSpaceInfo;
    property WaitingForCdi: Boolean read FWaitingForCdi write FWaitingForCdi;
    property StartingAddress: DWord read FStartingAddress write FStartingAddress;
    property CurrentAddressPointer: DWord read FCurrentAddressPointer write FCurrentAddressPointer;

    property Data: TLccDynamicByteArray read FData write FData;
  end;

  { TStateVariableMultiFrame }

  TStateVariableMultiFrame = class

  private
    FDatagram: TLccDynamicByteArray;
    FSnip: TLccDynamicByteArray;
  public
    property Datagram: TLccDynamicByteArray read FDatagram write FDatagram;
    property Snip: TLccDynamicByteArray read FSnip write FSnip;
  end;

  { TFormNodeInterrogator }

  TFormNodeInterrogator = class(TForm)
    ButtonMultiFrame_SnipConvert: TButton;
    ButtonSelector_FindNodes: TButton;
    ButtonCdi_Read: TButton;
    ButtonMultiFrame_DatagramFirst: TButton;
    ButtonMultiFrame_SnipFirst: TButton;
    ButtonMultiFrame_SnipLast: TButton;
    ButtonMultiFrame_DatagramMiddle: TButton;
    ButtonMultiFrame_DatagramLast: TButton;
    ButtonMultiFrame_SnipMiddle: TButton;
    ButtonMultiframe_DatagramSendSequence: TButton;
    ButtonMultiframe_SnipSendSequence: TButton;
    ButtonOptionalInteractionRejected_ExpandAll: TButton;
    ButtonOptionalInteractionRejected_CollapseAll: TButton;
    ButtonOptionalInteractionRejected_Cancel: TButton;
    ButtonOptionalInteractionRejected_Clear: TButton;
    ButtonOptionalInteractionRejected_Run: TButton;
    ButtonProtocolSupport_Request: TButton;
    ButtonSnip_Request: TButton;
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
    CheckBox1: TCheckBox;
    CheckBoxProtcolSupport_ReservedBitClear: TCheckBox;
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
    CheckBoxSnip_UseDots: TCheckBox;
    CheckBoxCdi_UseDots: TCheckBox;
    CheckGroupProtocolSupport: TCheckGroup;
    ComboBoxDatagramRead_AcdiOffsets: TComboBox;
    ComboBoxDatagramWrite_AcdiOffsets: TComboBox;
    ComboBoxDatagramWrite_ReadableAddressSpaces: TComboBox;
    ComboBoxDatagramRead_WriteableAddressSpaces: TComboBox;
    ComboBoxDatagramRead_ReadableAddressSpaces: TComboBox;
    ComboBoxConfigMemAddressSpace: TComboBox;
    ComboBoxDatagramWrite_WriteableAddressSpaces: TComboBox;
    ComboBoxSelector: TComboBox;
    ComboBoxComPorts: TComboBox;
    EditMultiFrame_MfgVersion: TEdit;
    EditMultiFrame_SnipData: TEdit;
    EditMultiFrame_MfgName: TEdit;
    EditMultiFrame_MfgHardwareVersion: TEdit;
    EditMultiFrame_MfgSoftwareVersion: TEdit;
    EditMultiFrame_MfgManufacturer: TEdit;
    EditMultiFrame_UserName: TEdit;
    EditMultiFrame_UserDescription: TEdit;
    EditMultiFrameDatagram_Sequence: TEdit;
    EditMultiFrame_SnipSequence: TEdit;
    EditMultiFrame_DatagramData: TEdit;
    EditMultiFrame_UserVersion: TEdit;
    EditProtocolSupport_RawData: TEdit;
    EditSnip_ManufacturerVersion: TEdit;
    EditSnip_Manufacturer: TEdit;
    EditSnip_ManufacturerModel: TEdit;
    EditSnip_ManufacturerHardwareVersion: TEdit;
    EditSnip_ManufacturerSoftwareVersion: TEdit;
    EditSnip_UserDescription: TEdit;
    EditSnip_UserVersion: TEdit;
    EditSnip_UserName: TEdit;
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
    GroupBox1: TGroupBox;
    GroupBoxMultiFrame_User: TGroupBox;
    GroupBoxMultiFrame_Manufacturer: TGroupBox;
    GroupBoxMultiFrame_Datagrams: TGroupBox;
    GroupBoxMultiFrame_Snip: TGroupBox;
    GroupBoxOptionalInteractionRejected: TGroupBox;
    GroupBoxDataRead_ReplyMessage: TGroupBox;
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
    GroupBoxDataWrite_ReplyMessage: TGroupBox;
    ImageDatagramRead_Ok: TImage;
    ImageDatagramWrite_Ok: TImage;
    ImageDatagram_ReadRejected: TImage;
    ImageDatagram_WriteRejected: TImage;
    ImageListMain: TImageList;
    LabelMultiFrame_SnipData: TLabel;
    LabelMultiFrame_UserVersion: TLabel;
    LabelSelector: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelMultiFrame_UserDescription: TLabel;
    LabelMultiFrame_UserName: TLabel;
    LabelMultiFrame_ManufacturerSoftwareVersion: TLabel;
    LabelMultiFrame_ManufacturerHardwareVersion: TLabel;
    LabelMultiFrame_ManufacturerVersion: TLabel;
    LabelMultiFrame_Manufacturer: TLabel;
    LabelMultiFrame_ManufacturerName: TLabel;
    LabelMultiFrame_DatagramSequence: TLabel;
    LabelMultiFrame_DatagramData: TLabel;
    LabelMultiFrame_DatagramSequence1: TLabel;
    LabelProtocolSupport_RawData: TLabel;
    LabelSnip_RawData: TLabel;
    LabelSnip_Manufacturer: TLabel;
    LabelSnip_ManufacturerModel: TLabel;
    LabelSnip_ManufacturerHardwareVersion: TLabel;
    LabelSnip_ManufacturerSoftwareVersion: TLabel;
    LabelSnip_UserVersion: TLabel;
    LabelSnip_UserName: TLabel;
    LabelSnip_UserDescription: TLabel;
    LabelSnip_ManufacturerVersion: TLabel;
    LabelDatagramRead_AcdiOffsets: TLabel;
    LabelDatagramWrite_AcdiOffsets: TLabel;
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
    MemoCdi_RawData: TMemo;
    MemoSnip_RawData: TMemo;
    MemoComPort: TMemo;
    PageControlMain: TPageControl;
    PanelCdi: TPanel;
    PanelDatagramTransport: TPanel;
    PanelTabSnip: TPanel;
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
    RadioGroupDatagramRead_ReplyMessage: TRadioGroup;
    RadioGroupDatagramWrite_ReplyMessage: TRadioGroup;
    RadioGroupDatagramWrite_MessageOptions: TRadioGroup;
    RadioGroupDatagramRead_ViewOptions: TRadioGroup;
    RadioGroupDatagramRead_MessageOptions: TRadioGroup;
    RadioGroupDatagramWrite_ViewOptions: TRadioGroup;
    RadioGroupSnip_ViewOptions: TRadioGroup;
    RadioGroupCdi_ViewOptions: TRadioGroup;
    Splitter1: TSplitter;
    StatusBarMain: TStatusBar;
    TabSheetCdi: TTabSheet;
    TabSheetMultiFrame: TTabSheet;
    TabSheetSnip: TTabSheet;
    TabSheetDatagramWrite: TTabSheet;
    TabSheetEvents: TTabSheet;
    TabSheetMessageNetwork: TTabSheet;
    TabSheetConfigMem: TTabSheet;
    TabSheetDatagramRead: TTabSheet;
    TreeViewOptionalInteractionRejected: TTreeView;
    TreeViewConfigMemAddressSpaceInfo: TTreeView;
    procedure ButtonCdi_ReadClick(Sender: TObject);
    procedure ButtonMultiFrame_DatagramFirstClick(Sender: TObject);
    procedure ButtonMultiFrame_DatagramLastClick(Sender: TObject);
    procedure ButtonMultiFrame_DatagramMiddleClick(Sender: TObject);
    procedure ButtonMultiframe_DatagramSendSequenceClick(Sender: TObject);
    procedure ButtonMultiFrame_SnipConvertClick(Sender: TObject);
    procedure ButtonMultiFrame_SnipFirstClick(Sender: TObject);
    procedure ButtonMultiFrame_SnipLastClick(Sender: TObject);
    procedure ButtonMultiFrame_SnipMiddleClick(Sender: TObject);
    procedure ButtonMultiframe_SnipSendSequenceClick(Sender: TObject);
    procedure ButtonOptionalInteractionRejected_CancelClick(Sender: TObject);
    procedure ButtonOptionalInteractionRejected_CollapseAllClick(Sender: TObject);
    procedure ButtonOptionalInteractionRejected_ExpandAllClick(Sender: TObject);
    procedure ButtonOptionalInteractionRejected_RunClick(Sender: TObject);
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
    procedure ButtonProtocolSupport_RequestClick(Sender: TObject);
    procedure ButtonSelector_FindNodesClick(Sender: TObject);
    procedure ButtonSendConfigMemConfigOptionsClick(Sender: TObject);
    procedure ButtonSnip_RequestClick(Sender: TObject);
    procedure ButtonVerifyNodesAddressedClick(Sender: TObject);
    procedure ButtonVerifyNodesGlobalClick(Sender: TObject);
    procedure ButtonClearMemoClick(Sender: TObject);
    procedure ButtonCreateNodeClick(Sender: TObject);
    procedure ButtonRefreshComPortClick(Sender: TObject);
    procedure ButtonComPortConnectClick(Sender: TObject);
    procedure CheckBoxCdi_UseDotsClick(Sender: TObject);
    procedure CheckBoxDatagramRead_UseDotsChange(Sender: TObject);
    procedure CheckBoxDatagramWrite_UseDotsChange(Sender: TObject);
    procedure CheckBoxSnip_UseDotsClick(Sender: TObject);
    procedure ComboBoxConfigMemAddressSpaceChange(Sender: TObject);
    procedure ComboBoxDatagramRead_AcdiOffsetsChange(Sender: TObject);
    procedure ComboBoxDatagramWrite_AcdiOffsetsChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelTabDatagramReadClick(Sender: TObject);
    procedure PanelTabSnipClick(Sender: TObject);
    procedure RadioGroupCdi_ViewOptionsClick(Sender: TObject);
    procedure RadioGroupDatagramRead_ViewOptionsClick(Sender: TObject);
    procedure RadioGroupDatagramWrite_ViewOptionsClick(Sender: TObject);
    procedure RadioGroupSnip_ViewOptionsClick(Sender: TObject);
    procedure TabSheetMultiFrameContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);

  private
    FAckWorker: TLccMessage;
    FKnownMtiList: TStringList;
    FStateCdi: TStateVariableCdi;
    FStateMultiFrame: TStateVariableMultiFrame;
    FStateOIR: TStateVariableOIR;
    FStatesAddressSpace: TStateVariablesAddressSpaceMessage;
    FStatesDatagramRead: TStateVariablesReadDatagram;
    FStatesDatagramWrite: TStateVariablesTWriteDatagram;
    FEmulateCANBus: boolean;
    FNodeManager: TLccNodeManager;

    FSerialLink: TLccComPort;
    FStateSnip: TStateVariabledSnip;
    FTargetReplyEdit: TEdit;
    FWorkerMessage: TLccMessage;
    function GetNode: TLccNode;
    function GetTargetNode: TNodeInfoObject;

  protected

    property StatesAddressSpace: TStateVariablesAddressSpaceMessage read FStatesAddressSpace write FStatesAddressSpace;
    property StatesDatagramRead: TStateVariablesReadDatagram read FStatesDatagramRead write FStatesDatagramRead;
    property StatesDatagramWrite: TStateVariablesTWriteDatagram read FStatesDatagramWrite write FStatesDatagramWrite;
    property StatesSnip: TStateVariabledSnip read FStateSnip write FStateSnip;
    property StateOIR: TStateVariableOIR read FStateOIR write FStateOIR;
    property StateCdi: TStateVariableCdi read FStateCdi write FStateCdi;
    property StateMultiFrame: TStateVariableMultiFrame read FStateMultiFrame write FStateMultiFrame;

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
    procedure ClearConfigMemOperationTab;
    procedure ClearSnipTab;
    procedure DisconnectSerialLink;
    procedure PrintReadDataArray;
    procedure PrintWriteDataArray;
    procedure PrintSnipDataArray;
    procedure LoadKnownMtiList;
    function NextOIR_MtiMessage(MTI: Word): Word;
    procedure PrintCdiDataArray;

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
    procedure HandleSnipReply(ReceivedMessage: TLccMessage);
    procedure HandleProtocolSupportReply(ReceivedMessage: TLccMessage);
    procedure HandleProtocolOIR(ReceivedMessage: TLccMessage);
    procedure HandleGetAddressSpaceInfoPresentReply(ReceivedMessage: TLccMessage);
    procedure HandleGetAddressSpaceInfoNotPresentReply(ReceivedMessage: TLccMessage);


    procedure HandleStateCdi_ReadCdi(ReceivedMessage: TLccMessage);
    procedure HandleStateCdi_ReadCdiFailure(ReceivedMessage: TLccMessage);


  public

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property SerialLink: TLccComPort read FSerialLink write FSerialLink;

    property EmulateCANBus: boolean read FEmulateCANBus write FEmulateCANBus;

    property KnownMtiList: TStringList read FKnownMtiList write FKnownMtiList;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property AckWorker: TLccMessage read FAckWorker write FAckWorker;
    property Node: TLccNode read GetNode;
    property TargetNode: TNodeInfoObject read GetTargetNode;

  end;

var
  FormNodeInterrogator: TFormNodeInterrogator;

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

{ TStateVariableOIR }

constructor TStateVariableOIR.Create;
begin
  FResults := TObjectList.Create;
  Results.OwnsObjects := True;
end;

destructor TStateVariableOIR.Destroy;
begin
  Results.Free;
  inherited Destroy;
end;


{ TFormNodeInterrogator }

procedure TFormNodeInterrogator.FormCreate(Sender: TObject);
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

  KnownMtiList := TStringList.Create;
  LoadKnownMtiList;

  StatesAddressSpace := TStateVariablesAddressSpaceMessage.Create;
  StatesDatagramRead := TStateVariablesReadDatagram.Create;
  StatesDatagramWrite := TStateVariablesTWriteDatagram.Create;
  StatesSnip := TStateVariabledSnip.Create;
  StateOIR := TStateVariableOIR.Create;
  StateCdi := TStateVariableCdi.Create;
  StateMultiFrame := TStateVariableMultiFrame.Create;

  ConnectionFactory.OnLccMessageSend := @OnConnectionFactorySendMessage;
end;

procedure TFormNodeInterrogator.FormDestroy(Sender: TObject);
begin
  SerialLink.Free;
  AckWorker.Free;
  WorkerMessage.Free;
  StatesAddressSpace.Free;
  StatesDatagramRead.Free;
  StatesDatagramWrite.Free;
  StatesSnip.Free;
  KnownMtiList.Free;
  StateOIR.Free;
  StateCdi.Free;
  StateMultiFrame.Free;
end;

procedure TFormNodeInterrogator.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  DisconnectSerialLink
end;

procedure TFormNodeInterrogator.ButtonComPortConnectClick(Sender: TObject);
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

procedure TFormNodeInterrogator.CheckBoxCdi_UseDotsClick(Sender: TObject);
begin
  PrintCdiDataArray;
end;

procedure TFormNodeInterrogator.CheckBoxDatagramRead_UseDotsChange(Sender: TObject);
begin
  PrintReadDataArray;
end;

procedure TFormNodeInterrogator.CheckBoxDatagramWrite_UseDotsChange(Sender: TObject);
begin
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.CheckBoxSnip_UseDotsClick(Sender: TObject);
begin
  PrintSnipDataArray;
end;

procedure TFormNodeInterrogator.ComboBoxConfigMemAddressSpaceChange(Sender: TObject);
begin

end;

procedure TFormNodeInterrogator.ComboBoxDatagramRead_AcdiOffsetsChange(Sender: TObject);
begin
  begin
  case ComboBoxDatagramRead_AcdiOffsets.ItemIndex of
    0: EditDatagramRead_StartAddress.Text := '0x00';
    1: EditDatagramRead_StartAddress.Text := '0x01';
    2: EditDatagramRead_StartAddress.Text := '0x2A';
    3: EditDatagramRead_StartAddress.Text := '0x53';
    4: EditDatagramRead_StartAddress.Text := '0x68';
    5: EditDatagramRead_StartAddress.Text := '0x00';
    6: EditDatagramRead_StartAddress.Text := '0x01';
    7: EditDatagramRead_StartAddress.Text := '0x40';
  end;
end;
end;

procedure TFormNodeInterrogator.ComboBoxDatagramWrite_AcdiOffsetsChange(Sender: TObject);
begin
  case ComboBoxDatagramWrite_AcdiOffsets.ItemIndex of
    0: EditDatagramWrite_StartAddress.Text := '0x01';
    1: EditDatagramWrite_StartAddress.Text := '0x40';
  end;
end;

procedure TFormNodeInterrogator.ButtonRefreshComPortClick(Sender: TObject);
begin
  ComboBoxComPorts.Items.DelimitedText := GetSerialPortNames;
end;

procedure TFormNodeInterrogator.ButtonClearMemoClick(Sender: TObject);
begin
  MemoComPort.Clear;
end;

procedure TFormNodeInterrogator.ButtonVerifyNodesGlobalClick(Sender: TObject);
begin

  if Assigned(Node) then
  begin
    WorkerMessage.LoadVerifyNodeID(Node.NodeID, Node.AliasID, NULL_NODE_ID);

    SendMessage(WorkerMessage);

  end;
end;

procedure TFormNodeInterrogator.ButtonSendConfigMemConfigOptionsClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     ClearConfigMemOperationTab;
     WorkerMessage.LoadConfigMemOptions(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias);
     SendMessage(WorkerMessage);
  end else
    ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonSnip_RequestClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     ClearSnipTab;
     WorkerMessage.LoadSimpleNodeIdentInfoRequest(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias);
     SendMessage(WorkerMessage);
  end else
    ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonVerifyNodesAddressedClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
       ShowMessage('Working on it');
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonConfigMemAddressSpaceInfoClick(Sender: TObject);

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
     StatesAddressSpace.AddressSpaceQueried := IndexToAddressSpace(ComboBoxConfigMemAddressSpace.ItemIndex);
     SendAddressSpaceQuery(StatesAddressSpace.AddressSpaceQueried);
  end else
    ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonConfigMemAddressSpaceInfoWellKnownClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     StatesAddressSpace.EnumeratingWellKnown := True;
     StatesAddressSpace.Enumerating := False;
     TreeViewConfigMemAddressSpaceInfo.Items.Clear;
     StatesAddressSpace.AddressSpaceQueried := $FF;
     SendAddressSpaceQuery(StatesAddressSpace.AddressSpaceQueried);
  end else
    ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonConfigMemClearAddressInfoClick(Sender: TObject);
begin
  TreeViewConfigMemAddressSpaceInfo.Items.Clear;
end;

procedure TFormNodeInterrogator.ButtonConfigMemOpClearClick(Sender: TObject);
begin
  ClearConfigMemOperationTab;
end;

procedure TFormNodeInterrogator.ButtonDatagramRead_FindWritableAddressSpacesClick(Sender: TObject);
begin
  ComboBoxDatagramRead_ReadableAddressSpaces.Clear;
  ComboBoxDatagramRead_WriteableAddressSpaces.Clear;
  ComboBoxDatagramWrite_ReadableAddressSpaces.Clear;
  ComboBoxDatagramWrite_WriteableAddressSpaces.Clear;
  ButtonConfigMemAddressSpaceInfoAllClick(Sender);
end;

procedure TFormNodeInterrogator.ButtonDatagramReadClick(Sender: TObject);
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
    RadioGroupDatagramRead_ReplyMessage.ItemIndex := -1;

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

procedure TFormNodeInterrogator.ButtonDatagramRead_ScanAddressSpacesCancelClick(Sender: TObject);
begin
  StatesAddressSpace.Enumerating := False;
end;

procedure TFormNodeInterrogator.ButtonDatagram_WriteClick(Sender: TObject);
var
  AddressSpace: Byte;
begin

  if Assigned(TargetNode) and Assigned(Node) then
  begin

    if DefaultMessageBox('WARNING: Are you sure you want to write to this Node. It could cause unintended consquences!', 'WARNING', MB_OKCANCEL) <> mrOK then
      Exit;

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
    RadioGroupDatagramWrite_ReplyMessage.ItemIndex := -1;

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

procedure TFormNodeInterrogator.ButtonMemConfgAddressSpaceInfoCancelClick(Sender: TObject);
begin
  StatesAddressSpace.Enumerating := False;
  StatesAddressSpace.EnumeratingWellKnown := False;
end;

procedure TFormNodeInterrogator.ButtonProtocolSupport_RequestClick(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
    for i := 0 to CheckGroupProtocolSupport.ControlCount - 1 do
      CheckGroupProtocolSupport.Buttons[i].Checked := False;
      WorkerMessage.LoadProtocolIdentifyInquiry(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias);
      SendMessage(WorkerMessage);
  end else
    ShowNoTargetMessage;

end;

procedure TFormNodeInterrogator.ButtonSelector_FindNodesClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComboBoxSelector.Items.Count - 1 do
    ComboBoxSelector.Items.Objects[i].Free;

  ComboBoxSelector.Clear;

  if Assigned(Node) then
  begin
    WorkerMessage.LoadVerifyNodeID(Node.NodeID, Node.AliasID, NULL_NODE_ID);

    SendMessage(WorkerMessage);

  end;
end;

procedure TFormNodeInterrogator.ButtonConfigMemAddressSpaceInfoAllClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     StatesAddressSpace.Enumerating := True;
     StatesAddressSpace.EnumeratingWellKnown := False;
     TreeViewConfigMemAddressSpaceInfo.Items.Clear;
     StatesAddressSpace.AddressSpaceQueried := $FF;
     SendAddressSpaceQuery(StatesAddressSpace.AddressSpaceQueried);
  end else
    ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonOptionalInteractionRejected_RunClick(Sender: TObject);

begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
      StateOIR.Enumerating := True;
      StateOir.MtiQueried := NextOIR_MtiMessage($1000);
      StateOIR.Results.Clear;
      WorkerMessage.SourceID := Node.NodeID;
      WorkerMessage.SourceAlias := Node.AliasID;
      WorkerMessage.DestID := TargetNode.NodeID;
      WorkerMessage.DestAlias := TargetNode.Alias;
      WorkerMessage.MTI := StateOir.MtiQueried;
      WorkerMessage.DataCount := 0;
      SendMessage(WorkerMessage);
    end else
      ShowNoTargetMessage;

end;

procedure TFormNodeInterrogator.ButtonOptionalInteractionRejected_CancelClick(Sender: TObject);
begin
  StateOIR.Enumerating := False;
end;

procedure TFormNodeInterrogator.ButtonMultiFrame_DatagramFirstClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
       ShowMessage('Working on it');
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonCdi_ReadClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
      MemoCdi_RawData.Clear;
      StateCdi.WaitingForSpaceInfo := True;
      WorkerMessage.LoadConfigMemAddressSpaceInfo(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias, ADDRESS_SPACE_CONFIG_DEFINITION_INFO);
      SendMessage(WorkerMessage);
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonMultiFrame_DatagramLastClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
       ShowMessage('Working on it');
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonMultiFrame_DatagramMiddleClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
  begin
     ShowMessage('Working on it');
  end else
    ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonMultiframe_DatagramSendSequenceClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
       ShowMessage('Working on it');
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonMultiFrame_SnipConvertClick(Sender: TObject);
var
  Index: Integer;
  i: Integer;
begin
  SetLength(StateMultiFrame.FSnip,
            EditMultiFrame_MfgManufacturer.GetTextLen +
            EditMultiFrame_MfgName.GetTextLen +
            EditMultiFrame_MfgHardwareVersion.GetTextLen +
            EditMultiFrame_MfgSoftwareVersion.GetTextLen +
            EditMultiFrame_UserName.GetTextLen +
            EditMultiFrame_UserDescription.GetTextLen +
            8 // Nulls + 2 Version bytes
            );


   Index := 0;
   StateMultiFrame.Snip[Index] := StrToInt(EditMultiFrame_MfgVersion.Text);
   Inc(Index);

   for i := 1 to EditMultiFrame_MfgManufacturer.GetTextLen do
   begin
     StateMultiFrame.Snip[Index] := Ord( EditMultiFrame_MfgManufacturer.Text[i]);
     Inc(Index);
   end;

   StateMultiFrame.Snip[Index] := $00;
   Inc(Index);

   for i := 1 to EditMultiFrame_MfgName.GetTextLen do
   begin
     StateMultiFrame.Snip[Index] := Ord( EditMultiFrame_MfgName.Text[i]);
     Inc(Index);
   end;

   StateMultiFrame.Snip[Index] := $00;
   Inc(Index);

   for i := 1 to EditMultiFrame_MfgHardwareVersion.GetTextLen do
   begin
     StateMultiFrame.Snip[Index] := Ord( EditMultiFrame_MfgHardwareVersion.Text[i]);
     Inc(Index);
   end;

   StateMultiFrame.Snip[Index] := $00;
   Inc(Index);

   for i := 1 to EditMultiFrame_MfgSoftwareVersion.GetTextLen do
   begin
     StateMultiFrame.Snip[Index] := Ord( EditMultiFrame_MfgSoftwareVersion.Text[i]);
     Inc(Index);
   end;

   StateMultiFrame.Snip[Index] := $00;
   Inc(Index);

   StateMultiFrame.Snip[Index] := StrToInt(EditMultiFrame_UserVersion.Text);
   Inc(Index);

   for i := 1 to EditMultiFrame_UserName.GetTextLen do
   begin
     StateMultiFrame.Snip[Index] := Ord( EditMultiFrame_UserName.Text[i]);
     Inc(Index);
   end;

   StateMultiFrame.Snip[Index] := $00;
   Inc(Index);

   for i := 1 to EditMultiFrame_UserDescription.GetTextLen do
   begin
     StateMultiFrame.Snip[Index] := Ord( EditMultiFrame_UserDescription.Text[i]);
     Inc(Index);
   end;

   StateMultiFrame.Snip[Index] := $00;

   EditMultiFrame_SnipData.Text := ByteArrayAsHexStr(StateMultiFrame.Snip, True);

end;

procedure TFormNodeInterrogator.ButtonMultiFrame_SnipFirstClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
       ShowMessage('Working on it');
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonMultiFrame_SnipLastClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
       ShowMessage('Working on it');
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonMultiFrame_SnipMiddleClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
       ShowMessage('Working on it');
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonMultiframe_SnipSendSequenceClick(Sender: TObject);
begin
  if Assigned(TargetNode) and Assigned(Node) then
    begin
       ShowMessage('Working on it');
    end else
      ShowNoTargetMessage;
end;

procedure TFormNodeInterrogator.ButtonOptionalInteractionRejected_CollapseAllClick(
  Sender: TObject);
begin
  TreeViewOptionalInteractionRejected.FullCollapse;
end;

procedure TFormNodeInterrogator.ButtonOptionalInteractionRejected_ExpandAllClick(
  Sender: TObject);
begin
  TreeViewOptionalInteractionRejected.FullExpand;
end;

procedure TFormNodeInterrogator.ButtonCreateNodeClick(Sender: TObject);
begin

end;

procedure TFormNodeInterrogator.FormShow(Sender: TObject);
begin
  ComboBoxComPorts.Items.Delimiter := ';';
  ComboBoxComPorts.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  ComboBoxComPorts.ItemIndex := 2;

  ImageDatagramRead_Ok.ImageIndex := -1;
  ImageDatagram_ReadRejected.ImageIndex := -1;
  ImageDatagramWrite_Ok.ImageIndex := -1;
  ImageDatagram_WriteRejected.ImageIndex := -1;
end;

procedure TFormNodeInterrogator.PanelTabDatagramReadClick(Sender: TObject);
begin

end;

procedure TFormNodeInterrogator.PanelTabSnipClick(Sender: TObject);
begin
  PrintSnipDataArray;
end;

procedure TFormNodeInterrogator.RadioGroupCdi_ViewOptionsClick(Sender: TObject);
begin
  PrintCdiDataArray;
end;

procedure TFormNodeInterrogator.RadioGroupDatagramRead_ViewOptionsClick(Sender: TObject);
begin
    PrintReadDataArray;
end;

procedure TFormNodeInterrogator.RadioGroupDatagramWrite_ViewOptionsClick(Sender: TObject);
begin
   PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.RadioGroupSnip_ViewOptionsClick(Sender: TObject);
begin
  PrintSnipDataArray;
end;

procedure TFormNodeInterrogator.TabSheetMultiFrameContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

function TFormNodeInterrogator.GetNode: TLccNode;
begin
  if NodeManager.Nodes.Count > 0 then
    Result := NodeManager.Node[0]
  else
    Result := nil;
end;

function TFormNodeInterrogator.GetTargetNode: TNodeInfoObject;
begin
  if ComboBoxSelector.ItemIndex < 0 then
  begin
    result := nil;
    exit;
  end;

   Result := ComboBoxSelector.Items.Objects[ComboBoxSelector.ItemIndex] as TNodeInfoObject;
end;

procedure TFormNodeInterrogator.OnComPortRecieveGridConnectStr(Sender: TObject; ReceiveGridConnectStr: ansistring);
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

procedure TFormNodeInterrogator.OnComPortRecieveMessage(Sender: TObject; ReceiveMessage: TLccMessage);
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
        HandleProtocolSupportReply(ReceiveMessage);
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
        HandleSnipReply(ReceiveMessage);
      end;

      MTI_DATAGRAM_OK_REPLY:
      begin
        HandleDatagramAckOk(ReceiveMessage);
      end;

      MTI_DATAGRAM_REJECTED_REPLY:
      begin
        HandleDatagramAckRejected(ReceiveMessage);
      end;

      MTI_OPTIONAL_INTERACTION_REJECTED:
      begin
        HandleProtocolOIR(ReceiveMessage);
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

procedure TFormNodeInterrogator.OnComPortAssemblerErrorReply(Sender: TObject; ReceiveMessage: TLccMessage);
begin
  // TODO:  Signal user somehow that this occured
   SendMessage(ReceiveMessage);
end;

procedure TFormNodeInterrogator.OnComPortError(Sender: TObject; ErrorString: string; ErrorCode: word);
begin

  ShowMessage('ComPort Error: ' + ErrorString + '.  Error Code: ' + IntToStr(ErrorCode));

end;

procedure TFormNodeInterrogator.OnComPortLogIn(Sender: TObject);
begin
  NodeManager.AddNodeByClass('', TInterrogatorNode, True, NULL_NODE_ID);
end;

procedure TFormNodeInterrogator.OnComPortLogOut(Sender: TObject);
begin

end;

procedure TFormNodeInterrogator.OnConnectionFactorySendMessage(Sender: TObject; LccMessage: TLccMessage);
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

procedure TFormNodeInterrogator.OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelAliasID.Caption := LccSourceNode.AliasIDStr;
end;

procedure TFormNodeInterrogator.OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelNodeID.Caption := LccSourceNode.NodeIDStr[True];
end;

procedure TFormNodeInterrogator.OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
begin
  ButtonVerifyNodesGlobalClick(Self);
end;

procedure TFormNodeInterrogator.OnNodeManagerAliasRelease(Sender: TObject; ALccNode: TLccNode);
begin
  LabelNodeID.Caption := '[None]';
end;

procedure TFormNodeInterrogator.OnNodeManagerNodeDestroy(Sender: TObject; ALccNode: TLccNode);
begin

  LabelAliasID.Caption := '[None]';
  LabelNodeID.Caption := '[None]';

end;

procedure TFormNodeInterrogator.OnComPortSendMessage(Sender: TObject; var GridConnectStyleMessage: string);
begin

end;

procedure TFormNodeInterrogator.ShowNoTargetMessage;
begin
  ShowMessage('There is no target Node selected... please select a node to test');
end;

procedure TFormNodeInterrogator.SendMessage(AMessage: TLccMessage);
begin
  OnConnectionFactorySendMessage(Self, AMessage);
end;

procedure TFormNodeInterrogator.SendAck(Message: TLccMessage);
begin
  AckWorker.LoadDatagramAck(Message.DestID, Message.DestAlias, Message.SourceID, Message.SourceAlias, True, False, 0);
  SendMessage(AckWorker);
end;

procedure TFormNodeInterrogator.SendAddressSpaceQuery(AddressSpace: Byte);
begin
  WorkerMessage.LoadConfigMemAddressSpaceInfo(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias, AddressSpace);
  SendMessage(WorkerMessage);
end;

procedure TFormNodeInterrogator.ClearConfigMemOperationTab;
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

procedure TFormNodeInterrogator.ClearSnipTab;
begin
  EditSnip_Manufacturer.Text := '';
  EditSnip_ManufacturerHardwareVersion.Text := '';
  EditSnip_ManufacturerModel.Text := '';
  EditSnip_ManufacturerSoftwareVersion.Text := '';
  EditSnip_ManufacturerVersion.Text := '';
  EditSnip_UserDescription.Text := '';
  EditSnip_UserName.Text := '';
  EditSnip_UserVersion.Text := '';
  MemoSnip_RawData.Clear;
end;

procedure TFormNodeInterrogator.DisconnectSerialLink;
begin
  NodeManager.Clear;
  Sleep(500);
  SerialLink.Disconnect;
end;

procedure TFormNodeInterrogator.PrintReadDataArray();
begin
  case RadioGroupDatagramRead_ViewOptions.ItemIndex of
    0: EditDatagramRead.Text := ByteArrayAsHexStr(StatesDatagramRead.Data, CheckBoxDatagramRead_UseDots.Checked);
    1: EditDatagramRead.Text := ByteArrayAsDecStr(StatesDatagramRead.Data, CheckBoxDatagramRead_UseDots.Checked);
    2: EditDatagramRead.Text := ByteArrayAsCharStr(StatesDatagramRead.Data, CheckBoxDatagramRead_UseDots.Checked);
  end;
end;

procedure TFormNodeInterrogator.PrintWriteDataArray;
begin
  case RadioGroupDatagramWrite_ViewOptions.ItemIndex of
    0: EditDatagramWrite.Text := ByteArrayAsHexStr(StatesDatagramWrite.Data, CheckBoxDatagramWrite_UseDots.Checked);
    1: EditDatagramWrite.Text := ByteArrayAsDecStr(StatesDatagramWrite.Data, CheckBoxDatagramWrite_UseDots.Checked);
    2: EditDatagramWrite.Text := ByteArrayAsCharStr(StatesDatagramWrite.Data, CheckBoxDatagramWrite_UseDots.Checked);
  end;
end;

procedure TFormNodeInterrogator.PrintSnipDataArray;
begin
   case RadioGroupSnip_ViewOptions.ItemIndex of
    0: MemoSnip_RawData.Text := ByteArrayAsHexStr(StatesSnip.Data, CheckBoxSnip_UseDots.Checked);
    1: MemoSnip_RawData.Text := ByteArrayAsDecStr(StatesSnip.Data, CheckBoxSnip_UseDots.Checked);
    2: MemoSnip_RawData.Text := ByteArrayAsCharStr(StatesSnip.Data, CheckBoxSnip_UseDots.Checked);
  end;
end;

procedure TFormNodeInterrogator.LoadKnownMtiList;
begin
  KnownMtiList.Sorted := True;

  // Basic
  KnownMtiList.Add( IntToStr($0100));
  KnownMtiList.Add( IntToStr($0488));
  KnownMtiList.Add( IntToStr($0490));
  KnownMtiList.Add( IntToStr($0170));
  KnownMtiList.Add( IntToStr($0068));
  KnownMtiList.Add( IntToStr($00A8));
  // Protocol Support
  KnownMtiList.Add( IntToStr($0828));
  KnownMtiList.Add( IntToStr($0668));
  // Event Exchange
  KnownMtiList.Add( IntToStr($08F4));
  KnownMtiList.Add( IntToStr($04A4));
  KnownMtiList.Add( IntToStr($04C7));
  KnownMtiList.Add( IntToStr($04C4));
  KnownMtiList.Add( IntToStr($04C5));
  KnownMtiList.Add( IntToStr($04C6));
  KnownMtiList.Add( IntToStr($0914));
  KnownMtiList.Add( IntToStr($0524));
  KnownMtiList.Add( IntToStr($0547));
  KnownMtiList.Add( IntToStr($0544));
  KnownMtiList.Add( IntToStr($0545));
  KnownMtiList.Add( IntToStr($0546));
  KnownMtiList.Add( IntToStr($0968));
  KnownMtiList.Add( IntToStr($0970));
  KnownMtiList.Add( IntToStr($0594));
  KnownMtiList.Add( IntToStr($05B4));
  KnownMtiList.Add( IntToStr($0F14));
  KnownMtiList.Add( IntToStr($0F15));
  KnownMtiList.Add( IntToStr($0F16));
  // Traction
  KnownMtiList.Add( IntToStr($05E8));
  KnownMtiList.Add( IntToStr($01E8));
  KnownMtiList.Add( IntToStr($09E9));
  KnownMtiList.Add( IntToStr($05E9));
  // Other
  KnownMtiList.Add( IntToStr($0820));
  // Remote Button
  KnownMtiList.Add( IntToStr($0948));
  KnownMtiList.Add( IntToStr($0549));
  //Traction SNIP
  KnownMtiList.Add( IntToStr($0DA8));
  KnownMtiList.Add( IntToStr($09C8));
  // Node Ident SNIP
  KnownMtiList.Add( IntToStr($0DE8));
  KnownMtiList.Add( IntToStr($0A08));
  // Datagram
  KnownMtiList.Add( IntToStr($1C48));
  KnownMtiList.Add( IntToStr($0A28));
  KnownMtiList.Add( IntToStr($0A48));
  // Stream
  KnownMtiList.Add( IntToStr($0A28));
  KnownMtiList.Add( IntToStr($0A48));
  KnownMtiList.Add( IntToStr($0CC8));
  KnownMtiList.Add( IntToStr($0868));
  KnownMtiList.Add( IntToStr($0888));
  KnownMtiList.Add( IntToStr($08A8));
  // Extra
  KnownMtiList.Add( IntToStr($2000));
  KnownMtiList.Add( IntToStr($2020));

end;

function TFormNodeInterrogator.NextOIR_MtiMessage(MTI: Word): Word;
var
  S: String;
  Index: Integer;
begin
  MTI := MTI - 1;
  S := IntToStr(MTI);
  while ((MTI and MTI_ADDRESSED_MASK <> MTI_ADDRESSED_MASK) or (KnownMtiList.Find(S, Index))) and (MTI > 0) do
  begin
    MTI := MTI - 1;
    S := IntToStr(MTI);
  end;

  if MTI = 0 then
    beep;

  Result := MTI;
end;

procedure TFormNodeInterrogator.PrintCdiDataArray;
begin
   case RadioGroupCdi_ViewOptions.ItemIndex of
    0: MemoCdi_RawData.Text := ByteArrayAsHexStr(StateCdi.Data, CheckBoxCdi_UseDots.Checked);
    1: MemoCdi_RawData.Text := ByteArrayAsDecStr(StateCdi.Data, CheckBoxCdi_UseDots.Checked);
    2: MemoCdi_RawData.Text := ByteArrayAsCharStr(StateCdi.Data, CheckBoxCdi_UseDots.Checked);
  end;
end;


procedure TFormNodeInterrogator.HandleVerifiedNode(ReceivedMessage: TLccMessage);
var
  NodeID: TNodeID;
  Str: string;
  NodeObj: TNodeInfoObject;
begin

  NodeID := NULL_NODE_ID;
  NodeID := ReceivedMessage.ExtractDataBytesAsNodeID(0, NodeID);

  NodeObj :=  TNodeInfoObject.Create(ReceivedMessage.ExtractDataBytesAsNodeID(0, NodeID), ReceivedMessage.SourceAlias);
  Str := '[0x' + IntToHex(ReceivedMessage.SourceAlias) + '] 0x' + NodeIDToString(NodeID, True);

  ComboBoxSelector.Items.AddObject(Str, NodeObj);

  if ComboBoxSelector.ItemIndex < 0 then
    ComboBoxSelector.ItemIndex := 0;
end;

procedure TFormNodeInterrogator.HandleGetConfigOptionsReply(ReceivedMessage: TLccMessage);
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

procedure TFormNodeInterrogator.HandleReadReply(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramRead_ReplyMessage.ItemIndex := 0;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData, 0);
  PrintReadDataArray;

  HandleStateCdi_ReadCdi(ReceivedMessage);

end;

procedure TFormNodeInterrogator.HandleReadReplyCDI(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramRead_ReplyMessage.ItemIndex := 1;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData, 0);
  PrintReadDataArray;

  HandleStateCdi_ReadCdi(ReceivedMessage);

end;

procedure TFormNodeInterrogator.HandleReadReplyAll(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramRead_ReplyMessage.ItemIndex := 2;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData, 0);
  PrintReadDataArray;
end;

procedure TFormNodeInterrogator.HandleReadReplyConfig(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramRead_ReplyMessage.ItemIndex := 3;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData, 0);
  PrintReadDataArray;
end;

procedure TFormNodeInterrogator.HandleReadReplyFailure(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramRead_ReplyMessage.ItemIndex := 4;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData, 0);
  PrintReadDataArray;

  HandleStateCdi_ReadCdiFailure(ReceivedMessage);
end;

procedure TFormNodeInterrogator.HandleReadReplyFailureCDI(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramRead_ReplyMessage.ItemIndex := 5;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData, 0);
  PrintReadDataArray;

  HandleStateCdi_ReadCdiFailure(ReceivedMessage);
end;

procedure TFormNodeInterrogator.HandleReadReplyFailureAll(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramRead_ReplyMessage.ItemIndex := 6;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData, 0);
  PrintReadDataArray;
end;

procedure TFormNodeInterrogator.HandleReadReplyFailureConfig(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramRead_ReplyMessage.ItemIndex := 7;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramRead.FData, 0);
  PrintReadDataArray;
end;

procedure TFormNodeInterrogator.HandleWriteReply(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramWrite_ReplyMessage.ItemIndex := 0;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramWrite.FData, 0);
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.HandleWriteReplyCDI(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramWrite_ReplyMessage.ItemIndex := 1;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramWrite.FData, 0);
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.HandleWriteReplyAll(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramWrite_ReplyMessage.ItemIndex := 2;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramWrite.FData, 0);
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.HandleWriteReplyConfig(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramWrite_ReplyMessage.ItemIndex := 3;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramWrite.FData, 0);
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.HandleWriteReplyFailure(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramWrite_ReplyMessage.ItemIndex := 4;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramWrite.FData, 0);
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.HandleWriteReplyFailureCDI(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramWrite_ReplyMessage.ItemIndex := 5;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramWrite.FData, 0);
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.HandleWriteReplyFailureAll(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramWrite_ReplyMessage.ItemIndex := 6;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramWrite.FData, 0);
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.HandleWriteReplyFailureConfig(ReceivedMessage: TLccMessage);
begin
  RadioGroupDatagramWrite_ReplyMessage.ItemIndex := 7;
  ReceivedMessage.CopyDataToDataArray(StatesDatagramWrite.FData, 0);
  PrintWriteDataArray;
end;

procedure TFormNodeInterrogator.HandleDatagramAckOk(ReceivedMessage: TLccMessage);
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

    ImageDatagramRead_Ok.ImageIndex := 0;
    ImageDatagram_ReadRejected.ImageIndex := -1;

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

    ImageDatagramWrite_Ok.ImageIndex := 0;
    ImageDatagram_WriteRejected.ImageIndex := -1;

    StatesDatagramWrite.WaitingForAck := False;
  end;
end;

procedure TFormNodeInterrogator.HandleDatagramAckRejected(ReceivedMessage: TLccMessage);
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

      ImageDatagramRead_Ok.ImageIndex := -1;
      ImageDatagram_ReadRejected.ImageIndex := 1;

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

      ImageDatagramWrite_Ok.ImageIndex := -1;
      ImageDatagram_WriteRejected.ImageIndex := 1;

      EditDatagramWrite_AckErrorCode.Text := '[0x' + IntToHex(ReceivedMessage.ExtractDataBytesAsWord(0), 4) + '] ' + ErrorCodeToStr( ReceivedMessage.ExtractDataBytesAsWord(0));
    end;
    StatesDatagramWrite.WaitingForAck := False;
  end;


end;

procedure TFormNodeInterrogator.HandleSnipReply(ReceivedMessage: TLccMessage);
var
  MfgVersion, UserVersion: Byte;
  Manufacturer, Model, HardwareVersion, SoftwareVersion, UserName, UserDescription: String;
begin

  ReceivedMessage.CopyDataToDataArray(StatesSnip.FData, 0);

  PrintSnipDataArray;

  MfgVersion := 0;
  Manufacturer := '';
  Model := '';
  HardwareVersion := '';
  SoftwareVersion := '';
  UserVersion := 0;
  UserName := '';
  UserDescription := '';

  ReceivedMessage.ExtractSimpleNodeIdentInfo(MfgVersion, Manufacturer, Model, HardwareVersion, SoftwareVersion, UserVersion, UserName, UserDescription);

  EditSnip_UserVersion.Text := IntToStr(UserVersion);
  EditSnip_UserName.Text := UserName;
  EditSnip_UserDescription.Text := UserDescription;
  EditSnip_ManufacturerVersion.Text := IntToStr(MfgVersion);
  EditSnip_Manufacturer.Text := Manufacturer;
  EditSnip_ManufacturerModel.Text := Model;
  EditSnip_ManufacturerHardwareVersion.Text := HardwareVersion;
  EditSnip_ManufacturerSoftwareVersion.Text := SoftwareVersion;
end;

procedure TFormNodeInterrogator.HandleProtocolSupportReply(ReceivedMessage: TLccMessage);
var
  Value: DWord;
begin
  Value := ReceivedMessage.ExtractDataBytesAsInt(0, 2);
  CheckGroupProtocolSupport.Buttons[0].Checked := Value and PSI_SIMPLE = PSI_SIMPLE;
  CheckGroupProtocolSupport.Buttons[1].Checked := Value and PSI_DATAGRAM = PSI_DATAGRAM;
  CheckGroupProtocolSupport.Buttons[2].Checked := Value and PSI_STREAM = PSI_STREAM;
  CheckGroupProtocolSupport.Buttons[3].Checked := Value and PSI_MEMORY_CONFIGURATION = PSI_MEMORY_CONFIGURATION;
  CheckGroupProtocolSupport.Buttons[4].Checked := Value and PSI_RESERVATION = PSI_RESERVATION;
  CheckGroupProtocolSupport.Buttons[5].Checked := Value and PSI_EVENT_EXCHANGE = PSI_EVENT_EXCHANGE;
  CheckGroupProtocolSupport.Buttons[6].Checked := Value and PSI_IDENTIFICATION = PSI_IDENTIFICATION;
  CheckGroupProtocolSupport.Buttons[7].Checked := Value and PSI_TEACHING_LEARNING = PSI_TEACHING_LEARNING;
  CheckGroupProtocolSupport.Buttons[8].Checked := Value and PSI_REMOTE_BUTTON = PSI_REMOTE_BUTTON;
  CheckGroupProtocolSupport.Buttons[9].Checked := Value and PSI_ABBREVIATED_DEFAULT_CDI = PSI_ABBREVIATED_DEFAULT_CDI;
  CheckGroupProtocolSupport.Buttons[10].Checked := Value and PSI_DISPLAY = PSI_DISPLAY;
  CheckGroupProtocolSupport.Buttons[11].Checked := Value and PSI_SIMPLE_NODE_INFORMATION = PSI_SIMPLE_NODE_INFORMATION;
  CheckGroupProtocolSupport.Buttons[12].Checked := Value and PSI_CONFIGURATION_DESCRIPTION_INFO = PSI_CONFIGURATION_DESCRIPTION_INFO;
  CheckGroupProtocolSupport.Buttons[13].Checked := Value and PSI_TRAIN_CONTROL = PSI_TRAIN_CONTROL;
  CheckGroupProtocolSupport.Buttons[14].Checked := Value and PSI_FUNCTION_DESCRIPTION = PSI_FUNCTION_DESCRIPTION;
  CheckGroupProtocolSupport.Buttons[15].Checked := Value and PSI_FUNCTION_CONFIGURATION = PSI_FUNCTION_CONFIGURATION;
  CheckGroupProtocolSupport.Buttons[16].Checked := Value and PSI_FIRMWARE_UPGRADE = PSI_FIRMWARE_UPGRADE;
  CheckGroupProtocolSupport.Buttons[17].Checked := Value and PSI_FIRMWARE_UPGRADE_ACTIVE = PSI_FIRMWARE_UPGRADE_ACTIVE;

  EditProtocolSupport_RawData.Text := ReceivedMessage.ExtractDataBytesAsHex(0, 2);
  CheckBoxProtcolSupport_ReservedBitClear.Checked := Value and $180 = 0;
end;

procedure TFormNodeInterrogator.HandleProtocolOIR(ReceivedMessage: TLccMessage);
var
  TreeNode: TTreeNode;
  NextMTI: Word;
  OirResult: TStateVariableResultOIR;
begin
  if StateOIR.Enumerating then
    begin
      OirResult := TStateVariableResultOIR.Create;
      StateOIR.Results.Add(OirResult);

      TreeNode := TreeViewOptionalInteractionRejected.Items.Add(NIL, 'MTI: ' + IntToHex(StateOIR.MtiQueried, 4));

      if ReceivedMessage.DataCount < 4 then
      begin
         TreeViewOptionalInteractionRejected.Items.AddChild(TreeNode, 'Missing Error Code and/or Mti in reply');
         TreeNode.ImageIndex := 1;
         OirResult.ErrorCode := 0;
         OirResult.Mti := 0;
         SetLength(OirResult.FOptional, 0);
      end else
      begin
        OirResult.ErrorCode := ReceivedMessage.ExtractDataBytesAsWord(0);
        OirResult.Mti := ReceivedMessage.ExtractDataBytesAsWord(2);
        ReceivedMessage.CopyDataToDataArray(OirResult.FOptional, 4);  // Checks length internally

        TreeViewOptionalInteractionRejected.Items.AddChild(TreeNode, 'ErrorCode: 0x' + IntToHex(OirResult.ErrorCode) + ' ' + ErrorCodeToStr(OirResult.ErrorCode));

        if Length(OirResult.Optional) = 0 then
          TreeViewOptionalInteractionRejected.Items.AddChild(TreeNode, '[No Optional Info]')
        else
          TreeViewOptionalInteractionRejected.Items.AddChild(TreeNode, 'Optional Info: ' + ByteArrayAsHexStr(OirResult.Optional, True));

        TreeNode.ImageIndex := 0;
      end;

      TreeNode.MakeVisible;

      NextMTI := NextOIR_MtiMessage(StateOIR.MtiQueried);
      if NextMTI = 0 then
        StateOIR.Enumerating := False
      else begin
        StateOir.MtiQueried := NextMTI;
        WorkerMessage.SourceID := Node.NodeID;
        WorkerMessage.SourceAlias := Node.AliasID;
        WorkerMessage.DestID := TargetNode.NodeID;
        WorkerMessage.DestAlias := TargetNode.Alias;
        WorkerMessage.MTI := NextMTI;
        WorkerMessage.DataCount := 0;
        SendMessage(WorkerMessage);
    end;
  end;
end;

procedure TFormNodeInterrogator.HandleGetAddressSpaceInfoPresentReply(ReceivedMessage: TLccMessage);

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

    ComboBoxDatagramRead_ReadableAddressSpaces.Items.AddPair('Space: 0x' + IntToHex(StatesAddressSpace.AddressSpaceQueried, 2) + ' - ' + AddressSpaceToStr(StatesAddressSpace.AddressSpaceQueried), IntToStr(StatesAddressSpace.AddressSpaceQueried));
    if AddressWritable then
      ComboBoxDatagramRead_WriteableAddressSpaces.Items.AddPair('Space: 0x' + IntToHex(StatesAddressSpace.AddressSpaceQueried, 2) + ' - ' + AddressSpaceToStr(StatesAddressSpace.AddressSpaceQueried), IntToStr(StatesAddressSpace.AddressSpaceQueried));      ;

    if (ComboBoxDatagramRead_WriteableAddressSpaces.Items.Count > 0) and (ComboBoxDatagramRead_WriteableAddressSpaces.ItemIndex < 0) then
      ComboBoxDatagramRead_WriteableAddressSpaces.ItemIndex := 0;

    if (ComboBoxDatagramRead_ReadableAddressSpaces.Items.Count > 0) and (ComboBoxDatagramRead_ReadableAddressSpaces.ItemIndex < 0) then
      ComboBoxDatagramRead_ReadableAddressSpaces.ItemIndex := 0;

  end;

  procedure UpdateDatagramWritePage(AddressWritable: Boolean);
  begin

    ComboBoxDatagramWrite_ReadableAddressSpaces.Items.AddPair('Space: 0x' + IntToHex(StatesAddressSpace.AddressSpaceQueried, 2) + ' - ' + AddressSpaceToStr(StatesAddressSpace.AddressSpaceQueried), IntToStr(StatesAddressSpace.AddressSpaceQueried));
    if AddressWritable then
      ComboBoxDatagramWrite_WriteableAddressSpaces.Items.AddPair('Space: 0x' + IntToHex(StatesAddressSpace.AddressSpaceQueried, 2) + ' - ' + AddressSpaceToStr(StatesAddressSpace.AddressSpaceQueried), IntToStr(StatesAddressSpace.AddressSpaceQueried));      ;

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

  TreeNode := TreeViewConfigMemAddressSpaceInfo.Items.AddChild(nil, 'Address Space 0x' + IntToHex(StatesAddressSpace.AddressSpaceQueried) + ' Present');
  TreeNode.ImageIndex := 2;

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
    StatesAddressSpace.AddressSpaceQueried := StatesAddressSpace.AddressSpaceQueried - 1;
    if StatesAddressSpace.AddressSpaceQueried <= ADDRESS_SPACE_FUNCTION_MEMORY then
       StatesAddressSpace.EnumeratingWellKnown := False;
    SendAddressSpaceQuery(StatesAddressSpace.AddressSpaceQueried);
  end;

  if StatesAddressSpace.Enumerating then
  begin
    UpdateDatagramReadPage(AddressWritable);
    UpdateDatagramWritePage(AddressWritable);

    StatesAddressSpace.AddressSpaceQueried := StatesAddressSpace.AddressSpaceQueried - 1;
    if StatesAddressSpace.AddressSpaceQueried <= 0 then
       StatesAddressSpace.Enumerating := False;
    SendAddressSpaceQuery(StatesAddressSpace.AddressSpaceQueried);
  end;


  if StateCdi.WaitingForSpaceInfo then
  begin
    if LowAddressPresent then
      StateCdi.StartingAddress := ReceivedMessage.ExtractDataBytesAsDWord(8)
    else
      StateCdi.StartingAddress := 0;

    StateCdi.CurrentAddressPointer := StateCdi.StartingAddress;
    StateCdi.WaitingForCdi := True;
    StateCdi.WaitingForSpaceInfo := False;
    SetLength(StateCdi.FData, 0);

    WorkerMessage.LoadCDIRequest(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias, StateCdi.StartingAddress );
    SendMessage(WorkerMessage);

  end;

end;

procedure TFormNodeInterrogator.HandleGetAddressSpaceInfoNotPresentReply(ReceivedMessage: TLccMessage);
var
  TreeNode: TTreeNode;
begin

  TreeNode := TreeViewConfigMemAddressSpaceInfo.Items.AddChild(nil, 'Address Space 0x' + IntToHex(StatesAddressSpace.AddressSpaceQueried) + ' Not Present');
  TreeNode.ImageIndex := 3;

  if StatesAddressSpace.EnumeratingWellKnown then
  begin
    StatesAddressSpace.AddressSpaceQueried := StatesAddressSpace.AddressSpaceQueried - 1;
    if StatesAddressSpace.AddressSpaceQueried <= ADDRESS_SPACE_FUNCTION_MEMORY then
       StatesAddressSpace.EnumeratingWellKnown := False;
    SendAddressSpaceQuery(StatesAddressSpace.AddressSpaceQueried);
  end;

  if StatesAddressSpace.Enumerating then
  begin
    StatesAddressSpace.AddressSpaceQueried := StatesAddressSpace.AddressSpaceQueried - 1;
    if StatesAddressSpace.AddressSpaceQueried <= 0 then
       StatesAddressSpace.Enumerating := False;
    SendAddressSpaceQuery(StatesAddressSpace.AddressSpaceQueried);
  end;

end;

procedure TFormNodeInterrogator.HandleStateCdi_ReadCdi(ReceivedMessage: TLccMessage);
var
  i: Integer;
  Done: Boolean;
  CopiedCount: Integer;
begin

  if StateCdi.WaitingForCdi then
  begin

    Done := False;

    if ReceivedMessage.DataArray[1] = MCP_READ_REPLY then
      CopiedCount := ReceivedMessage.AppendDataToDataArray(StateCdi.FData, 7)
    else
      CopiedCount := ReceivedMessage.AppendDataToDataArray(StateCdi.FData, 6);

    // Find the terminating Null
    for i := 0 to Length(StateCdi.Data) - 1 do
      if StateCdi.Data[i] = $00 then
        Done := True;

    if Done then
    begin
      PrintCdiDataArray;
      StateCdi.WaitingForCdi := False;
    end else
    begin
      StateCdi.CurrentAddressPointer := StateCdi.CurrentAddressPointer + CopiedCount;
      WorkerMessage.LoadCDIRequest(Node.NodeID, Node.AliasID, TargetNode.NodeID, TargetNode.Alias, StateCdi.CurrentAddressPointer);
      SendMessage(WorkerMessage);
    end;

  end;

end;

procedure TFormNodeInterrogator.HandleStateCdi_ReadCdiFailure(ReceivedMessage: TLccMessage);
var
  ErrorCode: Word;
begin


  MemoCdi_RawData.Lines.Add('Unable to read the CDI');
  if ReceivedMessage.DataArray[1] = MCP_READ_REPLY then
     ErrorCode := ReceivedMessage.ExtractDataBytesAsWord(7)
  else
     ErrorCode := ReceivedMessage.ExtractDataBytesAsWord(6);

    MemoCdi_RawData.Lines.Add('Error Code: ' + IntToHex(ErrorCode, 4) + ' ' + ErrorCodeToStr(ErrorCode));

end;


end.

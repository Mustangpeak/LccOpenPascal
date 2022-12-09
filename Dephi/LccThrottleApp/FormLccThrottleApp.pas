unit FormLccThrottleApp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, FMX.MultiView, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Layouts,
  System.Math.Vectors, FMX.Objects, FMX.Edit, FMX.Controls3D, FMX.Layers3D, System.IOUtils,
  lcc_node_manager,
  lcc_ethernet_server,
  lcc_node_controller,
  lcc_ethernet_client,
  lcc_ethernet_common,
  lcc_common_classes,
  lcc_defines,
  lcc_xmlutilities,
  lcc_utilities,
  lcc_node,
  lcc_node_messages,
  lcc_train_server,
  lcc_alias_server,
  lcc_cdi_parser_2,

  FMX.Menus, FMX.Platform, FMX.ListBox, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Header;

const
  FILENAME_SETTINGS = 'settings.xml';
  FILENAME_MEMORY_CONFIG = 'memconfig.xml';
  FOLDERNAME_APP = 'LccThrottleApp';
  DEFAULT_IP_ADDRESS = '192.168.0.35';
  DEFAULT_PORT = 12021;
  DEFAULT_NODE_ID = '02.02.04.05.0A.0B';

type
  TLccThrottleAppForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ToolBar3: TToolBar;
    lblTitle3: TLabel;
    TabItem3: TTabItem;
    ToolBar4: TToolBar;
    lblTitle4: TLabel;
    TabItemSettings: TTabItem;
    ToolBarSettingsTop: TToolBar;
    LabelSettingsHeader: TLabel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    MultiViewConsist: TMultiView;
    ListViewTrainRoster: TListView;
    SpeedButtonTab2Hamburger: TSpeedButton;
    LayoutSettingsTab: TLayout;
    ButtonSettingsResetConnection: TButton;
    Layout3D1: TLayout3D;
    CheckBoxSettingsRawTCP: TCheckBox;
    EditSettingsNodeID: TEdit;
    TextSettingsNodeID: TText;
    TextSettingsPort: TText;
    EditSettingsPort: TEdit;
    EditSettingsIpAddress: TEdit;
    TextSettingsIpAddress: TText;
    TimerLogin: TTimer;
    LabelSystemDocumentsPath: TLabel;
    PopupMenuLabelPath: TPopupMenu;
    MenuItemSettingsLabelPath: TMenuItem;
    TextSettingsConnectionStatus: TText;
    ButtonSettingsDeleteSettingsFile: TButton;
    ButtonSettingsDeleteAppFolder: TButton;
    LabelSettingsSystemDocumentsPathHeader: TLabel;
    LabeSettingslApplicationDocumentsHeader: TLabel;
    LabelSettingsApplicationDocumentsPath: TLabel;
    TextSettingsDebugHeader: TText;
    Layout1: TLayout;
    TabControlTrainRoster: TTabControl;
    TabItemTrainRosterSelect: TTabItem;
    TabItemTrainRosterDetails: TTabItem;
    ListBoxTrainRosterItem: TListBox;
    ToolBarTrainRosterDetails: TToolBar;
    LabelTrainRosterHeader: TLabel;
    SpeedButtonTrainRosterBack: TSpeedButton;
    TabItemTrainRosterEdit: TTabItem;
    ToolBarTrainRosterEdit: TToolBar;
    LabelTrainRosterEdit: TLabel;
    SpeedButtonTrainRosterEdit: TSpeedButton;
    ActionTrainRosterTabNext: TNextTabAction;
    ActionTrainRosterTabPrev: TPreviousTabAction;
    LayoutLog: TLayout;
    HeaderLogHeader: THeader;
    LabelLogHeader: TLabel;
    MemoLog: TMemo;
    ListBoxGroupHeaderTrainDetails: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    LayoutTrainRosterEdit: TLayout;
    TextSettingsNodeAlias: TText;
    TextSettingsNodeAliasID: TText;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure TimerLoginTimer(Sender: TObject);
    procedure ButtonSettingsResetConnectionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemSettingsLabelPathClick(Sender: TObject);
    procedure EditSettingsIpAddressKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsPortKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsNodeIDKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsIpAddressExit(Sender: TObject);
    procedure EditSettingsPortExit(Sender: TObject);
    procedure EditSettingsNodeIDExit(Sender: TObject);
    procedure ButtonSettingsDeleteSettingsFileClick(Sender: TObject);
    procedure ButtonSettingsDeleteAppFolderClick(Sender: TObject);
    procedure SpeedButtonTrainRosterBackClick(Sender: TObject);
    procedure ListViewTrainRosterItemClickEx(const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure MultiViewConsistHidden(Sender: TObject);
    procedure TabControlTrainRosterChange(Sender: TObject);
  private
    FNodeManager: TLccNodeManager;
    FEthernetClient: TLccEthernetClient;
    FControllerNode: TLccTrainController;
    FConnectionState: TLccConnectionState;
    FShownOnce: Boolean;
    FClipboard: IFMXClipboardService;
    FCurrentNodeID: TNodeID;
    FCurrentPort: Word;
    FCurrentIpAddress: string;
    FPathSettingsFile: string;
    FPathApplicationFiles: string;
    FPathMemoryConfig: string;
    FActiveTrainObject: TListViewItem;
    FCdiParserFrame: TLccCdiParser;
    { Private declarations }

  protected

    procedure XmlLoadSettingsFromFile;
    procedure XmlWriteDefaultFile;

  public
    { Public declarations }
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property Controller: TLccTrainController read FControllerNode write FControllerNode;
    property ConnectionState: TLccConnectionState read FConnectionState write FConnectionState;
    property ShownOnce: Boolean read FShownOnce;
    property Clipboard: IFMXClipboardService read FClipboard write FClipboard;
    property CurrentIpAddress: string read FCurrentIpAddress write FCurrentIpAddress;
    property CurrentPort: Word read FCurrentPort write FCurrentPort;
    property CurrentNodeID: TNodeID read FCurrentNodeID write FCurrentNodeID;
    property PathApplicationFiles: string read FPathApplicationFiles write FPathApplicationFiles;
    property PathSettingsFile: string read FPathSettingsFile write FPathSettingsFile;
    property PathMemoryConfig: string read FPathMemoryConfig write FPathMemoryConfig;
    property ActiveTrainObject: TListViewItem read FActiveTrainObject write FActiveTrainObject;
    property CdiParserFrame: TLccCdiParser read FCdiParserFrame write FCdiParserFrame;

    // Callbacks
    procedure OnNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnAliasMappingChange(Sender: TObject; LccSourceNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
    procedure OnSNIPChange(TractionObject: TLccTractionObject);
    procedure OnTrainSNIPChange(TractionObject: TLccTractionObject);

    procedure OnRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);

    procedure OnSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    procedure OnNodeIDChanged(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeAliasChanged(Sender: TObject; ALccNode: TLccNode);

    procedure OnClientServerConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnClientServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);

    function ValidEditBoxKey(Key: Word): Boolean;
    function FindTrainRosterListItem(TractionObject: TLccTractionObject): TListViewItem;
    function ConnectionLogin: Boolean;
  end;

var
  LccThrottleAppForm: TLccThrottleAppForm;

implementation

{$R *.fmx}

function TLccThrottleAppForm.ConnectionLogin: Boolean;
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  Result := True;

  LocalInfo := TLccEthernetConnectionInfo.Create;
  try
    TextSettingsConnectionStatus.Text := 'connecting';
    TextSettingsConnectionStatus.TextSettings.FontColor := TAlphaColors.Black;

    LocalInfo.AutoResolveIP := False;
    LocalInfo.ListenerPort := CurrentPort;
    LocalInfo.ListenerIP := CurrentIpAddress;
    LocalInfo.GridConnect := not CheckBoxSettingsRawTCP.IsChecked;

    EthernetClient.OpenConnection(LocalInfo);
  finally
    LocalInfo.Free;
  end;
end;

procedure TLccThrottleAppForm.EditSettingsIpAddressExit(Sender: TObject);
begin
  if ValidateIPString(EditSettingsIpAddress.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'ipaddress', EditSettingsIpAddress.Text, True);
end;

procedure TLccThrottleAppForm.EditSettingsIpAddressKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if not( CharInSet(KeyChar, ['0'..'9', '.']) or ValidEditBoxKey(Key) ) then
  begin
    Key := 0;
    KeyChar := #0;
  end;
  if (Key = vkReturn) and ValidateIPString(EditSettingsIpAddress.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'ipaddress', EditSettingsIpAddress.Text, True);
end;

procedure TLccThrottleAppForm.EditSettingsNodeIDExit(Sender: TObject);
begin
  if ValidateNodeIDString(EditSettingsNodeID.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'nodeid', EditSettingsNodeID.Text, True);
end;

procedure TLccThrottleAppForm.EditSettingsNodeIDKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if CharInSet(KeyChar, ['a'..'f']) then
   KeyChar := UpCase(KeyChar);

  if not( CharInSet(KeyChar, ['0'..'9', 'A'..'F', '.']) or ValidEditBoxKey(Key) ) then
  begin
    Key := 0;
    KeyChar := #0;
  end;

  if (Key = vkReturn) and ValidateNodeIDString(EditSettingsNodeID.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'nodeid', EditSettingsNodeID.Text, True);
end;

procedure TLccThrottleAppForm.EditSettingsPortExit(Sender: TObject);
begin
  if ValidatePort(EditSettingsPort.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'port', EditSettingsPort.Text, True);
end;

procedure TLccThrottleAppForm.EditSettingsPortKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if not( CharInSet(KeyChar, ['0'..'9']) or ValidEditBoxKey(Key) ) then
  begin
    Key := 0;
    KeyChar := #0;
  end;
  if (Key = vkReturn) and ValidatePort(EditSettingsPort.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'port', EditSettingsPort.Text, True);
end;

procedure TLccThrottleAppForm.ButtonSettingsDeleteAppFolderClick(Sender: TObject);
var
  Files: TStringDynArray;
  i: Integer;
begin
  if TDirectory.Exists(PathApplicationFiles) then
  begin
    Files := TDirectory.GetFiles(PathApplicationFiles);
    for i  := 0 to Length(Files) - 1 do
      TFile.Delete(Files[i]);
    TDirectory.Delete(PathApplicationFiles)
  end;
end;

procedure TLccThrottleAppForm.ButtonSettingsDeleteSettingsFileClick(Sender: TObject);
begin
  if TFile.Exists(PathSettingsFile) then
    TFile.Delete(PathSettingsFile)
end;

procedure TLccThrottleAppForm.ButtonSettingsResetConnectionClick(Sender: TObject);
begin
  if not ValidateIPString(EditSettingsIpAddress.Text) then
  begin
    TextSettingsConnectionStatus.Text := 'Invalid IP Address';
    TextSettingsConnectionStatus.TextSettings.FontColor := TAlphaColors.Red;
    TextSettingsIpAddress.TextSettings.FontColor := TAlphaColors.Red;
    Exit
  end;
  if not ValidateNodeIDString(EditSettingsNodeID.Text) then
  begin
    TextSettingsConnectionStatus.Text := 'Invalid NodeID';
    TextSettingsConnectionStatus.TextSettings.FontColor := TAlphaColors.Red;
    TextSettingsNodeID.TextSettings.FontColor := TAlphaColors.Red;
    Exit
  end;
  if not ValidatePort(EditSettingsPort.Text) then
  begin
    TextSettingsConnectionStatus.Text := 'Invalid Port (must be 65535 or less)';
    TextSettingsConnectionStatus.TextSettings.FontColor := TAlphaColors.Red;
    TextSettingsPort.TextSettings.FontColor := TAlphaColors.Red;
    Exit
  end;

  TextSettingsConnectionStatus.TextSettings.FontColor := TAlphaColors.Black;
  TextSettingsIpAddress.TextSettings.FontColor := TAlphaColors.Black;
  TextSettingsPort.TextSettings.FontColor := TAlphaColors.Black;

  CurrentIpAddress := EditSettingsIpAddress.Text;
  CurrentPort := StrToInt(EditSettingsPort.Text);
  CurrentNodeID := StrToNodeID(EditSettingsNodeID.Text, True);

  ConnectionLogin
end;

function TLccThrottleAppForm.FindTrainRosterListItem(TractionObject: TLccTractionObject): TListViewItem;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < ListViewTrainRoster.Items.Count do
  begin
    if TractionObject = TLccTractionObject( ListViewTrainRoster.Items[i].Tag) then
      Result := ListViewTrainRoster.Items[i];
    Inc(i);
  end;
end;

procedure TLccThrottleAppForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  TimerLogin.Enabled := False;
  NodeManager.ReleaseAliasAll;
  EthernetClient.CloseConnection;
end;

procedure TLccThrottleAppForm.FormCreate(Sender: TObject);
begin

  // Local field setup

  // Lcc library setup
  NodeManager := TLccNodeManager.Create(nil, True);
  EthernetClient := TLccEthernetClient.Create(nil, NodeManager);

  EthernetClient.OnConnectionStateChange := OnClientServerConnectionChange;
  EthernetClient.OnErrorMessage := OnClientServerErrorMessage;
  EthernetClient.OnLccMessageReceive := OnReceiveMessage;
  EthernetClient.OnLccMessageSend := OnSendMessage;

  NodeManager.OnNodeAliasIDChanged := OnNodeAliasChanged;
  NodeManager.OnNodeIDChanged := OnNodeIdChanged;

  // Default values to the settings
  CurrentIpAddress := DEFAULT_IP_ADDRESS;
  CurrentPort := DEFAULT_PORT;
  CurrentNodeID := StrToNodeID( DEFAULT_NODE_ID, True);

  // Default Paths for files
  PathApplicationFiles := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP;
  PathSettingsFile := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_SETTINGS;
  PathMemoryConfig := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_MEMORY_CONFIG;
end;

procedure TLccThrottleAppForm.FormDestroy(Sender: TObject);
begin
  NodeManager.ReleaseAliasAll;
  FreeAndNil(FEthernetClient);
  FreeAndNil(FNodeManager);
end;

procedure TLccThrottleAppForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
 //   if (TabControl1.ActiveTab = TabItem1) and (TabControl2.ActiveTab = TabItem6) then
 //   begin
 //     TabControl2.Previous;
 //     Key := 0;
 //   end;
  end;
end;

procedure TLccThrottleAppForm.FormShow(Sender: TObject);
begin
  if not ShownOnce then
  begin
    FShownOnce := True;

    // Setup common variables to use
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, FClipboard);
    LabelSystemDocumentsPath.Text := TPath.GetDocumentsPath;
    LabelSettingsApplicationDocumentsPath.Text := PathApplicationFiles;

    // Setup components to a standard state in case forgotten in the designer
    TabControl1.ActiveTab := TabItem1;    // This defines the default active tab at runtime
    MultiViewConsist.Mode := TMultiViewMode.Drawer;
    TabControlTrainRoster.ActiveTab := TabItemTrainRosterSelect;
    TimerLogin.Enabled := True; // Try to connect


    CdiParserFrame := TLccCdiParser.Create;
    CdiParserFrame.Render_CDI_Interface(LayoutTrainRosterEdit, nil);

    if FileExists(PathSettingsFile) then
      XmlLoadSettingsFromFile
    else
      XmlWriteDefaultFile;

    EditSettingsIpAddress.Text := CurrentIpAddress;
    EditSettingsPort.Text := IntToStr( CurrentPort);
    EditSettingsNodeID.Text := NodeIDToString(CurrentNodeID, True);

    ButtonSettingsResetConnectionClick(Self)
  end;
end;

procedure TLccThrottleAppForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[TabControl1.TabCount - 1] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex + 1];
        Handled := True;
      end;

    sgiRight:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[0] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex - 1];
        Handled := True;
      end;
  end;
end;

procedure TLccThrottleAppForm.ListViewTrainRosterItemClickEx(const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
var
  ListItem: TListItem;
begin
  if ItemObject is TListItemAccessory then
    ActionTrainRosterTabNext.Execute
  else begin
    ListItem := ListViewTrainRoster.Items[ItemIndex];
    MultiViewConsist.HideMaster
  end;
end;

procedure TLccThrottleAppForm.MenuItemSettingsLabelPathClick(Sender: TObject);
begin
  if Assigned(Clipboard) then
    Clipboard.SetClipboard(LabelSystemDocumentsPath.Text)
end;

procedure TLccThrottleAppForm.MultiViewConsistHidden(Sender: TObject);
begin
  ActiveTrainObject := nil;
end;

procedure TLccThrottleAppForm.OnAliasMappingChange(Sender: TObject;
  LccSourceNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
begin

end;

procedure TLccThrottleAppForm.OnClientServerConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  if Sender is TLccConnectionThread then
  begin
    ConnectionState := Info.ConnectionState;

    case Info.ConnectionState of
      lcsConnecting :
        begin
          TextSettingsConnectionStatus.Text := 'connecting';
        end;
     lcsConnected :
        begin
          TextSettingsConnectionStatus.Text := 'connected';
          if NodeManager.Nodes.Count = 0 then
          begin
            Controller := NodeManager.AddNodeByClass('', TLccTrainController, True, NULL_NODE_ID) as TLccTrainController;
            Controller.TractionServer.OnSNIPChange := OnSNIPChange;
            Controller.TractionServer.OnTrainSNIPChange := OnTrainSNIPChange;
            Controller.TractionServer.OnRegisterChange := OnRegisterChange;
        //    Controller.TractionServer.OnEmergencyStopChange := OnEmergencyStopChange;
       //     Controller.TractionServer.OnFunctionChange := OnFunctionChange;
       //     Controller.TractionServer.OnSpeedChange := OnSpeedChange;
       //     Controller.OnControllerAssignChange := OnControllerAssignChange;
          end;
        end;
      lcsDisconnecting :
        begin
          TextSettingsConnectionStatus.Text := 'disconnecting';
        end;
      lcsDisconnected :
        begin
          TextSettingsConnectionStatus.Text := 'disconnected';
        end;
    end;
  end;
end;

procedure TLccThrottleAppForm.OnClientServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
end;

procedure TLccThrottleAppForm.OnSNIPChange(TractionObject: TLccTractionObject);
var
  ListViewItem: TListViewItem;
begin
  ListViewItem := FindTrainRosterListItem(TractionObject);
  if Assigned(ListViewItem) then
  begin
    ListViewItem.Text := TractionObject.SNIP.UserName
  end;
end;

procedure TLccThrottleAppForm.OnTrainSNIPChange(TractionObject: TLccTractionObject);
begin

end;

procedure TLccThrottleAppForm.OnNodeAliasChanged(Sender: TObject; ALccNode: TLccNode);
begin
  TextSettingsNodeAliasID.Text := ALccNode.AliasIDStr;
end;

procedure TLccThrottleAppForm.OnNodeIDChanged(Sender: TObject; ALccNode: TLccNode);
begin
  EditSettingsNodeID.Text := ALccNode.NodeIDStr
end;

procedure TLccThrottleAppForm.OnNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = Controller then
    Controller.FindAllTrains;
end;

procedure TLccThrottleAppForm.OnReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Add('R: ' + MessageToDetailedMessage(LccMessage))
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TLccThrottleAppForm.OnSendMessage(Sender: TObject;  LccMessage: TLccMessage);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Add('S: ' + MessageToDetailedMessage(LccMessage))
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TLccThrottleAppForm.OnRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);
var
  TrainListViewItem: TListViewItem;
begin
  if IsRegistered then
  begin
    TrainListViewItem := ListViewTrainRoster.Items.Add;
    TrainListViewItem.Tag := nativeint( TractionObject);   /// HOW DO WE HOLD REFERENCES FOR FUTURE USE OF UPDATES IN EVENTS?

    if TractionObject.SNIP.Valid then
    begin
      TrainListViewItem.Text := 'Train: ' + TractionObject.SNIP.UserName;
    end else
    begin
      TrainListViewItem.Text := 'Train: ' + NodeIDToString(TractionObject.NodeID, True);
    end;
  end else
  begin
    TrainListViewItem := FindTrainRosterListItem(TractionObject);
    if Assigned(TrainListViewItem) then
    begin
      if ActiveTrainObject = TrainListViewItem then
      begin
        TabControlTrainRoster.TabIndex := 0;
        ActiveTrainObject := nil;
      end;

    end;

  end;
end;

procedure TLccThrottleAppForm.SpeedButtonTrainRosterBackClick(Sender: TObject);
begin
  ActionTrainRosterTabPrev.Execute
end;

procedure TLccThrottleAppForm.TabControlTrainRosterChange(Sender: TObject);
begin
  if TabControlTrainRoster.TabIndex = 0 then
    ActiveTrainObject := nil;
end;

procedure TLccThrottleAppForm.TimerLoginTimer(Sender: TObject);
begin
  if not (EthernetClient.Connected or EthernetClient.Connecting) then
    ButtonSettingsResetConnectionClick(Self)
end;

function TLccThrottleAppForm.ValidEditBoxKey(Key: Word): Boolean;
begin
  // HardwareBack is to handle Android
  Result := (Key = vkReturn) or (Key = vkHardwareBack) or (Key = vkBack) or (Key = vkDelete) or (Key = vkLeft) or (Key = vkRight)
end;

procedure TLccThrottleAppForm.XmlLoadSettingsFromFile;
var
  SettingsXML: LccXmlDocument;
  RootNode, ChildNode: LccXmlNode;
begin
  // Read in the Setting File
  SettingsXML := XmlLoadFromFile(PathSettingsFile);
  RootNode := XmlFindRootNode(SettingsXML, 'settings');
  if Assigned(RootNode) then
  begin
    ChildNode := XmlFindChildNode(RootNode, 'ipaddress');
    if Assigned(ChildNode) then
    begin
      if ValidateIPString( XmlNodeTextContent(ChildNode)) then
        CurrentIpAddress := XmlNodeTextContent(ChildNode);
    end;
    ChildNode := XmlFindChildNode(RootNode, 'port');
    if Assigned(ChildNode) then
    begin
      if ValidatePort( string( XmlNodeTextContent(ChildNode))) then
        CurrentPort := StrToInt( string( XmlNodeTextContent(ChildNode)));
    end;
    ChildNode := XmlFindChildNode(RootNode, 'nodeid');
    if Assigned(ChildNode) then
    begin
      if ValidateNodeIDString(XmlNodeTextContent(ChildNode)) then
        CurrentNodeID := StrToNodeID( XmlNodeTextContent(ChildNode), True);
    end;
  end;
end;

procedure TLccThrottleAppForm.XmlWriteDefaultFile;
var
  SettingsXML: LccXmlDocument;
  RootNode, ChildNode: LccXmlNode;
begin
  if not DirectoryExists(PathApplicationFiles) then
    ForceDirectories(PathApplicationFiles);

  if DirectoryExists(PathApplicationFiles) then
  begin
     SettingsXML := XmlCreateEmptyDocument;
     RootNode := XmlCreateRootNode(SettingsXML, 'settings', '');
     ChildNode := XmlCreateChildNode(SettingsXML, RootNode, 'ipaddress', CurrentIpAddress);
     ChildNode := XmlCreateChildNode(SettingsXML, RootNode, 'port', IntToStr(CurrentPort));
     ChildNode := XmlCreateChildNode(SettingsXML, RootNode, 'nodeid', NodeIDToString( CurrentNodeID, True));
     XmlWriteToFile(PathSettingsFile, SettingsXML);
     XmlFreeDocument(SettingsXML);
  end;
end;

end.


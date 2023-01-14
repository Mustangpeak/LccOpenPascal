unit FormLccThrottleApp;

// TODO:
// Still hang on shutdown once in a while

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, FMX.MultiView, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Layouts,
  System.Math.Vectors, FMX.Objects, FMX.Edit, FMX.Controls3D, FMX.Layers3D, System.IOUtils,
  FMX.Menus, FMX.Platform, FMX.ListBox, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Header, FMX.EditBox, FMX.SpinBox, System.ImageList, FMX.ImgList,
  FMX.TreeView, FMX.Colors, FMX.Effects, FMX.SearchBox,
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
  lcc_base_classes,
  lcc_cdi_parser;

const
  FILENAME_SETTINGS = 'settings.xml';
  FILENAME_MEMORY_CONFIG = 'memconfig.xml';
  FOLDERNAME_APP = 'LccThrottleApp';
  DEFAULT_IP_ADDRESS = '192.168.0.35';
  DEFAULT_PORT = 12021;
  DEFAULT_NODE_ID = '02.02.04.05.0A.0B';

type
  TLccThrottleAppForm = class(TForm)
    TabControlMain: TTabControl;
    TabItemTrains: TTabItem;
    TabItemLog: TTabItem;
    ToolBarMain: TToolBar;
    lblTitleMain: TLabel;
    TabItem3: TTabItem;
    ToolBar4: TToolBar;
    lblTitle4: TLabel;
    TabItemSettings: TTabItem;
    ToolBarSettingsTop: TToolBar;
    LabelSettingsHeader: TLabel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    ActionTabMainNext: TNextTabAction;
    ActionTabMainPrev: TPreviousTabAction;
    MultiViewRoster: TMultiView;
    ListViewTrainRoster: TListView;
    SpeedButtonTrainsRoster: TSpeedButton;
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
    LayoutTrains: TLayout;
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
    ActionTabTrainRosterNext: TNextTabAction;
    ActionTabTrainRosterPrev: TPreviousTabAction;
    LayoutLog: TLayout;
    MemoLog: TMemo;
    ListBoxGroupHeaderTrainDetails: TListBoxGroupHeader;
    ListBoxItemTrainsDetailsManufacturer: TListBoxItem;
    ListBoxItemTrainsDetailsModel: TListBoxItem;
    ListBoxItemTrainsDetailsSofwareVersion: TListBoxItem;
    ListBoxItemTrainsDetailsHardwareVersion: TListBoxItem;
    ListBoxItemTrainsDetailsUserName: TListBoxItem;
    ListBoxItemTrainsDetailsUserDescription: TListBoxItem;
    TextSettingsNodeAlias: TText;
    TextSettingsNodeAliasID: TText;
    ListBoxGroupHeaderConsists: TListBoxGroupHeader;
    ListBoxItemTrainsDetailsConsists: TListBoxItem;
    MultiViewConsists: TMultiView;
    TabControlTrainConsists: TTabControl;
    TabItemTrainConsists: TTabItem;
    TabItem2: TTabItem;
    LayoutConsistConsists: TLayout;
    LabelConsistConsists: TLabel;
    ToolBarConsistConsists: TToolBar;
    TreeViewConsistConsists: TTreeView;
    SpeedButtonTrainsConsist: TSpeedButton;
    LayoutTrainsThrottle: TLayout;
    VertScrollBoxFunctions: TVertScrollBox;
    ButtonF0: TButton;
    ButtonF19: TButton;
    ButtonF18: TButton;
    ButtonF17: TButton;
    ButtonF16: TButton;
    ButtonF15: TButton;
    ButtonF14: TButton;
    ButtonF13: TButton;
    ButtonF12: TButton;
    ButtonF11: TButton;
    ButtonF10: TButton;
    ButtonF1: TButton;
    ButtonF2: TButton;
    ButtonF3: TButton;
    ButtonF4: TButton;
    ButtonF5: TButton;
    ButtonF6: TButton;
    ButtonF7: TButton;
    ButtonF8: TButton;
    ButtonF9: TButton;
    LayoutTrainsThrottleLever: TLayout;
    TrackBarTrainsThrottleLever: TTrackBar;
    ButtonF20: TButton;
    ButtonF21: TButton;
    ButtonF22: TButton;
    ButtonF23: TButton;
    ButtonF24: TButton;
    ButtonF25: TButton;
    ButtonF26: TButton;
    ButtonF27: TButton;
    Layout1: TLayout;
    SpeedButtonTrainsRev: TSpeedButton;
    SpeedButtonTrainsFwd: TSpeedButton;
    SpeedButtonTrainsStop: TSpeedButton;
    LayoutTrainsThrottleLeverBkgnd: TLayout;
    LayoutTrainsThrottleInfo: TLayout;
    LabelTrainsSpeedHeader: TLabel;
    LabelTrainsSpeed: TLabel;
    LabelTrainRosterEditContainer: TLabel;
    ToolBarLog: TToolBar;
    SpeedButtonLogClear: TSpeedButton;
    SpeedButtonLogEnable: TSpeedButton;
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
    procedure MultiViewRosterHidden(Sender: TObject);
    procedure TabControlTrainRosterChange(Sender: TObject);
    procedure ListBoxItemTrainsDetailsUserNameClick(Sender: TObject);
    procedure ListBoxItemTrainsDetailsUserDescriptionClick(Sender: TObject);
    procedure ButtonFnClick(Sender: TObject);
    procedure SpeedButtonTrainsFwdClick(Sender: TObject);
    procedure SpeedButtonTrainsRevClick(Sender: TObject);
    procedure TrackBarTrainsThrottleLeverChange(Sender: TObject);
    procedure SpeedButtonTrainsStopClick(Sender: TObject);
    procedure SpeedButtonLogClearClick(Sender: TObject);
    procedure ListViewTrainRosterSearchChange(Sender: TObject);
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
    FCDILayoutFrame: TLayout;
    { Private declarations }

  protected

    property CDILayoutFrame: TLayout read FCDILayoutFrame write FCDILayoutFrame;
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

    procedure OnEngineMemorySpaceAccessCallback(MemorySpaceAccessEngine: TLccEngineMemorySpaceAccess);
    procedure OnEngineMemorySpaceAccessProgressCallback(AEngineMemorySpaceAccess: TLccEngineMemorySpaceAccess);
    procedure OnCdiParserProcessCallback(ACdiParser: TLccCdiParser; ProgressInfo: string);

    function ValidEditBoxKey(Key: Word): Boolean;
    function ConnectionLogin: Boolean;
    function FindListviewItemByTagTractionObject(AListview: TListView; ATagObject: TObject): TListviewItem;
    function SelectedRosterEqualsTractionObject(TractionObject: TLccTractionObject): Boolean;
    procedure TrainTabDetailsClear;
    procedure TrainTabDetailsLoad(TractionObject: TLccTractionObject);
    procedure TrainTabCDIClear;
    procedure TrainTabCDILoad(TractionObject: TLccTractionObject);
    procedure TrainTabCDISelect;
    procedure RenderCDI(TractionObject: TLccTractionObject);
    procedure SelectTrain(TractionObject: TLccTractionObject);
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

procedure TLccThrottleAppForm.ButtonFnClick(Sender: TObject);
begin
  beep;
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

function TLccThrottleAppForm.FindListviewItemByTagTractionObject(AListview: TListView; ATagObject: TObject): TListviewItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to AListview.Items.UnfilteredItems.Count - 1 do
  begin
    if AListview.Items.UnfilteredItems[i].TagObject = ATagObject then
    begin
      Result := AListview.Items.UnfilteredItems[i] as TListviewItem;
      Break
    end;
  end;
end;

procedure TLccThrottleAppForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  TrainTabCDIClear;
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
  CdiParserFrame := TLccCdiParser.Create(nil);

  EthernetClient.OnConnectionStateChange := OnClientServerConnectionChange;
  EthernetClient.OnErrorMessage := OnClientServerErrorMessage;
  EthernetClient.OnLccMessageReceive := OnReceiveMessage;
  EthernetClient.OnLccMessageSend := OnSendMessage;

  NodeManager.OnNodeAliasIDChanged := OnNodeAliasChanged;
  NodeManager.OnNodeIDChanged := OnNodeIdChanged;
  NodeManager.OnNodeLogin := OnNodeLogin;

  // Default values to the settings
  CurrentIpAddress := DEFAULT_IP_ADDRESS;
  CurrentPort := DEFAULT_PORT;
  CurrentNodeID := StrToNodeID( DEFAULT_NODE_ID, True);

  // Default Paths for files

 {

 PathApplicationFiles := TPath.GetDocumentsPath;
 if DirectoryExists(PathApplicationFiles) then
 begin
   if CreateDir(TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP) then
   begin
     beep;
   end;
 end;

 if DirectoryExists(TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP) then
 begin
   beep;
 end;

 if FileExists(TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_SETTINGS) then
 begin
   if DeleteFile(TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_SETTINGS) then
   begin
     beep;
   end;
 end;

 if FileExists(TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_MEMORY_CONFIG) then
 begin
   if DeleteFile(TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_MEMORY_CONFIG) then
   begin
     beep;
   end;
 end;
           }

  PathApplicationFiles := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP;
  PathSettingsFile := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_SETTINGS;
  PathMemoryConfig := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_MEMORY_CONFIG;
end;

procedure TLccThrottleAppForm.FormDestroy(Sender: TObject);
begin
  NodeManager.ReleaseAliasAll;
  FreeAndNil(FEthernetClient);
  FreeAndNil(FNodeManager);
  FreeAndNil(FCdiParserFrame);
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
    TabControlMain.ActiveTab := TabItemTrains;    // This defines the default active tab at runtime
    MultiViewRoster.Mode := TMultiViewMode.Drawer;
    MultiViewConsists.Mode := TMultiViewMode.Drawer;
    TabControlTrainRoster.ActiveTab := TabItemTrainRosterSelect;
    TimerLogin.Enabled := True; // Try to connect

    if FileExists(PathSettingsFile) then
      XmlLoadSettingsFromFile
    else
      XmlWriteDefaultFile;

    EditSettingsIpAddress.Text := CurrentIpAddress;
    EditSettingsPort.Text := IntToStr( CurrentPort);
    EditSettingsNodeID.Text := NodeIDToString(CurrentNodeID, True);

    TabControlTrainRoster.TabPosition := TTabPosition.None;

    ButtonSettingsResetConnectionClick(Self)
  end;
end;

procedure TLccThrottleAppForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControlMain.ActiveTab <> TabControlMain.Tabs[TabControlMain.TabCount - 1] then
          TabControlMain.ActiveTab := TabControlMain.Tabs[TabControlMain.TabIndex + 1];
        Handled := True;
      end;

    sgiRight:
      begin
        if TabControlMain.ActiveTab <> TabControlMain.Tabs[0] then
          TabControlMain.ActiveTab := TabControlMain.Tabs[TabControlMain.TabIndex - 1];
        Handled := True;
      end;
  end;
end;

procedure TLccThrottleAppForm.ListBoxItemTrainsDetailsUserDescriptionClick(Sender: TObject);
begin
  TrainTabCDISelect;
end;

procedure TLccThrottleAppForm.ListBoxItemTrainsDetailsUserNameClick(Sender: TObject);
begin
  TrainTabCDISelect;
end;

procedure TLccThrottleAppForm.ListViewTrainRosterItemClickEx(const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
var
  ListItem: TListItem;
  TractionObject: TLccTractionObject;
begin
  TractionObject := ListViewTrainRoster.Items[ItemIndex].TagObject as TLccTractionObject;

  // Did we click on the Accessory to move to the Details tab?
  if ItemObject is TListItemAccessory then
  begin
    // Move to the Details tab
    ActionTabTrainRosterNext.Execute;

    // Reset the CDI Tab
    TrainTabCDIClear;

    // Need to load the information into that tab if we can.  If not we need to call for it
    if TractionObject.SNIP.Valid then
    begin
      TrainTabDetailsLoad(TractionObject);
    end else
    begin
      // Clear the details so they are not stale from a previous load
      TrainTabDetailsClear;
      Controller.SendSNIPRequest(TractionObject.NodeID, TractionObject.NodeAlias)
    end;

    if not TractionObject.NodeCDI.Valid  then
    begin
      Controller.EngineMemorySpaceAccess.Reset;
      Controller.EngineMemorySpaceAccess.Assign(lems_Read, MSI_CDI, True, 0, 0, False, TractionObject.NodeID, TractionObject.NodeAlias, OnEngineMemorySpaceAccessCallback, OnEngineMemorySpaceAccessProgressCallback);
      Controller.EngineMemorySpaceAccess.TagObject := TractionObject;
      Controller.EngineMemorySpaceAccess.Start;
    end;

  end else
  begin
    ListItem := ListViewTrainRoster.Items[ItemIndex];
    ShowMessage(TractionObject.DisplayName);
    MultiViewRoster.HideMaster
  end;
end;

procedure TLccThrottleAppForm.ListViewTrainRosterSearchChange(Sender: TObject);
var
  I: Integer;
  SearchBox: TSearchBox;
  List: TListView;
begin

beep;
  SearchBox := nil;
  List := Sender as TListView;
  for I := 0 to List.Controls.Count-1 do
    if List.Controls[I].ClassType = TSearchBox then
    begin
      SearchBox := TSearchBox(List.Controls[I]);
      Break;
    end;
  if Assigned(SearchBox) then
  begin

  end;
 // StatusBar.Text := IntToStr(List.Items.Count) + ' list items match ' + QuotedStr(SearchBox.Text) + '.';
end;

procedure TLccThrottleAppForm.MenuItemSettingsLabelPathClick(Sender: TObject);
begin
  if Assigned(Clipboard) then
    Clipboard.SetClipboard(LabelSystemDocumentsPath.Text)
end;

procedure TLccThrottleAppForm.MultiViewRosterHidden(Sender: TObject);
begin
  ActiveTrainObject := nil;
end;

procedure TLccThrottleAppForm.OnAliasMappingChange(Sender: TObject; LccSourceNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
begin

end;

procedure TLccThrottleAppForm.OnCdiParserProcessCallback(ACdiParser: TLccCdiParser; ProgressInfo: string);
begin
  LabelTrainRosterEdit.BeginUpdate;
  LabelTrainRosterEdit.Text := ProgressInfo;
  LabelTrainRosterEdit.EndUpdate;
  Application.ProcessMessages;
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

procedure TLccThrottleAppForm.OnEngineMemorySpaceAccessProgressCallback(AEngineMemorySpaceAccess: TLccEngineMemorySpaceAccess);
begin
  if AEngineMemorySpaceAccess.MemorySpace = MSI_CDI then
    LabelTrainRosterEditContainer.Text := 'Loading... ' + IntToStr(AEngineMemorySpaceAccess.MemoryStream.Size) + ' bytes';
end;

procedure TLccThrottleAppForm.OnEngineMemorySpaceAccessCallback(MemorySpaceAccessEngine: TLccEngineMemorySpaceAccess);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := MemorySpaceAccessEngine.TagObject as TLccTractionObject;
  if MemorySpaceAccessEngine.EngineState = lesComplete then
  begin
    // The CDI was updated by the TractionServer code
    if TractionObject.NodeCDI.Valid then
      RenderCDI(TractionObject);
  end;
end;

procedure TLccThrottleAppForm.OnSNIPChange(TractionObject: TLccTractionObject);
var
  ListViewItem: TListViewItem;
begin
  ListViewItem := FindListviewItemByTagTractionObject(ListViewTrainRoster, TractionObject);
  if Assigned(ListViewItem) then
  begin
    ListViewItem.Text := TractionObject.DisplayName;
    TrainTabDetailsLoad(TractionObject);
  end;
end;

procedure TLccThrottleAppForm.OnTrainSNIPChange(TractionObject: TLccTractionObject);
var
  ListViewItem: TListViewItem;
begin
  ListViewItem := FindListviewItemByTagTractionObject(ListViewTrainRoster, TractionObject);
  if Assigned(ListViewItem) then
  begin
    ListViewItem.Text := TractionObject.DisplayName;
    TrainTabDetailsLoad(TractionObject);
  end;
end;

procedure TLccThrottleAppForm.RenderCDI(TractionObject: TLccTractionObject);
var
  XMLDoc: TLccXmlDocument;
  LocalNodeIdentification: TLccNodeIdentificationObject;
begin
  if TractionObject.NodeCDI.Valid then
  begin
    LocalNodeIdentification := TLccNodeIdentificationObject.Create;
    LocalNodeIdentification.AssignID(TractionObject.NodeID, TractionObject.NodeAlias);

    ShowMessage(TractionObject.NodeCDI.CDI);

    XMLDoc := XmlLoadFromText( LccDOMString( TractionObject.NodeCDI.CDI));
    try
       CDILayoutFrame := CdiParserFrame.Build_CDI_Interface(Controller, LocalNodeIdentification, LabelTrainRosterEditContainer, XMLDoc, OnCdiParserProcessCallback);
    finally
      XmlFreeDocument(XMLDoc);
      LocalNodeIdentification.Free;
    end;
  end;
end;

procedure TLccThrottleAppForm.OnNodeAliasChanged(Sender: TObject; ALccNode: TLccNode);
begin
  TextSettingsNodeAliasID.Text := ALccNode.AliasIDStr;
end;

procedure TLccThrottleAppForm.OnNodeIDChanged(Sender: TObject; ALccNode: TLccNode);
begin
  EditSettingsNodeID.Text := ALccNode.NodeIDStr[True]
end;

procedure TLccThrottleAppForm.OnNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = Controller then
    Controller.FindAllTrains;
end;

procedure TLccThrottleAppForm.OnReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
 if SpeedButtonLogEnable.IsPressed then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      MemoLog.Lines.Add('R: ' + MessageToDetailedMessage(LccMessage))
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TLccThrottleAppForm.OnSendMessage(Sender: TObject;  LccMessage: TLccMessage);
begin
  if SpeedButtonLogEnable.IsPressed then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      MemoLog.Lines.Add('S: ' + MessageToDetailedMessage(LccMessage))
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TLccThrottleAppForm.OnRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);
var
  ListViewItem: TListViewItem;
begin
  ListviewItem := FindListviewItemByTagTractionObject(ListViewTrainRoster, TractionObject);

  if IsRegistered then
  begin
    if Assigned(ListviewItem) then
      ListviewItem.Text := TractionObject.DisplayName
    else begin
      ListViewItem := ListViewTrainRoster.Items.Add;
      ListViewItem.TagObject := TractionObject;
      ListViewItem.Text := TractionObject.DisplayName;
      if not TractionObject.SNIP.Valid then
        Controller.SendSNIPRequest(TractionObject.NodeID, TractionObject.NodeAlias);
    end;
  end else
  begin
    if Assigned(ListViewItem) then
    begin
      if (ActiveTrainObject = ListViewItem) then
      begin
        TabControlTrainRoster.TabIndex := 0;
        ActiveTrainObject := nil;
      end;
      ListViewTrainRoster.Items.Delete(ListviewItem.Index)
    end;
  end;
end;

function TLccThrottleAppForm.SelectedRosterEqualsTractionObject(
  TractionObject: TLccTractionObject): Boolean;
begin
  Result := False;
  if ListViewTrainRoster.Selected <> nil then
    Result := ListViewTrainRoster.Selected.TagObject = TractionObject
end;

procedure TLccThrottleAppForm.SelectTrain(TractionObject: TLccTractionObject);
begin

end;

procedure TLccThrottleAppForm.SpeedButtonLogClearClick(Sender: TObject);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Clear;
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TLccThrottleAppForm.SpeedButtonTrainRosterBackClick(Sender: TObject);
begin
  ActionTabTrainRosterPrev.Execute;
  TrainTabCDIClear;
end;

procedure TLccThrottleAppForm.SpeedButtonTrainsFwdClick(Sender: TObject);
begin
  SpeedButtonTrainsRev.IsPressed := not SpeedButtonTrainsFwd.IsPressed;
end;

procedure TLccThrottleAppForm.SpeedButtonTrainsRevClick(Sender: TObject);
begin
  SpeedButtonTrainsFwd.IsPressed := not SpeedButtonTrainsRev.IsPressed;
end;

procedure TLccThrottleAppForm.SpeedButtonTrainsStopClick(Sender: TObject);
begin
  TrackBarTrainsThrottleLever.Value := 100;
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

procedure TLccThrottleAppForm.TrackBarTrainsThrottleLeverChange(Sender: TObject);
begin
  LabelTrainsSpeed.Text := FloatToStr(100.0 - TrackBarTrainsThrottleLever.Value)
end;

procedure TLccThrottleAppForm.TrainTabCDIClear;
begin
  // Clear Tab 2, CDI
  if Assigned(CDILayoutFrame) then
  begin
    try
      CdiParserFrame.Clear_CDI_Interface(False);
    finally
      CDILayoutFrame := nil;
    end;
  end;
end;

procedure TLccThrottleAppForm.TrainTabCDILoad(TractionObject: TLccTractionObject);
begin
  if SelectedRosterEqualsTractionObject(TractionObject) then
  begin
    TrainTabCDIClear;
    if TractionObject.NodeCDI.Valid then
    begin
      RenderCDI(TractionObject);
    end
  end;
end;

procedure TLccThrottleAppForm.TrainTabCDISelect;
begin
   if Assigned(ListViewTrainRoster.Selected) then
   begin
      ActionTabTrainRosterNext.Execute;
      TrainTabCDILoad(ListViewTrainRoster.Selected.TagObject as TLccTractionObject)
   end;
end;

procedure TLccThrottleAppForm.TrainTabDetailsClear;
begin
  // Initialize Tab 1, Details
  ListBoxItemTrainsDetailsManufacturer.ItemData.Detail := 'loading...';
  ListBoxItemTrainsDetailsModel.ItemData.Detail := 'loading...';
  ListBoxItemTrainsDetailsSofwareVersion.ItemData.Detail := 'loading...';
  ListBoxItemTrainsDetailsHardwareVersion.ItemData.Detail := 'loading...';
  ListBoxItemTrainsDetailsUserName.ItemData.Detail := 'loading...';
  ListBoxItemTrainsDetailsUserDescription.ItemData.Detail := 'loading...';
  ListBoxItemTrainsDetailsConsists.ItemData.Detail := 'loading...';
end;

procedure TLccThrottleAppForm.TrainTabDetailsLoad(TractionObject: TLccTractionObject);
begin
  if SelectedRosterEqualsTractionObject(TractionObject) then
  begin
    ListBoxItemTrainsDetailsManufacturer.ItemData.Detail := TractionObject.SNIP.Manufacturer;
    ListBoxItemTrainsDetailsModel.ItemData.Detail := TractionObject.SNIP.Model;
    ListBoxItemTrainsDetailsSofwareVersion.ItemData.Detail := TractionObject.SNIP.SoftwareVersion;
    ListBoxItemTrainsDetailsHardwareVersion.ItemData.Detail := TractionObject.SNIP.HardwareVersion;
    ListBoxItemTrainsDetailsUserName.ItemData.Detail := TractionObject.SNIP.UserName;
    ListBoxItemTrainsDetailsUserDescription.ItemData.Detail := TractionObject.SNIP.UserDescription;
    ListBoxItemTrainsDetailsConsists.ItemData.Detail := 'Bob';
  end;
end;

function TLccThrottleAppForm.ValidEditBoxKey(Key: Word): Boolean;
begin
  // HardwareBack is to handle Android
  Result := (Key = vkReturn) or (Key = vkHardwareBack) or (Key = vkBack) or (Key = vkDelete) or (Key = vkLeft) or (Key = vkRight)
end;

procedure TLccThrottleAppForm.XmlLoadSettingsFromFile;
var
  SettingsXML: TLccXmlDocument;
  RootNode, ChildNode: TLccXmlNode;
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
  SettingsXML: TLccXmlDocument;
  RootNode, ChildNode: TLccXmlNode;
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


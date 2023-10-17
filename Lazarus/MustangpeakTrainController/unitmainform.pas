unit unitMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  LCLINTF,
  Types,
  LCLType,
  ActnList,
  Buttons,
  CheckLst,
  strutils,
  lcc_ethernet_client,
  lcc_node_manager,
  lcc_train_server,
  lcc_ethernet_common,
  lcc_node_messages,
  lcc_node_train,
  lcc_node_traindatabase,
  lcc_node_controller,
  lcc_connection_common,
  lcc_node,
  lcc_defines,
  lcc_base_classes,
  lcc_utilities,
  lcc_cdi_parser;


const
  IS_GRIDCONNECT = True;

const
  OBJID_VSCROLL =   $FFFFFFFB;
  OBJID_HSCROLL =   $FFFFFFFA;


const
  MAX_LED_SEGMENTS = 100;
  LED_TAPER = 0.010;
  MAX_DCC_ADDRESS = 10239;

  ICON_MORE_DOTS_IMAGE_INDEX = 4;

type

  TLEDShapeArray = array[0..MAX_LED_SEGMENTS-1] of TShape;


  { TFormTrainController }

  TFormTrainController = class(TForm)
    ActionRosterForward: TAction;
    ActionRosterBack: TAction;
    ActionLogDetailed: TAction;
    ActionLogClear: TAction;
    ActionLogEnable: TAction;
    ActionListMain: TActionList;
    ButtonLogClear: TButton;
    ButtonThrottleSelectGo: TButton;
    ButtonThrottleStop: TButton;
    ButtonSettingsRestartConnection: TButton;
    ComboBoxThrottleSpeedSteps: TComboBox;
    ComboBoxTrainSelect: TComboBox;
    EditSettingsIP: TEdit;
    EditSettingsPort: TEdit;
    EditSettingsNodeID: TEdit;
    ImageThrottleHamburger: TImage;
    ImageListMain: TImageList;
    ImageScrollLeft: TImage;
    ImageScrollRight: TImage;
    Label1: TLabel;
    Label2: TLabel;
    LabelSettingsAliasID: TLabel;
    LabelSettings1: TLabel;
    LabelSettings2: TLabel;
    LabelSettings3: TLabel;
    LabelSettingsConnectionState: TLabel;
    ListBoxRosterDetails: TListBox;
    ListBoxRoster: TListBox;
    MemoLog: TMemo;
    PageControlRoster: TPageControl;
    PageControlMain: TPageControl;
    PanelRosterEditorConfigurationBkGnd: TPanel;
    PanelThrottleHamburger: TPanel;
    Panel2: TPanel;
    PanelRosterHeader: TPanel;
    PanelThrottle: TPanel;
    PanelRosterSlider: TPanel;
    PanelMainRosterBackground: TPanel;
    PanelLogHeader: TPanel;
    PanelThrottleContainer: TPanel;
    PanelThrottleSlider: TPanel;
    PanelThrottleFooter: TPanel;
    PanelThrottleHeader: TPanel;
    PanelThrottleLever: TPanel;
    PanelSettings7: TPanel;
    PanelSettings2: TPanel;
    PanelSettings3: TPanel;
    PanelSettings4: TPanel;
    PanelSettings6: TPanel;
    PanelSettings1: TPanel;
    ScrollBoxFunctions: TScrollBox;
    TabSheetRosterList: TTabSheet;
    TabSheetRosterDetails: TTabSheet;
    TabSheetRosterEditor: TTabSheet;
    TabSheetLog: TTabSheet;
    TabSheetThrottle: TTabSheet;
    TabSheetRoster: TTabSheet;
    TabSheetSettings: TTabSheet;
    TimerMain: TTimer;
    ToggleBoxLogDetailed: TToggleBox;
    ToggleBoxLogEnable: TToggleBox;
    ToggleBoxThrottleSelectShort: TToggleBox;
    ToggleBoxThrottleForward: TToggleBox;
    ToggleBoxThrottleReverse: TToggleBox;
    ToggleBoxF0: TToggleBox;
    ToggleBoxF1: TToggleBox;
    ToggleBoxF10: TToggleBox;
    ToggleBoxF11: TToggleBox;
    ToggleBoxF12: TToggleBox;
    ToggleBoxF13: TToggleBox;
    ToggleBoxF14: TToggleBox;
    ToggleBoxF15: TToggleBox;
    ToggleBoxF16: TToggleBox;
    ToggleBoxF17: TToggleBox;
    ToggleBoxF18: TToggleBox;
    ToggleBoxF19: TToggleBox;
    ToggleBoxF2: TToggleBox;
    ToggleBoxF3: TToggleBox;
    ToggleBoxF4: TToggleBox;
    ToggleBoxF5: TToggleBox;
    ToggleBoxF6: TToggleBox;
    ToggleBoxF7: TToggleBox;
    ToggleBoxF8: TToggleBox;
    ToggleBoxF9: TToggleBox;
    TrackBarThrottle: TTrackBar;
    procedure ActionLogClearExecute(Sender: TObject);
    procedure ActionRosterRefreshExecute(Sender: TObject);
    procedure ButtonSettingsRestartConnectionClick(Sender: TObject);
    procedure ButtonThrottleSelectGoClick(Sender: TObject);
    procedure ButtonThrottleStopClick(Sender: TObject);
    procedure ComboBoxTrainSelectKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageScrollLeftClick(Sender: TObject);
    procedure ImageScrollRightClick(Sender: TObject);
    procedure ListBoxRosterDetailsDrawItem(Control: TWinControl;Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure ListBoxRosterDetailsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListBoxRosterDrawItem(Control: TWinControl; Index: Integer;ARect: TRect; State: TOwnerDrawState);
    procedure ListBoxRosterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnFunctionClick(Sender: TObject);
    procedure PageControlRosterChange(Sender: TObject);
    procedure PageControlRosterChanging(Sender: TObject;var AllowChange: Boolean);
    procedure PanelThrottleLeverResize(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TimerMainTimer(Sender: TObject);
    procedure ToggleBoxFunctionChange(Sender: TObject);
    procedure ToggleBoxThrottleForwardChange(Sender: TObject);
    procedure ToggleBoxThrottleReverseChange(Sender: TObject);
    procedure TrackBarThrottleChange(Sender: TObject);
  private
    FBitmapDetails: TBitmap;
    FCDIParser: TLccCdiParser;
    FConsistPanelShown: Boolean;
    FController: TLccTrainController;
    FDetailsTractionObject: TLccTractionObject;
    FEthernetClient: TLccEthernetClientThreadManager;
    FNodeManager: TLccNodeManager;
    FShownOnce: Boolean;
    FLEDArray: TLEDShapeArray;

  protected
    property CDIParser: TLccCdiParser read FCDIParser write FCDIParser;
    property ConsistPanelShown: Boolean read FConsistPanelShown;
    property DetailsTractionObject: TLccTractionObject read FDetailsTractionObject write FDetailsTractionObject;

    property ShownOnce: Boolean read FShownOnce write FShownOnce;

    procedure OnConnectionFactoryConnectionState(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; Info: TLccConnectionInfo);
    procedure OnConnectionErrorMessage(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; Info: TLccConnectionInfo);

    procedure OnMessageReceive(Sender: TObject; LccMessage: TLccMessage);
    procedure OnMessageSend(Sender: TObject; LccMessage: TLccMessage);

    procedure OnSNIPChange(TractionObject: TLccTractionObject);
    procedure OnTrainSNIPChange(TractionObject: TLccTractionObject);
    procedure OnEmergencyStopChange(TractionObject: TLccTractionObject);
    procedure OnFunctionChange(TractionObject: TLccTractionObject);
    procedure OnSpeedChange(TractionObject: TLccTractionObject);
    procedure OnRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);

    procedure OnNodeIDChanged(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeAliasChanged(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeLogin(Sender: TObject; ALccNode: TLccNode);

    procedure OnControllerAssignChange(Sender: TObject; ATractionServer: TLccTractionServer; ATractionObject: TLccTractionObject; IsAssigned: Boolean);

    procedure AllocateTrainCallback(EngineAllocateTrain: TLccEngineSearchAndAllocateTrain; AEngineSearchTrain: TLccEngineSearchTrain);

    procedure OnCDIReadCallback(MemorySpaceReadEnging: TLccEngineMemorySpaceAccess);

    procedure EnableControls(DoEnable: Boolean);

    procedure OnLEDClick(Sender: TObject);
    procedure UpdateRoster;
    function ValidateExtendedDccAddress(AddressStr: String; var DccAddress: Integer; var IsLong: Boolean): Boolean;
    procedure UpdateRosterHeaderScrolledLeft;
    procedure UpdateRosterHeaderScrolledRight;
    procedure LoadCDIUserInterface;

  public
    property Controller: TLccTrainController read FController write FController;
    property EthernetClient: TLccEthernetClientThreadManager read FEthernetClient write FEthernetClient;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property BitmapDetails: TBitmap read FBitmapDetails write FBitmapDetails;
  end;

var
  FormTrainController: TFormTrainController;

implementation

{$R *.lfm}

{ TFormTrainController }


procedure TFormTrainController.FormCreate(Sender: TObject);
begin
  NodeManager := TLccNodeManager.Create(nil);
  BitmapDetails := TBitmap.Create;
  CDIParser := TLccCdiParser.Create(nil);
  ImageListMain.GetBitmap(ICON_MORE_DOTS_IMAGE_INDEX, BitmapDetails);

  ConnectionFactory.OnStateChange := @OnConnectionFactoryConnectionState;
  ConnectionFactory.OnError := @OnConnectionErrorMessage;

  NodeManager.OnNodeAliasIDChanged := @OnNodeAliasChanged;
  NodeManager.OnNodeIDChanged := @OnNodeIdChanged;
  NodeManager.OnNodeLogin := @OnNodeLogin;

  PanelRosterSlider.Width := 0;
end;

procedure TFormTrainController.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  TimerMain.Enabled := False; // Stop trying to log in
  ConnectionFactory.DestroyConnection(EthernetClient);
  EthernetClient := nil;
  NodeManager.ReleaseAliasAll;
end;

procedure TFormTrainController.ButtonSettingsRestartConnectionClick(Sender: TObject);
var
  ConnectionInfo: TLccEthernetConnectionInfo;
begin
  ConnectionFactory.DestroyConnection(EthernetClient);
  ConnectionInfo := TLccEthernetConnectionInfo.Create;
  ConnectionInfo.ListenerIP := EditSettingsIP.Text;
  ConnectionInfo.ListenerPort := StrToInt(EditSettingsPort.Text);
  ConnectionInfo.SuppressErrorMessages := False;
  EthernetClient := ConnectionFactory.CreateConnection(TLccEthernetClientThreadManager, ConnectionInfo) as TLccEthernetClientThreadManager;

  EthernetClient.OpenConnection;
end;

procedure TFormTrainController.ActionRosterRefreshExecute(Sender: TObject);
begin
  if Assigned(Controller) then
    Controller.FindAllTrains;
end;

procedure TFormTrainController.ActionLogClearExecute(Sender: TObject);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Clear;
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TFormTrainController.ButtonThrottleSelectGoClick(Sender: TObject);
var
  AddressStr: String;
  AddressInt: LongInt;
  IsLong: Boolean;
  AddressWord: Word;
begin
  AddressStr := ComboBoxTrainSelect.Text;
  AddressInt := -1;
  IsLong := False;
  if ValidateExtendedDccAddress(AddressStr, AddressInt, IsLong) then
  begin
    AddressWord := AddressInt;
    if Assigned(Controller) then
    begin
      Controller.EngineSearchAndAllocateTrain.Reset;
      Controller.EngineSearchAndAllocateTrain.Assign(AddressWord, IsLong, TLccDccSpeedStep( ComboBoxThrottleSpeedSteps.ItemIndex), @AllocateTrainCallback);
      Controller.EngineSearchAndAllocateTrain.Start;

   //   Controller.AssignTrainByDccAddress(AddressWord, IsLong, TLccDccSpeedStep( ComboBoxThrottleSpeedSteps.ItemIndex));
    end;
  end;
end;

procedure TFormTrainController.ButtonThrottleStopClick(Sender: TObject);
begin
  TrackBarThrottle.Position := 0;
end;

procedure TFormTrainController.ComboBoxTrainSelectKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    ButtonThrottleSelectGoClick(Self);

  if (Key < VK_0) or (Key > VK_9) then
    if not ( (Key = VK_S) or
             (Key = VK_L) or
             (Key = VK_BACK) or
             (Key = VK_LEFT) or
             (Key = VK_RIGHT) or
             ((Key = VK_A) and (ssMeta in Shift)) or
             ((Key = VK_A) and (ssCtrl in Shift))
             ) then
      Key := 0;
end;

procedure TFormTrainController.FormDestroy(Sender: TObject);
begin
  NodeManager.ReleaseAliasAll;
  NodeManager.Free;    // Must be last as previous 2 use it
  BitmapDetails.Free;
  CDIParser.Free;
end;

procedure TFormTrainController.FormShow(Sender: TObject);
var
  i: Integer;
  LED: TShape;
begin
  if not ShownOnce then
  begin
    ShownOnce := True;
 // WriteLn('Restarting on Show');
    for i := 0 to MAX_LED_SEGMENTS - 1 do
    begin
      LED := TShape.Create(PanelThrottleLever);
      LED.Parent := PanelThrottleLever;
      LED.Shape := stRoundRect;
      LED.Brush.Color := $004BFF00;
      LED.Tag := i;
      LED.OnClick := @OnLEDClick;
      FLEDArray[i] := LED;
      EnableControls(False);
      ToggleBoxThrottleForward.Checked := True;
    end;
    PanelThrottleLeverResize(Self);
    PageControlRoster.ShowTabs := False;
    ButtonSettingsRestartConnectionClick(Self);
    UpdateRosterHeaderScrolledLeft;
    PageControlRoster.PageIndex := 0;
  end;
end;

procedure TFormTrainController.ImageScrollLeftClick(Sender: TObject);
begin
  UpdateRosterHeaderScrolledLeft;
end;

procedure TFormTrainController.ImageScrollRightClick(Sender: TObject);
begin
  UpdateRosterHeaderScrolledRight;
end;

procedure TFormTrainController.ListBoxRosterDetailsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aColor: TColor;                       //Background color
  TextRect, ImageRect: TRect;
  DetailsText: String;
begin
  if (Index mod 2 = 0)                  //Index tells which item it is
    then aColor:=$FFFFFF                //every second item gets white as the background color
    else aColor:=$EEEEFF;               //every second item gets pink background color
  if odSelected in State then aColor:=$0000FF;  //If item is selected, then red as background color
  ListBoxRosterDetails.Canvas.Brush.Color:=aColor;  //Set background color
  ListBoxRosterDetails.Canvas.FillRect(ARect);      //Draw a filled rectangle


  TextRect := ARect;
  ListBoxRosterDetails.Canvas.Font.Bold := True;      //Set the font to "bold"
  ListBoxRosterDetails.Canvas.TextRect(TextRect, 2, TextRect.Top+2, ListBoxRosterDetails.Items[Index]);  //Draw Itemtext

  DetailsText := 'Unknown...';
  if Assigned(DetailsTractionObject) then
  begin
    if DetailsTractionObject.SNIP.Valid then
    begin
      case Index of
        0 : DetailsText := DetailsTractionObject.SNIP.Manufacturer;
        1 : DetailsText := DetailsTractionObject.SNIP.Model;
        2 : DetailsText := DetailsTractionObject.SNIP.HardwareVersion;
        3 : DetailsText := DetailsTractionObject.SNIP.SoftwareVersion;
        4 : DetailsText := DetailsTractionObject.SNIP.UserName;
        5 : DetailsText := DetailsTractionObject.SNIP.UserDescription;
      end;
    end
  end;
  ListBoxRosterDetails.Canvas.Font.Bold := False;
  ListBoxRosterDetails.Canvas.TextOut(TextRect.Left + 20, TextRect.Top + (TextRect.Height div 2), DetailsText);

  if (Index > 3) then
  begin
    ImageRect := ARect;
    ImageRect.Left := ImageRect.Right - ImageRect.Height;
    InflateRect(ImageRect, -2, -2);
    OffsetRect(ImageRect, -4, 0);
    ListBoxRosterDetails.Canvas.StretchDraw(ImageRect, BitmapDetails);
  end;
end;

procedure TFormTrainController.ListBoxRosterDetailsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
  HitItemIndex: Integer;
  DetailsRect: TRect;
  YScroll: TScrollInfo;
begin
  if PageControlRoster.PageIndex = 1 then
  begin
    Point.X := X;
    Point.Y := Y;
    HitItemIndex := ListBoxRosterDetails.ItemAtPos(Point, True);
    if HitItemIndex > -1 then
    begin
      DetailsRect := ListBoxRosterDetails.ItemRect(HitItemIndex);
      DetailsRect.Left := DetailsRect.Right - DetailsRect.Height;

      // No idea how or why this works....
    //    LclIntF.GetScrollInfo(ListBoxRoster.Handle, Integer(OBJID_VSCROLL), YScroll);
    //    LclIntF.GetScrollInfo(ListBoxRoster.Handle, 0, YScroll);
      LclIntF.GetScrollInfo(ListBoxRosterDetails.Handle, 1, YScroll);

      DetailsRect := ListBoxRosterDetails.ItemRect(HitItemIndex);
      DetailsRect.Left := DetailsRect.Right - DetailsRect.Height;
      OffsetRect(DetailsRect, 0, -YScroll.nPos);

      if PtInRect(DetailsRect, Point) then
      begin
        UpdateRosterHeaderScrolledRight;
        if Assigned(DetailsTractionObject) then
        begin
          if not DetailsTractionObject.NodeCDI.Valid then
          begin
            PanelRosterEditorConfigurationBkGnd.Caption := 'Reading Configuration Data......';
            Controller.EngineMemorySpaceAccess.Reset;
            Controller.EngineMemorySpaceAccess.Assign(lems_Read, MSI_CDI, True, 0, 0, False, DetailsTractionObject.NodeID, DetailsTractionObject.NodeAlias, @OnCDIReadCallback);
            Controller.EngineMemorySpaceAccess.Start;
          end else
            LoadCDIUserInterface;
        end;
      end;
    end;
  end;

end;

procedure TFormTrainController.ListBoxRosterDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  aColor: TColor;                       //Background color
  DetailsRect: TRect;
begin
  if (Index mod 2 = 0)                  //Index tells which item it is
    then aColor:=$FFFFFF                //every second item gets white as the background color
    else aColor:=$EEEEFF;               //every second item gets pink background color
  if odSelected in State then aColor:=$0000FF;  //If item is selected, then red as background color
  ListBoxRoster.Canvas.Brush.Color:=aColor;  //Set background color
  ListBoxRoster.Canvas.FillRect(ARect);      //Draw a filled rectangle

  ListBoxRoster.Canvas.Font.Bold:=True;      //Set the font to "bold"
  ListBoxRoster.Canvas.TextRect(ARect, 2, ARect.Top+2, ListBoxRoster.Items[Index]);  //Draw Itemtext

  DetailsRect := ARect;
  DetailsRect.Left := DetailsRect.Right - DetailsRect.Height;
  InflateRect(DetailsRect, -2, -2);
  OffsetRect(DetailsRect, -4, 0);
  ListBoxRoster.Canvas.StretchDraw(DetailsRect, BitmapDetails);

end;

type
  THackListBox = class(TListBox);

procedure TFormTrainController.ListBoxRosterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
  HitItemIndex: Integer;
  DetailsRect: TRect;
  YScroll: TScrollInfo;
begin
  if PageControlRoster.PageIndex = 0 then
  begin
    Point.X := X;
    Point.Y := Y;
    HitItemIndex := ListBoxRoster.ItemAtPos(Point, True);
    if HitItemIndex > -1 then
    begin
      FillChar(YScroll, SizeOf(YScroll), #0);

    // No idea how or why this works....
  //    LclIntF.GetScrollInfo(ListBoxRoster.Handle, Integer(OBJID_VSCROLL), YScroll);
  //    LclIntF.GetScrollInfo(ListBoxRoster.Handle, 0, YScroll);
      LclIntF.GetScrollInfo(ListBoxRoster.Handle, 1, YScroll);

      DetailsRect := ListBoxRoster.ItemRect(HitItemIndex);
      DetailsRect.Left := DetailsRect.Right - DetailsRect.Height;
      OffsetRect(DetailsRect, 0, -YScroll.nPos);

      if PtInRect(DetailsRect, Point) then
      begin
        DetailsTractionObject := ListBoxRoster.Items.Objects[HitItemIndex] as TLccTractionObject;
        UpdateRosterHeaderScrolledRight;
      end;
    end;
  end;
end;

procedure TFormTrainController.OnFunctionClick(Sender: TObject);
begin
  if Assigned(Controller) then
  begin
    if Controller.IsTrainAssigned then
    begin
   //   Controller.assigned
    end;
  end;
end;

procedure TFormTrainController.PageControlRosterChange(Sender: TObject);
begin
  // Broken does not fire
end;

procedure TFormTrainController.PageControlRosterChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Useless as it does not say what the next one is...
end;

procedure TFormTrainController.PanelThrottleLeverResize(Sender: TObject);
var
  i, LEDHeight, LastBottom: Integer;
  LED: TShape;
begin
  if ShownOnce then
  begin
    LastBottom := PanelThrottleLever.Height;
    LEDHeight := PanelThrottleLever.Height div MAX_LED_SEGMENTS;
    LEDHeight := LEDHeight;
    for i := 0 to MAX_LED_SEGMENTS - 1 do
    begin
      LED := FLEDArray[i];
      LED.Top := LastBottom - LEDHeight;
      LED.Height := LEDHeight;
      LED.Left := 0 {Trunc(MAX_LED_SEGMENTS * LED_TAPER * i)};
      LED.Width := PanelThrottleLever.Width {- (Trunc(MAX_LED_SEGMENTS * LED_TAPER * i))};
      LastBottom := LED.Top;
    end;
  end;
end;

procedure TFormTrainController.SpeedButton1Click(Sender: TObject);
var
  i: Integer;
begin
  if ConsistPanelShown then
  begin
    PanelRosterSlider.Width  := 0;
  {  for i := PanelRosterSlider.Width downto 0 do
    begin
      PanelRosterSlider.Width := i;
   //   Sleep(0);
      Invalidate;
      Update;
    end;   }
    FConsistPanelShown := False;
  end else
  begin
    PanelRosterSlider.Width  := 200;
  {  for i := 0 to 150 do
    begin
      PanelRosterSlider.Width := i;
  //    Sleep(0);
      Invalidate;
      Update;
    end;      }
    FConsistPanelShown := True;
  end;
end;

procedure TFormTrainController.TimerMainTimer(Sender: TObject);
begin
  TimerMain.Interval := 5000;
  if not (EthernetClient.Connected or EthernetClient.Connecting) then
    ButtonSettingsRestartConnectionClick(Self)
end;

procedure TFormTrainController.ToggleBoxFunctionChange(Sender: TObject);
begin
  if Assigned(Controller) then
    Controller.AssignedTrain.SetFunction( (Sender as TToggleBox).Tag, Word( (Sender as TToggleBox).Checked));
end;

procedure TFormTrainController.ToggleBoxThrottleForwardChange(Sender: TObject);
begin
  ToggleBoxThrottleReverse.Checked := not ToggleBoxThrottleForward.Checked;
end;

procedure TFormTrainController.ToggleBoxThrottleReverseChange(Sender: TObject);
begin
  ToggleBoxThrottleForward.Checked := not ToggleBoxThrottleReverse.Checked;
end;

procedure TFormTrainController.TrackBarThrottleChange(Sender: TObject);
begin
  if Assigned(Controller) then
  begin
    if ToggleBoxThrottleForward.Checked then
      Controller.AssignedTrain.SetSpeed(TrackBarThrottle.Position)
    else
      Controller.AssignedTrain.SetSpeed(-TrackBarThrottle.Position)
  end
end;


procedure TFormTrainController.OnConnectionFactoryConnectionState(
  Sender: TObject; Manager: TLccConnectionThreadManager;
  Thread: TLccConnectionThread; Info: TLccConnectionInfo);
begin

  if Manager = EthernetClient then
  begin

    case TLccEthernetConnectionInfo(Info).ConnectionState of
      lcsDisconnected :
        begin
          LabelSettingsConnectionState.Caption := 'Waiting for Connection';
        end;
      lcsConnecting :
        begin
          LabelSettingsConnectionState.Caption := 'Connecting';
        end;
      lcsConnected :
        begin
          LabelSettingsConnectionState.Caption := 'Connected';
          if NodeManager.Nodes.Count = 0 then
          begin
            Controller := NodeManager.AddNodeByClass('', TLccTrainController, True, NULL_NODE_ID) as TLccTrainController;
            Controller.TractionServer.OnSNIPChange := @OnSNIPChange;
            Controller.TractionServer.OnTrainSNIPChange := @OnTrainSNIPChange;
            Controller.TractionServer.OnRegisterChange := @OnRegisterChange;
            Controller.TractionServer.OnEmergencyStopChange := @OnEmergencyStopChange;
            Controller.TractionServer.OnFunctionChange := @OnFunctionChange;
            Controller.TractionServer.OnSpeedChange := @OnSpeedChange;
            Controller.OnControllerAssignChange := @OnControllerAssignChange;
            Controller.TractionServer.Enabled := True;
          end;
        end;
      lcsDisconnecting :
        begin
          LabelSettingsConnectionState.Caption := 'Disconnecting';
        end;
    end;
  end;
end;

procedure TFormTrainController.OnConnectionErrorMessage(Sender: TObject;
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  Info: TLccConnectionInfo);
begin
  LabelSettingsConnectionState.Caption := Info.ErrorMessage;
  TimerMain.Interval := 500;  // Fire immediatly once this Syncronize call returns
end;

procedure TFormTrainController.OnMessageReceive(Sender: TObject; LccMessage: TLccMessage);
var
  ByteArray: TLccDynamicByteArray;
begin
  if ToggleBoxLogEnable.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      if IS_GRIDCONNECT then
      begin
        if ToggleBoxLogDetailed.Checked then
          MemoLog.Lines.Add('R: ' + MessageToDetailedMessage(LccMessage))
        else
          MemoLog.Lines.Add('R: ' + LccMessage.ConvertToGridConnectStr('', False));
      end else
      begin
        LccMessage.ConvertToLccTcp(ByteArray);
        MemoLog.Lines.Add('R: ' + LccMessage.ConvertToLccTcpString(ByteArray));
      end;

      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainController.OnMessageSend(Sender: TObject; LccMessage: TLccMessage);
var
  ByteArray: TLccDynamicByteArray;
begin
  if ToggleBoxLogEnable.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      if IS_GRIDCONNECT then
      begin
        if ToggleBoxLogDetailed.Checked then
          MemoLog.Lines.Add('S: ' + MessageToDetailedMessage(LccMessage))
        else
          MemoLog.Lines.Add('S: ' + LccMessage.ConvertToGridConnectStr('', False));
      end else
      begin
        LccMessage.ConvertToLccTcp(ByteArray);
        MemoLog.Lines.Add('S: ' + LccMessage.ConvertToLccTcpString(ByteArray));
      end;
      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainController.OnSNIPChange(TractionObject: TLccTractionObject);
var
  ItemIndex: Integer;
begin
  if DetailsTractionObject = TractionObject then
  begin
    // Redraw it
    ListBoxRosterDetails.Invalidate;
    ListBoxRosterDetails.Update;
  end;

  ItemIndex := ListBoxRoster.Items.IndexOfObject(TractionObject);
  if ItemIndex > -1 then
    ListBoxRoster.Items[ItemIndex] := TractionObject.DisplayName;
end;

procedure TFormTrainController.OnTrainSNIPChange(TractionObject: TLccTractionObject);
begin

end;

procedure TFormTrainController.OnEmergencyStopChange(TractionObject: TLccTractionObject);
begin

end;

procedure TFormTrainController.OnFunctionChange(TractionObject: TLccTractionObject);
begin

end;

procedure TFormTrainController.OnSpeedChange(TractionObject: TLccTractionObject);
begin

end;

procedure TFormTrainController.OnRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);
var
  ItemIndex: Integer;
begin
  ListBoxRoster.Items.BeginUpdate;
  try
    if IsRegistered then
    begin
      ItemIndex := ListBoxRoster.Items.IndexOfObject(TractionObject);
      if ItemIndex < 0 then
         ItemIndex := ListBoxRoster.Items.AddObject(TractionObject.DisplayName, TractionObject);
      Controller.SendSNIPRequest(TractionObject.NodeID, TractionObject.NodeAlias);
      Controller.SendTrainSNIPRequest(TractionObject.NodeID, TractionObject.NodeAlias);
    end else
    begin
      ItemIndex := ListBoxRoster.Items.IndexOfObject(TractionObject);
      if ItemIndex > -1 then
      begin
        if ListBoxRoster.Selected[ItemIndex] then
        begin
          ListBoxRoster.Selected[ItemIndex] := False;
          PageControlRoster.PageIndex := 0;   // Item is going away jump back
          if TractionObject =  DetailsTractionObject then
            DetailsTractionObject := nil; // don't reference the traction object anymore
        end;
        ListBoxRoster.Items.Delete(ItemIndex);
      end;
    end
  finally
    ListBoxRoster.Items.EndUpdate;
  end;
end;

procedure TFormTrainController.OnNodeIDChanged(Sender: TObject; ALccNode: TLccNode);
begin
  EditSettingsNodeID.Text := ALccNode.NodeIDStr[True];
end;

procedure TFormTrainController.OnNodeAliasChanged(Sender: TObject; ALccNode: TLccNode);
begin
 LabelSettingsAliasID.Caption := ALccNode.AliasIDStr;
end;

procedure TFormTrainController.OnNodeLogin(Sender: TObject; ALccNode: TLccNode);
begin
  if ALccNode = Controller then
    Controller.FindAllTrains;
end;

procedure TFormTrainController.OnControllerAssignChange(Sender: TObject; ATractionServer: TLccTractionServer; ATractionObject: TLccTractionObject; IsAssigned: Boolean);
begin
  EnableControls(IsAssigned);
  if Assigned(Controller) then
  begin
    if IsAssigned then
    begin
      Controller.ListenerDetachFromAssignedTrain;
      Controller.ListenerAttach(ATractionObject);
    end else
      Controller.ListenerDetachFromAssignedTrain;
  end;
end;

procedure TFormTrainController.AllocateTrainCallback(
  EngineAllocateTrain: TLccEngineSearchAndAllocateTrain;
  AEngineSearchTrain: TLccEngineSearchTrain);
begin
  Controller.TractionServer.Find(AEngineSearchTrain.SearchTrain.NodeIdentification.NodeID);
end;

procedure TFormTrainController.OnCDIReadCallback(MemorySpaceReadEnging: TLccEngineMemorySpaceAccess);
begin
  LoadCDIUserInterface;

end;

procedure TFormTrainController.EnableControls(DoEnable: Boolean);
begin
  ScrollBoxFunctions.Enabled := DoEnable;
  PanelThrottleFooter.Enabled := DoEnable;
  PanelThrottleContainer.Enabled := DoEnable;
end;

procedure TFormTrainController.OnLEDClick(Sender: TObject);
begin
  ShowMessage('Clicked: ' + IntToStr((Sender as TShape).Tag));
end;

procedure TFormTrainController.UpdateRoster;
var
  i: Integer;
begin
  if Assigned(Controller) then
  begin
    ComboBoxTrainSelect.Items.BeginUpdate;
    try
      ComboBoxTrainSelect.Items.Clear;
      for i := 0 to Controller.TractionServer.List.Count - 1 do
      begin
        ComboBoxTrainSelect.Items.Add(Controller.TractionServer.Item[i].SNIP.UserName);
      end;

    finally
      ComboBoxTrainSelect.Items.EndUpdate;
    end;
  end;
end;

function TFormTrainController.ValidateExtendedDccAddress(AddressStr: String; var DccAddress: Integer; var IsLong: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;
  IsLong := not ToggleBoxThrottleSelectShort.Checked;
  DccAddress := -1;
  AddressStr := ComboBoxTrainSelect.Text;
  if AddressStr = '' then
    Result := False
  else begin
    if Length(AddressStr) = 1 then
    begin
      if not TryStrToInt(AddressStr, DccAddress) then
        Result := False;
    end else
    begin
      for i := 1 to Length(AddressStr) do
      begin
        if i < Length(AddressStr) then
        begin
          if (AddressStr[i] < '0') or (AddressStr[i] > '9') then
            Result := False;
        end else
        begin
          if (AddressStr[i] >= '0') and (AddressStr[i] <= '9') then
          begin // all numbers
            if not TryStrToInt(AddressStr, DccAddress) then   // This should always succeed
              Result := False;
          end else
          begin
            if (AddressStr[i] = 'L') or (AddressStr[i] = 'l') or (AddressStr[i] = 'S') or (AddressStr[i] = 's') then
            begin
              IsLong := (AddressStr[i] = 'L') or (AddressStr[i] = 'l');
              SetLength(AddressStr, Length(AddressStr) - 1);  // strip it off
              if not TryStrToInt(AddressStr, DccAddress) then   // This should always succeed
                Result := False;
            end else
               Result := False
          end
        end;
      end;
      Result := (DccAddress > 0) and (DccAddress <= MAX_DCC_ADDRESS);
    end
  end;
end;

procedure TFormTrainController.UpdateRosterHeaderScrolledLeft;
begin
  if PageControlRoster.PageIndex > 0 then
  begin
    PageControlRoster.PageIndex := PageControlRoster.PageIndex - 1 ;
    if PageControlRoster.PageIndex = 0 then
    begin
      DetailsTractionObject := nil;
      ListBoxRosterDetails.ClearSelection;
    end;
  end;
  ImageScrollLeft.Visible := PageControlRoster.PageIndex > 0;
  ImageScrollRight.Visible := True;
  PanelRosterHeader.Caption := PageControlRoster.Pages[PageControlRoster.PageIndex].Caption;
end;

procedure TFormTrainController.UpdateRosterHeaderScrolledRight;
begin
  if PageControlRoster.PageIndex < PageControlRoster.PageCount - 1 then
    PageControlRoster.PageIndex := PageControlRoster.PageIndex + 1;

  if PageControlRoster.PageIndex = 1 then
  begin
    if Assigned(DetailsTractionObject) then
    begin
      if not DetailsTractionObject.SNIP.Valid then
        Controller.SendSNIPRequest(DetailsTractionObject.NodeID, DetailsTractionObject.NodeAlias);
      if not DetailsTractionObject.TrainSNIP.Valid then
        Controller.SendTrainSNIPRequest(DetailsTractionObject.NodeID, DetailsTractionObject.NodeAlias);
    end;
  end;

  ImageScrollLeft.Visible :=  True;
  ImageScrollRight.Visible := PageControlRoster.PageIndex < (PageControlRoster.PageCount - 1);
  PanelRosterHeader.Caption := PageControlRoster.Pages[PageControlRoster.PageIndex].Caption;
end;

procedure TFormTrainController.LoadCDIUserInterface;
var
  ATargetNode: TLccNodeIdentificationObject;
begin
  if Assigned(DetailsTractionObject) then
  begin
    ATargetNode := TLccNodeIdentificationObject.Create;
    try
      ATargetNode.AssignID(DetailsTractionObject.NodeID, DetailsTractionObject.NodeAlias);
      PanelRosterEditorConfigurationBkGnd.Caption := 'Building User Interface......';
      CDIParser.Build_CDI_Interface(Controller, ATargetNode, PanelRosterEditorConfigurationBkGnd, DetailsTractionObject.NodeCDI.CDI);
      PanelRosterEditorConfigurationBkGnd.Caption := '';

    finally
      ATargetNode.Free;
    end;
  end;
end;


end.


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
  lcc_ethernet_client,
  lcc_node_manager,
  lcc_ethernet_common,
  lcc_node_messages,
  lcc_node_train,
  lcc_node_controller,
  lcc_connection_common,
  lcc_node,
  lcc_alias_server,
  lcc_defines,
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
    ButtonThrottleSelectRelease: TButton;
    ButtonLogClear: TButton;
    ButtonThrottleSelectGo: TButton;
    ButtonSettingsRestartConnection: TButton;
    ButtonThrottleEStop: TButton;
    ButtonThrottleStop: TButton;
    ComboBoxTrainSelect: TComboBox;
    EditSettingsIP: TEdit;
    EditSettingsPort: TEdit;
    EditSettingsNodeID: TEdit;
    ImageThrottleHamburger: TImage;
    ImageListMain: TImageList;
    ImageScrollLeft: TImage;
    ImageScrollRight: TImage;
    Label2: TLabel;
    LabelStatus: TLabel;
    LabelErrorMsg: TLabel;
    LabelSettingsAliasID: TLabel;
    LabelSettings1: TLabel;
    LabelSettings2: TLabel;
    LabelSettings3: TLabel;
    LabelSettingsConnectionState: TLabel;
    ListBoxRosterDetails: TListBox;
    ListBoxRoster: TListBox;
    MemoLog: TMemo;
    PageControl1: TPageControl;
    PageControlRoster: TPageControl;
    PageControlMain: TPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
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
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
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
    procedure Button1Click(Sender: TObject);
    procedure ButtonSettingsRestartConnectionClick(Sender: TObject);
    procedure ButtonThrottleSelectGoClick(Sender: TObject);
    procedure ButtonThrottleSelectReleaseClick(Sender: TObject);
    procedure ButtonThrottleEStopClick(Sender: TObject);
    procedure ButtonThrottleStopClick(Sender: TObject);
    procedure ComboBoxTrainSelectKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboBoxTrainSelectSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageScrollLeftClick(Sender: TObject);
    procedure ImageScrollRightClick(Sender: TObject);
    procedure ImageThrottleHamburgerClick(Sender: TObject);
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
    procedure ToggleBoxThrottleSelectLongChange(Sender: TObject);
    procedure TrackBarThrottleChange(Sender: TObject);
  private
    FBitmapDetails: TBitmap;
    FCDIParser: TLccCdiParser;
    FConsistPanelShown: Boolean;
    FController: TLccTrainController;
    FEmulateCanBus: Boolean;
    FEthernetClient: TLccEthernetClientThreadManager;
    FNodeManager: TLccNodeManager;
    FShownOnce: Boolean;
    FLEDArray: TLEDShapeArray;

  protected
    property CDIParser: TLccCdiParser read FCDIParser write FCDIParser;
    property ConsistPanelShown: Boolean read FConsistPanelShown;

    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    property EmulateCanBus: Boolean read FEmulateCanBus write FEmulateCanBus;

    procedure OnConnectionConnectionState(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; Info: TLccConnectionInfo);
    procedure OnConnectionErrorMessage(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; Info: TLccConnectionInfo);
    procedure OnConnectionManagerReceiveMessage(Sender: TObject; ALccMessage: TLccMessage);
    procedure OnConnectionManagerSendMessage(Sender: TObject; ALccMessage: TLccMessage);

    procedure OnNodeIDChanged(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeAliasChanged(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeLogin(Sender: TObject; ALccNode: TLccNode);

    procedure OnCDIReadCallback(MemorySpaceReadEnging: TLccTaskMemorySpaceAccess);

    procedure EnableControls(DoEnable: Boolean);

    procedure CallbackSearchByDccAddress(ATask: TLccTaskBase); // TLccTaskSearchTrain
    procedure CallbackAssignToTrain(ATask: TLccTaskBase);      // TLccTaskControllerAttach
    procedure CallbackQueryTrain(ATask: TLccTaskBase);         // TLccTaskControllerQuery

    procedure CallbackQuerySpeedDir(ATask: TLccTaskBase);      // TLccTaskQuerySpeed
    procedure CallbackQueryFunction(ATask: TLccTaskBase);     // TLccTaskQueryFunction

    procedure CallbackCdi(ATask: TLccTaskBase);                // TLccTaskMemorySpaceAccess

    procedure CallbackTrainRosterNotify(ATask: TLccTaskBase);      // TLccTaskTrainRoster

    procedure CallbackQueryAttachedListeners(ATask: TLccTaskBase);  // TLccTaskQueryAttachedListeners

    procedure OnLEDClick(Sender: TObject);
    procedure UpdateRoster;
    procedure UpdateRosterHeaderScrolledLeft;
    procedure UpdateRosterHeaderScrolledRight;
    procedure LoadCDIUserInterface;

  public
    property Controller: TLccTrainController read FController write FController;
    property EthernetClient: TLccEthernetClientThreadManager read FEthernetClient write FEthernetClient;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property BitmapDetails: TBitmap read FBitmapDetails write FBitmapDetails;

    procedure SelectTrainFromComboBox;
  end;

var
  FormTrainController: TFormTrainController;

implementation

{$R *.lfm}


{ TFormTrainController }


procedure TFormTrainController.FormCreate(Sender: TObject);
begin
  EmulateCanBus := True;

  NodeManager := TLccNodeManager.Create(nil);
  NodeManager.EmulateCanNetworkLogin := EmulateCanBus;
  BitmapDetails := TBitmap.Create;
  CDIParser := TLccCdiParser.Create(nil);
  ImageListMain.GetBitmap(ICON_MORE_DOTS_IMAGE_INDEX, BitmapDetails);

  ConnectionFactory.OnStateChange := @OnConnectionConnectionState;
  ConnectionFactory.OnError := @OnConnectionErrorMessage;
  ConnectionFactory.OnLccMessageReceive := @OnConnectionManagerReceiveMessage;
  ConnectionFactory.OnLccMessageSend := @OnConnectionManagerSendMessage;

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
  LocalPort: LongInt;
  LocalPortStr: String;
  LocalIPStr: String;
  LocalNodeIDStr: String;
begin
  if EditSettingsNodeID.Text = '' then
    EditSettingsNodeID.Text := '08.09.0A.0B.0C.0D';

  LocalPortStr := EditSettingsPort.Text;
  LocalIPStr := EditSettingsIP.Text;
  LocalNodeIDStr := EditSettingsNodeID.Text;

  LabelErrorMsg.Caption := '';
  LabelErrorMsg.Font.Color := clRed;
  LabelErrorMsg.Font.Style := [fsBold];

  if TryStrToInt(LocalPortStr, LocalPort) then
  begin
    if ValidateIPString(LocalIPStr) then
    begin
      if ValidateNodeIDString(LocalNodeIDStr) then
      begin
        ConnectionFactory.DestroyConnection(EthernetClient);
        ConnectionInfo := TLccEthernetConnectionInfo.Create;
        ConnectionInfo.ListenerIP := LocalIPStr;
        ConnectionInfo.ListenerPort := LocalPort;
        ConnectionInfo.SuppressErrorMessages := False;
        EthernetClient := ConnectionFactory.CreateLccMessageConnection(TLccEthernetClientThreadManager, ConnectionInfo, EmulateCanBus) as TLccEthernetClientThreadManager;
        EthernetClient.OpenConnection;
      end else
        LabelErrorMsg.Caption := 'Invalid NodeID: ' + LocalNodeIDStr;
    end else
      LabelErrorMsg.Caption := 'Invalid Ip address: ' + LocalIPStr;
  end else
    LabelErrorMsg.Caption := 'Invalid Port number: ' + LocalPortStr;
end;

procedure TFormTrainController.ActionRosterRefreshExecute(Sender: TObject);
begin
  if Assigned(Controller) then
    Controller.FindAllTrains;
end;

procedure TFormTrainController.Button1Click(Sender: TObject);
begin

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
begin
  SelectTrainFromComboBox;
end;

procedure TFormTrainController.ButtonThrottleSelectReleaseClick(Sender: TObject);
begin
  if Assigned(Controller) then
    if Assigned( Controller.TrainRoster.ActiveTrain) then
    begin
      Controller.ControllerRelease(Controller.TrainRoster.ActiveTrain.NodeID, Controller.NodeID);
      Controller.TrainRoster.ActivateTrain(NULL_NODE_ID);
      ComboBoxTrainSelect.Caption := '';
    end;

  EnableControls(False);
  Caption := 'Mustangpeak TrainController';
end;

procedure TFormTrainController.ButtonThrottleEStopClick(Sender: TObject);
begin
  if not Assigned(Controller) then Exit;

  if Assigned( Controller.TrainRoster.ActiveTrain) then
  begin
    Controller.EmergencyStop(Controller.TrainRoster.ActiveTrain.NodeID);
    Controller.SetSpeedDir(Controller.TrainRoster.ActiveTrain.NodeID, 0, not ToggleBoxThrottleReverse.Checked);
  end;
end;

procedure TFormTrainController.ButtonThrottleStopClick(Sender: TObject);
begin
  TrackBarThrottle.Position := 0;
end;

procedure TFormTrainController.ComboBoxTrainSelectKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    SelectTrainFromComboBox;

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

procedure TFormTrainController.ComboBoxTrainSelectSelect(Sender: TObject);
begin
  SelectTrainFromComboBox
end;

procedure TFormTrainController.FormDestroy(Sender: TObject);
begin
  NodeManager.ReleaseAliasAll;
  FreeAndNil(FNodeManager);    // Must be last as previous 2 use it
  FreeAndNil(FBitmapDetails);
  FreeAndNil(FCDIParser);
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
    Width := Width + 1;
    Width := Width - 1;
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

procedure TFormTrainController.ImageThrottleHamburgerClick(Sender: TObject);
var
  i: Integer;
begin
  if PanelRosterSlider.Width < 5 then
  begin
    for i := 1 to ClientWidth do
    begin
      PanelRosterSlider.Width := i;
      Application.ProcessMessages;

    end;
  end else
  begin
    for i := PanelRosterSlider.Width downto 0 do
    begin
      PanelRosterSlider.Width := i;
      Application.ProcessMessages;
    end;
  end;

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
 { if Assigned(DetailsTractionObject) then
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
  end;  }
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
      {  if Assigned(DetailsTractionObject) then
        begin
          if not DetailsTractionObject.NodeCDI.Valid then
          begin
            PanelRosterEditorConfigurationBkGnd.Caption := 'Reading Configuration Data......';
            Controller.TaskMemorySpaceAccess.Reset;
            Controller.TaskMemorySpaceAccess.Assign(lems_Read, MSI_CDI, True, 0, 0, False, DetailsTractionObject.NodeID, DetailsTractionObject.NodeAlias, @OnCDIReadCallback);
            Controller.TaskMemorySpaceAccess.Start;
          end else
            LoadCDIUserInterface;
        end;   }
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
//        DetailsTractionObject := ListBoxRoster.Items.Objects[HitItemIndex] as TLccTractionObject;
        UpdateRosterHeaderScrolledRight;
      end;
    end;
  end;
end;

procedure TFormTrainController.OnFunctionClick(Sender: TObject);
begin
  if not Assigned(Controller) then Exit;

  if Assigned(Controller.TrainRoster.ActiveTrain) then
    Controller.SetFunction(Controller.TrainRoster.ActiveTrain.NodeID, (Sender as TToggleBox).Tag, Word( (Sender as TToggleBox).Checked));
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
  if TimerMain.Interval <> 5000 then
    TimerMain.Interval := 5000;

  if Assigned(EthernetClient) then
  begin
    if not (EthernetClient.Connected or EthernetClient.Connecting) then
      ButtonSettingsRestartConnectionClick(Self)
  end
end;

procedure TFormTrainController.ToggleBoxFunctionChange(Sender: TObject);
begin
  if not Assigned(Controller) then Exit;

  if Assigned(Controller.TrainRoster.ActiveTrain) then
    Controller.SetFunction(Controller.TrainRoster.ActiveTrain.NodeID, (Sender as TToggleBox).Tag, Word( (Sender as TToggleBox).Checked));
end;

procedure TFormTrainController.ToggleBoxThrottleForwardChange(Sender: TObject);
begin
  ToggleBoxThrottleReverse.Checked := not ToggleBoxThrottleForward.Checked;

  if not Assigned(Controller) then Exit;

  if Assigned(Controller.TrainRoster.ActiveTrain) then
    Controller.SetSpeedDir(Controller.TrainRoster.ActiveTrain.NodeID, TrackBarThrottle.Position, ToggleBoxThrottleForward.Checked);
end;

procedure TFormTrainController.ToggleBoxThrottleReverseChange(Sender: TObject);
begin
  ToggleBoxThrottleForward.Checked := not ToggleBoxThrottleReverse.Checked;

  if not Assigned(Controller) then Exit;

  if Assigned(Controller.TrainRoster.ActiveTrain) then
    Controller.SetSpeedDir(Controller.TrainRoster.ActiveTrain.NodeID, TrackBarThrottle.Position, ToggleBoxThrottleForward.Checked);
end;

procedure TFormTrainController.ToggleBoxThrottleSelectLongChange(Sender: TObject);
begin
end;

procedure TFormTrainController.TrackBarThrottleChange(Sender: TObject);
begin
  if not Assigned(Controller) then Exit;

  if Assigned(Controller.TrainRoster.ActiveTrain) then
    Controller.SetSpeedDir(Controller.TrainRoster.ActiveTrain.NodeID, TrackBarThrottle.Position, ToggleBoxThrottleForward.Checked);
end;


procedure TFormTrainController.OnConnectionConnectionState(Sender: TObject;
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  Info: TLccConnectionInfo);
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
            Controller.TrainRoster.Callback := @CallbackTrainRosterNotify;
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

procedure TFormTrainController.OnConnectionManagerReceiveMessage(Sender: TObject; ALccMessage: TLccMessage);
 var
  ByteArray: TLccDynamicByteArray;
begin
  ByteArray := nil;
  if ToggleBoxLogEnable.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      if EmulateCanBus then
      begin
        if ToggleBoxLogDetailed.Checked then
          MemoLog.Lines.Add('R: ' + MessageToDetailedMessage(ALccMessage))
        else
          MemoLog.Lines.Add('R: ' + ALccMessage.ConvertToGridConnectStr('', False));
      end else
      begin
        ALccMessage.ConvertToLccTcp(ByteArray);
        MemoLog.Lines.Add('R: ' + ALccMessage.ConvertToLccTcpString(ByteArray));
      end;

      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainController.OnConnectionManagerSendMessage(Sender: TObject; ALccMessage: TLccMessage);
 var
   ByteArray: TLccDynamicByteArray;
 begin
   ByteArray := nil;
   if ToggleBoxLogEnable.Checked then
   begin
     MemoLog.Lines.BeginUpdate;
     try
       if EmulateCanBus then
       begin
         if ToggleBoxLogDetailed.Checked then
           MemoLog.Lines.Add('S: ' + MessageToDetailedMessage(ALccMessage))
         else
           MemoLog.Lines.Add('S: ' + ALccMessage.ConvertToGridConnectStr('', False));
       end else
       begin
         ALccMessage.ConvertToLccTcp(ByteArray);
         MemoLog.Lines.Add('S: ' + ALccMessage.ConvertToLccTcpString(ByteArray));
       end;
       MemoLog.SelStart := Length(MemoLog.Lines.Text);
     finally
       MemoLog.Lines.EndUpdate;
     end;
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


procedure TFormTrainController.OnCDIReadCallback(MemorySpaceReadEnging: TLccTaskMemorySpaceAccess);
begin
  LoadCDIUserInterface;
end;

procedure TFormTrainController.EnableControls(DoEnable: Boolean);
begin
  ScrollBoxFunctions.Enabled := DoEnable;
  PanelThrottleFooter.Enabled := DoEnable;
  PanelThrottleContainer.Enabled := DoEnable;
end;

procedure TFormTrainController.CallbackSearchByDccAddress(ATask: TLccTaskBase);
var
  LccTaskSearchTrain: TLccTaskSearchTrain;
begin
  LccTaskSearchTrain := ATask as TLccTaskSearchTrain;

  case ATask.TaskState of
    lesComplete :
      begin
        Controller.ControllerAssign(LccTaskSearchTrain.TrainNodeIDReply, @CallbackAssignToTrain);
        LabelStatus.Caption := 'Assigning Controller to: ' + ComboBoxTrainSelect.Caption;
      end;
    lesAbort   : LabelStatus.Caption := 'Search DCC Address Aborted';
    lesTimeout : LabelStatus.Caption := 'Search DCC Address Timeout';
    lesError   : LabelStatus.Caption := 'Error: Search DCC Address - Code=' + IntToStr(ATask.ErrorCode) + ' ' + ATask.ErrorMessage;
  end;
end;

procedure TFormTrainController.CallbackAssignToTrain(ATask: TLccTaskBase);
var
  TaskControllerAttach: TLccTaskControllerAttach;
  LocalTrainInfo: TTrainInfo;

  i: Integer;
begin
  TaskControllerAttach := ATask as TLccTaskControllerAttach;

  case ATask.TaskState of
    lesComplete :
      begin
        // If not in the list then we beat the call IsTrain Event if the train needed to be created
        // (HIGHLY UNLIKELY) but if so create a LocalTrainInfo here for the Roster and the SNIP/PIP will be handled
        // in the Roster loop to fill it in
        LocalTrainInfo := Controller.TrainRoster.ActivateTrain(TaskControllerAttach.Target);
        if not Assigned(LocalTrainInfo) then
          LocalTrainInfo := Controller.TrainRoster.Add( TTrainInfo.Create(TaskControllerAttach.Target, 'Loading..'));

        // Do UI Updates
        EnableControls(True);

        LabelStatus.Caption := 'Requesting Train Info...';

        Controller.QuerySpeedDir(LocalTrainInfo.NodeID, @CallbackQuerySpeedDir);
        for i := 0 to MAX_FUNCTIONS - 1 do
          Controller.QueryFunction(LocalTrainInfo.NodeID, i, @CallbackQueryFunction);
      end;
    lesAbort   : LabelStatus.Caption := 'Train Assign Aborted';
    lesTimeout : LabelStatus.Caption := 'Train Assign Timeout';
    lesError   : LabelStatus.Caption := 'Error: Train Assign - Code=' + IntToStr(ATask.ErrorCode) + ' ' + ATask.ErrorMessage;
  end;
end;

procedure TFormTrainController.CallbackQueryTrain(ATask: TLccTaskBase);
var
  LocalTask: TLccTaskControllerQuery;
begin
  LocalTask := ATask as TLccTaskControllerQuery;
end;


procedure TFormTrainController.CallbackQuerySpeedDir(ATask: TLccTaskBase);
var
  TaskQuerySpeed: TLccTaskQuerySpeed;
  OldOnChange: TNotifyEvent;
begin
  TaskQuerySpeed := ATask as TLccTaskQuerySpeed;

  case ATask.TaskState of
    lesComplete :
      begin
        OldOnChange := TrackBarThrottle.OnChange;
        TrackBarThrottle.OnChange := nil;
        try
          TrackBarThrottle.Position := Integer( Round( TaskQuerySpeed.SetSpeedReply));
          LabelStatus.Caption := 'Received Speed/Dir';
        finally
          TrackBarThrottle.OnChange := OldOnChange;
        end;
      end;
    lesAbort   : LabelStatus.Caption := 'Query Speed/Dir Aborted';
    lesTimeout : LabelStatus.Caption := 'Query Speed/Dir Aborted Timeout';
    lesError   : LabelStatus.Caption := 'Error: Query Speed/Dir Aborted - Code=' + IntToStr(ATask.ErrorCode) + ' ' + ATask.ErrorMessage;
  end;
end;

procedure TFormTrainController.CallbackQueryFunction(ATask: TLccTaskBase);

  function FindFunctionButton(Index: DWord): TToggleBox;
  var
    i: Integer;
    TBox: TToggleBox;
  begin
    Result := nil;
    if Index < ScrollBoxFunctions.ControlCount then
    begin
      for i := 0 to ScrollBoxFunctions.ControlCount - 1 do
      begin
        if (ScrollBoxFunctions.Controls[i] is TToggleBox) then
        begin
          TBox :=  ScrollBoxFunctions.Controls[i] as TToggleBox;
          if TBox.Tag = Index then
          begin
            Result := TBox;
            Break
          end;
        end
      end;
    end;
  end;

var
  TaskQueryFunction: TLccTaskQueryFunction;
  FunctionBox: TToggleBox;
  OldOnChange: TNotifyEvent;
begin
  TaskQueryFunction := ATask as TLccTaskQueryFunction;

    case ATask.TaskState of
    lesComplete :
      begin
        FunctionBox := FindFunctionButton(TaskQueryFunction.Address);
        if Assigned(FunctionBox) then
        begin
          OldOnChange := FunctionBox.OnChange;
          FunctionBox.OnChange := nil;
          try
            if TaskQueryFunction.ValueReply = 0 then
              FunctionBox.Checked := False
            else
              FunctionBox.Checked := True;
            LabelStatus.Caption := 'Received Function: ' + IntToStr(TaskQueryFunction.Address);
          finally
            FunctionBox.OnChange := OldOnChange;
          end;
        end;
        if TaskQueryFunction.Address = MAX_DCC_FUNCTIONS - 1 then
          LabelStatus.Caption := '';
      end;
    lesAbort   : LabelStatus.Caption := 'QueryAttachedListeners Aborted';
    lesTimeout : LabelStatus.Caption := 'QueryAttachedListeners Timeout';
    lesError   : LabelStatus.Caption := 'Error: QueryAttachedListeners - Code=' + IntToStr(ATask.ErrorCode) + ' ' + ATask.ErrorMessage;
  end;
end;

procedure TFormTrainController.CallbackCdi(ATask: TLccTaskBase);
var
  TaskMemorySpaceAccess: TLccTaskMemorySpaceAccess;
  LocalTrainInfo: TTrainInfo;
begin
  TaskMemorySpaceAccess := ATask as TLccTaskMemorySpaceAccess;

  case ATask.TaskState of
    lesComplete :
      begin
        LocalTrainInfo := Controller.TrainRoster.FindByNodeID(TaskMemorySpaceAccess.Target);
        TaskMemorySpaceAccess.CopyTo(LocalTrainInfo.CDI);
        LabelStatus.Caption := IntToStr(TaskMemorySpaceAccess.AddressCurrent) + ' of ' + IntToStr(TaskMemorySpaceAccess.AddressHi-TaskMemorySpaceAccess.AddressLo) + ' bytes';
      end;
    lesRunning :
      begin
        LabelStatus.Caption := IntToStr(TaskMemorySpaceAccess.AddressCurrent) + ' of ' + IntToStr(TaskMemorySpaceAccess.AddressHi-TaskMemorySpaceAccess.AddressLo) + ' bytes';
      end;
    lesAbort   : LabelStatus.Caption := 'TaskMemorySpaceAccess Read Aborted';
    lesTimeout : LabelStatus.Caption := 'TaskMemorySpaceAccess Read Timeout';
    lesError   : LabelStatus.Caption := 'Error: TaskMemorySpaceAccess Read - Code=' + IntToStr(ATask.ErrorCode) + ' ' + ATask.ErrorMessage;
  end;
end;

procedure TFormTrainController.CallbackTrainRosterNotify(ATask: TLccTaskBase);
var
  TaskTrainRoster: TLccTaskTrainRoster;
  i: Integer;
begin
  TaskTrainRoster := ATask as TLccTaskTrainRoster;


  case ATask.TaskState of
    lesComplete :
      begin
        // should never occur as it always is running
      end;
    lesRunning :
      begin
        ListBoxRoster.Items.BeginUpdate;
        ComboBoxTrainSelect.Items.BeginUpdate;
        try
          ListBoxRoster.Items.Clear;
          ComboBoxTrainSelect.Items.Clear;
          for i := 0 to TaskTrainRoster.Count - 1 do
          begin
            ListBoxRoster.Items.Add(TaskTrainRoster.Train[i].UserName);
            ComboBoxTrainSelect.Items.Add(TaskTrainRoster.Train[i].UserName)
          end;
        finally
          ListBoxRoster.Items.EndUpdate;
          ComboBoxTrainSelect.Items.EndUpdate;
        end;
      end;
    lesAbort   : LabelStatus.Caption := 'Train TaskTrainRoster Notify Aborted';
    lesTimeout : LabelStatus.Caption := 'Train TaskTrainRoster Notify Timeout';
    lesError   : LabelStatus.Caption := 'Error: Train TaskTrainRoster Notify - Code=' + IntToStr(ATask.ErrorCode) + ' ' + ATask.ErrorMessage;
  end;
end;

procedure TFormTrainController.CallbackQueryAttachedListeners(ATask: TLccTaskBase);
var
  TaskQueryAttachedListeners: TLccTaskQueryAttachedListeners;
begin
  TaskQueryAttachedListeners := ATask as TLccTaskQueryAttachedListeners;

  case ATask.TaskState of
    lesComplete :
      begin

        LabelStatus.Caption := '';
      end;
    lesRunning :
      begin
        LabelStatus.Caption := 'Listener Number: ' + IntToStr(TaskQueryAttachedListeners.iListener) + ' of ' + IntToStr(TaskQueryAttachedListeners.ListenerCount)
      end;
    lesAbort   : LabelStatus.Caption := 'Query Attached Listeners Aborted';
    lesTimeout : LabelStatus.Caption := 'Query Attached Listeners Timeout';
    lesError   : LabelStatus.Caption := 'Error: Query Attached Listeners - Code=' + IntToStr(ATask.ErrorCode) + ' ' + ATask.ErrorMessage;
  end;
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
 //     for i := 0 to Controller.TractionServer.List.Count - 1 do
 //     begin
  //      ComboBoxTrainSelect.Items.Add(Controller.TractionServer.Item[i].SNIP.UserName);
  //    end;

    finally
      ComboBoxTrainSelect.Items.EndUpdate;
    end;
  end;
end;

procedure TFormTrainController.UpdateRosterHeaderScrolledLeft;
begin
  if PageControlRoster.PageIndex > 0 then
  begin
    PageControlRoster.PageIndex := PageControlRoster.PageIndex - 1 ;
    if PageControlRoster.PageIndex = 0 then
    begin
 //     DetailsTractionObject := nil;
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
  //  if Assigned(DetailsTractionObject) then
    begin
//      if not DetailsTractionObject.SNIP.Valid then
  //      Controller.SendSNIPRequest(DetailsTractionObject.NodeID, DetailsTractionObject.NodeAlias);
  //    if not DetailsTractionObject.TrainSNIP.Valid then
  //      Controller.SendTrainSNIPRequest(DetailsTractionObject.NodeID, DetailsTractionObject.NodeAlias);
    end;
  end;

  ImageScrollLeft.Visible :=  True;
  ImageScrollRight.Visible := PageControlRoster.PageIndex < (PageControlRoster.PageCount - 1);
  PanelRosterHeader.Caption := PageControlRoster.Pages[PageControlRoster.PageIndex].Caption;
end;

procedure TFormTrainController.LoadCDIUserInterface;
var
  ATargetNode: TLccAliasMappingRec;
begin
 { if Assigned(DetailsTractionObject) then
  begin
    try
      ATargetNode := DetailsTractionObject;
      PanelRosterEditorConfigurationBkGnd.Caption := 'Building User Interface......';
      CDIParser.Build_CDI_Interface(Controller, ATargetNode, PanelRosterEditorConfigurationBkGnd, DetailsTractionObject.NodeCDI.CDI);
      PanelRosterEditorConfigurationBkGnd.Caption := '';

    finally
      ATargetNode.Free;
    end;
  end;     }
end;

procedure TFormTrainController.SelectTrainFromComboBox;
var
  AddressInt: LongInt;
  IsLong: Boolean;
  i: Integer;
begin
  AddressInt := -1;
  IsLong := True; // Address string will catch it with "L", "S" or nothing = "L"
  if ValidateExtendedDccAddress(ComboBoxTrainSelect.Text, AddressInt, IsLong) then
  begin
    if Assigned(Controller) then
    begin
      EnableControls(False);
      Controller.SearchByDccAddress(AddressInt, IsLong, TLccDccSpeedStep( ComboBoxTrainSelect.ItemIndex), @CallbackSearchByDccAddress);
      LabelStatus.Caption := 'Searching for Train: ' + ComboBoxTrainSelect.Caption;
    end;
  end;
end;



end.


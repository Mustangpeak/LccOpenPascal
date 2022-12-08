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
  LCLType,
  strutils,
  lcc_ethernet_client,
  lcc_node_manager,
  lcc_train_server,
  lcc_ethernet_common,
  lcc_node_messages,
  lcc_node_controller,
  lcc_common_classes,
  lcc_node,
  lcc_defines;

const
  MAX_LED_SEGMENTS = 100;
  LED_TAPER = 0.010;
  MAX_DCC_ADDRESS = 10239;

type

  TLEDShapeArray = array[0..MAX_LED_SEGMENTS-1] of TShape;

  { TFormTrainController }

  TFormTrainController = class(TForm)
    ButtonThrottleSelectGo: TButton;
    ButtonThrottleStop: TButton;
    ButtonSettingsRestartConnection: TButton;
    ComboBoxThrottleSpeedSteps: TComboBox;
    ComboBoxTrainSelect: TComboBox;
    EditSettingsIP: TEdit;
    EditSettingsPort: TEdit;
    EditSettingsNodeID: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LabelSettingsAliasID: TLabel;
    LabelSettings1: TLabel;
    LabelSettings2: TLabel;
    LabelSettings3: TLabel;
    LabelSettingsConnectionState: TLabel;
    PageControlMain: TPageControl;
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
    Throttle: TTabSheet;
    Roster: TTabSheet;
    Settings: TTabSheet;
    TimerMain: TTimer;
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
    procedure ButtonSettingsRestartConnectionClick(Sender: TObject);
    procedure ButtonThrottleSelectGoClick(Sender: TObject);
    procedure ButtonThrottleStopClick(Sender: TObject);
    procedure ComboBoxTrainSelectKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnFunctionClick(Sender: TObject);
    procedure PanelThrottleLeverResize(Sender: TObject);
    procedure TimerMainTimer(Sender: TObject);
    procedure ToggleBoxFunctionChange(Sender: TObject);
    procedure ToggleBoxThrottleForwardChange(Sender: TObject);
    procedure ToggleBoxThrottleReverseChange(Sender: TObject);
    procedure TrackBarThrottleChange(Sender: TObject);
  private
    FController: TLccTrainController;
    FEthernetClient: TLccEthernetClient;
    FNodeManager: TLccNodeManager;
    FShownOnce: Boolean;
    FLEDArray: TLEDShapeArray;

  protected
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
    procedure OnConnectionStateChange(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo);
    procedure OnErrorMessage(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo);

    procedure OnSNIPChange(TractionObject: TLccTractionObject);
    procedure OnTrainSNIPChange(TractionObject: TLccTractionObject);
    procedure OnEmergencyStopChange(TractionObject: TLccTractionObject);
    procedure OnFunctionChange(TractionObject: TLccTractionObject);
    procedure OnSpeedChange(TractionObject: TLccTractionObject);
    procedure OnRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);

    procedure OnNodeIDChanged(Sender: TObject; ALccNode: TLccNode);
    procedure OnNodeAliasChanged(Sender: TObject; ALccNode: TLccNode);

    procedure OnControllerAssignChange(Sender: TObject; ATractionServer: TLccTractionServer; ATractionObject: TLccTractionObject; IsAssigned: Boolean);

    procedure EnableControls(DoEnable: Boolean);

    procedure OnLEDClick(Sender: TObject);
    procedure UpdateRoster;
    function ValidateExtendedDccAddress(AddressStr: String; var DccAddress: Integer; var IsLong: Boolean): Boolean;

  public
    property Controller: TLccTrainController read FController write FController;
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
  end;

var
  FormTrainController: TFormTrainController;

implementation

{$R *.lfm}

{ TFormTrainController }


procedure TFormTrainController.FormCreate(Sender: TObject);
begin
  NodeManager := TLccNodeManager.Create(nil, True);
  EthernetClient := TLccEthernetClient.Create(Self, NodeManager);

  EthernetClient.OnConnectionStateChange := @OnConnectionStateChange;
  EthernetClient.OnErrorMessage := @OnErrorMessage;

  NodeManager.OnNodeAliasIDChanged := @OnNodeAliasChanged;
  NodeManager.OnNodeIDChanged := @OnNodeIdChanged;
end;

procedure TFormTrainController.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  TimerMain.Enabled := False; // Stop trying to log in
  NodeManager.ReleaseAliasAll;
  EthernetClient.CloseConnection;
end;

procedure TFormTrainController.ButtonSettingsRestartConnectionClick(Sender: TObject);
var
  Info: TLccEthernetConnectionInfo;
begin
  Info := TLccEthernetConnectionInfo.Create;
  begin
    Info.ListenerIP := EditSettingsIP.Text;
    Info.ListenerPort := StrToInt(EditSettingsPort.Text);
    Info.GridConnect := True;
    Info.Hub := False;
    Info.SuppressErrorMessages := False;
    EthernetClient.OpenConnection(Info);
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
    Controller.AssignTrainByDccAddress(AddressWord, IsLong, TLccDccSpeedStep( ComboBoxThrottleSpeedSteps.ItemIndex));
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
  EthernetClient.Free;
  NodeManager.Free;    // Must be last as previous 2 use it
end;

procedure TFormTrainController.FormShow(Sender: TObject);
var
  i: Integer;
  LED: TShape;
begin
  if not ShownOnce then
  begin
    ShownOnce := True;
    WriteLn('Restarting on Show');
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
    ButtonSettingsRestartConnectionClick(Self);
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

procedure TFormTrainController.TimerMainTimer(Sender: TObject);
begin
  WriteLn('Timer Tick');
  if not (EthernetClient.Connected or EthernetClient.Connecting) then
  begin
    WriteLn('Restarting on Timer');
    ButtonSettingsRestartConnectionClick(Self)
  end;
end;

procedure TFormTrainController.ToggleBoxFunctionChange(Sender: TObject);
begin
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
  if ToggleBoxThrottleForward.Checked then
    Controller.AssignedTrain.SetSpeed(TrackBarThrottle.Position)
  else
    Controller.AssignedTrain.SetSpeed(-TrackBarThrottle.Position)
end;

procedure TFormTrainController.OnConnectionStateChange(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  case TLccEthernetConnectionInfo(ConnectionInfo).ConnectionState of
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
        end;
      end;
    lcsDisconnecting :
      begin
        LabelSettingsConnectionState.Caption := 'Disconnecting';
      end;
  end;
end;

procedure TFormTrainController.OnErrorMessage(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo);
begin

end;

procedure TFormTrainController.OnSNIPChange(TractionObject: TLccTractionObject);
begin
  UpdateRoster
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
begin

end;

procedure TFormTrainController.OnNodeIDChanged(Sender: TObject; ALccNode: TLccNode);
begin
  EditSettingsNodeID.Text := ALccNode.NodeIDStr;
end;

procedure TFormTrainController.OnNodeAliasChanged(Sender: TObject; ALccNode: TLccNode);
begin
 LabelSettingsAliasID.Caption := ALccNode.AliasIDStr;
end;

procedure TFormTrainController.OnControllerAssignChange(Sender: TObject; ATractionServer: TLccTractionServer; ATractionObject: TLccTractionObject; IsAssigned: Boolean);
begin
  EnableControls(IsAssigned);
  if IsAssigned then
  begin
    Controller.ListenerDetachFromAssignedTrain;
    Controller.ListenerAttach(ATractionObject);
  end else
    Controller.ListenerDetachFromAssignedTrain;
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

end.


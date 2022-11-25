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
  CheckBoxThemed,
  lcc_ethernet_client,
  lcc_node_manager,
  lcc_train_server,
  lcc_ethernet_common,
  lcc_node_messages,
  lcc_node_controller,
  lcc_common_classes,
  lcc_defines;

const
  MAX_LED_SEGMENTS = 100;
  LED_TAPER = 0.010;

type

  TLEDShapeArray = array[0..MAX_LED_SEGMENTS-1] of TShape;

  { TFormTrainController }

  TFormTrainController = class(TForm)
    ButtonThrottleSelectGo: TButton;
    ButtonThrottleStop: TButton;
    ButtonSettingsRestartConnection: TButton;
    ComboBoxTrainSelect: TComboBox;
    EditSettingsIP: TEdit;
    EditSettingsPort: TEdit;
    EditSettingsNodeID: TEdit;
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
    ScrollBox1: TScrollBox;
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
    procedure ComboBoxTrainSelectKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelThrottleLeverResize(Sender: TObject);
    procedure TimerMainTimer(Sender: TObject);
    procedure ToggleBoxF1Change(Sender: TObject);
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

    procedure OnLEDClick(Sender: TObject);

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
  Controller := TLccTrainController.Create(NodeManager, '', True);
  EthernetClient := TLccEthernetClient.Create(Self, NodeManager);

  EthernetClient.OnConnectionStateChange := @OnConnectionStateChange;
  EthernetClient.OnErrorMessage := @OnErrorMessage;;
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
  Test: String;
  Valid: Boolean;
  DccAddress: LongInt;
  IsDccAddressLong: Boolean;
  i: Integer;
begin
  Valid := True;
  IsDccAddressLong := False;
  Test := ComboBoxTrainSelect.Text;
  if Test = '' then
    Valid := False
  else begin
    if Length(Test) = 1 then
    begin
      if not TryStrToInt(Test, DccAddress) then
        Valid := False;
    end else
    begin
      for i := 1 to Length(Test) do
      begin
        if i < Length(Test) then
        begin
          if (Test[i] < '0') or (Test[i] > '9') then
            Valid := False;
        end else
        begin
          if (Test[i] >= '0') and (Test[i] <= '9') then
          begin // all numbers
            IsDccAddressLong := True;
            if not TryStrToInt(Test, DccAddress) then   // This should always succeed
              Valid := False;
          end else
          begin
            if (Test[i] = 'L') or (Test[i] = 'l') or (Test[i] = 'S') or (Test[i] = 's') then
            begin
              IsDccAddressLong := (Test[i] = 'L') or (Test[i] = 'l');
              SetLength(Test, Length(Test) - 1);  // strip it off
              if not TryStrToInt(Test, DccAddress) then   // This should always succeed
                Valid := False;
            end else
               Valid := False
          end
        end;
      end
    end
  end;

  if not Valid then
    ShowMessage('Not a valid Address format valid examples are: 40L, 40S, 40s, 40s, 40....')
  else begin
    if IsDccAddressLong then
      ShowMessage(IntToStr( DccAddress) + 'L')
    else
      ShowMessage(IntToStr( DccAddress) + 'S')
  end
end;

procedure TFormTrainController.ComboBoxTrainSelectKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    ButtonThrottleSelectGoClick(Self);
  if (Key < VK_0) or (Key > VK_9) then
    if not ((Key = VK_S) or (Key = VK_L) or (Key = VK_BACK) or (Key = VK_LEFT) or (Key = VK_RIGHT)) then
      Key := 0;
end;

procedure TFormTrainController.FormDestroy(Sender: TObject);
begin
  Controller.Free;
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
    end;
    PanelThrottleLeverResize(Self);
    ButtonSettingsRestartConnectionClick(Self);
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
      LED.Left := Trunc(MAX_LED_SEGMENTS * LED_TAPER * i);
      LED.Width := PanelThrottleLever.Width - (Trunc(MAX_LED_SEGMENTS * LED_TAPER * i));
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

procedure TFormTrainController.ToggleBoxF1Change(Sender: TObject);
begin

end;

procedure TFormTrainController.TrackBarThrottleChange(Sender: TObject);
begin

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

procedure TFormTrainController.OnLEDClick(Sender: TObject);
begin
  ShowMessage('Clicked: ' + IntToStr((Sender as TShape).Tag));
end;

end.


unit frame_left_hand_turnout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TFrame_left_hand_turnoutboss }

  TFrame_left_hand_turnoutboss = class(TFrame)
    Button_facia_button_diverging: TButton;
    Button_facia_button_normal: TButton;
    CheckBox_detection_diverging_track: TCheckBox;
    CheckBox_detection_normal_track: TCheckBox;
    CheckBox_detection_turnout: TCheckBox;
    GroupBox_approach: TGroupBox;
    GroupBox_turnout: TGroupBox;
    Panel1: TPanel;
    RadioGroup_signal_approach_diverging: TRadioGroup;
    RadioGroup_signal_approach_normal: TRadioGroup;
    RadioGroup_signal_diverging: TRadioGroup;
    RadioGroup_signal_normal: TRadioGroup;
    RadioGroup_turnout_commanded_position: TRadioGroup;
    RadioGroup_turnout_sensed_position: TRadioGroup;
    procedure Button_facia_button_divergingClick(Sender: TObject);
    procedure Button_facia_button_normalClick(Sender: TObject);
  private

  public

    procedure ThrowTurnoutNormal;
    procedure ThrowTurnoutDiverging;

  end;

implementation

{$R *.lfm}

{ TFrame_left_hand_turnoutboss }

procedure TFrame_left_hand_turnoutboss.Button_facia_button_normalClick(
  Sender: TObject);
begin
  ThrowTurnoutNormal;
end;

procedure TFrame_left_hand_turnoutboss.Button_facia_button_divergingClick(
  Sender: TObject);
begin
  ThrowTurnoutDiverging;
end;

procedure TFrame_left_hand_turnoutboss.ThrowTurnoutNormal;
begin

end;

procedure TFrame_left_hand_turnoutboss.ThrowTurnoutDiverging;
begin

end;

end.


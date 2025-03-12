unit frame_right_hand;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
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
  private

  public

  end;

implementation

{$R *.lfm}

end.


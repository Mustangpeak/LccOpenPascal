program eventbuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, form_main, frame_workspace, unit_types, unit_turnout,
  unit_switchmachine, unit_layout, unit_division, unit_subdivision,
  unit_junction, unit_actuator, unit_sensor, unit_yard, unit_station,
  unit_base_location, unit_animations, unit_base_layout_item, unit_town, 
unit_workspace, unit_base_frame, unit_defines, unit_popup_menus
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.{%H-}MainFormOnTaskbar:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


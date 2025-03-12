unit form_main;

{$mode objfpc}{$H+}

{$UNDEF FGLINLINE}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Buttons,
  ComCtrls,
  ActnList,
  frame_workspace,
  unit_base_frame,
  unit_types,
  unit_defines,
  unit_popup_menus,
  unit_workspace;

type

  TFrameClass = class of TFrame;

  { TForm1 }

  TForm1 = class(TForm)
    Panel_nodes: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel_turnouts: TPanel;
    Panel_pushbuttons: TPanel;
    Panel3: TPanel;
    Panel_occupancy_detectors: TPanel;
    Panel_sidebar: TPanel;
    Panel_layout: TPanel;
    Panel_network: TPanel;
    Panel_network_header: TPanel;
    Panel_layout_header: TPanel;
    PanelFrameContainer: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel_turnoutsClick(Sender: TObject);
  private
    FActiveFrame: TFrame;
  private
    property ActiveFrame: TFrame read FActiveFrame write FActiveFrame;
    procedure NewFrame(NewFrameType: TBaseFrameClass);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Panel_turnoutsClick(Sender: TObject);
begin
  NewFrame(TFrameWorkspace);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  ActiveFrame.Free;
  ActiveFrame := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormShow(Sender: TObject);
begin

end;

procedure TForm1.NewFrame(NewFrameType: TBaseFrameClass);
var
  NeedsNewFrame: Boolean;
begin
  NeedsNewFrame := not Assigned(ActiveFrame);

  if NeedsNewFrame or (NewFrameType <> ActiveFrame.ClassType) then
  begin

    if Assigned(ActiveFrame) then ActiveFrame.Free;

    ActiveFrame := NewFrameType.Create(Self);
    ActiveFrame.Parent := PanelFrameContainer;
    ActiveFrame.Align := alClient;
  end;
end;

end.


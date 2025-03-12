unit unit_base_frame;

{$mode ObjFPC}{$H+}

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
  unit_popup_menus,
  unit_workspace;

type

  { TBaseFrame }

  TBaseFrame = class(TFrame)

  private

  public


    constructor Create(AOwner: TComponent); reintroduce; virtual;

  end;

  TBaseFrameClass = class of TBaseFrame;

implementation

{ TBaseFrame }

constructor TBaseFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

end.

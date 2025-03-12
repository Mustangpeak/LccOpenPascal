unit unit_base_location;

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
  Menus,
  unit_popup_menus,
  unit_animations,
  unit_sensor,
  unit_base_layout_item,
  unit_defines,
  unit_types,
  unit_turnout;

type

  { TBaseLocation }

  TBaseLocation = class(TBaseLayoutItem)

  private
    FAnimations: TAnimationList;
    FSensors: TSensorList;
    FTurnouts: TTurnoutList;

  protected

  public
    property Turnouts: TTurnoutList read FTurnouts write FTurnouts;
    property Animations: TAnimationList read FAnimations write FAnimations;
    property Sensors: TSensorList read FSensors write FSensors;

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;


implementation

{ TBaseLocation }

procedure TBaseLocation.BuildMenu(Pallet: TPanel);
begin

end;


constructor TBaseLocation.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  FTurnouts := TTurnoutList.Create;
  FAnimations := TAnimationList.Create;
  FSensors := TSensorList.Create;
end;

destructor TBaseLocation.Destroy;
begin
  Turnouts.Free;
  Animations.Free;
  Sensors.Free;
  inherited Destroy;
end;

end.

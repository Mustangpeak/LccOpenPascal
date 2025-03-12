unit unit_turnout;

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
  my_fgl,
  unit_popup_menus,
  unit_switchmachine,
  unit_defines,
  unit_types,
  unit_actuator,
  unit_base_layout_item,
  unit_sensor;

type

  TTurnoutPositions = (tp_TwoWay, tpThreeway);


  { TTurnout }

  TTurnout = class(TBaseLayoutItem)

  private
    FActuator: TActuator;
    FHasFeedback: boolean;
    FIdentifier: string;
    FLocation: string;
    FPostions: TTurnoutPositions;
    FSensor: TSensor;

  protected

  public
    property Location: string read FLocation write FLocation;
    property Identifier: string read FIdentifier write FIdentifier;
    property Positions: TTurnoutPositions read FPostions write FPostions;
    property HasFeedback: boolean read FHasFeedback write FHasFeedback;

    property Actuator: TActuator read FActuator write FActuator;
    property Sensor: TSensor read FSensor write FSensor;

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;
  end;

  TTurnoutList = specialize TFPGInterfacedObjectList<TTurnout>;

implementation

{ TTurnout }

procedure TTurnout.BuildMenu(Pallet: TPanel);
begin

end;

constructor TTurnout.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_TURNOUT);
  FActuator := TActuator.Create(APopupMenuFactory);
  FSensor := TSensor.Create(APopupMenuFactory);
end;

destructor TTurnout.Destroy;
begin
  Actuator.Free;
  Sensor.Free;
  inherited Destroy;
end;

end.

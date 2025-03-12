unit unit_actuator;

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
  unit_defines,
  unit_types,
  unit_base_layout_item,
  my_fgl;

type

  { TActuator }

  TActuator = class(TBaseLayoutItem)

  protected

  public
    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;

    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  { TStallMotorActuator }

  TStallMotorActuator = class(TActuator)

  end;

  { TSolonoidActuator }

  TSolonoidActuator = class(TActuator)

  end;

  { TServoActuator }

  TServoActuator = class(TActuator)


  end;

  TActuatorList = specialize TFPGInterfacedObjectList<TActuator>;

implementation

{ TActuator }

procedure TActuator.BuildMenu(Pallet: TPanel);
begin

end;

constructor TActuator.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_ACUTACTOR);
end;

destructor TActuator.Destroy;
begin
  inherited Destroy;
end;

end.

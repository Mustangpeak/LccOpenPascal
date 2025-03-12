unit unit_sensor;

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
  unit_defines,
  unit_types,
  unit_popup_menus,
  unit_base_layout_item,
  my_fgl;

type

  { TSensor }

  TSensor = class(TBaseLayoutItem)

  protected

  public
    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  { TSensorCoil }

  TSensorCoil = class(TSensor)

  end;

  { TSensorDigital }

  TSensorDigital = class(TSensor)

  end;

  TSensorList = specialize TFPGInterfacedObjectList<TSensor>;

implementation

{ TSensor }

procedure TSensor.BuildMenu(Pallet: TPanel);
begin

end;

constructor TSensor.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_SENSOR);
end;

destructor TSensor.Destroy;
begin
  inherited Destroy;
end;

end.

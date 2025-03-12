unit unit_district;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  unit_switchmachine,
  unit_turnout;

implementation

type

  { TDistrict }

  TDistrict = class(TInterfacedObject)

  private
    FTurnouts: TTurnoutList;
  public
    property Turnouts: TTurnoutList read FTurnouts write FTurnouts;

    constructor Create; reintroduce;
    destructor Destroy; override;

  end;

constructor TDistrict.Create;
begin
  inherited Create();
  FTurnouts := TTurnoutList.Create;
end;

destructor TDistrict.Destroy;
begin
  Turnouts.Free;
  inherited Destroy;
end;

end.

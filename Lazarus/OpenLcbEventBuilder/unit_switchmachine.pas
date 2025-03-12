unit unit_switchmachine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl;

type


  { TSwitchMachine }

  TSwitchMachine = class(TInterfacedObject)


  end;

  { TSwitchMachineStallMotor }

  TSwitchMachineStallMotor = class(TSwitchMachine)


  end;

  { TSwitchMachineSolonid }

  TSwitchMachineSolonid = class(TSwitchMachine)


  end;

  { TSwitchMachineServo }

  TSwitchMachineServo = class(TSwitchMachine)


  end;

  { TSwichMachinePostion }

  TSwichMachinePostion = class(TInterfacedObject)

  end;

implementation

end.

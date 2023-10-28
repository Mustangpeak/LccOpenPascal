unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  lcc_comport,
  lcc_ethernet_client,
  lcc_ethernet_common,
  lcc_ethernet_http,
  lcc_ethernet_server,
  lcc_ethernet_tcp,
  lcc_ethernet_websocket,

  lcc_cdi_parser_2,
  lcc_common_classes,
  lcc_gridconnect,
  lcc_node_messages_can_assembler_disassembler,
  lcc_threaded_circulararray,
  lcc_threaded_stringlist,
  lcc_xmlutilities,

  lcc_utilities,

  lcc_node,
  lcc_node_train,
  lcc_node_messages,
  lcc_node_manager,
  lcc_node_controller,
  lcc_node_commandstation,
  lcc_math_float16,
  lcc_defines,
  lcc_alias_server,
  lcc_alias_mappings;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.


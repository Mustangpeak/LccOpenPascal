unit lcc_ethernet_common;

{$I ../../lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface



uses
  Classes,
  SysUtils,

  {$IFDEF LCC_WINDOWS}
    Windows,
    IdBaseComponent,
    IdComponent,
    IdIPWatch,
  {$ELSE}
    {$IFDEF LCC_FPC}
      baseUnix,
      sockets,
    {$ELSE}
      strutils,
      Posix.NetinetIn,
      Posix.ArpaInet,
      Posix.SysSocket,
      Posix.Errno,
      Posix.Unistd,
    {$ENDIF}
  {$ENDIF}


  {$IFDEF LCC_FPC}
    {$IFNDEF FPC_CONSOLE_APP}
      Forms,
    {$ENDIF}
  {$ELSE}
    FMX.Forms,
    System.Generics.Collections,
  {$ENDIF}
  IdIOHandler,
  lcc_node_messages,
  lcc_node,
  lcc_app_common_settings,
  lcc_defines,
  lcc_connection_common,
  lcc_node_messages_can_assembler_disassembler;

const
  THREAD_SLEEP_TIME = 50;

type

  { TLccEthernetConnectionInfo }

  TLccEthernetConnectionInfo = class(TLccConnectionInfo)
  private
    FAutoResolve: Boolean;           // IN
    FClientIP: string;               // IN
    FClientPort: word;               // IN
    FListenerIP: string;             // IN
    FListenerPort: word;             // IN
  public
    property AutoResolveIP: Boolean read FAutoResolve write FAutoResolve;                     // Tries to autoresolve the local unique netword IP of the machine
    property ClientIP: string read FClientIP write FClientIP;
    property ClientPort: word read FClientPort write FClientPort;
    property ListenerIP: string read FListenerIP write FListenerIP;
    property ListenerPort: word read FListenerPort write FListenerPort;
  end;


  {$IFDEF LCC_WINDOWS}
 //   function ResolveWindowsIp(Socket: TBlockSocket): string; overload;
    function ResolveWindowsIp: string; overload;
  {$ELSE}
      function ResolveUnixIp: String;
  {$ENDIF}

implementation


{$IFDEF LCC_WINDOWS}

  function ResolveWindowsIp: string;
  var
    IdIpWatch: TIdIpWatch;
  begin
    IdIpWatch := nil;
    try
      Result := '';
      IdIpWatch := TIdIpWatch.Create(nil);
      if IdIpWatch.LocalIP <> '' then
        Result := IdIpWatch.LocalIP;
    finally
      IdIpWatch.Free;
    end;
  end;

{   function ResolveWindowsIp: string;
  var
    Socket: TTCPBlockSocket;
  begin
    Socket := TTCPBlockSocket.Create;          // Created in context of the thread
    Socket.Family := SF_IP4;                  // IP4
    Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
    Socket.HeartbeatRate := 0;
    Socket.SetTimeout(0);
    if Socket.LastError <> 0 then
      Result := 'Unknown'
    else
      Result := ResolveWindowsIp(Socket);
    try
    finally
      Socket.Free;
    end;
  end;


  function ResolveWindowsIp(Socket: TBlockSocket): String;
  var
    IpStrings: TStringList;
    LocalName: String;
    i: Integer;
    LocalSocket: TTCPBlockSocket;
  begin
    Result := '';
    LocalSocket := nil;
    if not Assigned(Socket) then
    begin
      LocalSocket := TTCPBlockSocket.Create;
      Socket := LocalSocket;
    end;
    LocalName := Socket.LocalName;
    IpStrings := TStringList.Create;
    try
       Socket.ResolveNameToIP(LocalName, IpStrings) ;  // '192.168.0.8';
       for i := 0 to IpStrings.Count - 1 do
         Result := IpStrings[i];
    finally
      IpStrings.Free;
      if Assigned(LocalSocket) then
        LocalSocket.Free;
    end;
  end;   }
{$ELSE}
  {$IFDEF LCC_FPC}
  function ResolveUnixIp: String;
  const
    CN_GDNS_ADDR = '127.0.0.1';
    CN_GDNS_PORT = 53;
  var
    sock: longint;
    err: longint;
    HostAddr: TSockAddr;
    l: Integer;
    UnixAddr: TInetSockAddr;

  begin
    err := 0;

    sock := fpsocket(AF_INET, SOCK_DGRAM, 0);
    assert(sock <> -1);

    UnixAddr.sin_family := AF_INET;
    UnixAddr.sin_port := htons(CN_GDNS_PORT);
    UnixAddr.sin_addr := StrToHostAddr(CN_GDNS_ADDR);

    if (fpConnect(sock, @UnixAddr, SizeOf(UnixAddr)) = 0) then
    begin
      try
        l := SizeOf(HostAddr);
        if (fpgetsockname(sock, @HostAddr, @l) = 0) then
        begin
          Result := NetAddrToStr(HostAddr.sin_addr);
        end
        else
        begin
          err:=socketError;
        end;
      finally
        if (fpclose(sock) <> 0) then
        begin
          err := socketError;
        end;
      end;
    end else
    begin
      err:=socketError;
    end;

    if (err <> 0) then
    begin
      // report error
    end;
  end;

{$ELSE}    // Delphi
  type
    TArray4Int = array[1..4] of byte;
    PArray4Int = ^TArray4Int;

  function StrToHostAddr(IP : String): in_addr ;

    Var
      Dummy: String;
      I, j, k: Longint;
      Temp: TArray4Int;
 //     Temp: in_addr;
    begin
      Result.s_addr := 0;              //:=NoAddress;
      For I := 1 to 4 do
        begin
          If I < 4 Then
            begin
              J := Pos('.', String( IP));
              If J = 0 then
                exit;
              Dummy := Copy(String( IP) , 1, J-1);
              Delete(IP, 1, J);
            end
           else
             Dummy:=IP;
          k := StrToInt(string( Dummy));
          Temp[i] := k;
      //    PArray4Int(temp.s_addr)^[i] := k;      // Crashes Delphi
       end;
       Result.s_addr := ntohl(UInt32( Temp));
   //    Result.s_addr := ntohl(Temp.s_addr);     // Crashes Delphi
    end;

    function NetAddrToStr (Entry : in_addr) : String;
    Var
      Dummy: String;
   //  j,
      i: Longint;
    begin
      NetAddrToStr := '';
  //    j := entry.s_addr;
      For i := 1 to 4 do
       begin
  //       Dummy := IntToStr( PArray4Int(j)^[i]);        // Crashes Delphi
         Dummy := IntToStr( Entry.s_addr and $000000FF);
         Entry.s_addr  :=  Entry.s_addr  shr 8;
         NetAddrToStr := Result + Dummy;
         If i < 4 Then
           NetAddrToStr := Result + '.';
       end;
    end;

  function ResolveUnixIp: String;
  const
    CN_GDNS_ADDR = '127.0.0.1';
    CN_GDNS_PORT = 53;
  var
    sock: longint;
    err: longint;
    HostAddr: SockAddr;
    l: {$IFDEF ANDROID32}Integer{$ELSE}Cardinal{$ENDIF};
    UnixAddr: sockaddr_in;

  begin
    err := 0;

    sock := socket(AF_INET, SOCK_DGRAM, 0);
    assert(sock <> -1);

    FillChar(UnixAddr, SizeOf(UnixAddr), #0);

    UnixAddr.sin_family := AF_INET;
    UnixAddr.sin_port := htons(CN_GDNS_PORT);
    UnixAddr.sin_addr := StrToHostAddr(CN_GDNS_ADDR);

    if (Connect(sock, psockaddr(@UnixAddr)^, SizeOf(UnixAddr)) = 0) then
    begin
      try
        l := SizeOf(HostAddr);
        if (getsockname(sock, HostAddr, l) = 0) then
          Result := NetAddrToStr(psockaddr_in( @HostAddr).sin_addr)
        else
        begin
          err:=errno;
        end;
      finally
        if (__close(sock) <> 0) then
        begin
          err := errno;
        end;
      end;
    end else
    begin
      err:=errno;
    end;

    if (err <> 0) then
    begin
      // report error
    end;
  end;
  {$ENDIF}
{$ENDIF}


end.


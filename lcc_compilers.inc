


{$IFDEF FPC}
  {$DEFINE LCC_FPC}
{$ELSE}
 {$DEFINE LCC_DELPHI}
 // {$DEFINE LCC_FPC}
 // {$DEFINE WEB_APP}
{$ENDIF}


{$IFDEF WINDOWS}   // Lazarus
  {$DEFINE LCC_WINDOWS}
  {$IFDEF WIN64}        // TODO NEED TO GET THE RIGHT VALUES FOR FPC AND DELPHI HERE
    {$DEFINE LCC_WINDOWS64}
  {$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS} // Delphi
  {$DEFINE LCC_WINDOWS}
  {$IFDEF WIN64}        // TODO NEED TO GET THE RIGHT VALUES FOR FPC AND DELPHI HERE
    {$DEFINE LCC_WIN64}
  {$ENDIF}
{$ENDIF}

{$IFDEF Darwin}   // Lazarus
  {$DEFINE LCC_OSX}
{$ENDIF}
{$IFDEF MACOS32} // Delphi
  {$DEFINE LCC_OSX}
{$ENDIF}
{$IFDEF MACOS} // Delphi
  {$DEFINE LCC_OSX}
{$ENDIF}

{$IFNDEF FPC}
  {$IFDEF IOS}
    {$DEFINE LCC_MOBILE}
    {$ZEROBASEDSTRINGS ON} // Keep the zero based index for strings on mobile apps
  {$ENDIF}
  {$IFDEF ANDROID}
    {$DEFINE LCC_MOBILE}
    {$ZEROBASEDSTRINGS ON} // Keep the zero based index for strings on mobile apps
  {$ENDIF}
{$ENDIF}


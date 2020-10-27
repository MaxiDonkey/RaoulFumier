unit uRecorderTools;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SpeechLib_TLB, OleServer, ActiveX, StdCtrls, MMSystem, ComCtrls,
  ShellApi, StrUtils, StrCopyUtils, uregistry;

function  DelDir(Dir: String): Boolean;
procedure SaveRecoAudioFile(const Result: ISpeechRecoResult; FileName: string);
function  WhatTime:string; overload;
function  WhatTime(Value: TDateTime):string; overload;
procedure ToKeyBoard(const ASt: string);

type
  TKeyBoardWriter = class
    class procedure SendTo(const Value: string);
  end;

implementation

uses
  SendKey32;

const
  BRIEF_TIME          = 40;
  NORMAL_TIME         = 30; //120;
  SHORT_TIME          = 500;
  MEDIUM_TIME         = 1500;
  LONG_TIME           = 3000;
  SCAN_TIME           = 7000;

{ - Supprimer un répertoire non vide - }
function DelDir(Dir: String): Boolean;
var
  fos: TSHFileOpStruct;
begin
  ZeroMemory(@fos, SizeOf(fos));
  with fos do begin
    wFunc  := FO_DELETE;
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
    pFrom  := PChar(Dir + #0);
  end;
  Result := ShFileOperation(fos) = 0;
end;

{ - Enregistrer la source audio de la reconnaissance effectuée - }
procedure SaveRecoAudioFile(const Result: ISpeechRecoResult; FileName: string);
begin
  with TSpFileStream.Create(nil) do
  try
    try
      Format.type_ := Result.AudioFormat.type_;
      Open(FileName, SSFMcreateForWrite, False);
      Write( Result.Audio(0, Result.PhraseInfo.Elements.Count).GetData );
      Close;
    except
    end;
  finally
    Free;
  end;
end;

function WhatTime(Value: TDateTime):string;
var
  h, m, s, ms: word;
  sh: string;

  procedure getHeure; begin
    if h = 1 then sh := 'une heure'
      else
    if (h = 0) and ((m = 0) or (m = 15) or (m = 30) or (m = 45) or (m = 35) or (m = 40) or (m = 50) or (m = 55)) then sh := 'mi nuit'
      else
    if (h = 12) and ((m = 0) or (m = 15) or (m = 30) or (m = 45) or (m = 35) or (m = 40) or (m = 50) or (m = 55)) then sh := 'midi'
      else
    if h = 9 then sh := 'neuve heure'
      else
    if h = 10 then sh := 'dise heure'
      else
    if h = 19 then sh := 'dise neuve heure' else sh := Format('%d heures', [h]);
  end;

  procedure IncHour; begin
    h := h + 1;
    if h > 23 then h := 0;
    getHeure;
  end;

  function TimeStr: string; begin
    getHeure;
    if m = 15 then begin
      Result := Format('%s é car', [sh]);
      Exit
    end;
    if (m = 30) and (h < 13) then begin
      Result := Format('%s é dmi', [sh]);
      Exit
    end;
    if (m = 35) and ((h = 23) or (h < 12)) then begin
      IncHour;
      Result := Format('%s moins vingt-cinq', [sh]);
      Exit
    end;
    if (m = 40) and ((h = 23) or (h < 12)) then begin
      IncHour;
      Result := Format('%s moins vingt', [sh]);
      Exit
    end;
    if (m = 45) and ((h = 23) or (h < 12)) then begin
      IncHour;
      Result := Format('%s moinl lquart', [sh]);
      Exit
    end;
    if (m = 50) and ((h = 23) or (h < 12)) then begin
      IncHour;
      Result := Format('%s moins dix', [sh]);
      Exit
    end;
    if (m = 55) and ((h = 23) or (h < 12)) then begin
      IncHour;
      Result := Format('%s moins cinq', [sh]);
      Exit
    end;


    if (m = 45) and (h = 12) then begin
      sh := '12 heures'
    end;

    if m = 0 then begin
      if Random(1000) mod 3 = 0
        then Result := Format('%s', [sh])
        else Result := Format('%s', [sh]);
    end else begin
      if Random(1000) mod 3 = 0
        then Result := Format('%s %d minutes', [sh, m])
        else Result := Format('%s %d', [sh, m]);
    end
  end;

begin
  Randomize;
  DecodeTime(Value, h, m, s, ms);
  Result := TimeStr
end;

function  WhatTime:string;
begin
  Result := WhatTime(Now)
end;

procedure SendOne(const ASt: string);
begin
  with TStringList.Create do
  try
    Text := ASt;
    with GetEnumerator do
    try
      while MoveNext do if Current = 'null' then Break else ProcessOnKey( Current )
      //soumettre à option 
    finally
      Free
    end
  finally
    Free
  end
end;

function StrToCharList(const ASt: string):string;
begin
  with TStringList.Create do
  try
    Result := ASt;
    while Pos(' ', Result) > 0 do begin
      Add( GetBeforStr(Result, ' ') );
      Result := GetAfterStr(Result, ' ');
    end;
    if Result <> EmptyStr then Add( Result );
    Result := Text
  finally
    Free
  end
end;

procedure ToKeyBoard(const ASt: string);
begin
  KeyWrite(AppKey, 'StrBuffer', ASt);
  SendOne( StrToCharList(ASt) )
end;

{ TKeyBoardWriter }

class procedure TKeyBoardWriter.SendTo(const Value: string);
begin
  ToKeyBoard(Value)
end;

end.

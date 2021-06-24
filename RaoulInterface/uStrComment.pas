unit uStrComment;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, StrCopyUtils, Math, StrUtils;

type
  TCommentBuilder = class
  private
    function LastSentence_(const Text: string):string;
    function NothingToSay_: string;
    function CantSpeak_: string;
  public
    constructor Create;
    class function LastSentence(const Text: string):string;
    class procedure NothingToSay;
    class procedure CantSpeak;
  end;

  TLimitedCommentBuilder = class
  private
    FCount      : Integer;
    FValue      : Integer;
    FOldValue   : Integer;
    FTimeMemory : array[0..2] of Cardinal;
    procedure Reset;
    procedure SetTimeStamp;
    function  Accepted: Boolean;
    function  TimeFailedMessage: string; virtual; abstract;
    procedure Process; virtual; abstract;
    function  Delay1: Integer; virtual;
    function  Delay2: Integer; virtual;
  public
    procedure TryTopronounce;

    constructor Create;
  end;

  TTimeCommentBuilder = class(TLimitedCommentBuilder)
  private
    function  TimeFailedMessage: string; override;
    procedure Process; override;
  end;

  TDateCommentBuilder = class(TLimitedCommentBuilder)
  private
    function  TimeFailedMessage: string; override;
    procedure Process; override;
  end;

  TFullDateCommentBuilder = class(TLimitedCommentBuilder)
  private
    function  TimeFailedMessage: string; override;
    procedure Process; override;
  end;

  TDOWCommentBuilder = class(TLimitedCommentBuilder)
  private
    function  TimeFailedMessage: string; override;
    procedure Process; override;
  end;

  TRepeatCommentBuilder = class(TLimitedCommentBuilder)
  private
    function  TimeFailedMessage: string; override;
    procedure Process; override;
  end;

  TResultatCommentBuilder = class(TLimitedCommentBuilder)
  private
    function  TimeFailedMessage: string; override;
    procedure Process; override;
  end;

  TRedoCommentBuilder = class(TLimitedCommentBuilder)
  private
    function  TimeFailedMessage: string; override;
    procedure Process; override;
    function  Delay1: Integer; override;
    function  Delay2: Integer; override;
  end;

  TReTalkCommentBuilder = class(TLimitedCommentBuilder)
  private
    function  TimeFailedMessage: string; override;
    procedure Process; override;
    function  Delay1: Integer; override;
    function  Delay2: Integer; override;
  end;

var
  TimeCommentBuilder     : TTimeCommentBuilder;
  DateCommentBuilder     : TDateCommentBuilder;
  FullDateCommentBuilder : TFullDateCommentBuilder;
  DOWCommentBuilder      : TDOWCommentBuilder;
  RepeatCommentBuilder   : TRepeatCommentBuilder;
  ResultatCommentBuilder : TResultatCommentBuilder;
  RedoCommentBuilder     : TRedoCommentBuilder;
  ReTalkCommentBuilder   : TReTalkCommentBuilder;

implementation

uses
  uNewRecorder, uRecorderTools;

var
  OldValue: Integer = -1;

function DateTimeComment(St1, St2, st3: string): string;
var
  Value: Integer;
begin
  Randomize;
  Value := Random(5);
  if Value = OldValue then Value := (Value + 1) mod 5;
  case Value of
    0 : Result := 'ça na toujours pa changé';
    1 : Result := Format('%s que tout à l''heure', [St1]);
    2 : Result := Format('%s est en panne', [St2]);
    3 : Result := Format('taka rgarder %s', [St3]);
    4 : Result := 'taka dmander à goo gueule !';
  end;
  OldValue := Value
end;

{ TCommentBuilder }

class procedure TCommentBuilder.CantSpeak;
begin
  with TCommentBuilder.Create do try CantSpeak_ finally Free end
end;

function TCommentBuilder.CantSpeak_: string;
begin
  case Random(5) of
    0 : Result := 'J''ai pas ldroit dparler !';
    1 : Result := 'tu m''as interdit de parler !';
    2 : Result := 'avant, tu dois m''autoriser à parler';
    3 : Result := 'permet moi de répondre avant ça';
    4 : Result := 'si tu mlaissais parler aussi !';
  end;
  Recorder.TalkFmt(15,5, Result, True)
end;

constructor TCommentBuilder.Create;
begin
  inherited Create;
  Randomize
end;

class function TCommentBuilder.LastSentence(const Text: string): string;
begin
  with TCommentBuilder.Create do try Result := LastSentence_(Text) finally Free end
end;

function TCommentBuilder.LastSentence_(const Text: string): string;
var
  Prf: string;
begin
  Result := Text;
  Prf := EmptyStr;
  case Random(8) of
    1 : Prf := 'tu as dis,';
    3 : Prf := 'tu viens de dire,';
    5 : Prf := 'tu m''as dis,';
    7 : Prf := 'tu viens dme dire,';
  end;
  if (Random(20) mod 2 = 0) and (AnsiPos('hey fumier', Result) > 0)
    then Result := GetAfterStr(Result, 'hey fumier');
  if (Random(20) mod 2 = 0) then Result := Trim( Format('%s %s', [prf, Result]) )
end;

class procedure TCommentBuilder.NothingToSay;
begin
  with TCommentBuilder.Create do try NothingToSay_ finally Free end
end;

function TCommentBuilder.NothingToSay_: string;
begin
  case Random(3) of
    0 : Result := 'ya rien na dire';
    1 : Result := 'que dire ?';
    2 : Result := 'jvois pas sque jpeu dire !';
    3 : Result := 'tu veux queuj dize quoi ?';
  end;
  Recorder.TalkFmt(10, 0, Result, True)
end;

{ TTimeCommentBuilder }

function TLimitedCommentBuilder.Accepted: Boolean;
begin
  Result := True;
  if FCount < 3 then Exit;

  Result := Abs( FTimeMemory[ Pred(FCount) mod 3] - FTimeMemory[ Pred(Pred(FCount)) mod 3] ) > Delay1;
  if not Result then
    Result := Abs( FTimeMemory[ Pred(Pred(FCount)) mod 3] - FTimeMemory[ Pred(Pred(Pred(FCount))) mod 3] ) > Delay2
end;

constructor TLimitedCommentBuilder.Create;
begin
  inherited Create;
  Reset;
  FOldValue := -1;
end;

function TLimitedCommentBuilder.Delay1: Integer;
begin
  Result := 15000
end;

function TLimitedCommentBuilder.Delay2: Integer;
begin
  Result := 28000
end;

procedure TLimitedCommentBuilder.Reset;
var
  i : Integer;
  X : Cardinal;
begin
  X := GetTickCount;
  for i := 0 to 2 do FTimeMemory[i] := X;
  FCount := 0
end;

procedure TLimitedCommentBuilder.SetTimeStamp;
begin
  FTimeMemory[ FCount mod 3 ] := GetTickCount;
  Inc( FCount )
end;

procedure TLimitedCommentBuilder.TryTopronounce;
begin
  SetTimeStamp;
  if Accepted then Process else Recorder.TalkFmt(15, 10, TimeFailedMessage, True)
end;

{ TTimeCommentBuilder }

procedure TTimeCommentBuilder.Process;
begin
  TSpeaker.TalkNP( WhatTime )
end;

function TTimeCommentBuilder.TimeFailedMessage: string;
begin
  Result := DateTimeComment('la même heure', 'mon horloge', 'ta montre')
end;

{ TDateCommentBuilder }

function TDateCommentBuilder.TimeFailedMessage: string;
begin
  Result := DateTimeComment('la même date', 'mon calendrier', 'ton calendrier')
end;

procedure TDateCommentBuilder.Process;
begin
  TSpeaker.TalkNP( WitchDate )
end;

{ TFullDateCommentBuilder }

function TFullDateCommentBuilder.TimeFailedMessage: string;
begin
  Result := DateCommentBuilder.TimeFailedMessage
end;

procedure TFullDateCommentBuilder.Process;
begin
  TSpeaker.TalkNP( WitchFullDate )
end;

{ TDOWCommentBuilder }

function TDOWCommentBuilder.TimeFailedMessage: string;
begin
  Result := DateTimeComment('le même jour', 'mon calendrier', 'ton calendrier')
end;

procedure TDOWCommentBuilder.Process;
begin
  TSpeaker.TalkNP( WitchDayOfWeek )
end;

{ TRepeatCommentBuilder }

function TRepeatCommentBuilder.TimeFailedMessage: string;
begin
  Randomize;
  FValue := Random(5);
  if FValue = FOldValue then FValue := (FValue + 1) mod 5;
  case FValue of
    0 : Result := 'ta la mémoire d''un poisson rouge ?';
    1 : Result := 'la mémoire sa stravaille ! tu sais ?';
    2 : Result := 't''arrive pas à reutnir ?';
    3 : Result := 'non, jeune répétrai pas !';
    4 : Result := 'mince ? j''ai tou toublié !';
  end;
  FOldValue := FValue
end;

procedure TRepeatCommentBuilder.Process;
begin
  TVoiceEntries.RaoulRepeater
end;

{ TResultatCommentBuilder }

function TResultatCommentBuilder.TimeFailedMessage: string;
begin
  Result := RepeatCommentBuilder.TimeFailedMessage
end;

procedure TResultatCommentBuilder.Process;
begin
  Recorder.CalcResultVoiceReadActivate
end;

{ TRedoCommentBuilder }

function TRedoCommentBuilder.TimeFailedMessage: string;
begin
  Randomize;
  FValue := Random(5);
  if FValue = FOldValue then FValue := (FValue + 1) mod 5;
  case FValue of
    0 : Result := 'jeune nvé pa passer le journée à faire ça !';
    1 : Result := 'non, j''en ai marre';
    2 : Result := 'sa mfatigues';
    3 : Result := 'mort de rire ? sé une rébélion';
    4 : Result := 'désolé mé ! jni arrive plu';
  end;
  FOldValue := FValue
end;

procedure TRedoCommentBuilder.Process;
begin
  TVoiceEntries.Redo
end;

function TRedoCommentBuilder.Delay2: Integer;
begin
  Result := 6500;
end;

function TRedoCommentBuilder.Delay1: Integer;
begin
  Result := 3500;
end;

{ TReTalkCommentBuilder }

function TReTalkCommentBuilder.TimeFailedMessage: string;
begin
  Result := RepeatCommentBuilder.TimeFailedMessage
end;

procedure TReTalkCommentBuilder.Process;
begin
  TVoiceEntries.ReTalk
end;

function TReTalkCommentBuilder.Delay2: Integer;
begin
  Result := 9000;
end;

function TReTalkCommentBuilder.Delay1: Integer;
begin
  Result := 6000;
end;

initialization
  TimeCommentBuilder     := TTimeCommentBuilder.Create;
  DateCommentBuilder     := TDateCommentBuilder.Create;
  FullDateCommentBuilder := TFullDateCommentBuilder.Create;
  DOWCommentBuilder      := TDOWCommentBuilder.Create;
  RepeatCommentBuilder   := TRepeatCommentBuilder.Create;
  ResultatCommentBuilder := TResultatCommentBuilder.Create;
  RedoCommentBuilder     := TRedoCommentBuilder.Create;
  ReTalkCommentBuilder   := TReTalkCommentBuilder.Create;
finalization
  TimeCommentBuilder.Free;
  DateCommentBuilder.Free;
  FullDateCommentBuilder.Free;
  DOWCommentBuilder.Free;
  RepeatCommentBuilder.Free;
  ResultatCommentBuilder.Free;
  RedoCommentBuilder.Free;
  ReTalkCommentBuilder.Free;
end.

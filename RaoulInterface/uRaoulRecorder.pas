unit uRaoulRecorder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uRegistry, Math,
  uRaoulDisplay,

  {Vocal}
  uNewRecorder, SpeechLib_TLB,

  {Racccourcis clavier}
  SendKey32;

type
  TInternRecorder = class
  private
    procedure DoOnDestroy;
    procedure RecorderInitialize;
  private
    procedure SoundTrackReset;
  private
    procedure AudioGauge(Sender: TObject; AudioLvl: Integer);
    procedure MetierChanged(Sender: TObject; NewMetier: TMetiers);
    procedure TauxChange(Sender: TObject; ATaux: Double);
    procedure AcceptRecognize(Sender: TObject; AText, SML: string);
    procedure RejectRecognize(Sender: TObject);
    procedure AcceptRecognizeEx(Sender: TObject; AText: string);
    procedure SleepMode(Sender: TObject);
    procedure AwakeMode(Sender: TObject);
    procedure EndStream(Sender: TObject);
    procedure EndSound(Sender: TObject);
    procedure PhraseStart(Sender: TObject);
    procedure StartStream(Sender: TObject);
    procedure StartSound(Sender: TObject);
    procedure Interference(Sender: TObject; Interfer: TInterferenceKind; Noises: TArrayOfNoises);

    procedure EliteSetFocus(Sender: TObject);
  public
    procedure DoOnClose;

    constructor Create;
    destructor Destroy; override;

    class procedure Initialize;
  end;

  TRecordLoader  = class(TThread)
  private
    ThRecorder: TNewRecorder;
    procedure Process;
  public
    procedure Execute; override;
    constructor Create(const ARecorder: TNewRecorder);
  end;

var
  InternRecorder: TInternRecorder;

implementation

uses
  cxProgressBar,
  {acces SQL distant}
  uRaoulDB, uRaoulUpdater,
  {Elite bindings}
  EliteBindingsTools;

{ TInternRecorder }

procedure TInternRecorder.AcceptRecognize(Sender: TObject; AText, SML: string);
var
  Taux: Double;
begin
  Taux := SimpleRoundTo(100 * TSMLConfiance.Confidence(SML),-1);
  with TalkativeFacade do TextColoration(Taux, Format('%s (%s)',
    [ AText,
      Format('%.1f%s', [Taux, '%'])]))
end;

procedure TInternRecorder.AcceptRecognizeEx(Sender: TObject; AText: string);
begin
  with TalkativeFacade do Text := AText
end;

procedure TInternRecorder.AudioGauge(Sender: TObject; AudioLvl: Integer);
begin

end;

procedure TInternRecorder.AwakeMode(Sender: TObject);
begin
  with TalkativeFacade do begin
    Text        := EmptyStr;
    CommandText := 'A l''écoute';
    with TalkativeGauge do Visible := KeyReadBoolean(BufferKey, 'TalkativeSoundView', True)
  end
end;

constructor TInternRecorder.Create;
begin
  inherited Create;
  RecorderInitialize;
end;

destructor TInternRecorder.Destroy;
begin
  DoOnDestroy;
  inherited;
end;

procedure TInternRecorder.DoOnClose;
begin
  TThreadManager.Finalize;
  TAllShortcut.Finalize;
end;

procedure TInternRecorder.DoOnDestroy;
begin
  PileMacros.Free;
  PileFumier.Free;
  PileGauss.Free;
  TRecorderParams.Finalize;
end;

procedure TInternRecorder.EliteSetFocus(Sender: TObject);
begin
  EliteForeGround
end;

procedure TInternRecorder.EndSound(Sender: TObject);
begin
  with TalkativeFacade do TryRecognize := False;
  { --- ne plus afficher l'hypothesys}
  with TalkativeFacade do Text := EmptyStr;
end;

procedure TInternRecorder.EndStream(Sender: TObject);
begin

end;

class procedure TInternRecorder.Initialize;
begin
  InternRecorder := TInternRecorder.Create;
end;

procedure TInternRecorder.Interference(Sender: TObject;
  Interfer: TInterferenceKind; Noises: TArrayOfNoises);
begin

end;

procedure TInternRecorder.MetierChanged(Sender: TObject; NewMetier: TMetiers);
begin
  KeyWrite(BufferKey, 'IndexMetier', Integer(NewMetier));
  with TalkativeFacade do case NewMetier of
    m_none   : CommandText := 'A l''écoute';
    m_fumier : CommandText := 'Conversation à l''écoute';
    m_gauss  : CommandText := 'Calcul à l''écoute';
    m_spell  : CommandText := 'Navigation à l''écoute';
    m_elite  : begin
       CommandText := 'Ok Commander';
       if IsEliteRunning then EliteForeGround;
    end
  end;
  TSQLRaoul.UserDataExport
end;

procedure TInternRecorder.PhraseStart(Sender: TObject);
begin
  with TalkativeFacade do begin
    Text         := EmptyStr;      
    TryRecognize := True;
    TextColor    := clGray
  end
end;

procedure TInternRecorder.RecorderInitialize;
begin
  SoundTrackReset;
  TRecorderParams.Initialize;
  TAllShortcut.Initialize;
  Recorder := TNewRecorder.Create(TalkativeGauge);
  with Recorder do begin
    OnAudioLevelChange  := AudioGauge;
    OnMetierChanged     := MetierChanged;
    OnTauxChange        := TauxChange;
    OnRecognizeAccepted := AcceptRecognize;
    OnRecognizeReject   := RejectRecognize;
    OnBuildHypothesis   := AcceptRecognizeEx;
    OnSleepMode         := SleepMode;
    OnAwakeMode         := AwakeMode;
    OnEndStream         := EndStream;
    OnEndSound          := EndSound;
    OnPhraseStart       := PhraseStart;
    OnStartStream       := StartStream;
    OnStartSound        := StartSound;
    OnGramReloaded      := AcceptRecognizeEx;
    OnGramStartReload   := AcceptRecognizeEx;
    OnInterference      := Interference;
    {Metier}
    OnEliteFocus        := EliteSetFocus;
  end;
  TThreadManager.Initialize;
end;

procedure TInternRecorder.RejectRecognize(Sender: TObject);
begin
  with TalkativeFacade do Text := EmptyStr
end;

procedure TInternRecorder.SleepMode(Sender: TObject);
begin
  with TalkativeFacade do begin
    Text        := EmptyStr;
    CommandText := 'En sommeil'
  end
end;

procedure TInternRecorder.SoundTrackReset;
begin
  with TalkativeGauge do begin
    Position := 0;
    Properties.PeakValue :=  0;
  end
end;

procedure TInternRecorder.StartSound(Sender: TObject);
begin
  
end;

procedure TInternRecorder.StartStream(Sender: TObject);
begin

end;

procedure TInternRecorder.TauxChange(Sender: TObject; ATaux: Double);
begin

end;

{ TRecordLoader }

constructor TRecordLoader.Create(const ARecorder: TNewRecorder);
begin
  inherited Create( False );
  {not Launch on create}
  ThRecorder      := ARecorder;
  Priority        := tpHigher;
  FreeOnTerminate := True;
end;

procedure TRecordLoader.Execute;
begin
  KeyWrite(BufferKey, 'RecorderStarted', False);
  Synchronize(Process);
  KeyWrite(BufferKey, 'RecorderStarted', True);
end;

procedure TRecordLoader.Process;
begin
  with ThRecorder do Initialize;
end;

end.

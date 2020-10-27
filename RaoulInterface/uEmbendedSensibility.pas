unit uEmbendedSensibility;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uEmbendedAncestor, StdCtrls, ExtCtrls, uRegistry,

  {Vocal}
  uNewRecorder,

  {dev express}
  cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxSkinDarkRoom, cxLabel, cxTrackBar;

type
  TEmbendedSensibility = class(TAncestorEmbendedForm)
    Panel4: TPanel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    cxTrackBar1: TcxTrackBar;
    cxLabel1: TcxLabel;
    procedure cxTrackBar1PropertiesChange(Sender: TObject);
  private
    FSensibility: Integer;
    procedure SetSensibility(const Value: Integer);
    procedure RetrieveSensibility;
    procedure OkClic(Sender: TObject);
  protected
    procedure DoOnCreate; override;
    procedure DoOnShow; override;
    procedure DoOnHide; override;
  public
    procedure HautePerformances;
    procedure MicroCasque;
    procedure MicroDeStudio;
    procedure MicroEnceinte;

    property Sensibility: Integer read FSensibility write SetSensibility;
  end;

var
  EmbendedSensibility: TEmbendedSensibility;

implementation

{$R *.dfm}

uses
  {SQL Data distant}
  uRaoulDB;

{ TEmbendedSensibility }

procedure TEmbendedSensibility.DoOnCreate;
begin
  inherited;
  RetrieveSensibility;
  LocalTrackBar := cxTrackBar1;
  with Panel4   do OnClick := DoFormClic;
  with cxLabel1 do OnClick := DoFormClic;
  with Label5   do OnClick := DoFormClic;
  with Label8   do OnClick := DoFormClic;
  with Label9   do OnClick := DoFormClic;
  with Label10  do OnClick := DoFormClic;
end;

procedure TEmbendedSensibility.SetSensibility(const Value: Integer);
begin
  FSensibility := Value;
  with cxTrackBar1, Properties do begin
    SelectionStart := FSensibility;
    Position       := FSensibility;
  end;
  TSensibilitySetting.AdjustInt( FSensibility );
  TSQLRaoul.UserDataExport
end;

procedure TEmbendedSensibility.cxTrackBar1PropertiesChange(Sender: TObject);
begin
  inherited;
  with cxTrackBar1 do Sensibility := Position
end;

procedure TEmbendedSensibility.RetrieveSensibility;
begin
  Sensibility := KeyReadInt(IniKey, 'Sensibility', 3);
end;

procedure TEmbendedSensibility.HautePerformances;
begin
  Sensibility := Integer(s_highperformance)
end;

procedure TEmbendedSensibility.MicroCasque;
begin
  Sensibility := Integer(s_headphonemic);
end;

procedure TEmbendedSensibility.MicroDeStudio;
begin
  Sensibility := Integer(s_studiomic);
end;

procedure TEmbendedSensibility.MicroEnceinte;
begin
  Sensibility := Integer(s_speakermic)
end;

procedure TEmbendedSensibility.DoOnHide;
begin
  inherited;

end;

procedure TEmbendedSensibility.OkClic(Sender: TObject);
begin
  if not Assigned(OkFunc) then Hide else begin
    Recorder.ModeFonctionnement := mf_sensibility;
    OkFunc(nil)
  end
end;

procedure TEmbendedSensibility.DoOnShow;
begin
  inherited;
  with Label3 do OnClick := OkClic
end;

end.

unit uEmbendedDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uEmbendedAncestor, StdCtrls, ExtCtrls,
  {Vocal}
  uNewRecorder, jpeg;

type
  TEmbendedDlg = class(TAncestorEmbendedForm)
    Label1: TLabel;
    Image1: TImage;
    Panel4: TPanel;
    Panel6: TPanel;
  private
    function  GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure YesClic(Sender: TObject);
    procedure NoClic(Sender: TObject);
  protected
    procedure DoOnCreate; override;
    procedure DoOnShow; override;
  public
    property Caption: string read GetCaption write SetCaption;
  end;

var
  EmbendedDlg: TEmbendedDlg;

implementation

{$R *.dfm}

{ TEmbendedDlg }

procedure TEmbendedDlg.DoOnCreate;
begin
  inherited;
  with Label1 do OnClick := DoFormClic;
end;

procedure TEmbendedDlg.DoOnShow;
begin
  inherited;
  with Label2 do OnClick := YesClic;
  with Label4 do OnClick := NoClic
end;

function TEmbendedDlg.GetCaption: string;
begin

  with Label1 do Result := Caption
end;

procedure TEmbendedDlg.NoClic(Sender: TObject);
begin
  if not Assigned(NoFunc) then Hide else NoFunc(nil)
end;

procedure TEmbendedDlg.SetCaption(const Value: string);
begin
  with Label1 do Caption := Value
end;

procedure TEmbendedDlg.YesClic(Sender: TObject);
begin
  if not Assigned(YesFunc) then Hide else YesFunc(nil)
end;

end.

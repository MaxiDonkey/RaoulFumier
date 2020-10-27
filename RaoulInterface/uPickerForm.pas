unit uPickerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uScreenTools, ExtCtrls;

type
  TPickerGridForm = class(TForm)
    GridPicker: TGridPicker;
    DelayedStart: TTimer;
    procedure DelayedStartTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridPickerShowPickList(const S: string);
    procedure GridPickerDeleteConfirm(var Accept: Boolean);
  private
  public
    property Grid: TGridPicker read GridPicker;
  end;

var
  PickerGridForm: TPickerGridForm;

implementation

{$R *.dfm}

uses
  main;

procedure TPickerGridForm.GridPickerDeleteConfirm(var Accept: Boolean);
begin
//   Accept := MessageDlg('Confirmer la suppression ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  Accept := True;
end;

procedure TPickerGridForm.GridPickerShowPickList(const S: string);
begin
  ShowMessage(S)
end;

procedure TPickerGridForm.FormCreate(Sender: TObject);
begin
   GridPicker.Initialize(Self);
end;

procedure TPickerGridForm.FormDestroy(Sender: TObject);
begin
  GridPicker.PickDestroy
end;

procedure TPickerGridForm.FormShow(Sender: TObject);
begin
  AlphaBlend      := True;
  AlphaBlendValue := 180;
  GridPicker.PickFormShow;
  SetForegroundWindow(Handle);
  with DelayedStart do Enabled := True
end;

procedure TPickerGridForm.DelayedStartTimer(Sender: TObject);
begin
  with DelayedStart do Enabled := False;
  GridPicker.PickFormShowDelayed
end;

end.

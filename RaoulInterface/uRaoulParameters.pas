unit uRaoulParameters;

interface

uses
  Windows, Messages, SysUtils, Variants,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uRegistry, StrUtils;

type
  TAppParameters = class
  private
    procedure ExternalInitialization;
    function  GetGrammarUpdated: Boolean;
    procedure SetGrammarUpdated(const Value: Boolean);
  public
    property GrammarUpdated: Boolean read GetGrammarUpdated write SetGrammarUpdated;

    class procedure Initialize;
  end;

var
  AppParameters: TAppParameters = nil;

implementation

{ TAppParameters }

procedure TAppParameters.ExternalInitialization;
begin
  GrammarUpdated := False;
  KeyWrite(AppKey, 'PickerGridVisible', False);
end;

function TAppParameters.GetGrammarUpdated: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'GrammarUpdated')
end;

class procedure TAppParameters.Initialize;
begin
  if not Assigned(AppParameters) then AppParameters := TAppParameters.Create;
  AppParameters.ExternalInitialization;
end;

procedure TAppParameters.SetGrammarUpdated(const Value: Boolean);
begin
  KeyWrite(AppKey, 'GrammarUpdated', Value)
end;

end.

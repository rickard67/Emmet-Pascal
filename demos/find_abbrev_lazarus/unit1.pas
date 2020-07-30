unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  StrUtils, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Edit1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses EmmetHelper;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Edit1Change(Sender: TObject);
var
  N: integer;
  Abr: string;
begin
  EmmetFindAbbrev(Edit1.Text, Length(Edit1.Text), N, Abr);
  Edit2.Text:= Abr;
end;

end.


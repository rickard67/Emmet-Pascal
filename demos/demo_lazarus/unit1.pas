unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Emmet;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonExpand: TButton;
    ComboSyntax: TComboBox;
    EditInput: TEdit;
    Label1: TLabel;
    MemoOut: TMemo;
    procedure ButtonExpandClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FEmmet: TEmmet;
    List: TStringList;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  dir: string;
begin
  dir:= ExtractFileDir(ExtractFileDir(ExtractFileDir(Application.ExeName)))+DirectorySeparator+'emmet';
  if not DirectoryExists(dir) then
  begin
    ShowMessage('Emmet data folder not found:'#10+dir);
    Application.Terminate;
  end;
  FEmmet:= TEmmet.Create(
    dir+DirectorySeparator+'Snippets.ini',
    dir+DirectorySeparator+'Lorem.txt'
    );
  List:= TStringList.Create;
end;

procedure TForm1.ButtonExpandClick(Sender: TObject);
var
  SResult, SSection: string;
  bMulti: boolean;
begin
  SResult:= FEmmet.ExpandAbbreviation(
    EditInput.Text,
    ComboSyntax.Text,
    '',
    SSection,
    bMulti
    );

  List.Clear;
  List.Text:= SResult;

  MemoOut.Lines.Clear;
  MemoOut.Lines.AddStrings(List);
end;

end.


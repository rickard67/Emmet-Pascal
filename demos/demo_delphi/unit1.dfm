object Form1: TForm1
  Left = 389
  Top = 158
  Caption = 'Emmet test'
  ClientHeight = 450
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    600
    450)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 65
    Height = 13
    Caption = 'Abbreviation:'
    Color = clBtnFace
    ParentColor = False
  end
  object EditInput: TEdit
    Left = 8
    Top = 31
    Width = 586
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object ButtonExpand: TButton
    Left = 8
    Top = 64
    Width = 150
    Height = 29
    Caption = 'Expand'
    Constraints.MinWidth = 150
    Default = True
    TabOrder = 1
    OnClick = ButtonExpandClick
  end
  object MemoOut: TMemo
    Left = 8
    Top = 99
    Width = 586
    Height = 345
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object ComboSyntax: TComboBox
    Left = 449
    Top = 64
    Width = 145
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemIndex = 0
    TabOrder = 3
    Text = 'html'
    Items.Strings = (
      'html'
      'css'
      'xsl'
      'svg')
  end
end

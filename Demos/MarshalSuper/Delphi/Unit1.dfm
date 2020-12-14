object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 227
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    375
    227)
  PixelsPerInch = 96
  TextHeight = 13
  object BMarshal: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Marshal'
    TabOrder = 0
    OnClick = BMarshalClick
  end
  object BUnMarshal: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'UnMarshal'
    TabOrder = 1
    OnClick = BUnMarshalClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 359
    Height = 180
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
    ExplicitWidth = 569
    ExplicitHeight = 228
  end
end

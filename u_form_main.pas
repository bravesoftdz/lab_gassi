unit u_form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, u_gassi_drawcontrol, u_gassi_visualobjects,
  u_gassi_logicaldraw, u_gassi_const, ComObj;

type

  { TFgassicMain }

  TFgassicMain = class(TForm)
    btnBlockInsert1: TButton;
    btnEllipse: TButton;
    btnDeselectAll: TButton;
    btnZoomToFit: TButton;
    btnPoint: TButton;
    btnCircle: TButton;
    btnLine: TButton;
    btnArc: TButton;
    btnPolyline: TButton;
    btnImportBlock: TButton;
    btnText: TButton;
    btnBlockInsert: TButton;
    btnBlockCreate: TButton;
    btnBlockCreate1: TButton;
    btnAddFrame: TButton;
    btnClearFrame: TButton;
    cbDevMode: TCheckBox;
    cbAxes: TCheckBox;
    cbReadOnly: TCheckBox;
    cbColorSelect: TComboBox;
    cbLineWeight: TComboBox;
    cbAAINSIDE: TCheckBox;
    cbAABASEPOINT: TCheckBox;
    cbAAVERTEX: TCheckBox;
    cbAABORDER: TCheckBox;
    edtscale: TEdit;
    edtBlockName: TEdit;
    Memo1: TMemo;
    PageControl1: TPageControl;
    pnlBox: TPanel;
    pnlRight: TPanel;
    pnlTop: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure btnAddFrameClick(Sender: TObject);
    procedure btnArcClick(Sender: TObject);
    procedure btnBlockCreateClick(Sender: TObject);
    procedure btnBlockInsert1Click(Sender: TObject);
    procedure btnBlockInsertClick(Sender: TObject);
    procedure btnCircleClick(Sender: TObject);
    procedure btnClearFrameClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnEllipseClick(Sender: TObject);
    procedure btnLineClick(Sender: TObject);
    procedure btnPointClick(Sender: TObject);
    procedure btnPolylineClick(Sender: TObject);
    procedure btnZoomToFitClick(Sender: TObject);
    procedure btnImportBlockClick(Sender: TObject);
    procedure btnTextClick(Sender: TObject);
    procedure btnBlockCreate1Click(Sender: TObject);
    procedure cbAABASEPOINTChange(Sender: TObject);
    procedure cbAABORDERChange(Sender: TObject);
    procedure cbAAINSIDEChange(Sender: TObject);
    procedure cbAAOUTSIDEChange(Sender: TObject);
    procedure cbAAVERTEXChange(Sender: TObject);
    procedure cbColorSelectChange(Sender: TObject);
    procedure cbLineWeightChange(Sender: TObject);
    procedure cbDevModeChange(Sender: TObject);
    procedure cbAxesChange(Sender: TObject);
    procedure cbReadOnlyChange(Sender: TObject);
    procedure edtscaleKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlBoxClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ChangeSelectList(Sender: TObject);
    procedure EntityBeforeDrawEvent(Sender: TObject; AEntity:TEntity; var CanDraw:Boolean);
    procedure EntityAfterDrawEvent(Sender: TObject; AEntity:TEntity);
  end;

var
  FgassicMain: TFgassicMain;
  AssiDrawControl:TAssiDrawControl;

implementation

{$R *.lfm}

{ TFgassicMain }

procedure TFgassicMain.pnlBoxClick(Sender: TObject);
begin

end;

procedure TFgassicMain.ChangeSelectList(Sender: TObject);
var
  EntityItem:TEntity;
  i:integer;
begin
  Memo1.Lines.Clear;
  for I := 0 to AssiDrawControl.ActiveDocument.SelectList.Count - 1 do
  begin
      EntityItem:=TEntity(AssiDrawControl.ActiveDocument.SelectList.Items[i]);
      Memo1.Lines.Add(EntityItem.ClassName);
  end;
end;

procedure TFgassicMain.EntityBeforeDrawEvent(Sender: TObject; AEntity: TEntity;
  var CanDraw: Boolean);
begin
  AEntity.Color:=0;
end;

procedure TFgassicMain.EntityAfterDrawEvent(Sender: TObject; AEntity: TEntity);
begin

end;

procedure TFgassicMain.FormCreate(Sender: TObject);
begin
  AssiDrawControl:= TAssiDrawControl.Create(FgassicMain);
  AssiDrawControl.Parent:=self.pnlBox;
  {$IFNDEF FPC}
  AssiDrawControl.OnSelectListChange:=ChangeSelectList;
  {$ELSE}
  AssiDrawControl.OnSelectListChange:=@ChangeSelectList;
  AssiDrawControl.OnEntityAfterDrawEvent:=@EntityAfterDrawEvent;
  AssiDrawControl.OnEntityBeforeDrawEvent:=@EntityBeforeDrawEvent;
  {$ENDIF}
  AssiDrawControl.Top:=0;
  AssiDrawControl.Left:=0;
  AssiDrawControl.Width:=pnlBox.Width;
  AssiDrawControl.Height:=pnlBox.Height;
  AssiDrawControl.Align:=alClient;
  AssiDrawControl.Show;
  AssiDrawControl.SetDefaultSettings;
end;

procedure TFgassicMain.btnEllipseClick(Sender: TObject);
var
  x:TEntity;
begin
  x:=TGraphicEllipse.Create;
  x.AddVertex(15,15,0);
  //x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
  //x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
  TGraphicEllipse(x).AxleY:=10;
  TGraphicEllipse(x).AxleX:=20;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  AssiDrawControl.AddMessageToUser('Эллипс');
end;

procedure TFgassicMain.btnLineClick(Sender: TObject);
var
  x:TEntity;
begin
  x:=TGraphicPolyline.Create;
  x.AddVertex(10,10,0);
  x.AddVertex(40,10,0);
  x.Color:=gaYellow;
  x.LineWeight:=gaLnWt030;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  x:=TGraphicPolyline.Create;
  x.AddVertex(10,20,0);
  x.AddVertex(40,20,0);
  x.Color:=gaCyan;
  x.LineWeight:=gaLnWt050;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  x:=TGraphicPolyline.Create;
  x.AddVertex(10,30,0);
  x.AddVertex(40,30,0);
  x.Color:=gaMagenta;
  x.LineWeight:=gaLnWt100;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  x:=TGraphicPolyline.Create;
  x.AddVertex(10,40,0);
  x.AddVertex(40,40,0);
  x.Color:=gaWhite;
  x.LineWeight:=gaLnWt200;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  AssiDrawControl.AddMessageToUser('Линия');
end;

procedure TFgassicMain.btnPointClick(Sender: TObject);
var
  x:TEntity;
begin
  x:=TGraphicpoint.Create;
  x.AddVertex(30,25,0);
  x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
  x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  AssiDrawControl.AddMessageToUser('Точка');
end;

procedure TFgassicMain.btnPolylineClick(Sender: TObject);
var
  x:TEntity;
begin
  x:=TGraphicPolyline.Create;
  x.AddVertex(10,10,0);
  x.AddVertex(160,10,0);
  x.AddVertex(160,26,0);
  x.AddVertex(10,26,0);
  //x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
  //x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  AssiDrawControl.AddMessageToUser('Полилиния');
end;

procedure TFgassicMain.btnDeselectAllClick(Sender: TObject);
begin
  AssiDrawControl.ActiveDocument.DeselectAll;
  AssiDrawControl.AddMessageToUser('Выполнено');
end;

procedure TFgassicMain.btnCircleClick(Sender: TObject);
var
  x:TEntity;
begin
  x:=TGraphicCircle.Create;
  x.AddVertex(30,25,0);
  TGraphicCircle(x).Radius:=16;
  x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
  x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  AssiDrawControl.AddMessageToUser('Круг');
end;

procedure TFgassicMain.btnClearFrameClick(Sender: TObject);
begin
  AssiDrawControl.FrameViewModeClear;
end;

procedure TFgassicMain.btnArcClick(Sender: TObject);
var
  x:TEntity;
begin
  x:=TGraphicArc.Create;
  x.AddVertex(46,46,0);
  x.AddVertex(46,30,0);
  x.AddVertex(62,46,0);
  TGraphicArc(x).Radius:=16;
  x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
  x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  AssiDrawControl.AddMessageToUser('Круг');
end;

procedure TFgassicMain.btnAddFrameClick(Sender: TObject);
begin
  AssiDrawControl.FrameViewModeSet('Эллипс',clBlue);
end;

procedure TFgassicMain.btnBlockCreateClick(Sender: TObject);
var
  x:TEntity;
  BlockItem:TBlockItem;
begin
   BlockItem:=TBlockItem.Create;
   BlockItem.Name:='Test';
   AssiDrawControl.ActiveDocument.Blocks.Add(BlockItem);

   x:=TGraphicPolyline.Create;
   x.AddVertex(1,1,0);
   x.AddVertex(1,10,0);
   x.AddVertex(10,10,0);
   x.AddVertex(10,1,0);
   x.AddVertex(1,1,0);
   x.Color:=gaByBlock;
   x.LineWeight:=gaLnWtByBlock;
   BlockItem.Objects.Add(x);
   x.Created;

   x:=TGraphicCircle.Create;
   x.AddVertex(10,10,0);
   TGraphicCircle(x).Radius:=16;
   x.Color:=gaByBlock;
   x.LineWeight:=gaLnWtByBlock;
   BlockItem.Objects.Add(x);
   x.Created;

   x:=TGraphicText.Create;
   x.AddVertex(15,5,0);
   TGraphicText(x).Align:=gaAttachmentPointMiddleCenter;
   TGraphicText(x).Color:=gaByBlock;
   x.LineWeight:=gaLnWtByBlock;
   TGraphicText(x).FontSize:=5;
   TGraphicText(x).FontStyle:=[fsUnderline];
   TGraphicText(x).FontName:='Arial';
   TGraphicText(x).Width:=0;
   TGraphicText(x).Height:=0;
   TGraphicText(x).Text:='I am Block!';
   BlockItem.Objects.Add(x);
   x.Created;
end;

procedure TFgassicMain.btnBlockInsert1Click(Sender: TObject);
var
  x:TEntity;
  i:integer;
begin
   x:=TGraphicBlock.Create;
   x.AddVertex(20,20,0);
   TGraphicBlock(x).BlockID:='Test2';
   TGraphicBlock(x).scale:=strtofloat(edtscale.Text);
   x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
   x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
   AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
   x.Created;
end;

procedure TFgassicMain.btnBlockInsertClick(Sender: TObject);
var
  x:TEntity;
  i:integer;
begin
   x:=TGraphicBlock.Create;
   x.AddVertex(20,20,0);
   TGraphicBlock(x).BlockID:='Test';
   TGraphicBlock(x).scale:=strtofloat(edtscale.Text);
   x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
   x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
   AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
   x.Created;
end;

procedure TFgassicMain.btnZoomToFitClick(Sender: TObject);
begin
  AssiDrawControl.ActiveDocument.ZoomToFit;
end;

procedure TFgassicMain.btnImportBlockClick(Sender: TObject);
var
 AutoCAD: OleVariant;
 BlockName:string;
 i:integer;
begin
 try
 //Если AutoCAD запущен, то подсоединяемся к нему
 AutoCAD := GetActiveOleObject('AutoCAD.Application');
 BlockName:=edtBlockName.Text;
 //if Item.ObjectName = 'AcDbBlockReference' then // если попался блок
 for i := 0 to AutoCAD.ActiveDocument.Blocks.Count - 1 do
 begin
      if AutoCAD.ActiveDocument.Blocks.Item(i).Name=BlockName then
      begin
        ///AutoCAD.ActiveDocument.Blocks.Item(i).ModelSpace...;
        break;
      end;
 end;
 finally

 end;
end;

procedure TFgassicMain.btnTextClick(Sender: TObject);
var
  x:TEntity;
begin
  x:=TGraphicText.Create;
  x.AddVertex(10,10,0);
  TGraphicText(x).Align:=gaAttachmentPointBottomLeft;
  TGraphicText(x).Color:=AssiDrawControl.ActiveDocument.DefaultColor;
  x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
  TGraphicText(x).FontSize:=10;
  TGraphicText(x).FontStyle:=[fsUnderline];
  TGraphicText(x).FontName:='Arial';
  TGraphicText(x).Width:=150;
  TGraphicText(x).Height:=16;
  TGraphicText(x).Text:='TGraphicText Hello word';
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  x:=TGraphicText.Create;
  x.AddVertex(35,35,0);
  TGraphicText(x).Align:=gaAttachmentPointBottomLeft;
  TGraphicText(x).Color:=AssiDrawControl.ActiveDocument.DefaultColor;
  x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
  TGraphicText(x).FontSize:=10;
  TGraphicText(x).FontStyle:=[fsUnderline];
  TGraphicText(x).FontName:='Arial';
  TGraphicText(x).Width:=150;
  TGraphicText(x).Height:=16;
  TGraphicText(x).Rotate:=45;
  TGraphicText(x).Text:='TGraphicText Hello word';
  AssiDrawControl.ActiveDocument.ModelSpace.Objects.Add(x);
  x.Created;

  AssiDrawControl.AddMessageToUser('Текст');
end;

procedure TFgassicMain.btnBlockCreate1Click(Sender: TObject);
var
  x:TEntity;
  BlockItem:TBlockItem;
begin
   BlockItem:=TBlockItem.Create;
   BlockItem.Name:='Test2';
   AssiDrawControl.ActiveDocument.Blocks.Add(BlockItem);

   x:=TGraphicBlock.Create;
   x.AddVertex(0,0,0);
   TGraphicBlock(x).BlockID:='Test';
   TGraphicBlock(x).scale:=strtofloat(edtscale.Text);
   x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
   x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
   BlockItem.Objects.Add(x);
   x.Created;

   x:=TGraphicBlock.Create;
   x.AddVertex(50,0,0);
   TGraphicBlock(x).BlockID:='Test';
   TGraphicBlock(x).scale:=0.5;
   x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
   x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
   BlockItem.Objects.Add(x);
   x.Created;

   x:=TGraphicBlock.Create;
   x.AddVertex(100,0,0);
   TGraphicBlock(x).BlockID:='Test';
   TGraphicBlock(x).scale:=0.25;
   x.Color:=AssiDrawControl.ActiveDocument.DefaultColor;
   x.LineWeight:=AssiDrawControl.ActiveDocument.DefaultLineWeight;
   BlockItem.Objects.Add(x);
   x.Created;

   x:=TGraphicText.Create;
   x.AddVertex(15,15,0);
   TGraphicText(x).Align:=gaAttachmentPointMiddleCenter;
   TGraphicText(x).Color:=gaByBlock;
   x.LineWeight:=gaLnWtByBlock;
   TGraphicText(x).FontSize:=5;
   TGraphicText(x).FontStyle:=[fsUnderline];
   TGraphicText(x).FontName:='Arial';
   TGraphicText(x).Width:=0;
   TGraphicText(x).Height:=0;
   TGraphicText(x).Text:='I am second Block!';
   BlockItem.Objects.Add(x);
   x.Created;
end;

procedure TFgassicMain.cbAABASEPOINTChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
  AssiDrawControl.SelectStyle:=AssiDrawControl.SelectStyle+[aasoBASEPOINT]
  else
  AssiDrawControl.SelectStyle:=AssiDrawControl.SelectStyle-[aasoBASEPOINT];
end;

procedure TFgassicMain.cbAABORDERChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
  AssiDrawControl.SelectStyle:=AssiDrawControl.SelectStyle+[aasoBORDER]
  else
  AssiDrawControl.SelectStyle:=AssiDrawControl.SelectStyle-[aasoBORDER];
end;

procedure TFgassicMain.cbAAINSIDEChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
  AssiDrawControl.SelectStyle:=AssiDrawControl.SelectStyle+[aasoINSIDE]
  else
  AssiDrawControl.SelectStyle:=AssiDrawControl.SelectStyle-[aasoINSIDE];
end;

procedure TFgassicMain.cbAAOUTSIDEChange(Sender: TObject);
begin

end;

procedure TFgassicMain.cbAAVERTEXChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
  AssiDrawControl.SelectStyle:=AssiDrawControl.SelectStyle+[aasoVERTEX]
  else
  AssiDrawControl.SelectStyle:=AssiDrawControl.SelectStyle-[aasoVERTEX];
end;

procedure TFgassicMain.cbColorSelectChange(Sender: TObject);
begin
 case cbColorSelect.ItemIndex of
 0:AssiDrawControl.ActiveDocument.DefaultColor:=gaByBlock;
 1:AssiDrawControl.ActiveDocument.DefaultColor:=gaByLayer;
 2:AssiDrawControl.ActiveDocument.DefaultColor:=gaRed;
 3:AssiDrawControl.ActiveDocument.DefaultColor:=gaYellow;
 4:AssiDrawControl.ActiveDocument.DefaultColor:=gaGreen;
 5:AssiDrawControl.ActiveDocument.DefaultColor:=gaCyan;
 6:AssiDrawControl.ActiveDocument.DefaultColor:=gaBlue;
 7:AssiDrawControl.ActiveDocument.DefaultColor:=gaMagenta;
 8:AssiDrawControl.ActiveDocument.DefaultColor:=gaWhite;
 end;
end;

procedure TFgassicMain.cbLineWeightChange(Sender: TObject);
begin
  case cbLineWeight.ItemIndex of
0:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWtByLwDefault;
1:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWtByLayer;
2:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWtByBlock;
3:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt000;
4:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt005;
5:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt009;
6:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt013;
7:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt015;
8:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt018;
9:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt020;
10:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt025;
11:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt030;
12:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt035;
13:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt040;
14:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt050;
15:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt053;
16:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt060;
17:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt070;
18:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt080;
19:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt090;
20:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt100;
21:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt106;
22:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt120;
23:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt140;
24:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt158;
25:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt200;
26:AssiDrawControl.ActiveDocument.DefaultLineWeight:=gaLnWt211;
end;
end;

procedure TFgassicMain.cbDevModeChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
      AssiDrawControl.DevelopMode:=true
  else
      AssiDrawControl.DevelopMode:=false;
end;

procedure TFgassicMain.cbAxesChange(Sender: TObject);
begin
    if (Sender as TCheckBox).Checked then
      AssiDrawControl.ShowAxes:=true
  else
      AssiDrawControl.ShowAxes:=false;
end;

procedure TFgassicMain.cbReadOnlyChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
      AssiDrawControl.ActiveDocument.EditMode:=eemReadOnly
  else
      AssiDrawControl.ActiveDocument.EditMode:=eemCanAll;
end;

procedure TFgassicMain.edtscaleKeyPress(Sender: TObject; var Key: char);
begin
  if (Key in ['.']) then Key:=char(',');
end;

procedure TFgassicMain.FormDestroy(Sender: TObject);
begin
  AssiDrawControl.free;
end;

end.


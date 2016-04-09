program gadc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, u_form_main, u_gassi_const, u_gassi_drawcontrol,
  u_gassi_logicaldraw, u_gassi_visualobjects, u_gassi_geometry;

{$R *.res}

begin
  Application.Title:='gassic';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFgassicMain, FgassicMain);
  Application.Run;
end.


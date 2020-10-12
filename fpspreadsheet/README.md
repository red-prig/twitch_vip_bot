fpspreadsheet
=============
The fpSpreadsheet library offers a convenient way to generate and read 
spreadsheet documents in various formats:
- Excel 2.x .xls
- Excel 5.0/Excel 95 .xls
- Excel 8.0 (Excel 97-XP) .xls
- Excel XML (Excel 2003) .xml
- Microsoft OOXML .xlsx
- LibreOffice/OpenOffice OpenDocument .ods
- Comma-separated text files 
- html files
- wikimedia wikitable formats
- the user can register his own readers/writers for other formats. An example
  is shown in the "spready" application for the .slk format.
  ("spready" is in the applications folder of CCR).

The library is written in a very flexible manner, capable of being extended to 
support any number of formats easily.


Installation
============
If you only need non-GUI components: in Lazarus: 
- Package/Open Package File 
- select laz_fpspreadsheet.lpk from folder "source"
- click Compile. 
Now the package is known to Lazarus (and should e.g. show up in Package/Package Links). 
Add it to your project like you add other packages.

If you also want GUI components (grid and chart): 
- Package/Open Package File
- select laz_fpspreadsheet_visual.lpk from the fpspreadsheet installation folder
- click Compile
- select laz_fpspreadsheet_visual_dsgn.lpk from the fpspreadsheet installation folder 
- then click Use, Install and follow the prompts to rebuild Lazarus with the new package.
Drop needed grid/chart components on your forms as usual

A third package contains code for export databases to spreadsheet files
- Package/Open Package File
- Select laz_fpspreadsheetexport_visual.lpk from the folder "source"
- Click Compile
- Then click Use, Install and follow the prompts to rebuild Lazarus with the new package.
Drop export component on your form as usual.

If you want to access encrypted spreadsheet files the following package must be made known 
to the IDE:
- Package/Open Package File
- select laz_fpspreadsheet_crypo.lpk
- Click Compile
- NOTE: this package has a dependence on the package DCPCrypt which must be compile first.
  (you can use the Online-Package-Manager to download this package).


License
=======
LGPL with static linking exception. This is the same license as is used in the Lazarus Component Library. 


More information
================
- FPSpreadsheet documentation in fpspreadsheet.chm (open e.g. with Lazarus lhelp)
- The fpspreadsheet article on the Lazarus wiki with lots of example:
  http://wiki.lazarus.freepascal.org/FPSpreadsheet
- The demo programs in the examples folder

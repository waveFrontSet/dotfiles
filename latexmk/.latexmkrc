# Using lualatex with synctex as the standard pdflatex command with latexmk.
$pdflatex = 'pdflatex -synctex=1 -interaction=nonstopmode %O %S';
$pdf_mode = 1;
$postscript_mode = $dvi_mode = 0;

# Using pvc.
$preview_continuous_mode = 1;

# Selects pdf_previewer based on the OS: open for OS X, evince for everything
# else (Linux on my university computer, that is).
use English qw' -no_match_vars ';
if ($OSNAME eq "darwin") {
    $pdf_previewer = "open %O %S";
} else {
    $pdf_previewer = "start evince %O %S";
}

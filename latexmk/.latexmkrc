# Using lualatex with synctex as the standard pdflatex command with latexmk.
$pdflatex = 'pdflatex -synctex=1 -interaction=nonstopmode %O %S';
$pdf_mode = 1;
$postscript_mode = $dvi_mode = 0;

# Using pvc.
$preview_continuous_mode = 1;

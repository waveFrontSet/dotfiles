# Using lualatex with synctex as the standard pdflatex command with latexmk.
$pdflatex = 'lualatex -synctex=1 %O %S';
$pdf_mode = 1;
$postscript_mode = $dvi_mode = 0;

# Using pvc.
$preview_continuous_mode = 1;

# TODO: How can I select a viewer based on my OS?
$pdf_previewer = "start okular %O %S"

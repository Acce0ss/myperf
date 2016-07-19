# myperf
A PCB script generator that shamelessly imitates perf+.

Creates a Cadsoft Eagle script that draws a perf board
with labeled matrix strips, given the board dimensions.

The perf vias are separated by 0.1 inches, so standard pin
header and PTH packages should fit without problems.

compilation:
ghc genperf.hs

usage:
./genperf <width(mm)> <height(mm)>

example:
./genperf 50 50

creates a 50mm x 50 mm board, with 17 columns and rows.
Open Eagle and run the script, so you may do fine tuning
by hand and generate manufacturing files.

The gerber files generated from the above example results
can be found under gerbers/ folder.

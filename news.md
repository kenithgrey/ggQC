
ggQC Version 0.0.2
==================

New Functions
-------------

-   **QC\_Violations()**: NEW function that calculates QC violations for Shewart Quality Control Charts.

-   **stat\_qc\_violations()**: NEW ggplot stat function to graphically view QC violations in a data set.

Updated Functions
-----------------

-   **QC\_Lines()**: Updated function output to include the calculated sigma value.

Resolved Issues
---------------

-   Fixed (\#32): Observed that when n &gt; 100 I got a convergence error for D5. Unfortunately this causes problems with all the quailty control charts if n &gt; 100.

ggQC Version 0.0.1
==================

New Functions
-------------

-   **QC\_Lines()** : Output quality control chart data in table format
-   **stat\_QC()** : ggplot stat to generate control charts supported methods
-   **stat\_mR()**: ggplot stat to generate mR chart.
-   **stat\_QC\_labels()**: ggplot stat to write text labels on control chart center line and limits.
-   30 plus functions to calculate specific control chart constants. (recommend using QC\_Lines())
